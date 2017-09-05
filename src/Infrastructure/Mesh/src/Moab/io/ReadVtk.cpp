/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**
 * \class ReadVtk
 * \brief VTK reader from Mesquite
 * \author Jason Kraftcheck
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ReadVtk.hpp"
#include "moab/Range.hpp"
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "FileTokenizer.hpp"
#include "moab/VtkUtil.hpp"

#define MB_VTK_MATERIAL_SETS
#ifdef MB_VTK_MATERIAL_SETS
#include "MBTagConventions.hpp"
#include <map>

namespace moab {

class Hash
{
public:
  unsigned long value;

  Hash()
  {
    this->value = 5381L;
  }

  Hash(const unsigned char* bytes, size_t len)
  { // djb2, a hash by dan bernstein presented on comp.lang.c for hashing strings.
    this->value = 5381L;
    for ( ; len; --len, ++bytes)
      this->value = this->value * 33 + (*bytes);
  }

  Hash(bool duh)
  {
    this->value = duh; // Hashing a single bit with a long is stupid but easy.
  }

  Hash(const Hash& src)
  {
    this->value = src.value;
  }

  Hash& operator=(const Hash& src)
  {
    this->value = src.value;
    return *this;
  }

  bool operator<(const Hash& other) const
  {
    return this->value < other.value;
  }
};

// Pass this class opaque data + a handle and it adds the handle to a set
// whose opaque data has the same hash. If no set exists for the hash,
// it creates one. Each set is tagged with the opaque data.
// When entities with different opaque data have the same hash, they
// will be placed into the same set.
// There will be no collisions when the opaque data is shorter than an
// unsigned long, and this is really the only case we need to support.
// The rest is bonus. Hash does quite well with strings, even those
// with identical prefixes.
class Modulator : public std::map<Hash, EntityHandle>
{
public:
  Modulator(Interface* iface, std::string tag_name, DataType mb_type, size_t sz, size_t per_elem)
  {
    this->mesh = iface;
    std::vector<unsigned char> default_val;
    default_val.resize(sz * per_elem);
    ErrorCode rval;
    rval = this->mesh->tag_get_handle(
        tag_name.c_str(), per_elem, mb_type, this->tag,
        MB_TAG_SPARSE | MB_TAG_BYTES | MB_TAG_CREAT,
        &default_val[0]
        );
    MB_CHK_SET_ERR_RET(rval, "can't tag_get_handle");
  }

  void add_entity(EntityHandle ent, const unsigned char* bytes, size_t len)
  {
    Hash h(bytes, len);
    EntityHandle mset = this->congruence_class(h, bytes);
    ErrorCode rval;
    rval = this->mesh->add_entities(mset, &ent, 1);MB_CHK_SET_ERR_RET(rval, "Failed to add entities to mesh");
  }

  void add_entities(Range& range, const unsigned char* bytes, size_t bytes_per_ent)
  {
    for (Range::iterator it = range.begin(); it != range.end(); ++it, bytes += bytes_per_ent) {
      Hash h(bytes, bytes_per_ent);
      EntityHandle mset = this->congruence_class(h, bytes);
      ErrorCode rval;
      rval = this->mesh->add_entities(mset, &*it, 1);MB_CHK_SET_ERR_RET(rval, "Failed to add entities to mesh");
    }
  }

  EntityHandle congruence_class(const Hash& h, const void* tag_data)
  {
    std::map<Hash, EntityHandle>::iterator it = this->find(h);
    if (it == this->end()) {
      EntityHandle mset;
      Range preexist;
      ErrorCode rval;
      rval = this->mesh->get_entities_by_type_and_tag(0, MBENTITYSET, &this->tag, &tag_data, 1, preexist);MB_CHK_SET_ERR_RET_VAL(rval, "Failed to get entities by type and tag", (EntityHandle) 0);
      if (preexist.size()) {
        mset = *preexist.begin();
      }
      else {
        rval = this->mesh->create_meshset(MESHSET_SET, mset);MB_CHK_SET_ERR_RET_VAL(rval, "Failed to create mesh set", (EntityHandle) 0);
        rval = this->mesh->tag_set_data(this->tag, &mset, 1, tag_data);MB_CHK_SET_ERR_RET_VAL(rval, "Failed to set tag data", (EntityHandle) 0);
      }
      (*this)[h] = mset;
      return mset;
    }
    return it->second;
  }

  Interface* mesh;
  Tag tag;
};
#endif // MB_VTK_MATERIAL_SETS

ReaderIface* ReadVtk::factory(Interface* iface)
{
  return new ReadVtk(iface);
}

ReadVtk::ReadVtk(Interface* impl)
  : mdbImpl(impl), mPartitionTagName(MATERIAL_SET_TAG_NAME)
{
  mdbImpl->query_interface(readMeshIface);
}

ReadVtk::~ReadVtk()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

const char* const vtk_type_names[] = {"bit",
                                      "char",
                                      "unsigned_char",
                                      "short",
                                      "unsigned_short",
                                      "int",
                                      "unsigned_int",
                                      "long",
                                      "unsigned_long",
                                      "float",
                                      "double",
                                      "vtkIdType",
                                      0};

ErrorCode ReadVtk::read_tag_values(const char* /* file_name */,
                                   const char* /* tag_name */,
                                   const FileOptions& /* opts */,
                                   std::vector<int>& /* tag_values_out */,
                                   const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadVtk::load_file(const char *filename,
                             const EntityHandle* /* file_set */,
                             const FileOptions& opts,
                             const ReaderIface::SubsetList* subset_list,
                             const Tag* file_id_tag)
{
  ErrorCode result;

  int major, minor;
  char vendor_string[257];
  std::vector<Range> element_list;
  Range vertices;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for VTK");
  }

  // Does the caller want a field to be used for partitioning the entities?
  // If not, we'll assume any scalar integer field named MATERIAL_SET specifies partitions.
  std::string partition_tag_name;
  result = opts.get_option("PARTITION", partition_tag_name);
  if (result == MB_SUCCESS)
    mPartitionTagName = partition_tag_name;

  FILE* file = fopen(filename, "r");
  if (!file)
    return MB_FILE_DOES_NOT_EXIST;

  // Read file header

  if (!fgets(vendor_string, sizeof(vendor_string), file)) {
    fclose(file);
    return MB_FAILURE;
  }

  if (!strchr(vendor_string, '\n') ||
      2 != sscanf(vendor_string, "# vtk DataFile Version %d.%d", &major, &minor)) {
    fclose(file);
    return MB_FAILURE;
  }

  if (!fgets(vendor_string, sizeof(vendor_string), file)) {
    fclose(file);
    return MB_FAILURE;
  }

  // VTK spec says this should not exceed 256 chars.
  if (!strchr(vendor_string, '\n')) {
    fclose(file);
    MB_SET_ERR(MB_FAILURE, "Vendor string (line 2) exceeds 256 characters");
  }

  // Check file type

  FileTokenizer tokens(file, readMeshIface);
  const char* const file_type_names[] = {"ASCII", "BINARY", 0};
  int filetype = tokens.match_token(file_type_names);
  switch (filetype) {
    case 2:  // BINARY
      MB_SET_ERR(MB_FAILURE, "Cannot read BINARY VTK files");
    default: // ERROR 
      return MB_FAILURE;
    case 1:  // ASCII
      break;
  }

  // Read the mesh
  if (!tokens.match_token("DATASET"))
    return MB_FAILURE;
  result = vtk_read_dataset(tokens, vertices, element_list);
  if (MB_SUCCESS != result) 
    return result;

  if (file_id_tag) {
    result = store_file_ids(*file_id_tag, vertices, element_list);
    if (MB_SUCCESS != result)
      return result;
  }

  // Count the number of elements read
  long elem_count = 0;
  for (std::vector<Range>::iterator it = element_list.begin(); it != element_list.end(); ++it)
    elem_count += it->size();

  // Read attribute data until end of file.
  const char* const block_type_names[] = {"POINT_DATA", "CELL_DATA", 0};
  std::vector<Range> vertex_list(1);
  vertex_list[0] = vertices;
  int blocktype = 0;
  while (!tokens.eof()) {
    // Get POINT_DATA or CELL_DATA
    int new_block_type = tokens.match_token(block_type_names, false);
    if (tokens.eof())
      break;

    if (!new_block_type) {
      // If next token was neither POINT_DATA nor CELL_DATA,
      // then there's another attribute under the current one.
      if (blocktype)
        tokens.unget_token();
      else
        break;
    }
    else {
      blocktype = new_block_type;
      long count;
      if (!tokens.get_long_ints(1, &count))
        return MB_FAILURE;

      if (blocktype == 1 && (unsigned long)count != vertices.size()) {
        MB_SET_ERR(MB_FAILURE, "Count inconsistent with number of vertices at line " << tokens.line_number());
      }
      else if (blocktype == 2 && count != elem_count) {
        MB_SET_ERR(MB_FAILURE, "Count inconsistent with number of elements at line " << tokens.line_number());
      }
    }

    if (blocktype == 1)
      result = vtk_read_attrib_data(tokens, vertex_list);
    else
      result = vtk_read_attrib_data(tokens, element_list);

    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::allocate_vertices(long num_verts,
                                     EntityHandle& start_handle_out,
                                     double*& x_coord_array_out,
                                     double*& y_coord_array_out,
                                     double*& z_coord_array_out)
{
  ErrorCode result;

  // Create vertices
  std::vector<double*> arrays;
  start_handle_out = 0;
  result = readMeshIface->get_node_coords(3, num_verts, MB_START_ID,
                                          start_handle_out, arrays);
  if (MB_SUCCESS != result)
    return result;

  x_coord_array_out = arrays[0];
  y_coord_array_out = arrays[1];
  z_coord_array_out = arrays[2];

  return MB_SUCCESS;
}

ErrorCode ReadVtk::read_vertices(FileTokenizer& tokens,
                                 long num_verts,
                                 EntityHandle& start_handle_out)
{
  ErrorCode result;
  double *x, *y, *z;

  result = allocate_vertices(num_verts, start_handle_out, x, y, z);
  if (MB_SUCCESS != result)
    return result;

  // Read vertex coordinates
  for (long vtx = 0; vtx < num_verts; ++vtx) {
    if (!tokens.get_doubles(1, x++) ||
        !tokens.get_doubles(1, y++) ||
        !tokens.get_doubles(1, z++))
      return MB_FAILURE;
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::allocate_elements(long num_elements,
                                     int vert_per_element,
                                     EntityType type,
                                     EntityHandle& start_handle_out,
                                     EntityHandle*& conn_array_out,
                                     std::vector<Range>& append_to_this)
{
  ErrorCode result;

  start_handle_out = 0;
  result = readMeshIface->get_element_connect(num_elements,
                                              vert_per_element,
                                              type,
                                              MB_START_ID,
                                              start_handle_out,
                                              conn_array_out);
  if (MB_SUCCESS != result)
    return result;

  Range range(start_handle_out, start_handle_out + num_elements - 1);
  append_to_this.push_back(range);
  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_read_dataset(FileTokenizer& tokens,
                                    Range& vertex_list,
                                    std::vector<Range>& element_list)
{
  const char* const data_type_names[] = {"STRUCTURED_POINTS",
                                         "STRUCTURED_GRID",
                                         "UNSTRUCTURED_GRID",
                                         "POLYDATA",
                                         "RECTILINEAR_GRID",
                                         "FIELD",
                                         0};
  int datatype = tokens.match_token(data_type_names);
  switch (datatype) {
    case 1:  return vtk_read_structured_points(tokens, vertex_list, element_list);
    case 2:  return vtk_read_structured_grid  (tokens, vertex_list, element_list);
    case 3:  return vtk_read_unstructured_grid(tokens, vertex_list, element_list);
    case 4:  return vtk_read_polydata         (tokens, vertex_list, element_list);
    case 5:  return vtk_read_rectilinear_grid (tokens, vertex_list, element_list);
    case 6:  return vtk_read_field            (tokens);
    default: return MB_FAILURE;
  }
}

ErrorCode ReadVtk::vtk_read_structured_points(FileTokenizer& tokens,
                                              Range& vertex_list,
                                              std::vector<Range>& elem_list)
{
  long i, j, k;
  long dims[3];
  double origin[3], space[3];
  ErrorCode result;

  if (!tokens.match_token("DIMENSIONS") ||
      !tokens.get_long_ints(3, dims) ||
      !tokens.get_newline())
    return MB_FAILURE; 

  if (dims[0] < 1 || dims[1] < 1 || dims[2] < 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid dimension at line " << tokens.line_number());
  }

  if (!tokens.match_token("ORIGIN")  ||
      !tokens.get_doubles(3, origin) ||
      !tokens.get_newline())
    return MB_FAILURE;

  const char* const spacing_names[] = {"SPACING", "ASPECT_RATIO", 0};
  if (!tokens.match_token(spacing_names) ||
      !tokens.get_doubles(3, space)      ||
      !tokens.get_newline())
    return MB_FAILURE;

  // Create vertices
  double *x, *y, *z;
  EntityHandle start_handle = 0;
  long num_verts = dims[0]*dims[1]*dims[2];
  result = allocate_vertices(num_verts, start_handle, x, y, z);
  if (MB_SUCCESS != result)
    return result;
  vertex_list.insert(start_handle, start_handle + num_verts - 1);
  
  for (k = 0; k < dims[2]; ++k)
    for (j = 0; j < dims[1]; ++j)
      for (i = 0; i < dims[0]; ++i) {
        *x = origin[0] + i*space[0]; ++x;
        *y = origin[1] + j*space[1]; ++y;
        *z = origin[2] + k*space[2]; ++z;
      }

  return vtk_create_structured_elems(dims, start_handle, elem_list);
}

ErrorCode ReadVtk::vtk_read_structured_grid(FileTokenizer& tokens,
                                            Range& vertex_list,
                                            std::vector<Range>& elem_list)
{
  long num_verts, dims[3];
  ErrorCode result;

  if (!tokens.match_token("DIMENSIONS") ||
      !tokens.get_long_ints(3, dims) ||
      !tokens.get_newline())
    return MB_FAILURE;

  if (dims[0] < 1 || dims[1] < 1 || dims[2] < 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid dimension at line " << tokens.line_number());
  }

  if (!tokens.match_token("POINTS") ||
      !tokens.get_long_ints(1, &num_verts) ||
      !tokens.match_token(vtk_type_names) ||
      !tokens.get_newline())
    return MB_FAILURE;

  if (num_verts != (dims[0] * dims[1] * dims[2])) {
    MB_SET_ERR(MB_FAILURE, "Point count not consistent with dimensions at line " << tokens.line_number());
  }

  // Create and read vertices
  EntityHandle start_handle = 0;
  result = read_vertices(tokens, num_verts, start_handle);
  if (MB_SUCCESS != result)
    return result;
  vertex_list.insert(start_handle, start_handle + num_verts - 1);

  return vtk_create_structured_elems(dims, start_handle, elem_list);
}

ErrorCode ReadVtk::vtk_read_rectilinear_grid(FileTokenizer& tokens,
                                             Range& vertex_list,
                                             std::vector<Range>& elem_list)
{
  int i, j, k;
  long dims[3];
  const char* labels[] = {"X_COORDINATES", "Y_COORDINATES", "Z_COORDINATES"};
  std::vector<double> coords[3];
  ErrorCode result;

  if (!tokens.match_token("DIMENSIONS") ||
      !tokens.get_long_ints(3, dims) ||
      !tokens.get_newline())
    return MB_FAILURE;

  if (dims[0] < 1 || dims[1] < 1 || dims[2] < 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid dimension at line " << tokens.line_number());
  }

  for (i = 0; i < 3; i++) {
    long count;
    if (!tokens.match_token(labels[i]) ||
        !tokens.get_long_ints(1, &count) ||
        !tokens.match_token(vtk_type_names))
      return MB_FAILURE;

    if (count != dims[i]) {
      MB_SET_ERR(MB_FAILURE, "Coordinate count inconsistent with dimensions at line " << tokens.line_number());
    }

    coords[i].resize(count);
    if (!tokens.get_doubles(count, &coords[i][0]))
      return MB_FAILURE;
  }

  // Create vertices
  double *x, *y, *z;
  EntityHandle start_handle = 0;
  long num_verts = dims[0]*dims[1]*dims[2];
  result = allocate_vertices(num_verts, start_handle, x, y, z);
  if (MB_SUCCESS != result)
    return result;
  vertex_list.insert(start_handle, start_handle + num_verts - 1);

  // Calculate vertex coordinates
  for (k = 0; k < dims[2]; ++k)
    for (j = 0; j < dims[1]; ++j)
      for (i = 0; i < dims[0]; ++i) {
        *x = coords[0][i]; ++x;
        *y = coords[1][j]; ++y;
        *z = coords[2][k]; ++z;
      }

  return vtk_create_structured_elems(dims, start_handle, elem_list);
}

ErrorCode ReadVtk::vtk_read_polydata(FileTokenizer& tokens,
                                     Range& vertex_list,
                                     std::vector<Range>& elem_list)
{
  ErrorCode result;
  long num_verts;
  const char* const poly_data_names[] = {"VERTICES",
                                         "LINES",
                                         "POLYGONS",
                                         "TRIANGLE_STRIPS",
                                          0};

  if (!tokens.match_token("POINTS") ||
      !tokens.get_long_ints(1, &num_verts) ||
      !tokens.match_token(vtk_type_names) ||
      !tokens.get_newline())
    return MB_FAILURE;

  if (num_verts < 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid point count at line " << tokens.line_number());
  }

  // Create vertices and read coordinates
  EntityHandle start_handle = 0;
  result = read_vertices(tokens, num_verts, start_handle);
  if (MB_SUCCESS != result)
    return result;
  vertex_list.insert(start_handle, start_handle + num_verts - 1);

  int poly_type = tokens.match_token(poly_data_names);
  switch (poly_type) {
    case 0:
      result = MB_FAILURE;
      break;
    case 1:
      MB_SET_ERR(MB_FAILURE, "Vertex element type at line " << tokens.line_number());
      break;
    case 2:
      MB_SET_ERR(MB_FAILURE, "Unsupported type: polylines at line " << tokens.line_number());
      result = MB_FAILURE;
      break;
    case 3:
      result = vtk_read_polygons(tokens, start_handle, elem_list);
      break;
    case 4:
      MB_SET_ERR(MB_FAILURE, "Unsupported type: triangle strips at line " << tokens.line_number());
      result = MB_FAILURE;
      break;
  }

  return result;
}

ErrorCode ReadVtk::vtk_read_polygons(FileTokenizer& tokens,
                                     EntityHandle first_vtx,
                                     std::vector<Range>& elem_list)
{
  ErrorCode result;
  long size[2];

  if (!tokens.get_long_ints(2, size) ||
      !tokens.get_newline())
    return MB_FAILURE;

  const Range empty;
  std::vector<EntityHandle> conn_hdl;
  std::vector<long> conn_idx;
  EntityHandle first = 0, prev = 0, handle;
  for (int i = 0; i < size[0]; ++i) {
    long count;
    if (!tokens.get_long_ints(1, &count))
      return MB_FAILURE;
    conn_idx.resize(count);
    conn_hdl.resize(count);
    if (!tokens.get_long_ints(count, &conn_idx[0]))
      return MB_FAILURE;
    
    for (long j = 0; j < count; ++j)
      conn_hdl[j] = first_vtx + conn_idx[j];
    
    result = mdbImpl->create_element(MBPOLYGON, &conn_hdl[0], count, handle);
    if (MB_SUCCESS != result)
      return result;

    if (prev + 1 != handle) {
      if (first) { // True except for first iteration (first == 0)
        if (elem_list.empty() || first < elem_list.back().front()) // Only need new range if order would get mixed up, or we just began inserting
          elem_list.push_back(empty);
        elem_list.back().insert(first, prev);
      }
      first = handle;
    }
    prev = handle;
  }
  if (first) { // True unless no elements (size[0] == 0)
    if (elem_list.empty() || first < elem_list.back().front()) // Only need new range if order would get mixed up, or we just began inserting
      elem_list.push_back(empty);
    elem_list.back().insert(first, prev);
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_read_unstructured_grid(FileTokenizer& tokens,
                                              Range& vertex_list,
                                              std::vector<Range>& elem_list)
{
  ErrorCode result;
  long i, num_verts, num_elems[2];
  EntityHandle tmp_conn_list[27];

  // Poorly formatted VTK legacy format document seems to
  // lead many to think that a FIELD block can occur within
  // an UNSTRUCTURED_GRID dataset rather than as its own data
  // set. So allow for field data between other blocks of
  // data.

  const char* pts_str[] = {"FIELD", "POINTS", 0};
  while (1 == (i = tokens.match_token(pts_str))) {
    result = vtk_read_field(tokens);
    if (MB_SUCCESS != result)
      return result;
  }
  if (i != 2)
    return MB_FAILURE;

  if (!tokens.get_long_ints(1, &num_verts) ||
      !tokens.match_token(vtk_type_names) ||
      !tokens.get_newline())
    return MB_FAILURE;

  if (num_verts < 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid point count at line " << tokens.line_number());
  }

  // Create vertices and read coordinates
  EntityHandle first_vertex = 0;
  result = read_vertices(tokens, num_verts, first_vertex);
  if (MB_SUCCESS != result)
    return result;
  vertex_list.insert(first_vertex, first_vertex + num_verts - 1);

  const char* cell_str[] = {"FIELD", "CELLS", 0};
  while (1 == (i = tokens.match_token(cell_str))) {
    result = vtk_read_field(tokens);
    if (MB_SUCCESS != result)
      return result;
  }
  if (i != 2)
    return MB_FAILURE;

  if (!tokens.get_long_ints(2, num_elems) ||
      !tokens.get_newline())
    return MB_FAILURE;

  // Read element connectivity for all elements
  std::vector<long> connectivity(num_elems[1]);
  if (!tokens.get_long_ints(num_elems[1], &connectivity[0]))
    return MB_FAILURE;

  if (!tokens.match_token("CELL_TYPES") ||
      !tokens.get_long_ints(1, &num_elems[1]) ||
      !tokens.get_newline())
    return MB_FAILURE;

  // Read element types
  std::vector<long> types(num_elems[0]);
  if (!tokens.get_long_ints(num_elems[0], &types[0]))
    return MB_FAILURE;

  // Create elements in blocks of the same type
  // It is important to preserve the order in
  // which the elements were read for later reading
  // attribute data.
  long id = 0;
  std::vector<long>::iterator conn_iter = connectivity.begin();
  while (id < num_elems[0]) {
    unsigned vtk_type = types[id];
    if (vtk_type >= VtkUtil::numVtkElemType)
      return MB_FAILURE;

    EntityType type = VtkUtil::vtkElemTypes[vtk_type].mb_type;
    if (type == MBMAXTYPE) {
      MB_SET_ERR(MB_FAILURE, "Unsupported VTK element type: " << VtkUtil::vtkElemTypes[vtk_type].name << " (" << vtk_type << ")");
    }

    int num_vtx = *conn_iter;
    if ( type != MBPOLYGON && type != MBPOLYHEDRON && num_vtx != (int)VtkUtil::vtkElemTypes[vtk_type].num_nodes) {
      MB_SET_ERR(MB_FAILURE, "Cell " << id << " is of type '" << VtkUtil::vtkElemTypes[vtk_type].name << "' but has " << num_vtx << " vertices");
    }



    // Find any subsequent elements of the same type
    // if polyhedra, need to look at the number of faces to put in the same range
    std::vector<long>::iterator conn_iter2 = conn_iter + num_vtx + 1;
    long end_id = id + 1; 
    if (MBPOLYHEDRON != type)
    {
      while (end_id < num_elems[0] &&
             (unsigned)types[end_id] == vtk_type &&
             *conn_iter2 == num_vtx) {
        ++end_id;
        conn_iter2 += num_vtx + 1;
      }
    }
    else
    {
      // advance only if next is polyhedron too, and if number of faces is the same
      int num_faces = conn_iter[1];
      while (end_id < num_elems[0] &&
             (unsigned)types[end_id] == vtk_type &&
             conn_iter2[1] == num_faces) {
        ++end_id;
        conn_iter2 += num_vtx + 1;
      }
      // num_vtx becomes in this case num_faces
      num_vtx = num_faces; // for polyhedra, this is what we want
      // trigger vertex adjacency call
      Range firstFaces;
      mdbImpl->get_adjacencies(&first_vertex, 1, 2, false, firstFaces);
    }

    // Allocate element block
    long num_elem = end_id - id;
    EntityHandle start_handle = 0;
    EntityHandle* conn_array;

    // if type is MBVERTEX, skip, we will not create elements with one vertex
    if (MBVERTEX==type)
    {
      id+=num_elem;
      conn_iter+=2*num_elem;// skip 2 * number of 1-vertex elements
      continue;
    }

    result = allocate_elements(num_elem, num_vtx, type, start_handle,
                               conn_array, elem_list);
    if (MB_SUCCESS != result)
      return result;

    EntityHandle *conn_sav = conn_array;

    // Store element connectivity
    if (type != MBPOLYHEDRON)
    {
      for ( ; id < end_id; ++id) {
        if (conn_iter == connectivity.end()) {
          MB_SET_ERR(MB_FAILURE, "Connectivity data truncated at cell " << id);
        }
        // Make sure connectivity length is correct.
        if (*conn_iter != num_vtx) {
          MB_SET_ERR(MB_FAILURE, "Cell " << id << " is of type '" << VtkUtil::vtkElemTypes[vtk_type].name << "' but has " << num_vtx << " vertices");
        }
        ++conn_iter;

        for (i = 0; i < num_vtx; ++i, ++conn_iter) {
          if (conn_iter == connectivity.end()) {
            MB_SET_ERR(MB_FAILURE, "Connectivity data truncated at cell " << id);
          }

          conn_array[i] = *conn_iter + first_vertex;
        }

        const unsigned* order = VtkUtil::vtkElemTypes[vtk_type].node_order;
        if (order) {
          assert(num_vtx * sizeof(EntityHandle) <= sizeof(tmp_conn_list));
          memcpy(tmp_conn_list, conn_array, num_vtx * sizeof(EntityHandle));
          for (int j = 0; j < num_vtx; ++j)
            conn_array[order[j]] = tmp_conn_list[j];
        }

        conn_array += num_vtx;
      }
    }
    else // type == MBPOLYHEDRON
    {
      // in some cases, we may need to create new elements; will it screw the tags?
      // not if the file was not from moab
      ErrorCode rv = MB_SUCCESS;
      for ( ; id < end_id; ++id) {
        if (conn_iter == connectivity.end()) {
          MB_SET_ERR(MB_FAILURE, "Connectivity data truncated at polyhedra cell " << id);
        }
        ++conn_iter;
        // iterator is now at number of faces
        // we should check it is indeed num_vtx
        int num_faces = *conn_iter;
        if (num_faces != num_vtx)
          MB_SET_ERR(MB_FAILURE, "Connectivity data wrong at polyhedra cell " << id);

        EntityHandle connec[20]; // we bet we will have only 20 vertices at most, in a face in a polyhedra
        for (int j=0; j<num_faces; j++)
        {
          conn_iter++;
          int numverticesInFace = (int) *conn_iter;
          if (numverticesInFace>20)
            MB_SET_ERR(MB_FAILURE, "too many vertices in face index " << j << " for polyhedra cell " << id);
          // need to find the face, but first fill with vertices
          for (int k=0; k<numverticesInFace; k++)
          {
            connec[k] = first_vertex + *(++conn_iter); //
          }
          Range adjFaces;
          // find a face with these vertices; if not, we need to create one, on the fly :(
          rv = mdbImpl->get_adjacencies(connec, numverticesInFace, 2, false, adjFaces); MB_CHK_ERR(rv);
          if (adjFaces.size() >=1)
          {
            conn_array[j] = adjFaces[0]; // get the first face found
          }
          else
          {
            // create the face; tri, quad or polygon
            EntityType etype = MBTRI;
            if (4 == numverticesInFace) etype = MBQUAD;
            if (4 < numverticesInFace) etype = MBPOLYGON;

            rv = mdbImpl->create_element(etype, connec, numverticesInFace,  conn_array[j]); MB_CHK_ERR(rv);
          }
        }

        conn_array += num_vtx; // advance for next polyhedra
        conn_iter++; // advance to the next field
      }

    }

    // Notify MOAB of the new elements
    result = readMeshIface->update_adjacencies(start_handle, num_elem,
                                               num_vtx, conn_sav);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_create_structured_elems(const long* dims,
                                               EntityHandle first_vtx,
                                               std::vector<Range>& elem_list)
{
  ErrorCode result;
  //int non_zero[3] = {0, 0, 0}; // True if dim > 0 for x, y, z respectively
  long elem_dim = 0;           // Element dimension (2->quad, 3->hex)
  long num_elems = 1;          // Total number of elements
  long vert_per_elem;          // Element connectivity length
  long edims[3] = {1, 1, 1}; // Number of elements in each grid direction

  // Populate above data
  for (int d = 0; d < 3; d++) {
    if (dims[d] > 1) {
      //non_zero[elem_dim] = d;
      ++elem_dim;
      edims[d] = dims[d] - 1;
      num_elems *= edims[d];
    }
  }
  vert_per_elem = 1 << elem_dim;

  // Get element type from element dimension
  EntityType type;
  switch (elem_dim) {
    case 1: type = MBEDGE; break;
    case 2: type = MBQUAD; break;
    case 3: type = MBHEX;  break;
    default:
      MB_SET_ERR(MB_FAILURE, "Invalid dimension for structured elements: " << elem_dim);
  }

  // Allocate storage for elements
  EntityHandle start_handle = 0;
  EntityHandle* conn_array;
  result = allocate_elements(num_elems, vert_per_elem, type, start_handle,
                             conn_array, elem_list);
  if (MB_SUCCESS != result)
    return MB_FAILURE;

  EntityHandle *conn_sav = conn_array;

  // Offsets of element vertices in grid relative to corner closest to origin
  long k = dims[0]*dims[1];
  const long corners[8] = {0, 1, 1 + dims[0], dims[0], k, k + 1, k + 1 + dims[0], k + dims[0]};

  // Populate element list
  for (long z = 0; z < edims[2]; ++z)
    for (long y = 0; y < edims[1]; ++y)
      for (long x = 0; x < edims[0]; ++x) {
        const long index = x + y*dims[0] + z*(dims[0]*dims[1]);
        for (long j = 0; j < vert_per_elem; ++j, ++conn_array)
          *conn_array = index + corners[j] + first_vtx;
      }

  // Notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(start_handle, num_elems,
                                             vert_per_elem, conn_sav);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_read_field(FileTokenizer& tokens)
{
  // This is not supported yet.
  // Parse the data but throw it away because
  // Mesquite has no internal representation for it.

  // Could save this in tags, but the only useful thing that
  // could be done with the data is to write it back out
  // with the modified mesh. As there's no way to save the
  // type of a tag in Mesquite, it cannot be written back
  // out correctly either.
  // FIXME: Don't know what to do with this data.
  // For now, read it and throw it out.

  long num_arrays;
  if (!tokens.get_string() || // Name
      !tokens.get_long_ints(1, &num_arrays))
    return MB_FAILURE;

  for (long i = 0; i < num_arrays; ++i) {
    /*const char* name =*/ tokens.get_string();

    long dims[2];
    if (!tokens.get_long_ints(2, dims) ||
        !tokens.match_token(vtk_type_names))
      return MB_FAILURE;

    long num_vals = dims[0] * dims[1];

    for (long j = 0; j < num_vals; j++) {
      double junk;
      if (!tokens.get_doubles(1, &junk))
        return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_read_attrib_data(FileTokenizer& tokens,
                                        std::vector<Range>& entities)
{
  const char* const type_names[] = {"SCALARS",
                                    "COLOR_SCALARS",
                                    "VECTORS",
                                    "NORMALS",
                                    "TEXTURE_COORDINATES",
                                    "TENSORS",
                                    "FIELD",
                                     0};

  int type = tokens.match_token(type_names);
  const char* tmp_name = tokens.get_string();
  if (!type || !tmp_name)
    return MB_FAILURE;

  std::string name_alloc(tmp_name);
  const char* name = name_alloc.c_str();
  switch (type) {
    case 1: return vtk_read_scalar_attrib (tokens, entities, name);
    case 2: return vtk_read_color_attrib  (tokens, entities, name);
    case 3: return vtk_read_vector_attrib (tokens, entities, name);
    case 4: return vtk_read_vector_attrib (tokens, entities, name);
    case 5: return vtk_read_texture_attrib(tokens, entities, name);
    case 6: return vtk_read_tensor_attrib (tokens, entities, name);
    case 7: return vtk_read_field_attrib  (tokens, entities, name);
  }

  return MB_FAILURE;
}

ErrorCode ReadVtk::vtk_read_tag_data(FileTokenizer& tokens,
                                     int type,
                                     size_t per_elem,
                                     std::vector<Range>& entities,
                                     const char* name)
{
  ErrorCode result;
  DataType mb_type;
  size_t size;
  if (type == 1) {
    mb_type = MB_TYPE_BIT;
    size = sizeof(bool);
  }
  else if (type >= 2 && type <= 9) {
    mb_type = MB_TYPE_INTEGER;
    size = sizeof(int);
  }
  else if (type == 10 || type == 11) {
    mb_type = MB_TYPE_DOUBLE;
    size = sizeof(double);
  }
  else if (type == 12) {
    mb_type = MB_TYPE_INTEGER;
    size = 4; // Could be 4 or 8, but we don't know. Hope it's 4 because MOAB doesn't support 64-bit ints.
  }
  else
    return MB_FAILURE;

#ifdef MB_VTK_MATERIAL_SETS
  Modulator materialMap(this->mdbImpl, this->mPartitionTagName, mb_type, size, per_elem);
  bool isMaterial =
    size * per_elem <= 4 &&                          // Must have int-sized values (ParallelComm requires it)
    ! this->mPartitionTagName.empty() &&             // Must have a non-empty field name...
    ! strcmp(name, this->mPartitionTagName.c_str()); // ... that matches our spec.
#endif // MB_VTK_MATERIAL_SETS

  // Get/create tag
  Tag handle;
  result = mdbImpl->tag_get_handle(name, per_elem, mb_type, handle,
                                   MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR(result, "Tag name conflict for attribute \"" << name << "\" at line " << tokens.line_number());

  std::vector<Range>::iterator iter;

  if (type == 1) {
    for (iter = entities.begin(); iter != entities.end(); ++iter) {
      bool *data = new bool[iter->size() * per_elem];
      if (!tokens.get_booleans(per_elem*iter->size(), data)) {
        delete [] data;
        return MB_FAILURE;
      }

      bool* data_iter = data;
      Range::iterator ent_iter = iter->begin();
      for ( ; ent_iter != iter->end(); ++ent_iter) {
        unsigned char bits = 0;
        for (unsigned j = 0; j < per_elem; ++j, ++data_iter)
          bits |= (unsigned char)(*data_iter << j);
#ifdef MB_VTK_MATERIAL_SETS
        if (isMaterial)
          materialMap.add_entity(*ent_iter, &bits, 1);
#endif // MB_VTK_MATERIAL_SETS
        result = mdbImpl->tag_set_data(handle, &*ent_iter, 1, &bits);
        if (MB_SUCCESS != result) {
          delete [] data;
          return result;
        }
      }
      delete [] data;
    }
  }
  else if ((type >= 2 && type <= 9) || type == 12) {
    std::vector<int> data;
    for (iter = entities.begin(); iter != entities.end(); ++iter) {
      data.resize(iter->size() * per_elem);
      if (!tokens.get_integers(iter->size() * per_elem, &data[0]))
        return MB_FAILURE;
#ifdef MB_VTK_MATERIAL_SETS
      if (isMaterial)
        materialMap.add_entities(*iter, (unsigned char*) &data[0], per_elem * size);
#endif // MB_VTK_MATERIAL_SETS
      result = mdbImpl->tag_set_data(handle, *iter, &data[0]);
      if (MB_SUCCESS != result)
        return result;
    }
  }
  else if (type == 10 || type == 11) {
    std::vector<double> data;
    for (iter = entities.begin(); iter != entities.end(); ++iter) {
      data.resize(iter->size() * per_elem);
      if (!tokens.get_doubles(iter->size() * per_elem, &data[0]))
        return MB_FAILURE;
#ifdef MB_VTK_MATERIAL_SETS
      if (isMaterial)
        materialMap.add_entities(*iter, (unsigned char*) &data[0], per_elem * size);
#endif // MB_VTK_MATERIAL_SETS
      result = mdbImpl->tag_set_data(handle, *iter, &data[0]);
      if (MB_SUCCESS != result)
        return result;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::vtk_read_scalar_attrib(FileTokenizer& tokens,
                                          std::vector<Range>& entities,
                                          const char* name)
{
  int type = tokens.match_token(vtk_type_names);
  if (!type)
    return MB_FAILURE;

  long size;
  const char* tok = tokens.get_string();
  if (!tok)
    return MB_FAILURE;

  const char* end = 0;
  size = strtol(tok, (char**)&end, 0);
  if (*end) {
    size = 1;
    tokens.unget_token();
  }

  // VTK spec says cannot be greater than 4--do we care?
  if (size < 1) { //|| size > 4)
    MB_SET_ERR(MB_FAILURE, "Scalar count out of range [1,4] at line " << tokens.line_number());
  }

  if (!tokens.match_token("LOOKUP_TABLE") ||
      !tokens.match_token("default"))
    return MB_FAILURE;

  return vtk_read_tag_data(tokens, type, size, entities, name);
}

ErrorCode ReadVtk::vtk_read_color_attrib(FileTokenizer& tokens,
                                         std::vector<Range>& entities,
                                         const char* name)
{
  long size;
  if (!tokens.get_long_ints(1, &size) || size < 1)
    return MB_FAILURE;

  return vtk_read_tag_data(tokens, 10, size, entities, name);
}

ErrorCode ReadVtk::vtk_read_vector_attrib(FileTokenizer& tokens,
                                          std::vector<Range>& entities,
                                          const char* name)
{
  int type = tokens.match_token(vtk_type_names);
  if (!type)
    return MB_FAILURE;

  return vtk_read_tag_data(tokens, type, 3, entities, name);
}

ErrorCode ReadVtk::vtk_read_texture_attrib(FileTokenizer& tokens,
                                           std::vector<Range>& entities,
                                           const char* name)
{
  int type, dim;
  if (!tokens.get_integers(1, &dim) ||
      !(type = tokens.match_token(vtk_type_names)))
    return MB_FAILURE;

  if (dim < 1 || dim > 3) {
    MB_SET_ERR(MB_FAILURE, "Invalid dimension (" << dim << ") at line " << tokens.line_number());
  }

  return vtk_read_tag_data(tokens, type, dim, entities, name);
}

ErrorCode ReadVtk::vtk_read_tensor_attrib(FileTokenizer& tokens,
                                          std::vector<Range>& entities,
                                          const char* name)
{
  int type = tokens.match_token(vtk_type_names);
  if (!type)
    return MB_FAILURE;

  return vtk_read_tag_data(tokens, type, 9, entities, name);
}

ErrorCode ReadVtk::vtk_read_field_attrib(FileTokenizer& tokens,
                                         std::vector<Range>& entities,
                                         const char*)
{
  long num_fields;
  if (!tokens.get_long_ints(1, &num_fields))
    return MB_FAILURE;

  long i;
  for (i = 0; i < num_fields; ++i) {
    const char* tok = tokens.get_string();
    if (!tok)
      return MB_FAILURE;

    std::string name_alloc(tok);

    long num_comp;
    if (!tokens.get_long_ints(1, &num_comp))
      return MB_FAILURE;

    long num_tuples;
    if (!tokens.get_long_ints(1, &num_tuples))
      return MB_FAILURE;

    int type = tokens.match_token(vtk_type_names);
    if (!type)
      return MB_FAILURE;

    ErrorCode result = vtk_read_tag_data(tokens, type, num_comp, entities,
                                         name_alloc.c_str());MB_CHK_SET_ERR(result, "Error reading data for field \"" << name_alloc << "\" (" << num_comp << " components, " << num_tuples << " tuples, type " << type << ") at line " << tokens.line_number());
  }

  return MB_SUCCESS;
}

ErrorCode ReadVtk::store_file_ids(Tag tag, const Range& verts,
                                  const std::vector<Range>& elems)
{
  ErrorCode rval;

  rval = readMeshIface->assign_ids(tag, verts);
  if (MB_SUCCESS != rval)
    return rval;

  int id = 0;
  for (size_t i = 0; i < elems.size(); ++i) {
    rval = readMeshIface->assign_ids(tag, elems[i], id);
    id += elems[i].size();
  }

  return MB_SUCCESS;
}

} // namespace moab
