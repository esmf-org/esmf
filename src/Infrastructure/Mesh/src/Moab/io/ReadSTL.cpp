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
 * \class ReadSTL
 * \brief ASCII and Binary Stereo Lithography File readers.
 * \author Jason Kraftcheck
 */

#include "ReadSTL.hpp"
#include "FileTokenizer.hpp" // For FileTokenizer
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "moab/FileOptions.hpp"
#include "SysUtil.hpp"

#include <errno.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <map>

namespace moab {

ReadSTL::ReadSTL(Interface* impl)
  : mdbImpl(impl)
{
  mdbImpl->query_interface(readMeshIface);
}

ReadSTL::~ReadSTL()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = NULL;
  }
}

// Used to put points in an STL tree-based container
bool ReadSTL::Point::operator<(const ReadSTL::Point& other) const
{
  return 0 > memcmp(this, &other, sizeof(ReadSTL::Point));
}

ErrorCode ReadSTL::read_tag_values(const char* /* file_name */,
                                   const char* /* tag_name */,
                                   const FileOptions& /* opts */,
                                   std::vector<int>& /* tag_values_out */,
                                   const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

// Generic load function for both ASCII and binary. Calls
// pure-virtual function implemented in subclasses to read
// the data from the file.
ErrorCode ReadSTL::load_file(const char* filename,
                             const EntityHandle* /* file_set */,
                             const FileOptions& opts,
                             const ReaderIface::SubsetList* subset_list,
                             const Tag* file_id_tag)
{
  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for STL");
  }

  ErrorCode result;

  std::vector<ReadSTL::Triangle> triangles;

  bool is_ascii = false, is_binary = false;
  if (MB_SUCCESS == opts.get_null_option("ASCII"))
    is_ascii = true;
  if (MB_SUCCESS == opts.get_null_option("BINARY"))
    is_binary = true;
  if (is_ascii && is_binary) {
    MB_SET_ERR(MB_FAILURE, "Conflicting options: BINARY ASCII");
  }

  bool big_endian = false, little_endian = false;
  if (MB_SUCCESS == opts.get_null_option("BIG_ENDIAN"))
    big_endian = true;
  if (MB_SUCCESS == opts.get_null_option("LITTLE_ENDIAN"))
    little_endian = true;
  if (big_endian && little_endian) {
    MB_SET_ERR(MB_FAILURE, "Conflicting options: BIG_ENDIAN LITTLE_ENDIAN");
  }
  ByteOrder byte_order =    big_endian ? STL_BIG_ENDIAN
                       : little_endian ? STL_LITTLE_ENDIAN
                       :                 STL_UNKNOWN_BYTE_ORDER;

  if (is_ascii)
    result = ascii_read_triangles(filename, triangles);
  else if (is_binary)
    result = binary_read_triangles(filename, byte_order, triangles);
  else {
    // Try ASCII first
    result = ascii_read_triangles(filename, triangles);
    if (MB_SUCCESS != result) 
      // ASCII failed, try binary
      result = binary_read_triangles(filename, byte_order, triangles);
  }
  if (MB_SUCCESS != result)
    return result;

  // Create a std::map from position->handle, and such
  // that all positions are specified, and handles are zero.
  std::map<Point, EntityHandle> vertex_map;
  for (std::vector<Triangle>::iterator i = triangles.begin(); i != triangles.end(); ++i) {
    vertex_map[i->points[0]] = 0;
    vertex_map[i->points[1]] = 0;
    vertex_map[i->points[2]] = 0;
  }

  // Create vertices
  std::vector<double*> coord_arrays;
  EntityHandle vtx_handle = 0;
  result = readMeshIface->get_node_coords(3, vertex_map.size(), MB_START_ID,
                                          vtx_handle, coord_arrays);
  if (MB_SUCCESS != result)
    return result;

  // Copy vertex coordinates into entity sequence coordinate arrays
  // and copy handle into vertex_map.
  double *x = coord_arrays[0], *y = coord_arrays[1], *z = coord_arrays[2];
  for (std::map<Point, EntityHandle>::iterator i = vertex_map.begin();
       i != vertex_map.end(); ++i) {
    i->second = vtx_handle; ++vtx_handle;
    *x = i->first.coords[0]; ++x;
    *y = i->first.coords[1]; ++y;
    *z = i->first.coords[2]; ++z;
  }

  // Allocate triangles
  EntityHandle elm_handle = 0;
  EntityHandle* connectivity;
  result = readMeshIface->get_element_connect(triangles.size(),
                                              3,
                                              MBTRI,
                                              MB_START_ID,
                                              elm_handle,
                                              connectivity);
  if (MB_SUCCESS != result)
    return result;

  // Use vertex_map to recover triangle connectivity from
  // vertex coordinates.
  EntityHandle *conn_sav = connectivity;
  for (std::vector<Triangle>::iterator i = triangles.begin(); i != triangles.end(); ++i) {
    *connectivity = vertex_map[i->points[0]]; ++connectivity;
    *connectivity = vertex_map[i->points[1]]; ++connectivity;
    *connectivity = vertex_map[i->points[2]]; ++connectivity;
  }

  // Notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(elm_handle, triangles.size(),
                                             3, conn_sav);
  if (MB_SUCCESS != result)
    return result;

  if (file_id_tag) {
    Range vertices(vtx_handle, vtx_handle + vertex_map.size() - 1);
    Range elements(elm_handle, elm_handle + triangles.size() - 1);
    readMeshIface->assign_ids(*file_id_tag, vertices);
    readMeshIface->assign_ids(*file_id_tag, elements);
  }

  return MB_SUCCESS;
}

// Read ASCII file
ErrorCode ReadSTL::ascii_read_triangles(const char* name,
                                        std::vector<ReadSTL::Triangle>& tris)
{
  FILE* file = fopen(name, "r");
  if (!file) {
    return MB_FILE_DOES_NOT_EXIST;
  }

  char header[81];
  if (!fgets(header, sizeof(header), file) || // Read header line
      strlen(header) < 6                   || // Must be at least 6 chars
      header[strlen(header) - 1] != '\n'   || // Cannot exceed 80 chars
      memcmp(header, "solid", 5)           || // Must begin with "solid"
      !isspace(header[5])) {                  // Followed by a whitespace char
    fclose(file);
    return MB_FILE_WRITE_ERROR;
  }

  // Use tokenizer for remainder of parsing
  FileTokenizer tokens(file, readMeshIface);

  Triangle tri;
  float norm[3];

  // Read until end of file. If we reach "endsolid", read
  // was successful. If EOF before "endsolid", return error.
  for (;;) {
    // Check for either another facet or the end of the list.
    const char* const expected[] = {"facet", "endsolid", 0};
    switch (tokens.match_token(expected)) {
      case 1:  break;                      // Found another facet
      case 2:  return MB_SUCCESS;          // Found "endsolid" -- done
      default: return MB_FILE_WRITE_ERROR; // Found something else, or EOF
    }

    if (!tokens.match_token("normal") || // Expect "normal" keyword
        !tokens.get_floats(3, norm)   || // Followed by normal vector
        !tokens.match_token("outer")  || // Followed by "outer loop"
        !tokens.match_token("loop"))
      return MB_FILE_WRITE_ERROR;

    // For each of three triangle vertices
    for (int i = 0; i < 3; i++) {
      if (!tokens.match_token("vertex") ||
          !tokens.get_floats(3, tri.points[i].coords))
        return MB_FILE_WRITE_ERROR;
    }

    if (!tokens.match_token("endloop") || // Facet ends with "endloop"
        !tokens.match_token("endfacet"))  // and then "endfacet"
      return MB_FILE_WRITE_ERROR;

    tris.push_back(tri);
  }

  fclose(file);
  return MB_SUCCESS;
}

// Header block from binary STL file (84 bytes long)
struct BinaryHeader {
  char comment[80]; // 80 byte comment string (null terminated?)
  uint32_t count;   // Number of triangles - 4 byte integer
};

// Triangle spec from file (50 bytes)
struct BinaryTri {
  float normal[3]; // Normal as 3 4-byte little-endian IEEE floats
  float coords[9]; // Vertex coords as 9 4-byte little-endian IEEE floats
  char pad[2];
};

// Read a binary STL file
ErrorCode ReadSTL::binary_read_triangles(const char* name,
                                         ReadSTL::ByteOrder byte_order,
                                         std::vector<ReadSTL::Triangle>& tris)
{
  FILE* file = fopen(name, "rb");
  if (!file) {
    return MB_FILE_DOES_NOT_EXIST;
  }

  // Read header block
  BinaryHeader header;
  if (fread(&header, 84, 1, file) != 1) {
    fclose(file);
    return MB_FILE_WRITE_ERROR;
  }

  // Allow user setting for byte order, default to little endian
  const bool want_big_endian = (byte_order == STL_BIG_ENDIAN);
  const bool am_big_endian = !SysUtil::little_endian();
  bool swap_bytes = (want_big_endian == am_big_endian);

  // Compare the number of triangles to the length of the file.
  // The file must contain an 80-byte description, a 4-byte
  // triangle count and 50 bytes per triangle.
  //
  // The triangle count *may* allow us to determine the byte order
  // of the file, if it is not an endian-symmetric value.
  //
  // We need to compare the expected size calculated from the triangle
  // count with the file size anyway, as an invalid file or a byte-
  // swapping issue could result in a very large (incorrect) value for
  // num_tri, resulting in a SEGFAULT.

  // Get expected number of triangles
  if (swap_bytes)
    SysUtil::byteswap(&header.count, 1);
  unsigned long num_tri = header.count;

  // Get the file length
  long filesize = SysUtil::filesize(file);
  if (filesize >= 0) { // -1 indicates could not determine file size (e.g. reading from FIFO)
      // Check file size, but be careful of numeric overflow
    if (ULONG_MAX / 50 - 84 < num_tri || // Next calc would have overflow
        84 + 50 * num_tri != (unsigned long)filesize) {
      // Unless the byte order was specified explicitly in the
      // tag, try the opposite byte order.
      uint32_t num_tri_tmp = header.count;
      SysUtil::byteswap(&num_tri_tmp, 1);
      unsigned long num_tri_swap = num_tri_tmp;
      if (byte_order != STL_UNKNOWN_BYTE_ORDER || // If byte order was specified, fail now
          ULONG_MAX / 50 - 84 < num_tri_swap || // Watch for overflow in next line
          84 + 50 * num_tri_swap != (unsigned long)filesize) {
        fclose(file);
        return MB_FILE_WRITE_ERROR;
      }
      swap_bytes = !swap_bytes;
      num_tri = num_tri_swap;
    }
  }

  // Allocate storage for triangles
  tris.resize(num_tri);

  // Read each triangle
  BinaryTri tri; // Binary block read from file
  for (std::vector<Triangle>::iterator i = tris.begin(); i != tris.end(); ++i) {
    if (fread(&tri, 50, 1, file) != 1) {
      fclose(file);
      return MB_FILE_WRITE_ERROR;
    }

    if (swap_bytes)
      SysUtil::byteswap(tri.coords, 9);

    for (unsigned j = 0; j < 9; ++j)
      i->points[j / 3].coords[j % 3] = tri.coords[j];
  }

  fclose(file);
  return MB_SUCCESS;
}

ReaderIface* ReadSTL::factory(Interface* iface)
{
  return new ReadSTL(iface);
}

} // namespace moab
