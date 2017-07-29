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

#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif

#include "WriteSLAC.hpp"

#include <utility>
#include <algorithm>
#include <time.h>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <assert.h>

#include "netcdf.h"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"
#include "ExoIIUtil.hpp"
#include "MBTagConventions.hpp"
#include "moab/WriteUtilIface.hpp"

#ifndef MOAB_HAVE_NETCDF
#error Attempt to compile WriteSLAC with NetCDF disabled.
#endif

namespace moab {

#define INS_ID(stringvar, prefix, id) \
  sprintf(stringvar, prefix, id)

#define GET_VAR(name, id, dims) \
  { \
    id = -1; \
    int gvfail = nc_inq_varid(ncFile, name, &id); \
    if (NC_NOERR == gvfail) { \
      int ndims; \
      gvfail = nc_inq_varndims(ncFile, id, &ndims); \
      if (NC_NOERR == gvfail) { \
        dims.resize(ndims); \
        gvfail = nc_inq_vardimid(ncFile, id, &dims[0]); \
      } \
    } \
  }

WriterIface* WriteSLAC::factory(Interface* iface)
{
  return new WriteSLAC(iface);
}

WriteSLAC::WriteSLAC(Interface* impl)
  : mbImpl(impl), ncFile(0)
{
  assert(impl != NULL);

  impl->query_interface(mWriteIface);

  // Initialize in case tag_get_handle fails below
  //! get and cache predefined tag handles
  int negone = -1, zero = 0;
  impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mMaterialSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mDirichletSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mNeumannSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mGlobalIdTag, MB_TAG_SPARSE | MB_TAG_CREAT, &zero);

  int dum_val = -1;
  impl->tag_get_handle("__matSetIdTag", 1, MB_TYPE_INTEGER, mMatSetIdTag,
                       MB_TAG_DENSE | MB_TAG_CREAT, &dum_val);

  impl->tag_get_handle("WriteSLAC element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);
}

WriteSLAC::~WriteSLAC() 
{
  mbImpl->release_interface(mWriteIface);
  mbImpl->tag_delete(mEntityMark);
}

void WriteSLAC::reset_matset(std::vector<WriteSLAC::MaterialSetData> &matset_info)
{
  std::vector<WriteSLAC::MaterialSetData>::iterator iter;

  for (iter = matset_info.begin(); iter != matset_info.end(); ++iter)
    delete (*iter).elements;
}

ErrorCode WriteSLAC::write_file(const char *file_name,
                                const bool overwrite,
                                const FileOptions&,
                                const EntityHandle *ent_handles,
                                const int num_sets,
                                const std::vector<std::string>& /* qa_list */,
                                const Tag* /* tag_list */,
                                int /* num_tags */,
                                int /* export_dimension */)
{
  assert(0 != mMaterialSetTag &&
         0 != mNeumannSetTag &&
         0 != mDirichletSetTag);

  // Check the file name
  if (NULL == strstr(file_name, ".ncdf"))
    return MB_FAILURE;

  std::vector<EntityHandle> matsets, dirsets, neusets, entities;

  fileName = file_name;

  // Separate into material sets, dirichlet sets, neumann sets

  if (num_sets == 0) {
    // Default to all defined sets
    Range this_range;
    mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mMaterialSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(matsets));
    this_range.clear();
    mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mDirichletSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(dirsets));
    this_range.clear();
    mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mNeumannSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(neusets));
  }
  else {
    int dummy;
    for (const EntityHandle *iter = ent_handles; iter < ent_handles + num_sets; ++iter) {
      if (MB_SUCCESS == mbImpl->tag_get_data(mMaterialSetTag, &(*iter), 1, &dummy))
        matsets.push_back(*iter);
      else if (MB_SUCCESS == mbImpl->tag_get_data(mDirichletSetTag, &(*iter), 1, &dummy))
        dirsets.push_back(*iter);
      else if (MB_SUCCESS == mbImpl->tag_get_data(mNeumannSetTag, &(*iter), 1, &dummy))
        neusets.push_back(*iter);
    }
  }

  // If there is nothing to write just return.
  if (matsets.empty() && dirsets.empty() && neusets.empty())
    return MB_FILE_WRITE_ERROR;

  std::vector<WriteSLAC::MaterialSetData> matset_info;
  std::vector<WriteSLAC::DirichletSetData> dirset_info;
  std::vector<WriteSLAC::NeumannSetData> neuset_info;

  MeshInfo mesh_info;

  matset_info.clear();
  if (gather_mesh_information(mesh_info, matset_info, neuset_info, dirset_info,
                             matsets, neusets, dirsets) != MB_SUCCESS) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  // Try to open the file after gather mesh info succeeds
  int fail = nc_create(file_name, overwrite ? NC_CLOBBER : NC_NOCLOBBER, &ncFile);
  if (NC_NOERR != fail) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (initialize_file(mesh_info) != MB_SUCCESS) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (write_nodes(mesh_info.num_nodes, mesh_info.nodes, mesh_info.num_dim) != MB_SUCCESS ) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (write_matsets(mesh_info, matset_info, neuset_info)) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  fail = nc_close(ncFile);
  if (NC_NOERR != fail) 
    return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::gather_mesh_information(MeshInfo &mesh_info,
                                             std::vector<WriteSLAC::MaterialSetData> &matset_info,
                                             std::vector<WriteSLAC::NeumannSetData> &neuset_info,
                                             std::vector<WriteSLAC::DirichletSetData> &dirset_info,
                                             std::vector<EntityHandle> &matsets,
                                             std::vector<EntityHandle> &neusets,
                                             std::vector<EntityHandle> &dirsets)
{
  std::vector<EntityHandle>::iterator vector_iter, end_vector_iter;

  mesh_info.num_nodes = 0;
  mesh_info.num_elements = 0;
  mesh_info.num_matsets = 0;

  int id = 0;

  vector_iter= matsets.begin();
  end_vector_iter = matsets.end();

  mesh_info.num_matsets = matsets.size();

  std::vector<EntityHandle> parent_meshsets;

  // Clean out the bits for the element mark
  mbImpl->tag_delete(mEntityMark);
  mbImpl->tag_get_handle("WriteSLAC element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);

  int highest_dimension_of_element_matsets = 0;

  for (vector_iter = matsets.begin(); vector_iter != matsets.end(); ++vector_iter) {
    WriteSLAC::MaterialSetData matset_data;
    matset_data.elements = new Range;

    // For the purpose of qa records, get the parents of these matsets
    if (mbImpl->get_parent_meshsets(*vector_iter, parent_meshsets) != MB_SUCCESS)
      return MB_FAILURE;

    // Get all Entity Handles in the mesh set
    Range dummy_range;
    mbImpl->get_entities_by_handle(*vector_iter, dummy_range, true);

    // Wait a minute, we are doing some filtering here that doesn't make sense at this level  CJS

    // Find the dimension of the last entity in this range
    Range::iterator entity_iter = dummy_range.end();
    entity_iter = dummy_range.end();
    --entity_iter;
    int this_dim = CN::Dimension(TYPE_FROM_HANDLE(*entity_iter));
    entity_iter = dummy_range.begin();
    while (entity_iter != dummy_range.end() &&
           CN::Dimension(TYPE_FROM_HANDLE(*entity_iter)) != this_dim)
      ++entity_iter;

    if (entity_iter != dummy_range.end())
      std::copy(entity_iter, dummy_range.end(), range_inserter(*(matset_data.elements)));

    assert(matset_data.elements->begin() == matset_data.elements->end() ||
           CN::Dimension(TYPE_FROM_HANDLE(*(matset_data.elements->begin()))) == this_dim);

    // Get the matset's id
    if (mbImpl->tag_get_data(mMaterialSetTag, &(*vector_iter), 1, &id) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get matset id from a tag for an element matset");
    }

    matset_data.id = id; 
    matset_data.number_attributes = 0;

    // Iterate through all the elements in the meshset
    Range::iterator elem_range_iter, end_elem_range_iter;
    elem_range_iter = matset_data.elements->begin();
    end_elem_range_iter = matset_data.elements->end();

    // Get the entity type for this matset, verifying that it's the same for all elements
    // THIS ASSUMES HANDLES SORT BY TYPE!!!
    EntityType entity_type = TYPE_FROM_HANDLE(*elem_range_iter);
    --end_elem_range_iter;
    if (entity_type != TYPE_FROM_HANDLE(*(end_elem_range_iter++))) {
      MB_SET_ERR(MB_FAILURE, "Entities in matset " << id << " not of common type");
    }

    int dimension = -1;
    if (entity_type == MBQUAD || entity_type == MBTRI)
      dimension = 3; // Output shells by default
    else if (entity_type == MBEDGE)
      dimension = 2;
    else
      dimension = CN::Dimension(entity_type);

    if (dimension > highest_dimension_of_element_matsets)
      highest_dimension_of_element_matsets = dimension;

    matset_data.moab_type = mbImpl->type_from_handle(*(matset_data.elements->begin()));
    if (MBMAXTYPE == matset_data.moab_type)
      return MB_FAILURE;

    std::vector<EntityHandle> tmp_conn;
    mbImpl->get_connectivity(&(*(matset_data.elements->begin())), 1, tmp_conn);
    matset_data.element_type =
      ExoIIUtil::get_element_type_from_num_verts(tmp_conn.size(), entity_type, dimension);

    if (matset_data.element_type == EXOII_MAX_ELEM_TYPE) {
      MB_SET_ERR(MB_FAILURE, "Element type in matset " << id << " didn't get set correctly");
    }

    matset_data.number_nodes_per_element = ExoIIUtil::VerticesPerElement[matset_data.element_type];

    // Number of nodes for this matset
    matset_data.number_elements = matset_data.elements->size();

    // Total number of elements
    mesh_info.num_elements += matset_data.number_elements;

    // Get the nodes for the elements
    mWriteIface->gather_nodes_from_elements(*matset_data.elements, mEntityMark, mesh_info.nodes);

    if (!neusets.empty()) {
      // If there are neusets, keep track of which elements are being written out
      for (Range::iterator iter = matset_data.elements->begin();
          iter != matset_data.elements->end(); ++iter) {
        unsigned char bit = 0x1;
        mbImpl->tag_set_data(mEntityMark, &(*iter), 1, &bit);
      }
    }

    matset_info.push_back(matset_data);
  }

  // If user hasn't entered dimension, we figure it out
  if (mesh_info.num_dim == 0 ) {
    // Never want 1 or zero dimensions
    if (highest_dimension_of_element_matsets < 2)
      mesh_info.num_dim = 3;
    else
      mesh_info.num_dim = highest_dimension_of_element_matsets;
  }

  Range::iterator range_iter, end_range_iter;
  range_iter = mesh_info.nodes.begin();
  end_range_iter = mesh_info.nodes.end();

  mesh_info.num_nodes = mesh_info.nodes.size();

  //------dirsets--------

  vector_iter= dirsets.begin();
  end_vector_iter = dirsets.end();

  for (; vector_iter != end_vector_iter; ++vector_iter) {
    WriteSLAC::DirichletSetData dirset_data;
    dirset_data.id = 0;
    dirset_data.number_nodes = 0;

    // Get the dirset's id
    if (mbImpl->tag_get_data(mDirichletSetTag,&(*vector_iter), 1,&id) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get id tag for dirset " << id);
    }

    dirset_data.id = id;

    std::vector<EntityHandle> node_vector;
    // Get the nodes of the dirset that are in mesh_info.nodes
    if (mbImpl->get_entities_by_handle(*vector_iter, node_vector, true) != MB_SUCCESS ) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get nodes in dirset " << id);
    }

    std::vector<EntityHandle>::iterator iter, end_iter;
    iter = node_vector.begin();
    end_iter = node_vector.end();

    int j = 0;
    unsigned char node_marked = 0;
    ErrorCode result;
    for (; iter != end_iter; ++iter) {
      if (TYPE_FROM_HANDLE(*iter) != MBVERTEX)
        continue;
      result = mbImpl->tag_get_data(mEntityMark, &(*iter), 1, &node_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

      if (0x1 == node_marked)
        dirset_data.nodes.push_back(*iter);
      j++;
    }

    dirset_data.number_nodes = dirset_data.nodes.size();
    dirset_info.push_back(dirset_data);
  }

  //------neusets--------
  vector_iter= neusets.begin();
  end_vector_iter = neusets.end();

  for (; vector_iter != end_vector_iter; ++vector_iter) {
    WriteSLAC::NeumannSetData neuset_data;

    // Get the neuset's id
    if (mbImpl->tag_get_data(mNeumannSetTag, &(*vector_iter), 1, &id) != MB_SUCCESS)
      return MB_FAILURE;

    neuset_data.id = id;
    neuset_data.mesh_set_handle = *vector_iter;

    // Get the sides in two lists, one forward the other reverse; starts with forward sense
    // by convention
    Range forward_elems, reverse_elems;
    if (get_neuset_elems(*vector_iter, 0, forward_elems, reverse_elems) == MB_FAILURE)
      return MB_FAILURE;

    ErrorCode result = get_valid_sides(forward_elems, 1, neuset_data);MB_CHK_SET_ERR(result, "Couldn't get valid sides data");
    result = get_valid_sides(reverse_elems, -1, neuset_data);MB_CHK_SET_ERR(result, "Couldn't get valid sides data");

    neuset_data.number_elements = neuset_data.elements.size();
    neuset_info.push_back(neuset_data);
  }

  // Get information about interior/exterior tets/hexes, and mark matset ids
  return gather_interior_exterior(mesh_info, matset_info, neuset_info);
}

ErrorCode WriteSLAC::get_valid_sides(Range &elems, const int sense,
                                     WriteSLAC::NeumannSetData &neuset_data)
{
  // This is where we see if underlying element of side set element is included in output

  unsigned char element_marked = 0;
  ErrorCode result;
  for (Range::iterator iter = elems.begin(); iter != elems.end(); ++iter) {
    // Should insert here if "side" is a quad/tri on a quad/tri mesh
    result = mbImpl->tag_get_data(mEntityMark, &(*iter), 1, &element_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

    if (0x1 == element_marked) {
      neuset_data.elements.push_back(*iter);

      // TJT TODO: the sense should really be # edges + 1or2
      neuset_data.side_numbers.push_back((sense == 1 ? 1 : 2));
    }
    else { // Then "side" is probably a quad/tri on a hex/tet mesh
      std::vector<EntityHandle> parents;
      int dimension = CN::Dimension(TYPE_FROM_HANDLE(*iter));

      // Get the adjacent parent element of "side"
      if (mbImpl->get_adjacencies(&(*iter), 1, dimension + 1, false, parents) != MB_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Couldn't get adjacencies for neuset");
      }

      if (!parents.empty()) {
        // Make sure the adjacent parent element will be output
        for (unsigned int k = 0; k < parents.size(); k++) {
          result = mbImpl->tag_get_data(mEntityMark, &(parents[k]), 1, &element_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

          int side_no, this_sense, this_offset;
          if (0x1 == element_marked &&
             mbImpl->side_number(parents[k], *iter, side_no,
                                 this_sense, this_offset) == MB_SUCCESS &&
             this_sense == sense) {
            neuset_data.elements.push_back(parents[k]);
            neuset_data.side_numbers.push_back(side_no + 1);
            break;
          }
        }
      }
      else {
        MB_SET_ERR(MB_FAILURE, "No parent element exists for element in neuset " << neuset_data.id);
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::write_nodes(const int num_nodes, const Range& nodes, const int dimension)
{
  // See if should transform coordinates
  ErrorCode result;
  Tag trans_tag;
  result = mbImpl->tag_get_handle(MESH_TRANSFORM_TAG_NAME, 16, MB_TYPE_DOUBLE, trans_tag);
  bool transform_needed = true;
  if (result == MB_TAG_NOT_FOUND)
    transform_needed = false;

  int num_coords_to_fill = transform_needed ? 3 : dimension;

  std::vector<double*> coord_arrays(3);
  coord_arrays[0] = new double[num_nodes];
  coord_arrays[1] = new double[num_nodes];
  coord_arrays[2] = NULL;

  if (num_coords_to_fill == 3)
    coord_arrays[2] = new double[num_nodes];

  result = mWriteIface->get_node_coords(dimension, num_nodes, nodes,
                                        mGlobalIdTag, 0, coord_arrays);
  if (result != MB_SUCCESS) {
    delete [] coord_arrays[0];
    delete [] coord_arrays[1];
    if (coord_arrays[2])
      delete [] coord_arrays[2];
    return result;
  }

  if (transform_needed ) {
    double trans_matrix[16];
    const EntityHandle mesh = 0;
    result = mbImpl->tag_get_data(trans_tag, &mesh, 1, trans_matrix);MB_CHK_SET_ERR(result, "Couldn't get transform data");

    for (int i = 0; i < num_nodes; i++) {
      double vec1[3];
      double vec2[3];

      vec2[0] = coord_arrays[0][i];
      vec2[1] = coord_arrays[1][i];
      vec2[2] = coord_arrays[2][i];

      for (int row = 0; row < 3; row++) {
        vec1[row] = 0.0;
        for (int col = 0; col < 3; col++)
          vec1[row] += (trans_matrix[(row*4) + col] * vec2[col]);
      }

      coord_arrays[0][i] = vec1[0];
      coord_arrays[1][i] = vec1[1];
      coord_arrays[2][i] = vec1[2];
    }
  }

  // Write the nodes
  int nc_var = -1;
  std::vector<int> dims;
  GET_VAR("coords", nc_var, dims);
  if (-1 == nc_var) return MB_FAILURE;
  size_t start[2] = {0, 0}, count[2] = {static_cast<size_t>(num_nodes), 1};
  int fail = nc_put_vara_double(ncFile, nc_var, start, count, coord_arrays[0]);
  if (NC_NOERR != fail)
    return MB_FAILURE;
  start[1] = 1;
  fail = nc_put_vara_double(ncFile, nc_var, start, count, coord_arrays[1]);
  if (NC_NOERR != fail)
    return MB_FAILURE;
  start[1] = 2;
  fail = nc_put_vara_double(ncFile, nc_var, start, count, coord_arrays[2]);
  if (NC_NOERR != fail)
    return MB_FAILURE;

  delete [] coord_arrays[0];
  delete [] coord_arrays[1];
  if (coord_arrays[2])
    delete [] coord_arrays[2];

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::gather_interior_exterior(MeshInfo &mesh_info,
                                              std::vector<WriteSLAC::MaterialSetData> &matset_data,
                                              std::vector<WriteSLAC::NeumannSetData> &neuset_data)
{
  // Need to assign a tag with the matset id
  Tag matset_id_tag;
  unsigned int i;
  int dum = -1;
  ErrorCode result = mbImpl->tag_get_handle("__matset_id", 4, MB_TYPE_INTEGER, matset_id_tag, MB_TAG_DENSE | MB_TAG_CREAT, &dum);
  if (MB_SUCCESS != result)
    return result;

  Range::iterator rit;
  mesh_info.num_int_hexes = mesh_info.num_int_tets = 0;

  for (i = 0; i < matset_data.size(); i++) {
    WriteSLAC::MaterialSetData matset = matset_data[i];
    if (matset.moab_type == MBHEX)
      mesh_info.num_int_hexes += matset.elements->size();
    else if (matset.moab_type == MBTET)
      mesh_info.num_int_tets += matset.elements->size();
    else {
      std::cout << "WriteSLAC doesn't support elements of type "
                << CN::EntityTypeName(matset.moab_type) << std::endl;
      continue;
    }

    for (rit = matset.elements->begin(); rit != matset.elements->end(); ++rit) {
      result = mbImpl->tag_set_data(mMatSetIdTag, &(*rit), 1, &(matset.id));
      if (MB_SUCCESS != result)
        return result;
    }
  }

  // Now go through the neumann sets, pulling out the hexes with faces on the
  // boundary
  std::vector<EntityHandle>::iterator vit;
  for (i = 0; i < neuset_data.size(); i++) {
    WriteSLAC::NeumannSetData neuset = neuset_data[i];
    for (vit = neuset.elements.begin(); vit != neuset.elements.end(); ++vit) {
      if (TYPE_FROM_HANDLE(*vit) == MBHEX)
        mesh_info.bdy_hexes.insert(*vit);
      else if (TYPE_FROM_HANDLE(*vit) == MBTET)
        mesh_info.bdy_tets.insert(*vit);
    }
  }

  // Now we have the number of bdy hexes and tets, we know how many interior ones
  // there are too
  mesh_info.num_int_hexes -= mesh_info.bdy_hexes.size();
  mesh_info.num_int_tets -= mesh_info.bdy_tets.size();

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::write_matsets(MeshInfo &mesh_info,
                                   std::vector<WriteSLAC::MaterialSetData> &matset_data,
                                   std::vector<WriteSLAC::NeumannSetData> &neuset_data)
{
  unsigned int i;
  std::vector<int> connect;
  const EntityHandle *connecth;
  int num_connecth;
  ErrorCode result;

  // First write the interior hexes
  int hex_conn = -1;
  std::vector<int> dims;
  if (mesh_info.bdy_hexes.size() != 0 || mesh_info.num_int_hexes != 0) {
    GET_VAR("hexahedron_interior", hex_conn, dims);
    if (-1 == hex_conn)
      return MB_FAILURE;
  }
  connect.reserve(13);
  Range::iterator rit;

  int elem_num = 0;
  WriteSLAC::MaterialSetData matset;
  size_t start[2] = {0, 0}, count[2] = {1, 1};
  int fail;
  for (i = 0; i < matset_data.size(); i++) {
    matset = matset_data[i];
    if (matset.moab_type != MBHEX)
      continue;

    int id = matset.id;
    connect[0] = id;

    for (rit = matset.elements->begin(); rit != matset.elements->end(); ++rit) {
      // Skip if it's on the bdy
      if (mesh_info.bdy_hexes.find(*rit) != mesh_info.bdy_hexes.end())
        continue;

      // Get the connectivity of this element
      result = mbImpl->get_connectivity(*rit, connecth, num_connecth);
      if (MB_SUCCESS != result)
        return result;

      // Get the vertex ids
      result = mbImpl->tag_get_data(mGlobalIdTag, connecth, num_connecth, &connect[1]);
      if (MB_SUCCESS != result)
        return result;

      // Put the variable at the right position
      start[0] = elem_num++;
      count[1] = 9;

      // Write the data
      fail = nc_put_vara_int(ncFile, hex_conn, start, count, &connect[0]);
      if (NC_NOERR != fail)
        return MB_FAILURE;
    }
  }

  int tet_conn = -1;
  if (mesh_info.bdy_tets.size() != 0 || mesh_info.num_int_tets != 0) {
    GET_VAR("tetrahedron_interior", tet_conn, dims);
    if (-1 == tet_conn)
      return MB_FAILURE;
  }

  // Now the interior tets
  elem_num = 0;
  for (i = 0; i < matset_data.size(); i++) {
    matset = matset_data[i];
    if (matset.moab_type != MBTET)
      continue;

    int id = matset.id;
    connect[0] = id;
    elem_num = 0;
    for (rit = matset.elements->begin(); rit != matset.elements->end(); ++rit) {
      // Skip if it's on the bdy
      if (mesh_info.bdy_tets.find(*rit) != mesh_info.bdy_tets.end())
        continue;

      // Get the connectivity of this element
      result = mbImpl->get_connectivity(*rit, connecth, num_connecth);
      if (MB_SUCCESS != result)
        return result;

      // Get the vertex ids
      result = mbImpl->tag_get_data(mGlobalIdTag, connecth, num_connecth, &connect[1]);
      if (MB_SUCCESS != result)
        return result;

      // Put the variable at the right position
      start[0] = elem_num++;
      count[1] = 5;
      fail = nc_put_vara_int(ncFile, tet_conn, start, count, &connect[0]);
      // Write the data
      if (NC_NOERR != fail)
        return MB_FAILURE;
    }
  }

  // Now the exterior hexes
  if (mesh_info.bdy_hexes.size() != 0) {
    hex_conn = -1;
    GET_VAR("hexahedron_exterior", hex_conn, dims);
    if (-1 == hex_conn)
      return MB_FAILURE;

    connect.reserve(15);
    elem_num = 0;

    // Write the elements
    for (rit = mesh_info.bdy_hexes.begin(); rit != mesh_info.bdy_hexes.end(); ++rit) {
      // Get the material set for this hex
      result = mbImpl->tag_get_data(mMatSetIdTag, &(*rit), 1, &connect[0]);
      if (MB_SUCCESS != result)
        return result;

      // Get the connectivity of this element
      result = mbImpl->get_connectivity(*rit, connecth, num_connecth);
      if (MB_SUCCESS != result)
        return result;

      // Get the vertex ids
      result = mbImpl->tag_get_data(mGlobalIdTag, connecth, num_connecth, &connect[1]);
      if (MB_SUCCESS != result)
        return result;

      // Preset side numbers
      for (i = 9; i < 15; i++)
        connect[i] = -1;

      // Now write the side numbers
      for (i = 0; i < neuset_data.size(); i++) {
        std::vector<EntityHandle>::iterator vit =
          std::find(neuset_data[i].elements.begin(), neuset_data[i].elements.end(), *rit);
        while (vit != neuset_data[i].elements.end()) {
          // Have a side - get the side # and put in connect array
          int side_no = neuset_data[i].side_numbers[vit - neuset_data[i].elements.begin()];
          connect[9+side_no] = neuset_data[i].id;
          ++vit;
          vit = std::find(vit, neuset_data[i].elements.end(), *rit);
        }
      }

      // Put the variable at the right position
      start[0] = elem_num++;
      count[1] = 15;
      fail = nc_put_vara_int(ncFile, hex_conn, start, count, &connect[0]);
      // Write the data
      if (NC_NOERR != fail)
        return MB_FAILURE;
    }
  }

  // Now the exterior tets
  if (mesh_info.bdy_tets.size() != 0) {
    tet_conn = -1;
    GET_VAR("tetrahedron_exterior", tet_conn, dims);
    if (-1 == tet_conn)
      return MB_FAILURE;

    connect.reserve(9);
    elem_num = 0;

    // Write the elements
    for (rit = mesh_info.bdy_tets.begin(); rit != mesh_info.bdy_tets.end(); ++rit) {
      // Get the material set for this tet
      result = mbImpl->tag_get_data(mMatSetIdTag, &(*rit), 1, &connect[0]);
      if (MB_SUCCESS != result)
        return result;

      // Get the connectivity of this element
      result = mbImpl->get_connectivity(*rit, connecth, num_connecth);
      if (MB_SUCCESS != result)
        return result;

      // Get the vertex ids
      result = mbImpl->tag_get_data(mGlobalIdTag, connecth, num_connecth, &connect[1]);
      if (MB_SUCCESS != result)
        return result;

      // Preset side numbers
      for (i = 5; i < 9; i++)
        connect[i] = -1;

      // Now write the side numbers
      for (i = 0; i < neuset_data.size(); i++) {
        std::vector<EntityHandle>::iterator vit =
          std::find(neuset_data[i].elements.begin(), neuset_data[i].elements.end(), *rit);
        while (vit != neuset_data[i].elements.end()) {
          // Have a side - get the side # and put in connect array
          int side_no = neuset_data[i].side_numbers[vit - neuset_data[i].elements.begin()];
          connect[5 + side_no] = neuset_data[i].id;
          ++vit;
          vit = std::find(vit, neuset_data[i].elements.end(), *rit);
        }
      }

      // Put the variable at the right position
      start[0] = elem_num++;
      count[1] = 9;
      fail = nc_put_vara_int(ncFile, tet_conn, start, count, &connect[0]);
      // Write the data
      if (NC_NOERR != fail)
        return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::initialize_file(MeshInfo &mesh_info)
{
  // Perform the initializations

  int coord_size = -1, ncoords = -1;
  // Initialization to avoid warnings on Linux
  int hexinterior = -1, hexinteriorsize, hexexterior = -1, hexexteriorsize = -1;
  int tetinterior = -1, tetinteriorsize, tetexterior = -1, tetexteriorsize = -1;
  
  if (nc_def_dim(ncFile, "coord_size", (size_t)mesh_info.num_dim, &coord_size) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of dimensions");
  }

  if (nc_def_dim(ncFile, "ncoords", (size_t)mesh_info.num_nodes, &ncoords) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of nodes");
  }

  if (0 != mesh_info.num_int_hexes &&
      nc_def_dim(ncFile, "hexinterior", (size_t)mesh_info.num_int_hexes, &hexinterior) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of interior hex elements");
  }

  if (nc_def_dim(ncFile, "hexinteriorsize", (size_t)9, &hexinteriorsize) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define interior hex element size");
  }

  if (0 != mesh_info.bdy_hexes.size() &&
      nc_def_dim(ncFile, "hexexterior", (size_t)mesh_info.bdy_hexes.size(), &hexexterior) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of exterior hex elements");
  }

  if (nc_def_dim(ncFile, "hexexteriorsize", (size_t)15, &hexexteriorsize) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define exterior hex element size");
  }

  if (0 != mesh_info.num_int_tets &&
      nc_def_dim(ncFile, "tetinterior", (size_t)mesh_info.num_int_tets, &tetinterior) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of interior tet elements");
  }

  if (nc_def_dim(ncFile, "tetinteriorsize", (size_t)5, &tetinteriorsize) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define interior tet element size");
  }

  if (0 != mesh_info.bdy_tets.size() &&
      nc_def_dim(ncFile, "tetexterior", (size_t)mesh_info.bdy_tets.size(), &tetexterior) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define number of exterior tet elements");
  }

  if (nc_def_dim(ncFile, "tetexteriorsize", (size_t)9, &tetexteriorsize) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define exterior tet element size");
  }

  /* ...and some variables */

  int dims[2];
  dims[0] = hexinterior;
  dims[1] = hexinteriorsize;
  int dum_var;
  if (0 != mesh_info.num_int_hexes &&
      NC_NOERR != nc_def_var(ncFile, "hexahedron_interior", NC_LONG, 2, dims, &dum_var)) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to create connectivity array for interior hexes");
  }

  dims[0] = hexexterior;
  dims[1] = hexexteriorsize;
  if (0 != mesh_info.bdy_hexes.size() &&
      NC_NOERR != nc_def_var(ncFile, "hexahedron_exterior", NC_LONG, 2, dims, &dum_var)) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to create connectivity array for exterior hexes");
  }

  dims[0] = tetinterior;
  dims[1] = tetinteriorsize;
  if (0 != mesh_info.num_int_tets &&
      NC_NOERR != nc_def_var(ncFile, "tetrahedron_exterior", NC_LONG, 2, dims, &dum_var)) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to create connectivity array for interior tets");
  }

  dims[0] = tetexterior;
  dims[1] = tetexteriorsize;
  if (0 != mesh_info.bdy_tets.size() &&
      NC_NOERR != nc_def_var(ncFile, "tetrahedron_exterior", NC_LONG, 2, dims, &dum_var)) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to create connectivity array for exterior tets");
  }

  /* Node coordinate arrays: */

  dims[0] = ncoords;
  dims[1] = coord_size;
  if (NC_NOERR != nc_def_var(ncFile, "coords", NC_DOUBLE, 2, dims, &dum_var)) {
    MB_SET_ERR(MB_FAILURE, "WriteSLAC: failed to define node coordinate array");
  }

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::open_file(const char* filename)
{
  // Not a valid filname
  if (strlen((const char*)filename) == 0) {
    MB_SET_ERR(MB_FAILURE, "Output filename not specified");
  }

  int fail = nc_create(filename, NC_CLOBBER, &ncFile);
  // File couldn't be opened
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Cannot open " << filename);
  }

  return MB_SUCCESS;
}

ErrorCode WriteSLAC::get_neuset_elems(EntityHandle neuset, int current_sense,
                                      Range &forward_elems, Range &reverse_elems)
{
  Range ss_elems, ss_meshsets;

  // Get the sense tag; don't need to check return, might be an error if the tag
  // hasn't been created yet
  Tag sense_tag = 0;
  mbImpl->tag_get_handle("SENSE", 1, MB_TYPE_INTEGER, sense_tag);

  // Get the entities in this set
  ErrorCode result = mbImpl->get_entities_by_handle(neuset, ss_elems, true);
  if (MB_FAILURE == result)
    return result;

  // Now remove the meshsets into the ss_meshsets; first find the first meshset,
  Range::iterator range_iter = ss_elems.begin();
  while (TYPE_FROM_HANDLE(*range_iter) != MBENTITYSET && range_iter != ss_elems.end())
    ++range_iter;

  // Then, if there are some, copy them into ss_meshsets and erase from ss_elems
  if (range_iter != ss_elems.end()) {
    std::copy(range_iter, ss_elems.end(), range_inserter(ss_meshsets));
    ss_elems.erase(range_iter, ss_elems.end());
  }

  // OK, for the elements, check the sense of this set and copy into the right range
  // (if the sense is 0, copy into both ranges)

  // Need to step forward on list until we reach the right dimension
  Range::iterator dum_it = ss_elems.end();
  --dum_it;
  int target_dim = CN::Dimension(TYPE_FROM_HANDLE(*dum_it));
  dum_it = ss_elems.begin();
  while (target_dim != CN::Dimension(TYPE_FROM_HANDLE(*dum_it)) &&
         dum_it != ss_elems.end())
    ++dum_it;

  if (current_sense == 1 || current_sense == 0)
    std::copy(dum_it, ss_elems.end(), range_inserter(forward_elems));
  if (current_sense == -1 || current_sense == 0)
    std::copy(dum_it, ss_elems.end(), range_inserter(reverse_elems));

  // Now loop over the contained meshsets, getting the sense of those and calling this
  // function recursively
  for (range_iter = ss_meshsets.begin(); range_iter != ss_meshsets.end(); ++range_iter) {
    // First get the sense; if it's not there, by convention it's forward
    int this_sense;
    if (0 == sense_tag ||
        MB_FAILURE == mbImpl->tag_get_data(sense_tag, &(*range_iter), 1, &this_sense))
      this_sense = 1;

    // Now get all the entities on this meshset, with the proper (possibly reversed) sense
    get_neuset_elems(*range_iter, this_sense*current_sense,
                     forward_elems, reverse_elems);
  }

  return result;
}

} // namespace moab
