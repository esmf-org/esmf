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

#include "WriteTemplate.hpp"

#include <utility>
#include <algorithm>
#include <time.h>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "assert.h"
#include "Internals.hpp"
#include "ExoIIUtil.hpp"
#include "MBTagConventions.hpp"
#include "moab/WriteUtilIface.hpp"

namespace moab {

#define INS_ID(stringvar, prefix, id) \
  sprintf(stringvar, prefix, id)

WriterIface* WriteTemplate::factory(Interface* iface)
{
  return new WriteTemplate(iface);
}

WriteTemplate::WriteTemplate(Interface* impl)
  : mbImpl(impl)
{
  assert(impl != NULL);

  impl->query_interface(mWriteIface);

  // Initialize in case tag_get_handle fails below
  //! Get and cache predefined tag handles
  int zero = 0, negone = -1;
  impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mMaterialSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mDirichletSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mNeumannSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);

  impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mGlobalIdTag, MB_TAG_SPARSE | MB_TAG_CREAT, &zero);

  impl->tag_get_handle("WriteTemplate element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);
}

WriteTemplate::~WriteTemplate()
{
  mbImpl->release_interface(mWriteIface);
  mbImpl->tag_delete(mEntityMark);
}

void WriteTemplate::reset_matset(std::vector<WriteTemplate::MaterialSetData> &matset_info)
{
  std::vector<WriteTemplate::MaterialSetData>::iterator iter;

  for (iter = matset_info.begin(); iter != matset_info.end(); ++iter)
    delete (*iter).elements;
}

ErrorCode WriteTemplate::write_file(const char *file_name,
                                    const bool /* overwrite (commented out to remove warning) */,
                                    const FileOptions& /*opts*/,
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
  if (NULL == strstr(file_name, ".template"))
    return MB_FAILURE;

  std::vector<EntityHandle> matsets, dirsets, neusets;

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

  std::vector<WriteTemplate::MaterialSetData> matset_info;
  std::vector<WriteTemplate::DirichletSetData> dirset_info;
  std::vector<WriteTemplate::NeumannSetData> neuset_info;

  MeshInfo mesh_info;

  matset_info.clear();
  if (gather_mesh_information(mesh_info, matset_info, neuset_info, dirset_info,
                              matsets, neusets, dirsets) != MB_SUCCESS) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  // Try to open the file after gather mesh info succeeds
  if (/* Test for file open failure */ false) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (initialize_file(mesh_info) != MB_SUCCESS) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (write_nodes(mesh_info.num_nodes, mesh_info.nodes, mesh_info.num_dim) != MB_SUCCESS) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  if (write_matsets(mesh_info, matset_info, neuset_info)) {
    reset_matset(matset_info);
    return MB_FAILURE;
  }

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::gather_mesh_information(MeshInfo &mesh_info,
                                                 std::vector<WriteTemplate::MaterialSetData> &matset_info,
                                                 std::vector<WriteTemplate::NeumannSetData> &neuset_info,
                                                 std::vector<WriteTemplate::DirichletSetData> &dirset_info,
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
  mbImpl->tag_get_handle("WriteTemplate element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);

  int highest_dimension_of_element_matsets = 0;

  for (vector_iter = matsets.begin(); vector_iter != matsets.end(); ++vector_iter) {
    WriteTemplate::MaterialSetData matset_data;
    matset_data.elements = new Range;

    // For the purpose of qa records, get the parents of these matsets
    if (mbImpl->get_parent_meshsets(*vector_iter, parent_meshsets) != MB_SUCCESS)
      return MB_FAILURE;

    // Get all Entity Handles in the mesh set
    Range dummy_range;
    mbImpl->get_entities_by_handle(*vector_iter, dummy_range, true);

    // Find the dimension of the last entity in this range
    Range::iterator entity_iter = dummy_range.end();
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

    int dimension = CN::Dimension(entity_type);

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
  if (mesh_info.num_dim == 0) {
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
    WriteTemplate::DirichletSetData dirset_data;
    dirset_data.id = 0;
    dirset_data.number_nodes = 0;

    // Get the dirset's id
    if (mbImpl->tag_get_data(mDirichletSetTag, &(*vector_iter), 1, &id) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get id tag for dirset " << id);
    }

    dirset_data.id = id; 

    std::vector<EntityHandle> node_vector;
    // Get the nodes of the dirset that are in mesh_info.nodes
    if (mbImpl->get_entities_by_handle(*vector_iter, node_vector, true) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get nodes in dirset " << id);
    }

    std::vector<EntityHandle>::iterator iter, end_iter;
    iter = node_vector.begin();
    end_iter= node_vector.end();

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
  vector_iter = neusets.begin();
  end_vector_iter = neusets.end();

  for (; vector_iter != end_vector_iter; ++vector_iter) {
    WriteTemplate::NeumannSetData neuset_data;

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

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::get_valid_sides(Range &elems, const int sense,
                                         WriteTemplate::NeumannSetData &neuset_data)
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

ErrorCode WriteTemplate::write_nodes(const int num_nodes, const Range& nodes, const int dimension)
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

  if (transform_needed) {
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

  /* Template - write nodes to file here in some way */

  // Clean up
  delete [] coord_arrays[0];
  delete [] coord_arrays[1];
  if (coord_arrays[2])
    delete [] coord_arrays[2];

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::write_matsets(MeshInfo & /* mesh_info (commented out to remove warning) */,
                                       std::vector<WriteTemplate::MaterialSetData> &matset_data,
                                       std::vector<WriteTemplate::NeumannSetData> &/* neuset_data (commented out to remove warning) */)
{
  unsigned int i;
  std::vector<int> connect;
  const EntityHandle *connecth;
  int num_connecth;
  ErrorCode result;

  // Don't usually have anywhere near 31 nodes per element
  connect.reserve(31);
  Range::iterator rit;

  WriteTemplate::MaterialSetData matset;
  for (i = 0; i < matset_data.size(); i++) {
    matset = matset_data[i];

    for (rit = matset.elements->begin(); rit != matset.elements->end(); ++rit) {
      // Get the connectivity of this element
      result = mbImpl->get_connectivity(*rit, connecth, num_connecth);
      if (MB_SUCCESS != result)
        return result;

      // Get the vertex ids
      result = mbImpl->tag_get_data(mGlobalIdTag, connecth, num_connecth, &connect[0]);
      if (MB_SUCCESS != result)
        return result;

      // Write the data
      /* Template - write element connectivity here */

      if (/* Template - check for error condition! */ false)
        return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::initialize_file(MeshInfo &mesh_info)
{
  // Perform the initializations

  int coord_size, ncoords;

  coord_size = mesh_info.num_dim;
  std::cout << "Coord_size = " << coord_size << std::endl;
  /* Template - write coord size */

  ncoords = mesh_info.num_nodes;
  std::cout << "ncoords = " << ncoords << std::endl;
  /* Template - write num nodes*/

  /* Template - write information on the element types & numbers (depends
     on material and other sets) */

  /* Node coordinate arrays: */
  /* Template - initialize variable to hold coordinate arrays */

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::open_file(const char* filename)
{
  // Not a valid filename
  if (strlen((const char*)filename) == 0) {
    MB_SET_ERR(MB_FAILURE, "Output filename not specified");
  }

  /* Template - open file & store somewhere */

  // File couldn't be opened
  if (/* Template - check for file open error here! */ false) {
    MB_SET_ERR(MB_FAILURE, "Cannot open " << filename);
  }

  return MB_SUCCESS;
}

ErrorCode WriteTemplate::get_neuset_elems(EntityHandle neuset, int current_sense,
                                          Range &forward_elems, Range &reverse_elems)
{
  Range neuset_elems, neuset_meshsets;

  // Get the sense tag; don't need to check return, might be an error if the tag
  // hasn't been created yet
  Tag sense_tag = 0;
  mbImpl->tag_get_handle("SENSE", 1, MB_TYPE_INTEGER, sense_tag);

  // Get the entities in this set
  ErrorCode result = mbImpl->get_entities_by_handle(neuset, neuset_elems, true);
  if (MB_FAILURE == result)
    return result;

  // Now remove the meshsets into the neuset_meshsets; first find the first meshset,
  Range::iterator range_iter = neuset_elems.begin();
  while (TYPE_FROM_HANDLE(*range_iter) != MBENTITYSET && range_iter != neuset_elems.end())
    ++range_iter;

  // Then, if there are some, copy them into neuset_meshsets and erase from neuset_elems
  if (range_iter != neuset_elems.end()) {
    std::copy(range_iter, neuset_elems.end(), range_inserter(neuset_meshsets));
    neuset_elems.erase(range_iter, neuset_elems.end());
  }

  // OK, for the elements, check the sense of this set and copy into the right range
  // (if the sense is 0, copy into both ranges)

  // Need to step forward on list until we reach the right dimension
  Range::iterator dum_it = neuset_elems.end();
  --dum_it;
  int target_dim = CN::Dimension(TYPE_FROM_HANDLE(*dum_it));
  dum_it = neuset_elems.begin();
  while (target_dim != CN::Dimension(TYPE_FROM_HANDLE(*dum_it)) &&
         dum_it != neuset_elems.end())
    ++dum_it;

  if (current_sense == 1 || current_sense == 0)
    std::copy(dum_it, neuset_elems.end(), range_inserter(forward_elems));
  if (current_sense == -1 || current_sense == 0)
    std::copy(dum_it, neuset_elems.end(), range_inserter(reverse_elems));

  // Now loop over the contained meshsets, getting the sense of those and calling this
  // function recursively
  for (range_iter = neuset_meshsets.begin(); range_iter != neuset_meshsets.end(); ++range_iter) {
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
