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

#include "WriteNCDF.hpp"

#include "netcdf.h"
#include <utility>
#include <algorithm>
#include <time.h>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "moab/FileOptions.hpp"
#include "MBTagConventions.hpp"
#include "Internals.hpp"
#include "ExoIIUtil.hpp"
#include "moab/WriteUtilIface.hpp"
#include "exodus_order.h"

#ifndef MOAB_HAVE_NETCDF
#error Attempt to compile WriteNCDF with NetCDF support disabled
#endif

namespace moab {

const int TIME_STR_LEN = 11;

#define INS_ID(stringvar, prefix, id) \
  sprintf(stringvar, prefix, id)

#define GET_DIM(ncdim, name, val) \
  { \
    int gdfail = nc_inq_dimid(ncFile, name, &ncdim); \
    if (NC_NOERR == gdfail) { \
      size_t tmp_val; \
      gdfail = nc_inq_dimlen(ncFile, ncdim, &tmp_val); \
      if (NC_NOERR != gdfail) { \
        MB_SET_ERR(MB_FAILURE, "WriteNCDF:: couldn't get dimension length"); \
      } \
      else \
        val = tmp_val; \
    } \
    else \
      val = 0; \
  }

#define GET_DIMB(ncdim, name, varname, id, val) \
 INS_ID(name, varname, id); \
 GET_DIM(ncdim, name, val);

#define GET_VAR(name, id, dims) \
  { \
    id = -1; \
    int gvfail = nc_inq_varid(ncFile, name, &id); \
    if (NC_NOERR == gvfail) { \
      int ndims;\
      gvfail = nc_inq_varndims(ncFile, id, &ndims); \
      if (NC_NOERR == gvfail) { \
        dims.resize(ndims); \
        gvfail = nc_inq_vardimid(ncFile, id, &dims[0]); \
      } \
    } \
  }

WriterIface* WriteNCDF::factory(Interface* iface)
{
  return new WriteNCDF(iface);
}

WriteNCDF::WriteNCDF(Interface *impl)
  : mdbImpl(impl), ncFile(0), mCurrentMeshHandle(0), mGeomDimensionTag(0), repeat_face_blocks(0)
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

  int dum_val_array[] = {-1, -1, -1, -1};
  impl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                       mHasMidNodesTag, MB_TAG_SPARSE | MB_TAG_CREAT, dum_val_array);

  impl->tag_get_handle("distFactor", 0, MB_TYPE_DOUBLE, mDistFactorTag,
                       MB_TAG_SPARSE | MB_TAG_VARLEN | MB_TAG_CREAT);

  impl->tag_get_handle("qaRecord", 0, MB_TYPE_OPAQUE, mQaRecordTag,
                       MB_TAG_SPARSE | MB_TAG_VARLEN | MB_TAG_CREAT);

  impl->tag_get_handle("WriteNCDF element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);
}

WriteNCDF::~WriteNCDF()
{
  mdbImpl->release_interface(mWriteIface);

  mdbImpl->tag_delete(mEntityMark);

  if (0 != ncFile)
    ncFile = 0;
}

void WriteNCDF::reset_block(std::vector<MaterialSetData> &block_info)
{
  std::vector<MaterialSetData>::iterator iter;

  for (iter = block_info.begin(); iter != block_info.end(); ++iter) {
    iter->elements.clear();
  }
}

void WriteNCDF::time_and_date(char* time_string, char* date_string)
{
   struct tm* local_time;
   time_t calendar_time;

   calendar_time = time(NULL);
   local_time = localtime(&calendar_time);

   assert(NULL != time_string && NULL != date_string);

   strftime(time_string, TIME_STR_LEN, "%H:%M:%S", local_time);
   strftime(date_string, TIME_STR_LEN, "%m/%d/%Y", local_time);

   // Terminate with NULL character
   time_string[10] = (char)NULL;
   date_string[10] = (char)NULL;
}

ErrorCode WriteNCDF::write_file(const char *exodus_file_name,
                                const bool overwrite,
                                const FileOptions& opts,
                                const EntityHandle *ent_handles,
                                const int num_sets,
                                const std::vector<std::string> &qa_records,
                                const Tag*,
                                int,
                                int user_dimension)
{
  assert(0 != mMaterialSetTag &&
         0 != mNeumannSetTag &&
         0 != mDirichletSetTag);

  if (user_dimension == 0)
   mdbImpl->get_dimension(user_dimension);

  if (opts.get_null_option( "REPEAT_FACE_BLOCKS" ) == MB_SUCCESS)
    repeat_face_blocks=1;

  std::vector<EntityHandle> blocks, nodesets, sidesets, entities;

  // Separate into blocks, nodesets, sidesets

  if (num_sets == 0) {
    // Default to all defined block, nodeset and sideset-type sets
    Range this_range;
    mdbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mMaterialSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(blocks));
    this_range.clear();
    mdbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mDirichletSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(nodesets));
    this_range.clear();
    mdbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mNeumannSetTag, NULL, 1, this_range);
    std::copy(this_range.begin(), this_range.end(), std::back_inserter(sidesets));

    // If there is nothing to write, write everything as one block.
    if (blocks.empty() && nodesets.empty() && sidesets.empty()) {
      this_range.clear();
      for (int d = user_dimension; d > 0 && this_range.empty(); --d)
        mdbImpl->get_entities_by_dimension(0, d, this_range, false);
      if (this_range.empty())
        return MB_FILE_WRITE_ERROR;

      EntityHandle block_handle;
      int block_id = 1;
      mdbImpl->create_meshset(MESHSET_SET, block_handle);
      mdbImpl->tag_set_data(mMaterialSetTag, &block_handle, 1, &block_id);
      mdbImpl->add_entities(block_handle, this_range);
      blocks.push_back(block_handle);
    }
  }
  else {
    int dummy;
    for (const EntityHandle *iter = ent_handles; iter < ent_handles + num_sets; ++iter) {
      if (MB_SUCCESS == mdbImpl->tag_get_data(mMaterialSetTag, &(*iter), 1, &dummy) &&
          -1 != dummy)
        blocks.push_back(*iter);
      else if (MB_SUCCESS == mdbImpl->tag_get_data(mDirichletSetTag, &(*iter), 1, &dummy) &&
               -1 != dummy)
        nodesets.push_back(*iter);
      else if (MB_SUCCESS == mdbImpl->tag_get_data(mNeumannSetTag, &(*iter), 1, &dummy) &&
               -1 != dummy)
        sidesets.push_back(*iter);
    }
  }

  // If there is nothing to write just return.
  if (blocks.empty() && nodesets.empty() && sidesets.empty())
    return MB_FILE_WRITE_ERROR;

  // Try to get mesh information
  ExodusMeshInfo mesh_info;

  std::vector<MaterialSetData> block_info;
  std::vector<NeumannSetData> sideset_info;
  std::vector<DirichletSetData> nodeset_info;

  mesh_info.num_dim = user_dimension;

  if (qa_records.empty()) {
    // qa records are empty - initialize some MB-standard ones
    mesh_info.qaRecords.push_back("MB");
    mesh_info.qaRecords.push_back("0.99");
    char string1[80], string2[80];
    time_and_date(string2, string1);
    mesh_info.qaRecords.push_back(string2);
    mesh_info.qaRecords.push_back(string1);
  }
  else {
    // Constrained to multiples of 4 qa records
    assert(qa_records.size() % 4 == 0);

    std::copy(qa_records.begin(), qa_records.end(),
              std::back_inserter(mesh_info.qaRecords));
  }

  block_info.clear();
  if (gather_mesh_information(mesh_info, block_info, sideset_info, nodeset_info,
                              blocks, sidesets, nodesets) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  // Try to open the file after gather mesh info succeeds
  int fail = nc_create(exodus_file_name, overwrite ? NC_CLOBBER : NC_NOCLOBBER, &ncFile);
  if (NC_NOERR != fail) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  if (write_header(mesh_info, block_info, sideset_info,
                   nodeset_info, exodus_file_name) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  {
    // write dummy time_whole
    double timev = 0.0; // dummy, to make paraview happy
    size_t start =0, count =1;
    int nc_var;
    std::vector<int> dims;
    GET_VAR("time_whole", nc_var, dims);
    fail = nc_put_vara_double(ncFile, nc_var, &start, &count, &timev);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "Failed writing dist factor array");
    }
  }


  if (write_nodes(mesh_info.num_nodes, mesh_info.nodes, mesh_info.num_dim) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  if (!mesh_info.polyhedronFaces.empty())
  {
    if (write_poly_faces(mesh_info) != MB_SUCCESS)
    {
      reset_block(block_info);
      return MB_FAILURE;
    }
  }

  if (write_elementblocks(mesh_info, block_info)) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  // Write the three maps
  if (write_global_node_order_map(mesh_info.num_nodes, mesh_info.nodes) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  if (write_global_element_order_map(mesh_info.num_elements) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  if (write_element_order_map(mesh_info.num_elements) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

/*
 if (write_elementmap(mesh_info) != MB_SUCCESS)
   return MB_FAILURE;
*/

  if (write_BCs(sideset_info, nodeset_info) != MB_SUCCESS) {
    reset_block(block_info);
    return MB_FAILURE;
  }

  if (write_qa_records(mesh_info.qaRecords) != MB_SUCCESS)
    return MB_FAILURE;

  // Copy the qa records into the argument
  // mesh_info.qaRecords.swap(qa_records);
  // Close the file
  fail = nc_close(ncFile);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::gather_mesh_information(ExodusMeshInfo &mesh_info,
                                             std::vector<MaterialSetData> &block_info,
                                             std::vector<NeumannSetData> &sideset_info,
                                             std::vector<DirichletSetData> &nodeset_info,
                                             std::vector<EntityHandle> &blocks,
                                             std::vector<EntityHandle> &sidesets,
                                             std::vector<EntityHandle> &nodesets)
{
  ErrorCode rval;
  std::vector<EntityHandle>::iterator vector_iter, end_vector_iter;

  mesh_info.num_nodes = 0;
  mesh_info.num_elements = 0;
  mesh_info.num_elementblocks = 0;
  mesh_info.num_polyhedra_blocks = 0;

  int id = 0;

  vector_iter= blocks.begin();
  end_vector_iter = blocks.end();

  std::vector<EntityHandle> parent_meshsets;

  // Clean out the bits for the element mark
  rval = mdbImpl->tag_delete(mEntityMark);
  if (MB_SUCCESS != rval)
    return rval;
  rval = mdbImpl->tag_get_handle("WriteNCDF element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);
  if (MB_SUCCESS != rval)
    return rval;

  int highest_dimension_of_element_blocks = 0;

  for (vector_iter = blocks.begin(); vector_iter != blocks.end(); ++vector_iter) {
    MaterialSetData block_data;

    // For the purpose of qa records, get the parents of these blocks
    if (mdbImpl->get_parent_meshsets(*vector_iter, parent_meshsets) != MB_SUCCESS)
      return MB_FAILURE;

    // Get all Entity Handles in the mesh set
    Range dummy_range;
    rval = mdbImpl->get_entities_by_handle(*vector_iter, dummy_range, true);
    if (MB_SUCCESS != rval)
      return rval;

    // Skip empty blocks
    if (dummy_range.empty())
      continue;

    // Get the block's id
    if (mdbImpl->tag_get_data(mMaterialSetTag, &(*vector_iter), 1, &id) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get block id from a tag for an element block");
    }

    block_data.id = id;
    block_data.number_attributes = 0;

    // Wait a minute, we are doing some filtering here that doesn't make sense at this level  CJS

    // Find the dimension of the last entity in this range
    int this_dim = CN::Dimension(TYPE_FROM_HANDLE(dummy_range.back()));
    if (this_dim > 3) {
      MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Block " << id << " contains entity sets");
    }
    block_data.elements = dummy_range.subset_by_dimension(this_dim);

    // End of -- wait a minute, we are doing some filtering here that doesn't make sense at this level CJS

    // Get the entity type for this block, verifying that it's the same for all elements
    EntityType entity_type = TYPE_FROM_HANDLE(block_data.elements.front());
    if (!block_data.elements.all_of_type(entity_type)) {
      MB_SET_ERR(MB_FAILURE, "Entities in block " << id << " not of common type");
    }

    int dimension = -1;
    if (entity_type == MBQUAD || entity_type == MBTRI)
      dimension = 2; // Output shells by default
    else if (entity_type == MBEDGE)
      dimension = 1;
    else
      dimension = CN::Dimension(entity_type);

    if (dimension > highest_dimension_of_element_blocks)
      highest_dimension_of_element_blocks = dimension;

    std::vector<EntityHandle> tmp_conn;
    rval = mdbImpl->get_connectivity(&(block_data.elements.front()), 1, tmp_conn);
    if (MB_SUCCESS != rval)
      return rval;
    block_data.element_type = ExoIIUtil::get_element_type_from_num_verts(tmp_conn.size(), entity_type, dimension);

    if (block_data.element_type == EXOII_MAX_ELEM_TYPE) {
      MB_SET_ERR(MB_FAILURE, "Element type in block " << id << " didn't get set correctly");
    }

    if (block_data.element_type == EXOII_POLYGON) {
      // get all poly connectivity
      int numconn=0;
      for (Range::iterator eit=block_data.elements.begin(); eit!=block_data.elements.end(); eit++)
      {
        EntityHandle polg= *eit;
        int nnodes=0;
        const EntityHandle * conn = NULL;
        rval = mdbImpl->get_connectivity(polg, conn, nnodes); MB_CHK_ERR(rval);
        numconn+=nnodes;
      }
      block_data.number_nodes_per_element = numconn;
    }
    else
      block_data.number_nodes_per_element = ExoIIUtil::VerticesPerElement[block_data.element_type];

    // Number of nodes for this block
    block_data.number_elements = block_data.elements.size();

    // Total number of elements
    mesh_info.num_elements += block_data.number_elements;

    // Get the nodes for the elements
    rval = mWriteIface->gather_nodes_from_elements(block_data.elements, mEntityMark, mesh_info.nodes);
    if (MB_SUCCESS != rval)
      return rval;

    // if polyhedra block
    if(EXOII_POLYHEDRON==block_data.element_type)
    {
      rval = mdbImpl->get_connectivity(block_data.elements,
          mesh_info.polyhedronFaces); MB_CHK_ERR(rval);
      mesh_info.num_polyhedra_blocks++;
    }

    if (!sidesets.empty()) {
      // If there are sidesets, keep track of which elements are being written out
      for (Range::iterator iter = block_data.elements.begin();
          iter != block_data.elements.end(); ++iter) {
        unsigned char bit = 0x1;
        rval = mdbImpl->tag_set_data(mEntityMark, &(*iter), 1, &bit);
        if (MB_SUCCESS != rval)
          return rval;
      }
    }

    block_info.push_back(block_data);

    const void* data = NULL;
    int size = 0;
    if (MB_SUCCESS == mdbImpl->tag_get_by_ptr(mQaRecordTag, &(*vector_iter), 1, &data, &size) &&
        NULL != data) {
      // There are qa records on this block - copy over to mesh qa records
      const char* qa_rec = static_cast<const char*>(data);
      int start = 0;
      int count = 0;
      for (int i = 0; i < size; i++) {
        if (qa_rec[i] == '\0') {
          std::string qa_string(&qa_rec[start], i - start);
          mesh_info.qaRecords.push_back(qa_string);
          start = i + 1;
          count++;
        }
      }

      // Constrained to multiples of 4 qa records
      if (count > 0)
        assert(count % 4 == 0);
    }
  }

  mesh_info.num_elementblocks = block_info.size();

  for (std::vector<MaterialSetData>::iterator blit=block_info.begin(); blit!=block_info.end(); blit++)
  {
    MaterialSetData & block = *blit;
    if (block.element_type!=EXOII_POLYHEDRON)
    {
      mesh_info.polyhedronFaces = subtract(mesh_info.polyhedronFaces, block.elements);
    }
  }

  // If user hasn't entered dimension, we figure it out
  if (mesh_info.num_dim == 0) {
    // Never want 1 or zero dimensions
    if (highest_dimension_of_element_blocks < 2)
      mesh_info.num_dim = 3;
    else
      mesh_info.num_dim = highest_dimension_of_element_blocks;
  }

  Range::iterator range_iter, end_range_iter;
  range_iter = mesh_info.nodes.begin();
  end_range_iter = mesh_info.nodes.end();

  mesh_info.num_nodes = mesh_info.nodes.size(); 

  //------nodesets--------

  vector_iter= nodesets.begin();
  end_vector_iter = nodesets.end();

  for ( ; vector_iter != end_vector_iter; ++vector_iter) {
    DirichletSetData nodeset_data;
    nodeset_data.id = 0;
    nodeset_data.number_nodes = 0;

    // Get the nodeset's id
    if (mdbImpl->tag_get_data(mDirichletSetTag,&(*vector_iter), 1,&id) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get id tag for nodeset " << id);
    }

    nodeset_data.id = id;

    std::vector<EntityHandle> node_vector;
    // Get the nodes of the nodeset that are in mesh_info.nodes
    if (mdbImpl->get_entities_by_handle(*vector_iter, node_vector, true) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Couldn't get nodes in nodeset " << id);
    }

    // Get the tag for distribution factors
    const double *dist_factor_vector;
    int dist_factor_size;
    const void* ptr = 0;

    int has_dist_factors = 0;
    if (mdbImpl->tag_get_by_ptr(mDistFactorTag, &(*vector_iter), 1, &ptr, &dist_factor_size) == MB_SUCCESS &&
       dist_factor_size)
      has_dist_factors = 1;
    dist_factor_size /= sizeof(double);
    dist_factor_vector = reinterpret_cast<const double*>(ptr);
    std::vector<EntityHandle>::iterator iter, end_iter;
    iter = node_vector.begin();
    end_iter= node_vector.end();

    int j = 0;
    unsigned char node_marked = 0;
    ErrorCode result;
    for ( ; iter != end_iter; ++iter) {
      if (TYPE_FROM_HANDLE(*iter) != MBVERTEX)
        continue;
      result = mdbImpl->tag_get_data(mEntityMark, &(*iter), 1, &node_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

      if (0x1 == node_marked) {
        nodeset_data.nodes.push_back(*iter);
        if (0 != has_dist_factors)
          nodeset_data.node_dist_factors.push_back(dist_factor_vector[j]);
        else
          nodeset_data.node_dist_factors.push_back(1.0);
      }
      j++;
    }

    nodeset_data.number_nodes = nodeset_data.nodes.size(); 
    nodeset_info.push_back(nodeset_data);
  }

  //------sidesets--------
  vector_iter= sidesets.begin();
  end_vector_iter = sidesets.end();

  for ( ; vector_iter != end_vector_iter; ++vector_iter) {
    NeumannSetData sideset_data;

    // Get the sideset's id
    if (mdbImpl->tag_get_data(mNeumannSetTag, &(*vector_iter), 1,&id) != MB_SUCCESS)
      return MB_FAILURE;

    sideset_data.id = id;
    sideset_data.mesh_set_handle = *vector_iter;

    // Get the sides in two lists, one forward the other reverse; starts with forward sense
    // by convention
    Range forward_elems, reverse_elems;
    if (get_sideset_elems(*vector_iter, 0, forward_elems, reverse_elems) == MB_FAILURE)
      return MB_FAILURE;

    ErrorCode result = get_valid_sides(forward_elems, mesh_info, 1, sideset_data);MB_CHK_SET_ERR(result, "Couldn't get valid sides data");
    result = get_valid_sides(reverse_elems, mesh_info, -1, sideset_data);MB_CHK_SET_ERR(result, "Couldn't get valid sides data");

    sideset_data.number_elements = sideset_data.elements.size();
    sideset_info.push_back(sideset_data);
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::get_valid_sides(Range &elems, ExodusMeshInfo& /*mesh_info*/,
                                     const int sense,
                                     NeumannSetData &sideset_data)
{
  // This is where we see if underlying element of side set element is included in output

  // Get the sideset-based info for distribution factors
  const double *dist_factor_vector = 0;
  int dist_factor_size = 0;

  // Initialize dist_fac_iter to get rid of compiler warning
  const double* dist_fac_iter = 0;
  const void* ptr = 0;
  bool has_dist_factors = false;
  if (mdbImpl->tag_get_by_ptr(mDistFactorTag,
                             &(sideset_data.mesh_set_handle), 1, &ptr, &dist_factor_size) == MB_SUCCESS &&
      dist_factor_size) {
    has_dist_factors = true;
    dist_factor_vector = reinterpret_cast<const double*>(ptr);
    dist_fac_iter = dist_factor_vector;
    dist_factor_size /= sizeof(double);
  }

  unsigned char element_marked = 0;
  ErrorCode result;
  for (Range::iterator iter = elems.begin(); iter != elems.end(); ++iter) {
    // Should insert here if "side" is a quad/tri on a quad/tri mesh
    result = mdbImpl->tag_get_data(mEntityMark, &(*iter), 1, &element_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

    if (0x1 == element_marked) {
      sideset_data.elements.push_back(*iter);

      // TJT TODO: the sense should really be # edges + 1or2
      sideset_data.side_numbers.push_back((sense == 1 ? 1 : 2));
    }
    else { // Then "side" is probably a quad/tri on a hex/tet mesh
      std::vector<EntityHandle> parents;
      int dimension = CN::Dimension(TYPE_FROM_HANDLE(*iter));

      // Get the adjacent parent element of "side"
      if (mdbImpl->get_adjacencies(&(*iter), 1, dimension + 1, false, parents) != MB_SUCCESS) {
#       if 0
        // This is not treated as an error, print warning messages for
        // debugging only
        fprintf(stderr, "[Warning]: Couldn't get adjacencies for sideset.\n");
#       endif
      }

      if (!parents.empty()) {
        // Make sure the adjacent parent element will be output
        for (unsigned int k = 0; k < parents.size(); k++) {
          result = mdbImpl->tag_get_data(mEntityMark, &(parents[k]), 1, &element_marked);MB_CHK_SET_ERR(result, "Couldn't get mark data");

          int side_no, this_sense, this_offset;
          if (0x1 == element_marked &&
              mdbImpl->side_number(parents[k], *iter, side_no,
                                   this_sense, this_offset) == MB_SUCCESS &&
              this_sense == sense) {
            sideset_data.elements.push_back(parents[k]);
            sideset_data.side_numbers.push_back(side_no + 1);
            break;
          }
        }
      }
      else {
#       if 0
        // This is not treated as an error, print warning messages for
        // debugging only
        fprintf(stderr, "[Warning]: No parent element exists for element in sideset %i\n", sideset_data.id);
#       endif
      }
    }

    if (sideset_data.elements.size() != 0) {
      // Distribution factors
      int num_nodes = CN::VerticesPerEntity(TYPE_FROM_HANDLE(*iter));
      // put some dummy dist factors for polygons; why do we need them?
      if (TYPE_FROM_HANDLE(*iter)==MBPOLYGON) num_nodes = 1; //dummy
      if (has_dist_factors) {
        std::copy(dist_fac_iter, dist_fac_iter + num_nodes, 
                  std::back_inserter(sideset_data.ss_dist_factors));
        dist_fac_iter += num_nodes;
      }
      else {
        for (int j = 0; j < num_nodes; j++)
          sideset_data.ss_dist_factors.push_back(1.0);
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_qa_records(std::vector<std::string> &qa_record_list)
{
  int i = 0;

  for (std::vector<std::string>::iterator string_it = qa_record_list.begin();
      string_it != qa_record_list.end(); ) {
    for (int j = 0; j < 4; j++)
      write_qa_string((*string_it++).c_str(), i, j);
    i++;
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_qa_string(const char *string,
                                     int record_number,
                                     int record_position)
{
  // Get the variable id in the exodus file

  std::vector<int> dims;
  int temp_var = -1;
  GET_VAR("qa_records", temp_var, dims);
  if (-1 == temp_var) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF:: Problem getting qa record variable");
  }
  size_t count[3], start[3];

  // Write out the record
  start[0] = record_number;
  start[1] = record_position;
  start[2] = 0;

  count[0] = 1;
  count[1] = 1;
  count[2] = (long)strlen(string) + 1;
  int fail = nc_put_vara_text(ncFile, temp_var, start, count, string);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Failed to position qa string variable");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_nodes(int num_nodes, Range& nodes, int dimension)
{
  // Write coordinates names
  int nc_var = -1;
  std::vector<int> dims;
  GET_VAR("coor_names", nc_var, dims);
  if (-1 == nc_var) {
    MB_SET_ERR(MB_FAILURE, "Trouble getting coordinate name variable");
  }

  size_t start[2] = {0, 0}, count[2] = {1, ExoIIInterface::MAX_STR_LENGTH};
  char dum_str[ExoIIInterface::MAX_STR_LENGTH];
  strcpy(dum_str, "x");
  int fail = nc_put_vara_text(ncFile, nc_var, start, count, dum_str);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble adding x coordinate name; netcdf message: " << nc_strerror(fail));
  }

  start[0] = 1;
  strcpy(dum_str, "y");
  fail = nc_put_vara_text(ncFile, nc_var, start, count, dum_str);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble adding y coordinate name; netcdf message: " << nc_strerror(fail));
  }

  start[0] = 2;
  strcpy(dum_str, "z");
  fail = nc_put_vara_text(ncFile, nc_var, start, count, dum_str);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble adding z coordinate name; netcdf message: " << nc_strerror(fail));
  }

  // See if should transform coordinates
  ErrorCode result;
  Tag trans_tag;
  result = mdbImpl->tag_get_handle(MESH_TRANSFORM_TAG_NAME, 16, MB_TYPE_DOUBLE, trans_tag);
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
 
  result = mWriteIface->get_node_coords(dimension, num_nodes, nodes, mGlobalIdTag, 1, coord_arrays);
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
    result = mdbImpl->tag_get_data(trans_tag, &mesh, 0, trans_matrix);MB_CHK_SET_ERR(result, "Couldn't get transform data");

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
  nc_var = -1;
  GET_VAR("coord", nc_var, dims);
  if (-1 == nc_var) {
    MB_SET_ERR(MB_FAILURE, "Trouble getting coordinate variable");
  }
  start[0] = 0;
  count[1] = num_nodes;
  fail = nc_put_vara_double(ncFile, nc_var, start, count, &(coord_arrays[0][0]));
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble writing x coordinate");
  }

  start[0] = 1;
  fail = nc_put_vara_double(ncFile, nc_var, start, count, &(coord_arrays[1][0]));
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble writing y coordinate");
  }

  start[0] = 2;
  fail = nc_put_vara_double(ncFile, nc_var, start, count, &(coord_arrays[2][0]));
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble writing z coordinate");
  }

  delete [] coord_arrays[0];
  delete [] coord_arrays[1];
  if (coord_arrays[2])
    delete [] coord_arrays[2];

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_poly_faces(ExodusMeshInfo& mesh_info)
{
  // write all polygons that are not in another element block;
  // usually they are nowhere else, but be sure, write in this block only ones that are not in the other blocks
  Range pfaces=mesh_info.polyhedronFaces;

  /*
   * int fbconn1(num_nod_per_fa1) ;
              fbconn1:elem_type = "nsided" ;
      int fbepecnt1(num_fa_in_blk1) ;
              fbepecnt1:entity_type1 = "NODE" ;
              fbepecnt1:entity_type2 = "FACE" ;
   */
  if (pfaces.empty())
    return MB_SUCCESS;
  char wname[80];
  int nc_var = -1;
  std::vector<int> dims;

  // write one for each element block, to make paraview and visit happy
  int num_faces_in_block = (int) pfaces.size();
  for (unsigned int bl = 0; bl<mesh_info.num_polyhedra_blocks; bl++)
  {
    INS_ID(wname, "fbconn%u",  bl+1); // it is the first block
    GET_VAR(wname, nc_var, dims); // fbconn# variable, 1 dimensional

    INS_ID(wname, "num_nod_per_fa%u", bl+1);
    int ncdim, num_nod_per_face;
    GET_DIM(ncdim, wname, num_nod_per_face);
    int * connectivity = new int [num_nod_per_face];
    int ixcon=0, j=0;
    std::vector<int> fbepe(num_faces_in_block); // fbepecnt1
    for (Range::iterator eit=pfaces.begin(); eit!=pfaces.end(); eit++)
    {
      EntityHandle polyg= *eit;
      int nnodes=0;
      const EntityHandle * conn = NULL;
      ErrorCode rval  = mdbImpl->get_connectivity(polyg, conn, nnodes); MB_CHK_ERR(rval);
      for (int k=0; k<nnodes; k++)
        connectivity[ixcon++] = conn[k];
      fbepe[j++]= nnodes;
    }
    size_t start[1] = { 0}, count[1] = {0};
    count[0]= ixcon;
    int fail = nc_put_vara_int(ncFile, nc_var, start, count, connectivity);
    if (NC_NOERR != fail) {
      delete [] connectivity;
      MB_SET_ERR(MB_FAILURE, "Couldn't write fbconn variable");
    }


    INS_ID(wname, "fbepecnt%u", bl+1);
    GET_VAR(wname, nc_var, dims); // fbconn# variable, 1 dimensional
    count[0]= num_faces_in_block;

    fail = nc_put_vara_int(ncFile, nc_var, start, count, &fbepe[0]);
    if (NC_NOERR != fail) {
      delete [] connectivity;
      MB_SET_ERR(MB_FAILURE, "Couldn't write fbepecnt variable");
    }

    int id = bl+1;
    if (write_exodus_integer_variable("fa_prop1", &id, bl, 1) != MB_SUCCESS) {
      MB_SET_ERR_CONT("Problem writing element block id " << id);
    }

    int status = 1;
    if (write_exodus_integer_variable("fa_status", &status, bl, 1) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Problem writing face block status");
    }

    delete [] connectivity;
    if (0==repeat_face_blocks)
      break; // do not repeat face blocks
  }

  return MB_SUCCESS;
}
ErrorCode WriteNCDF::write_header(ExodusMeshInfo& mesh_info,
                                  std::vector<MaterialSetData> &block_info,
                                  std::vector<NeumannSetData> &sideset_info,
                                  std::vector<DirichletSetData> &nodeset_info,
                                  const char *filename)
{
  // Get the date and time
  char time[TIME_STR_LEN];
  char date[TIME_STR_LEN];
  time_and_date(time, date);

  std::string title_string = "MOAB";
  title_string.append("(");
  title_string.append(filename);
  title_string.append("): ");
  title_string.append(date);
  title_string.append(": ");
  title_string.append("time ");

  if (title_string.length() > ExoIIInterface::MAX_LINE_LENGTH)
    title_string.resize(ExoIIInterface::MAX_LINE_LENGTH);

  // Initialize the exodus file

  int result = initialize_exodus_file(mesh_info,
                                      block_info, sideset_info,
                                      nodeset_info, title_string.c_str());

  if (result == MB_FAILURE)
    return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_elementblocks(ExodusMeshInfo& mesh_info, std::vector<MaterialSetData> &block_data)
{
  unsigned int i;
  int block_index = 0; // Index into block list, may != 1 if there are inactive blocks
  int exodus_id = 1;

  for (i = 0; i < block_data.size(); i++) {
    MaterialSetData& block = block_data[i];

    unsigned int num_nodes_per_elem = block.number_nodes_per_element;

    // Write out the id for the block

    int id = block.id;
    int num_values = 1;

    if (write_exodus_integer_variable("eb_prop1", &id, block_index, num_values) != MB_SUCCESS) {
      MB_SET_ERR_CONT("Problem writing element block id " << id);
    }

    // Write out the block status

    int status = 1;
    if (0 == block.number_elements) {
      MB_SET_ERR(MB_FAILURE, "No elements in block " << id);
    }

    if (write_exodus_integer_variable("eb_status", &status, block_index, num_values) != MB_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Problem writing element block status");
    }

    //
    // Map the connectivity to the new nodes
    const unsigned int num_elem = block.number_elements;
    unsigned int num_nodes = num_nodes_per_elem * num_elem;
    if (EXOII_POLYGON==block.element_type || EXOII_POLYHEDRON==block.element_type)
    {
      num_nodes = num_nodes_per_elem;
    }
    int* connectivity = new int[num_nodes];

    ErrorCode result =MB_SUCCESS;
    if (block.element_type != EXOII_POLYHEDRON)
      mWriteIface->get_element_connect(num_elem, num_nodes_per_elem,
        mGlobalIdTag, block.elements, mGlobalIdTag, exodus_id, connectivity);
    if (result != MB_SUCCESS) {
      delete [] connectivity;
      MB_SET_ERR(result, "Couldn't get element array to write from");
    }

    // If necessary, convert from EXODUS to CN node order
    const EntityType elem_type = ExoIIUtil::ExoIIElementMBEntity[block.element_type];
    assert(block.elements.all_of_type(elem_type));
    const int* reorder = 0;
    if (block.element_type != EXOII_POLYHEDRON && block.element_type != EXOII_POLYGON)
      reorder = exodus_elem_order_map[elem_type][block.number_nodes_per_element];
    if (reorder)
      WriteUtilIface::reorder(reorder, connectivity,
                              block.number_elements,
                              block.number_nodes_per_element);

    char wname[80];
    int nc_var = -1;
    std::vector<int> dims;
    if (block.element_type != EXOII_POLYHEDRON)
    {
      exodus_id += num_elem;
      INS_ID(wname, "connect%u", i + 1);

      GET_VAR(wname, nc_var, dims);
      if (-1 == nc_var) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't get connectivity variable");
      }
    }

    if (EXOII_POLYGON==block.element_type)
    {
      size_t start[1] = {0}, count[1] = {num_nodes_per_elem};
      int fail = nc_put_vara_int(ncFile, nc_var, start, count, connectivity);
      if (NC_NOERR != fail) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't write connectivity variable");
      }
      // now put also number ebepecnt1
      INS_ID(wname, "ebepecnt%u", i + 1);
      GET_VAR(wname, nc_var, dims);
      count[0] = block.number_elements;
      start[0]=0;
      // reuse connectivity array, to not allocate another one
      int j=0;
      for (Range::iterator eit=block.elements.begin(); eit!=block.elements.end(); j++, eit++)
      {
        EntityHandle polg= *eit;
        int nnodes=0;
        const EntityHandle * conn = NULL;
        ErrorCode rval = mdbImpl->get_connectivity(polg, conn, nnodes); MB_CHK_ERR(rval);
        connectivity[j]= nnodes;
      }
      fail = nc_put_vara_int(ncFile, nc_var, start, count, connectivity);
      if (NC_NOERR != fail) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't write ebepecnt variable");
      }
    }
    else if (block.element_type != EXOII_POLYHEDRON)
    {
      size_t start[2] = {0, 0}, count[2] = {num_elem, num_nodes_per_elem};
      int fail = nc_put_vara_int(ncFile, nc_var, start, count, connectivity);
      if (NC_NOERR != fail) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't write connectivity variable");
      }
    }
    else //if (block.element_type == EXOII_POLYHEDRON)
    {
      /* write a lot of stuff // faconn
      num_fa_in_blk1 = 15 ;
      num_nod_per_fa1 = 58 ;
      num_el_in_blk1 = 3 ;
      num_fac_per_el1 = 17 ;
      int fbconn1(num_nod_per_fa1) ;
              fbconn1:elem_type = "nsided" ;
      int fbepecnt1(num_fa_in_blk1) ;
              fbepecnt1:entity_type1 = "NODE" ;
              fbepecnt1:entity_type2 = "FACE" ;
      int facconn1(num_fac_per_el1) ;
              facconn1:elem_type = "NFACED" ;
      int ebepecnt1(num_el_in_blk1) ;
              ebepecnt1:entity_type1 = "FACE" ;
              ebepecnt1:entity_type2 = "ELEM" ;

       */
      Range & block_faces = mesh_info.polyhedronFaces;
      //ErrorCode rval = mdbImpl->get_connectivity(block.elements, block_faces); MB_CHK_ERR(rval);

      // reuse now connectivity for facconn1
      INS_ID(wname, "facconn%u", i + 1);
      GET_VAR(wname, nc_var, dims); // fbconn# variable, 1 dimensional

      std::vector<int> ebepe(block.elements.size()); // ebepecnt1
      int ixcon=0, j=0;
      size_t start[1] = {0}, count[1] = {0};

      for (Range::iterator eit=block.elements.begin(); eit!=block.elements.end(); eit++)
      {
        EntityHandle polyh= *eit;
        int nfaces=0;
        const EntityHandle * conn = NULL;
        ErrorCode rval = mdbImpl->get_connectivity(polyh, conn, nfaces); MB_CHK_ERR(rval);
        for (int k=0; k<nfaces; k++)
        {
          int index = block_faces.index(conn[k]);
          if (index==-1)
            MB_SET_ERR(MB_FAILURE, "Couldn't find face in polyhedron");
          connectivity[ixcon++] = index+1;
        }
        ebepe[j++] = nfaces;
        //num_faces+=nfaces;
      }
      count[0]= ixcon; // facconn1
      int fail = nc_put_vara_int(ncFile, nc_var, start, count, connectivity);
      if (NC_NOERR != fail) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't write fbconn variable");
      }


      INS_ID(wname, "ebepecnt%u", i + 1);
      GET_VAR(wname, nc_var, dims); // ebepecnt# variable, 1 dimensional
      count[0]= block.elements.size();

      fail = nc_put_vara_int(ncFile, nc_var, start, count, &ebepe[0]);
      if (NC_NOERR != fail) {
        delete [] connectivity;
        MB_SET_ERR(MB_FAILURE, "Couldn't write fbepecnt variable");
      }

    }
    block_index++;
    delete [] connectivity;
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_global_node_order_map(int num_nodes, Range& nodes)
{
  // Note: this routine bypasses the standard exodusII interface for efficiency!

  // Node order map
  int* map = new int[num_nodes];

  // For now, output a dummy map!

  Range::iterator range_iter, end_iter;
  range_iter = nodes.begin();
  end_iter = nodes.end();

  int i = 0;

  for ( ; range_iter != end_iter; ++range_iter) {
     // TODO -- do we really want to cast this to an int?
     map[i++] = (int)ID_FROM_HANDLE(*range_iter);
  }

  // Output array and cleanup

  int error = write_exodus_integer_variable("node_num_map",
                                            map,
                                            0,
                                            num_nodes);

  if (map)
    delete [] map;

  if (error < 0) {
    MB_SET_ERR(MB_FAILURE, "Failed writing global node order map");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_global_element_order_map(int num_elements)
{
  // Allocate map array
  int* map = new int[num_elements];

  // Many Sandia codes assume this map is unique, and CUBIT does not currently
  // have unique ids for all elements. Therefore, to make sure nothing crashes,
  // insert a dummy map...

  for (int i = 0; i < num_elements; i++)
     map[i] = i + 1;

  // Output array and cleanup

  int error = write_exodus_integer_variable("elem_num_map",
                                            map,
                                            0,
                                            num_elements);

  if (map)
    delete [] map;

  if (error < 0) {
    MB_SET_ERR(MB_FAILURE, "Failed writing global element order map");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_element_order_map(int num_elements)
{
  // Note: this routine bypasses the standard exodusII interface for efficiency!

  // Element order map
  int* map = new int[num_elements];

  // For now, output a dummy map!

  for (int i = 0; i < num_elements; i++) {
    map[i] = i + 1;
  }

  // Output array and cleanup

  int error = write_exodus_integer_variable("elem_map",
                                            map,
                                            0,
                                            num_elements);

  if (map)
    delete [] map;

  if (error < 0) {
    MB_SET_ERR(MB_FAILURE, "Failed writing element map");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_exodus_integer_variable(const char* variable_name,
                                                   int *variable_array,
                                                   int start_position,
                                                   int number_values)
{
  // Note: this routine bypasses the standard exodusII interface for efficiency!

  // Write directly to netcdf interface for efficiency

  // Get the variable id of the element map
  int nc_var = -1;
  std::vector<int> dims;
  GET_VAR(variable_name, nc_var, dims);
  if (-1 == nc_var) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to locate variable " << variable_name << " in file");
  }
  // This contortion is necessary because netCDF is expecting nclongs;
  // fortunately it's necessary only when ints and nclongs aren't the same size

  size_t start[1], count[1];
  start[0] = start_position;
  count[0] = number_values;

  int fail = NC_NOERR;
  if (sizeof(int) == sizeof(long)) {
    fail = nc_put_vara_int(ncFile, nc_var, start, count, variable_array);
  }
  else {
    long *lptr = new long[number_values];
    for (int jj = 0; jj < number_values; jj++)
      lptr[jj] = variable_array[jj];
    fail = nc_put_vara_long(ncFile, nc_var, start, count, lptr);
    delete [] lptr;
  }

  if (NC_NOERR != fail) {
     MB_SET_ERR(MB_FAILURE, "Failed to store variable " << variable_name);
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::write_BCs(std::vector<NeumannSetData> &sidesets,
                               std::vector<DirichletSetData> &nodesets)
{
  unsigned int i, j;
  int id;
  int ns_index = -1;

  for (std::vector<DirichletSetData>::iterator ns_it = nodesets.begin();
      ns_it != nodesets.end(); ++ns_it) {
    // Get number of nodes in set
    int number_nodes = (*ns_it).number_nodes;
    if (0 == number_nodes)
      continue;

    // If we're here, we have a non-empty nodeset; increment the index
    ns_index++;

    // Get the node set id
    id = (*ns_it).id;

    // Build new array to old exodus ids
    int * exodus_id_array = new int[number_nodes];
    double * dist_factor_array = new double[number_nodes];

    std::vector<EntityHandle>::iterator begin_iter, end_iter;
    std::vector<double>::iterator other_iter;
    begin_iter = (*ns_it).nodes.begin();
    end_iter = (*ns_it).nodes.end();
    other_iter = (*ns_it).node_dist_factors.begin();

    j = 0;
    int exodus_id;
    ErrorCode result;
    // Fill up node array and dist. factor array at the same time
    for ( ; begin_iter != end_iter; ++begin_iter) {
      result = mdbImpl->tag_get_data(mGlobalIdTag, &(*begin_iter), 1, &exodus_id);MB_CHK_SET_ERR(result, "Problem getting id tag data");

      exodus_id_array[j] = exodus_id;
      dist_factor_array[j] = *(other_iter);
      ++other_iter;
      j++;
    }

    // Write out the id for the nodeset

    int num_values = 1;

    result = write_exodus_integer_variable("ns_prop1",
                                           &id, ns_index, num_values);MB_CHK_SET_ERR_RET_VAL(result, "Problem writing node set id " << id, MB_FAILURE);

    // Write out the nodeset status

    int status = 1;
    if (!number_nodes)
      status = 0;

    result = write_exodus_integer_variable("ns_status",
                                           &status, ns_index, num_values);MB_CHK_SET_ERR_RET_VAL(result, "Problem writing node set status", MB_FAILURE);

    // Write it out
    char wname[80];
    int nc_var = -1;
    std::vector<int> dims;
    INS_ID(wname, "node_ns%d", ns_index + 1);
    GET_VAR(wname, nc_var, dims);
    if (-1 == nc_var) {
      MB_SET_ERR(MB_FAILURE, "Failed to get node_ns variable");
    }

    size_t start = 0, count = number_nodes;
    int fail = nc_put_vara_int(ncFile, nc_var, &start, &count, exodus_id_array);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "Failed writing exodus id array");
    }

    // Write out nodeset distribution factors
    INS_ID(wname, "dist_fact_ns%d", ns_index + 1);
    nc_var = -1;
    GET_VAR(wname, nc_var, dims);
    if (-1 == nc_var) {
      MB_SET_ERR(MB_FAILURE, "Failed to get dist_fact variable");
    }
    fail = nc_put_vara_double(ncFile, nc_var, &start, &count, dist_factor_array);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "Failed writing dist factor array");
    }

    delete [] dist_factor_array;
    delete [] exodus_id_array;
  }

  // Now do sidesets
  int ss_index = 0; // Index of sideset - not the same as 'i' because
                    // only writing non-empty side sets
  for (i = 0; i < sidesets.size(); i++) {
    NeumannSetData sideset_data = sidesets[i];

    // Get the side set id
    int side_set_id = sideset_data.id;

    // Get number of elements in set
    int number_elements = sideset_data.number_elements;
    if (0 == number_elements)
      continue;

    // Build new array to old exodus ids
    int * output_element_ids = new int[number_elements];
    int * output_element_side_numbers = new int[number_elements];

    std::vector<EntityHandle>::iterator begin_iter, end_iter;
    begin_iter = sideset_data.elements.begin();
    end_iter = sideset_data.elements.end();
    std::vector<int>::iterator side_iter = sideset_data.side_numbers.begin();

    // Get the tag handle
    j = 0;
    int exodus_id;

    // For each "side"
    for ( ; begin_iter != end_iter; ++begin_iter, ++side_iter) {
      ErrorCode result = mdbImpl->tag_get_data(mGlobalIdTag,
                                               &(*begin_iter),
                                               1, &exodus_id);MB_CHK_SET_ERR(result, "Problem getting exodus id for sideset element " << (long unsigned int)ID_FROM_HANDLE(*begin_iter));

      output_element_ids[j] = exodus_id;
      output_element_side_numbers[j++] = *side_iter;
    }

    if (0 != number_elements) {
      // Write out the id for the nodeset

      int num_values = 1;

      // ss_prop1[ss_index] = side_set_id
      ErrorCode result = write_exodus_integer_variable("ss_prop1",
                                                       &side_set_id,
                                                       ss_index, num_values);MB_CHK_SET_ERR_RET_VAL(result, "Problem writing node set id " << id, MB_FAILURE);

      // FIXME : Something seems wrong here.  The we are within a block
      // started with if (0 != number_elements), so this condition is always
      // false.  This code seems to indicate that we want to write all
      // sidesets, not just empty ones.  But the code both here and in
      // initialize_exodus_file() skip empty side sets.
      //  - j.k. 2007-03-09
      int status = 1;
      if (0 == number_elements)
        status = 0;

      // ss_status[ss_index] = status
      result = write_exodus_integer_variable("ss_status",
                                             &status,
                                             ss_index, num_values);MB_CHK_SET_ERR_RET_VAL(result, "Problem writing side set status", MB_FAILURE);

      // Increment ss_index now because we want a) we need to
      // increment it somewhere within the if (0 != number_elements)
      // block and b) the above calls need a zero-based index
      // while the following use a one-based index.
      ++ss_index;

      char wname[80];
      int nc_var;
      std::vector<int> dims;
      INS_ID(wname, "elem_ss%d", ss_index);
      GET_VAR(wname, nc_var, dims);
      if (-1 == nc_var) {
        MB_SET_ERR(MB_FAILURE, "Failed to get elem_ss variable");
      }
      size_t start = 0, count = number_elements;
      int fail = nc_put_vara_int(ncFile, nc_var, &start, &count, output_element_ids);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "Failed writing sideset element array");
      }

      INS_ID(wname, "side_ss%d", ss_index);
      nc_var = -1;
      GET_VAR(wname, nc_var, dims);
      if (-1 == nc_var) {
        MB_SET_ERR(MB_FAILURE, "Failed to get side_ss variable");
      }
      fail = nc_put_vara_int(ncFile, nc_var, &start, &count, output_element_side_numbers);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "Failed writing sideset side array");
      }

      INS_ID(wname, "dist_fact_ss%d", ss_index);
      nc_var = -1;
      GET_VAR(wname, nc_var, dims);
      if (-1 == nc_var) {
        MB_SET_ERR(MB_FAILURE, "Failed to get sideset dist factors variable");
      }
      count = sideset_data.ss_dist_factors.size();
      fail = nc_put_vara_double(ncFile, nc_var, &start, &count, &(sideset_data.ss_dist_factors[0]));
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "Failed writing sideset dist factors array");
      }
    }

    delete [] output_element_ids;
    delete [] output_element_side_numbers;
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::initialize_exodus_file(ExodusMeshInfo &mesh_info,
                                            std::vector<MaterialSetData> &block_data,
                                            std::vector<NeumannSetData> & sideset_data,
                                            std::vector<DirichletSetData> & nodeset_data,
                                            const char* title_string,
                                            bool write_maps,
                                            bool /* write_sideset_distribution_factors */)
{
  // This routine takes the place of the exodusii routine ex_put_init,
  // and additionally pre-defines variables such as qa, element blocks,
  // sidesets and nodesets in a single pass.
  //
  // This is necessary because of the way exodusII works.  Each time the
  // netcdf routine endef is called to take the file out of define mode,
  // the entire file is copied before the new information is added.
  //
  // With very large files, this is simply not workable.  This routine takes
  // the definition portions of all applicable exodus routines and puts them
  // in a single definition, preventing repeated copying of the file.
  //
  // Most of the code is copied directly from the applicable exodus routine,
  // and thus this routine may not seem very consistent in usage or variable
  // naming, etc.

  // Perform the initializations

  int element_block_index;

  // Inquire on defined string dimension and general dimension for qa

  int dim_str, dim_four, dim_line, dim_time;
  if (nc_def_dim(ncFile, "len_string", ExoIIInterface::MAX_STR_LENGTH, &dim_str) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to get string length in file");
  }

  if (nc_def_dim(ncFile, "len_line", ExoIIInterface::MAX_STR_LENGTH, &dim_line) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to get line length in file");
  }

  if (nc_def_dim(ncFile, "four", 4, &dim_four) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to locate four in file");
  }

  if (nc_def_dim(ncFile, "time_step", 1, &dim_time) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to locate time step in file");
  }
  // some whole_time dummy :(
  int dtime;
  if (NC_NOERR != nc_def_var(ncFile, "time_whole", NC_DOUBLE, 1, &dim_time, &dtime)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define time whole array");
  }

  /* Put file into define mode */

  // It is possible that an NT filename using backslashes is in the title string
  // this can give fits to unix codes where the backslash is assumed to be an escape
  // sequence.  For the exodus file, just flip the backslash to a slash to prevent
  // this problem

  // Get a working copy of the title_string;

  char working_title[80];
  strncpy(working_title, title_string, 79);

  int length = strlen(working_title);
  for (int pos = 0; pos < length; pos++) {
    if (working_title[pos] == '\\')
      strncpy(&working_title[pos], "/", 1);
  }

  if (NC_NOERR != nc_put_att_text(ncFile, NC_GLOBAL, "title", length, working_title)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define title attribute");
  }

  // Add other attributes while we're at it
  float dum_vers = 6.28F;
  if (NC_NOERR != nc_put_att_float(ncFile, NC_GLOBAL, "api_version", NC_FLOAT, 1, &dum_vers)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define api_version attribute");
  }
  dum_vers = 6.28F;
  if (NC_NOERR != nc_put_att_float(ncFile, NC_GLOBAL, "version", NC_FLOAT, 1, &dum_vers)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define version attribute");
  }
  int dum_siz = sizeof(double);
  if (NC_NOERR != nc_put_att_int(ncFile, NC_GLOBAL, "floating_point_word_size", NC_INT, 1, &dum_siz)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define floating pt word size attribute");
  }

  // Set up number of dimensions

  int num_el_blk, num_elem, num_nodes, num_dim, num_fa_blk, num_faces;
  if (nc_def_dim(ncFile, "num_dim", (size_t)mesh_info.num_dim, &num_dim) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of dimensions");
  }

  if (nc_def_dim(ncFile, "num_nodes", mesh_info.num_nodes, &num_nodes) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes");
  }

  int num_nod_per_fa; // it is needed for polyhedron only; need to compute it (connectivity of faces of polyhedra)
  if (mesh_info.polyhedronFaces.size()>0)
    if (nc_def_dim(ncFile, "num_faces", (int)mesh_info.polyhedronFaces.size(), &num_faces) != NC_NOERR) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes");
    }

  if (nc_def_dim(ncFile, "num_elem", mesh_info.num_elements, &num_elem) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of elements");
   }

  if (nc_def_dim(ncFile, "num_el_blk", mesh_info.num_elementblocks, &num_el_blk) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of element blocks");
  }

  /* ...and some variables */

  /* Element block id status array */
  int idstat = -1;
  if (NC_NOERR != nc_def_var(ncFile, "eb_status", NC_LONG, 1, &num_el_blk, &idstat)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define element block status array");
  }

  /* Element block id array */

  int idarr = -1;
  if (NC_NOERR != nc_def_var(ncFile, "eb_prop1", NC_LONG, 1, &num_el_blk, &idarr)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define element block id array");
  }

  /*   store property name as attribute of property array variable */
  if (NC_NOERR != nc_put_att_text(ncFile, idarr, "name", strlen("ID"), "ID")) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store element block property name ID");
  }

  // count how many are polyhedron blocks
  int num_fa_blocks=0, num_polyh_blocks = 0;
  for (unsigned int i = 0; i < block_data.size(); i++) {
    MaterialSetData & block = block_data[i];
    if (EXOII_POLYHEDRON==block.element_type)
    {
      num_fa_blocks++;
      num_polyh_blocks++;
    }
  }
  if (0==this->repeat_face_blocks && num_fa_blocks>1)
    num_fa_blocks=1;
  char wname[80];

  if (num_fa_blocks>0)
  {
    /* face block id status array */
    if (nc_def_dim(ncFile, "num_fa_blk", num_fa_blocks, &num_fa_blk) != NC_NOERR) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of face blocks");
    }

    int idstatf = -1;
    if (NC_NOERR != nc_def_var(ncFile, "fa_status", NC_LONG, 1, &num_fa_blk, &idstatf)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define face block status array");
    }

    /* Element block id array */

    int idarrf = -1;
    if (NC_NOERR != nc_def_var(ncFile, "fa_prop1", NC_LONG, 1, &num_fa_blk, &idarrf)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define face block id array");
    }

    /*   store property name as attribute of property array variable */
    if (NC_NOERR != nc_put_att_text(ncFile, idarrf, "name", strlen("ID"), "ID")) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store face block property name ID");
    }
    // determine the number of num_nod_per_face
    /*
      num_fa_in_blk1 = 15 ;
      num_nod_per_fa1 = 58 ;

      int fbconn1(num_nod_per_fa1) ;
              fbconn1:elem_type = "nsided" ;
      int fbepecnt1(num_fa_in_blk1) ;
              fbepecnt1:entity_type1 = "NODE" ;
              fbepecnt1:entity_type2 = "FACE" ;
       */



    int num_nodes_per_face=0;

    int dims[1]; // maybe 1 is enough here
    for (Range::iterator eit = mesh_info.polyhedronFaces.begin();
        eit != mesh_info.polyhedronFaces.end(); eit++)
    {
      EntityHandle polyg = *eit;
      int nnodes = 0;
      const EntityHandle * conn = NULL;
      ErrorCode rval = mdbImpl->get_connectivity(polyg, conn, nnodes);
      MB_CHK_ERR(rval);
      num_nodes_per_face += nnodes;
    }


    // duplicate if needed; default is not duplicate
    for (int j=1; j<=num_fa_blocks; j++)
    {
      INS_ID(wname, "num_nod_per_fa%d", j);
      if (nc_def_dim(ncFile, wname, (size_t)num_nodes_per_face, &num_nod_per_fa) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes for face block ");
      }
      dims[0] = num_nod_per_fa;
      INS_ID(wname, "fbconn%d", j); // first one, or more
      int fbconn;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &fbconn)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create connectivity array for face block " << 1);
      }
      std::string element_type_string("nsided");
      if (NC_NOERR != nc_put_att_text(ncFile, fbconn, "elem_type", element_type_string.length(), element_type_string.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store element type nsided ");
      }

      INS_ID(wname, "num_fa_in_blk%d", j); // first one, or more
      int num_fa_in_blk;
      if (nc_def_dim(ncFile, wname, (size_t)mesh_info.polyhedronFaces.size(), &num_fa_in_blk) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes for face block ");
      }

      // fbepecnt
      INS_ID(wname, "fbepecnt%d", j); // first one, or more
      int fbepecnt;
      dims[0] = num_fa_in_blk;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &fbepecnt)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create fbepecnt array for block " <<  1);
      }
      std::string enttype1("NODE");
      if (NC_NOERR != nc_put_att_text(ncFile, fbepecnt, "entity_type1", enttype1.length(), enttype1.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type 1  ");
      }
      std::string enttype2("FACE");
      if (NC_NOERR != nc_put_att_text(ncFile, fbepecnt, "entity_type2", enttype2.length(), enttype2.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type 2  ");
      }
    }
  }

  // Define element blocks

  for (unsigned int i = 0; i < block_data.size(); i++) {
    MaterialSetData & block = block_data[i];

    element_block_index = i + 1;
    int num_el_in_blk = -1, num_att_in_blk = -1;
    int blk_attrib, connect;

    /* Define number of elements in this block */

    INS_ID(wname, "num_el_in_blk%d", element_block_index);
    if (nc_def_dim(ncFile, wname, (size_t)block.number_elements, &num_el_in_blk) != NC_NOERR) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of elements/block for block " << i + 1);
    }

    /* Define number of nodes per element for this block */
    INS_ID(wname, "num_nod_per_el%d", element_block_index);
    int num_nod_per_el = -1;
    if (EXOII_POLYHEDRON!=block.element_type)
      if (nc_def_dim(ncFile, wname, (size_t)block.number_nodes_per_element, &num_nod_per_el) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes/element for block " << block.id);
      }

    /* Define element attribute array for this block */
    int dims[3];
    if (block.number_attributes > 0) {
      INS_ID(wname, "num_att_in_blk%d", element_block_index);
      if (nc_def_dim(ncFile, wname, (size_t)block.number_attributes, &num_att_in_blk) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of attributes in block " << block.id);
      }

      INS_ID(wname, "attrib%d", element_block_index);
      dims[0] = num_el_in_blk;
      dims[1] = num_att_in_blk;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_DOUBLE, 2, dims, &blk_attrib)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define attributes for element block " << block.id);
      }
    }

    /* Define element connectivity array for this block */

    if (EXOII_POLYGON != block.element_type && EXOII_POLYHEDRON!=block.element_type)
    {
      INS_ID(wname, "connect%d", element_block_index);
      dims[0] = num_el_in_blk;
      dims[1] = num_nod_per_el;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 2, dims, &connect)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create connectivity array for block " << i + 1);
      }

      /* Store element type as attribute of connectivity variable */

      std::string element_type_string(ExoIIUtil::ElementTypeNames[block.element_type]);
      if (NC_NOERR != nc_put_att_text(ncFile, connect, "elem_type", element_type_string.length(), element_type_string.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store element type name " << (int)block.element_type);
      }
    }
    else if (EXOII_POLYGON == block.element_type )
    {
      INS_ID(wname, "connect%d", element_block_index);
      // need to define num_nod_per_el as total number of nodes
      // ebepecnt1 as number of nodes per polygon
      /*
       * int connect1(num_nod_per_el1) ;
            connect1:elem_type = "nsided" ;
         int ebepecnt1(num_el_in_blk1) ;
            ebepecnt1:entity_type1 = "NODE" ;
            ebepecnt1:entity_type2 = "ELEM" ;*/
      dims[0] = num_nod_per_el;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &connect)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create connectivity array for block " << i + 1);
      }
      std::string element_type_string("nsided");
      if (NC_NOERR != nc_put_att_text(ncFile, connect, "elem_type", element_type_string.length(), element_type_string.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store element type name " << (int)block.element_type);
      }
      INS_ID(wname, "ebepecnt%d", element_block_index);
      int ebepecnt;
      dims[0] = num_el_in_blk;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &ebepecnt)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create ebepecnt array for block " << i + 1);
      }
      std::string etype1("NODE");
      if (NC_NOERR != nc_put_att_text(ncFile, ebepecnt, "entity_type1", etype1.length(), etype1.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type1 " << (int)block.element_type);
      }
      std::string etype2("ELEM");
      if (NC_NOERR != nc_put_att_text(ncFile, ebepecnt, "entity_type2", etype2.length(), etype2.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type2 " << (int)block.element_type);
      }
    }
    else if (EXOII_POLYHEDRON == block.element_type )
    {
      //INS_ID(wname, "connect%d", element_block_index);
      /*
      testn  face   example: 3 polyh, 15 total faces, 2 shared; 15+2 = 17
      num_elem = 3 ;
      num_face = 15 ; // not needed?
      num_el_blk = 1 ;

      num_el_in_blk1 = 3 ;
      num_fac_per_el1 = 17 ;

       * num faces will be total face conn
       * num_faces_in_block will be number of faces (non repeated)
       * num_nodes_per_face will have total face connectivity
       */
      int num_faces2=0;

      for (Range::iterator eit=block.elements.begin(); eit!=block.elements.end(); eit++)
      {
        EntityHandle polyh= *eit;
        int nfaces=0;
        const EntityHandle * conn = NULL;
        ErrorCode rval = mdbImpl->get_connectivity(polyh, conn, nfaces); MB_CHK_ERR(rval);
        num_faces2+=nfaces;
      }

      int num_fac_per_el;

      INS_ID(wname, "num_fac_per_el%d", element_block_index);
      if (nc_def_dim(ncFile, wname, (size_t)num_faces2, &num_fac_per_el) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of faces per block " << block.id);
      }

      /*
      int facconn1(num_fac_per_el1) ;
              facconn1:elem_type = "NFACED" ;
      int ebepecnt1(num_el_in_blk1) ;
              ebepecnt1:entity_type1 = "FACE" ;
              ebepecnt1:entity_type2 = "ELEM" ;
       */


      // facconn
      INS_ID(wname, "facconn%d", element_block_index);
      int facconn;
      dims[0] = num_fac_per_el;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &facconn)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create facconn array for block " << i + 1);
      }
      std::string etype("NFACED");
      if (NC_NOERR != nc_put_att_text(ncFile, facconn, "elem_type", etype.length(), etype.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store elem type " << (int)block.element_type);
      }

      // ebepecnt
      INS_ID(wname, "ebepecnt%d", element_block_index);
      int ebepecnt;
      dims[0] = num_el_in_blk;
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, dims, &ebepecnt)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create ebepecnt array for block " << i + 1);
      }
      std::string etype1("FACE");
      if (NC_NOERR != nc_put_att_text(ncFile, ebepecnt, "entity_type1", etype1.length(), etype1.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type1 " << (int)block.element_type);
      }
      std::string etype2("ELEM");
      if (NC_NOERR != nc_put_att_text(ncFile, ebepecnt, "entity_type2", etype2.length(), etype2.c_str())) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store entity type2 " << (int)block.element_type);
      }

      block.number_nodes_per_element = num_faces2;// connectivity for all polyhedra in block
    }

  }

  /* Node set id array: */

  int non_empty_nss = 0;
  // Need to go through nodesets to compute # nodesets, some might be empty
  std::vector<DirichletSetData>::iterator ns_it;
  for (ns_it = nodeset_data.begin();
       ns_it != nodeset_data.end(); ++ns_it) {
    if (0 != (*ns_it).number_nodes)
      non_empty_nss++;
  }

  int num_ns = -1;
  int ns_idstat = -1, ns_idarr = -1;
  if (non_empty_nss > 0) {
    if (nc_def_dim(ncFile, "num_node_sets", (size_t)(non_empty_nss), &num_ns) != NC_NOERR) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of node sets");
    }

    /* Node set id status array: */

    if (NC_NOERR != nc_def_var(ncFile, "ns_status", NC_LONG, 1, &num_ns, &ns_idstat)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create node sets status array");
    }

    /* Node set id array: */
    if (NC_NOERR != nc_def_var(ncFile, "ns_prop1", NC_LONG, 1, &num_ns, &ns_idarr)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create node sets property array");
    }

    /* Store property name as attribute of property array variable */
    if (NC_NOERR != nc_put_att_text(ncFile, NC_GLOBAL, "name", strlen("ID"), "ID")) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store node set property name ID");
    }

    // Now, define the arrays needed for each node set

    int index = 0;

    for (unsigned i = 0; i < nodeset_data.size(); i++) {
      DirichletSetData node_set = nodeset_data[i];

      if (0 == node_set.number_nodes) {
        MB_SET_ERR_CONT("WriteNCDF: empty nodeset " << node_set.id);
        continue;
      }
      index++;

      int num_nod_ns = -1;
      INS_ID(wname, "num_nod_ns%d", index);
      if (nc_def_dim(ncFile, wname, (size_t)node_set.number_nodes, &num_nod_ns) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of nodes for set " << node_set.id);
      }

      /* Create variable array in which to store the node set node list */
      int node_ns = -1;
      INS_ID(wname, "node_ns%d", index);
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, &num_nod_ns, &node_ns)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create node set " << node_set.id << " node list");
      }

      // Create distribution factor array
      int fact_ns = -1;
      INS_ID(wname, "dist_fact_ns%d", index);
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_DOUBLE, 1, &num_nod_ns, &fact_ns)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create node set " << node_set.id << " distribution factor list");
      }
    }
  }

  /* Side set id array: */

  long non_empty_ss = 0;
  // Need to go through nodesets to compute # nodesets, some might be empty
  std::vector<NeumannSetData>::iterator ss_it;
  for (ss_it = sideset_data.begin();
       ss_it != sideset_data.end(); ++ss_it) {
    if (0 != (*ss_it).number_elements)
      non_empty_ss++;
  }

  if (non_empty_ss > 0) {
    int num_ss = -1;
    if (nc_def_dim(ncFile, "num_side_sets", non_empty_ss, &num_ss) != NC_NOERR) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of side sets");
    }

    /* Side set id status array: */
    int ss_idstat = -1, ss_idarr = -1;
    if (NC_NOERR != nc_def_var(ncFile, "ss_status", NC_LONG, 1, &num_ss, &ss_idstat)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define side set status");
    }

    /* Side set id array: */
    if (NC_NOERR != nc_def_var(ncFile, "ss_prop1", NC_LONG, 1, &num_ss, &ss_idarr)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define side set property");
    }

    /* Store property name as attribute of property array variable */
    if (NC_NOERR != nc_put_att_text(ncFile, ss_idarr, "name", strlen("ID"), "ID")) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to store side set property name ID");
    }

    // Now, define the arrays needed for each side set

    int index = 0;
    for (unsigned int i = 0; i < sideset_data.size(); i++) {
      NeumannSetData side_set = sideset_data[i];

      // Don't define an empty set
      if (0 == side_set.number_elements)
        continue;

      index++;

      int num_side_ss = -1;
      int elem_ss = -1, side_ss = -1;
      INS_ID(wname, "num_side_ss%d", index);
      if (nc_def_dim(ncFile, wname, (size_t)side_set.number_elements, &num_side_ss) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of sides in side set " << side_set.id);
      }

      INS_ID(wname, "elem_ss%d", index);
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, &num_side_ss, &elem_ss)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create element list for side set " << side_set.id); /* Exit define mode and return */
      }
      INS_ID(wname, "side_ss%d", index);
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, &num_side_ss, &side_ss)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create side list for side set " << side_set.id); /* Exit define mode and return */
      }

      // sideset distribution factors
      int num_df_ss = -1;
      INS_ID(wname, "num_df_ss%d", index);
      if (nc_def_dim(ncFile, wname, (size_t)side_set.ss_dist_factors.size(), &num_df_ss) != NC_NOERR) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define number of dist factors in side set " << side_set.id); /* Exit define mode and return */
      }

      /* Create variable array in which to store the side set distribution factors */

      int fact_ss = -1;
      INS_ID(wname, "dist_fact_ss%d", index);
      if (NC_NOERR != nc_def_var(ncFile, wname, NC_LONG, 1, &num_df_ss, &fact_ss)) {
        MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create dist factors list for side set " << side_set.id); /* Exit define mode and return */
      }
    }
  }

  /* Node coordinate arrays: */

  int coord, name_coord, dims[3];
  dims[0] = num_dim;
  dims[1] = num_nodes;
  if (NC_NOERR != nc_def_var(ncFile, "coord", NC_DOUBLE, 2, dims, &coord)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define node coordinate array");
  }

  /* Coordinate names array */

  dims[0] = num_dim;
  dims[1] = dim_str;
  if (NC_NOERR != nc_def_var(ncFile, "coor_names", NC_CHAR, 2, dims, &name_coord)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define coordinate name array");
  }

  // Define genesis maps if required

  if (write_maps) {
    // Element map
    int elem_map = -1, elem_map2 = -1, node_map = -1;
    if (NC_NOERR != nc_def_var(ncFile, "elem_map", NC_LONG, 1, &num_elem, &elem_map)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create element map array"); /* Exit define mode and return */
    }

    // Create the element number map
    if (NC_NOERR != nc_def_var(ncFile, "elem_num_map", NC_LONG, 1, &num_elem, &elem_map2)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create element numbering map"); /* Exit define mode and return */
    }

    // Create node number map
    if (NC_NOERR != nc_def_var(ncFile, "node_num_map", NC_LONG, 1, &num_nodes, &node_map)) {
      MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to create node numbering map array"); /* Exit define mode and return */
    }
  }

  // Define qa records to be used

  int num_qa_rec = mesh_info.qaRecords.size() / 4;
  int num_qa = -1;

  if (nc_def_dim(ncFile, "num_qa_rec", (long)num_qa_rec, &num_qa) != NC_NOERR) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define qa record array size");
  }

  // Define qa array
  int qa_title;
  dims[0] = num_qa;
  dims[1] = dim_four;
  dims[2] = dim_str;
  if (NC_NOERR != nc_def_var(ncFile, "qa_records", NC_CHAR, 3, dims, &qa_title)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: failed to define qa record array");
  }

  // Take it out of define mode
  if (NC_NOERR != nc_enddef(ncFile)) {
    MB_SET_ERR(MB_FAILURE, "WriteNCDF: Trouble leaving define mode");
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::open_file(const char* filename)
{
  // Not a valid filename
  if (strlen((const char*)filename) == 0) {
    MB_SET_ERR(MB_FAILURE, "Output Exodus filename not specified");
  }

  int fail = nc_create(filename, NC_CLOBBER, &ncFile);

  // File couldn't be opened
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Cannot open " << filename);
  }

  return MB_SUCCESS;
}

ErrorCode WriteNCDF::get_sideset_elems(EntityHandle sideset, int current_sense,
                                       Range &forward_elems, Range &reverse_elems)
{
  Range ss_elems, ss_meshsets;

  // Get the sense tag; don't need to check return, might be an error if the tag
  // hasn't been created yet
  Tag sense_tag = 0;
  mdbImpl->tag_get_handle("SENSE", 1, MB_TYPE_INTEGER, sense_tag);

  // Get the entities in this set
  ErrorCode result = mdbImpl->get_entities_by_handle(sideset, ss_elems, true);
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
        MB_FAILURE == mdbImpl->tag_get_data(sense_tag, &(*range_iter), 1, &this_sense))
      this_sense = 1;

    // Now get all the entities on this meshset, with the proper (possibly reversed) sense
    get_sideset_elems(*range_iter, this_sense*current_sense,
                      forward_elems, reverse_elems);
  }

  return result;
}

} // namespace moab
