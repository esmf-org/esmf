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
#pragma warning(disable:4786)
#endif

#include "ReadNCDF.hpp"
#include "netcdf.h"

#include <algorithm>
#include <string>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <cmath>
#include <sstream>
#include <iostream>
#include <map>

#include "moab/CN.hpp"
#include "moab/Range.hpp"
#include "moab/Interface.hpp"
#include "ExoIIUtil.hpp"
#include "MBTagConventions.hpp"
#include "Internals.hpp"
#include "moab/ReadUtilIface.hpp"
#include "exodus_order.h"
#include "moab/FileOptions.hpp"
#include "moab/AdaptiveKDTree.hpp"
#include "moab/CartVect.hpp"

namespace moab {

#define INS_ID(stringvar, prefix, id) \
  sprintf(stringvar, prefix, id)

#define GET_DIM(ncdim, name, val) \
  { \
    int gdfail = nc_inq_dimid(ncFile, name, &ncdim); \
    if (NC_NOERR == gdfail) { \
      size_t tmp_val; \
      gdfail = nc_inq_dimlen(ncFile, ncdim, &tmp_val); \
      if (NC_NOERR != gdfail) { \
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Couldn't get dimension length"); \
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
      int ndims; \
      gvfail = nc_inq_varndims(ncFile, id, &ndims); \
      if (NC_NOERR == gvfail) { \
        dims.resize(ndims); \
        gvfail = nc_inq_vardimid(ncFile, id, &dims[0]); \
        if (NC_NOERR != gvfail) { \
          MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Couldn't get variable dimension IDs"); \
        } \
      } \
    } \
  }

#define GET_1D_INT_VAR(name, id, vals) \
  { \
    GET_VAR(name, id, vals); \
    if (-1 != id) { \
      size_t ntmp; \
      int ivfail = nc_inq_dimlen(ncFile, vals[0], &ntmp); \
      if (NC_NOERR != ivfail) { \
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Couldn't get dimension length"); \
      } \
      vals.resize(ntmp); \
      size_t ntmp1 = 0; \
      ivfail = nc_get_vara_int(ncFile, id, &ntmp1, &ntmp, &vals[0]); \
      if (NC_NOERR != ivfail) { \
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting variable " << name); \
      } \
    } \
  }

#define GET_1D_DBL_VAR(name, id, vals) \
  { \
    std::vector<int> dum_dims; \
    GET_VAR(name, id, dum_dims); \
    if (-1 != id) { \
      size_t ntmp; \
      int dvfail = nc_inq_dimlen(ncFile, dum_dims[0], &ntmp); \
      if (NC_NOERR != dvfail) { \
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Couldn't get dimension length"); \
      } \
      vals.resize(ntmp); \
      size_t ntmp1 = 0; \
      dvfail = nc_get_vara_double(ncFile, id, &ntmp1, &ntmp, &vals[0]); \
      if (NC_NOERR != dvfail) { \
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting variable " << name); \
      } \
    } \
  }

ReaderIface* ReadNCDF::factory(Interface* iface)
{
  return new ReadNCDF(iface);
}

ReadNCDF::ReadNCDF(Interface* impl)
  : mdbImpl(impl), max_line_length(-1), max_str_length(-1)
{
  assert(impl != NULL);
  reset();

  impl->query_interface(readMeshIface);

  // Initialize in case tag_get_handle fails below
  mMaterialSetTag  = 0;
  mDirichletSetTag = 0;
  mNeumannSetTag   = 0;
  mHasMidNodesTag  = 0;
  mDistFactorTag   = 0;
  mQaRecordTag     = 0;
  mGlobalIdTag     = 0;

  //! Get and cache predefined tag handles
  ErrorCode result;
  const int zero = 0, negone = -1;
  result = impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mMaterialSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  assert(MB_SUCCESS == result);
  result = impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mDirichletSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  assert(MB_SUCCESS == result);
  result = impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mNeumannSetTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  assert(MB_SUCCESS == result);
  const int mids[] = {-1, -1, -1, -1};
  result = impl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                                mHasMidNodesTag, MB_TAG_SPARSE | MB_TAG_CREAT, mids);
  assert(MB_SUCCESS == result);
  result = impl->tag_get_handle("distFactor", 0, MB_TYPE_DOUBLE, mDistFactorTag,
                                MB_TAG_SPARSE | MB_TAG_VARLEN | MB_TAG_CREAT);
  assert(MB_SUCCESS == result);
  result = impl->tag_get_handle("qaRecord", 0, MB_TYPE_OPAQUE, mQaRecordTag,
                                MB_TAG_SPARSE | MB_TAG_VARLEN | MB_TAG_CREAT);
  assert(MB_SUCCESS == result);
  result = impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mGlobalIdTag, MB_TAG_SPARSE | MB_TAG_CREAT, &zero);
  assert(MB_SUCCESS == result);
#ifdef NDEBUG
  if (MB_SUCCESS == result) {}; // Line to avoid compiler warning about unused variable
#endif
  ncFile = 0;
}

void ReadNCDF::reset()
{
  mCurrentMeshHandle = 0;
  vertexOffset = 0; 

  numberNodes_loading = 0;
  numberElements_loading = 0;
  numberElementBlocks_loading = 0;
  numberFaceBlocks_loading = 0;
  numberNodeSets_loading = 0;
  numberSideSets_loading = 0;
  numberDimensions_loading = 0;

  if (!blocksLoading.empty())
    blocksLoading.clear();

  if (!nodesInLoadedBlocks.empty())
    nodesInLoadedBlocks.clear();

  if (!faceBlocksLoading.empty())
    faceBlocksLoading.clear();
}

ReadNCDF::~ReadNCDF()
{
  mdbImpl->release_interface(readMeshIface);
}
  
ErrorCode ReadNCDF::read_tag_values(const char* file_name,
                                    const char* tag_name,
                                    const FileOptions& /* opts */,
                                    std::vector<int>& id_array,
                                    const SubsetList* subset_list)
{
  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "ExodusII reader supports subset read only by material ID");
  }

  // Open netcdf/exodus file
  int fail = nc_open(file_name, 0, &ncFile);
  if (NC_NOWRITE != fail) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, "ReadNCDF:: problem opening Netcdf/Exodus II file " << file_name);
  }

  // 1. Read the header
  ErrorCode rval = read_exodus_header();
  if (MB_FAILURE == rval) 
    return rval;

  int count = 0;
  const char* prop;
  const char* blocks   = "eb_prop1";
  const char* nodesets = "ns_prop1";
  const char* sidesets = "ss_prop1";
  
  if (!strcmp(tag_name, MATERIAL_SET_TAG_NAME)) {
    count = numberElementBlocks_loading;
    prop = blocks;
  }
  else if (!strcmp(tag_name, DIRICHLET_SET_TAG_NAME)) {
    count = numberNodeSets_loading;
    prop = nodesets;
  }
  else if (!strcmp(tag_name, NEUMANN_SET_TAG_NAME)) {
    count = numberSideSets_loading;
    prop = sidesets;
  }
  else {
    ncFile = 0;
    return MB_TAG_NOT_FOUND;
  }

  if (count) {
    int nc_var = -1;
    GET_1D_INT_VAR(prop, nc_var, id_array);
    if (!nc_var) {
      MB_SET_ERR(MB_FAILURE, "Problem getting prop variable");
    }
  }

  // Close the file
  fail = nc_close(ncFile);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");
  }

  ncFile = 0;
  return MB_SUCCESS;
}

ErrorCode ReadNCDF::load_file(const char *exodus_file_name,
                              const EntityHandle* file_set,
                              const FileOptions& opts,
                              const ReaderIface::SubsetList* subset_list,
                              const Tag* file_id_tag)
{
  ErrorCode status;
  int fail;

  int num_blocks = 0;
  const int* blocks_to_load = 0;
  if (subset_list) {
    if (subset_list->tag_list_length > 1 ||
        !strcmp(subset_list->tag_list[0].tag_name, MATERIAL_SET_TAG_NAME)) {
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "ExodusII reader supports subset read only by material ID");
    }
    if (subset_list->num_parts) {
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "ExodusII reader does not support mesh partitioning");
    }
    blocks_to_load = subset_list->tag_list[0].tag_values;
    num_blocks = subset_list->tag_list[0].num_tag_values;
  }

  // This function directs the reading of an exoii file, but doesn't do any of
  // the actual work

  // See if opts has tdata.
  ErrorCode rval;
  std::string s;
  rval = opts.get_str_option("tdata", s);
  if (MB_SUCCESS == rval && !s.empty())
    return update(exodus_file_name, opts, num_blocks, blocks_to_load, *file_set);

  reset();

  // 0. Open the file.

  // open netcdf/exodus file
  fail = nc_open(exodus_file_name, 0, &ncFile);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, "ReadNCDF:: problem opening Netcdf/Exodus II file " << exodus_file_name);
  }

  // 1. Read the header
  status = read_exodus_header();
  if (MB_FAILURE == status)
    return status;

  status = mdbImpl->get_entities_by_handle(0, initRange);
  if (MB_FAILURE == status)
    return status;

  // 2. Read the nodes unless they've already been read before
  status = read_nodes(file_id_tag);
  if (MB_FAILURE == status)
    return status;

  // 3.
  // extra for polyhedra blocks
  if (numberFaceBlocks_loading>0)
  {
    status = read_face_blocks_headers();
    if (MB_FAILURE == status)
      return status;
  }

  status = read_block_headers(blocks_to_load, num_blocks);
  if (MB_FAILURE == status)
    return status;

  // 4. Read elements (might not read them, depending on active blocks)
  if (numberFaceBlocks_loading>0)
  {
    status = read_polyhedra_faces();
    if (MB_FAILURE == status)
      return status;
  }
  status = read_elements(file_id_tag);
  if (MB_FAILURE == status)
    return status;

  // 5. Read global ids
  status = read_global_ids();
  if (MB_FAILURE == status)
    return status;

  // 6. Read nodesets
  status = read_nodesets();
  if (MB_FAILURE == status)
    return status;

  // 7. Read sidesets
  status = read_sidesets();
  if (MB_FAILURE == status)
    return status;

  // 8. Read qa records
  if (file_set) {
    status = read_qa_records(*file_set);
    if (MB_FAILURE == status)
      return status;
  }

  // What about properties???

  // Close the file
  fail = nc_close(ncFile);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");
  }

  ncFile = 0;
  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_exodus_header()
{
  CPU_WORD_SIZE = sizeof(double); // With ExodusII version 2, all floats
  IO_WORD_SIZE = sizeof(double); // should be changed to doubles

  // NetCDF doesn't check its own limits on file read, so check
  // them here so it doesn't corrupt memory any more than absolutely
  // necessary.
  int ndims;
  int fail = nc_inq_ndims(ncFile, &ndims);
  if (NC_NOERR != fail || ndims > NC_MAX_DIMS) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF: File contains " << ndims << " dims but NetCDF library supports only " << (int)NC_MAX_DIMS);
  }
  int nvars;
  fail = nc_inq_nvars(ncFile, &nvars);
  if (nvars > NC_MAX_VARS) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF: File contains " << nvars << " vars but NetCDF library supports only " << (int)NC_MAX_VARS);
  }

  // Get the attributes

  // Get the word size, scalar value
  nc_type att_type;
  size_t att_len;
  fail = nc_inq_att(ncFile, NC_GLOBAL, "floating_point_word_size", &att_type, &att_len);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting floating_point_word_size attribute");
  }
  if (att_type != NC_INT || att_len != 1) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Word size didn't have type int or size 1");
  }
  fail = nc_get_att_int(ncFile, NC_GLOBAL, "floating_point_word_size", &IO_WORD_SIZE);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Trouble getting word size");
  }

  // Exodus version
  fail = nc_inq_att(ncFile, NC_GLOBAL, "version", &att_type, &att_len);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting version attribute");
  }
  if (att_type != NC_FLOAT || att_len != 1) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Version didn't have type float or size 1");
  }
  //float version = temp_att->as_float(0);

  // Read in initial variables
  int temp_dim;
  GET_DIM(temp_dim, "num_dim", numberDimensions_loading);
  GET_DIM(temp_dim, "num_nodes", numberNodes_loading);
  GET_DIM(temp_dim, "num_elem", numberElements_loading);
  GET_DIM(temp_dim, "num_el_blk", numberElementBlocks_loading);
  GET_DIM(temp_dim, "num_fa_blk", numberFaceBlocks_loading); // for polyhedra blocks
  GET_DIM(temp_dim, "num_elem", numberElements_loading);
  GET_DIM(temp_dim, "num_node_sets", numberNodeSets_loading);
  GET_DIM(temp_dim, "num_side_sets", numberSideSets_loading);
  GET_DIM(temp_dim, "len_string", max_str_length);
  GET_DIM(temp_dim, "len_line", max_line_length);

  // Title; why are we even bothering if we're not going to keep it???
  fail = nc_inq_att(ncFile, NC_GLOBAL, "title", &att_type, &att_len);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting title attribute");
  }
  if (att_type != NC_CHAR) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: title didn't have type char");
  }
  char *title = new char[att_len + 1];
  fail = nc_get_att_text(ncFile, NC_GLOBAL, "title", title);
  if (NC_NOERR != fail) {
    delete[] title;
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: trouble getting title");
  }
  delete[] title;

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_nodes(const Tag* file_id_tag)
{
  // Read the nodes into memory

  assert(0 != ncFile);

  // Create a sequence to hold the node coordinates
  // Get the current number of entities and start at the next slot

  EntityHandle node_handle = 0;
  std::vector<double*> arrays;
  readMeshIface->get_node_coords(3, numberNodes_loading,
                                 MB_START_ID, node_handle, arrays);

  vertexOffset = ID_FROM_HANDLE(node_handle) - MB_START_ID;

  // Read in the coordinates
  int fail;
  int coord = 0;
  nc_inq_varid(ncFile, "coord", &coord);

  // Single var for all coords
  size_t start[2] = {0, 0}, count[2] = {1, static_cast<size_t>(numberNodes_loading)};
  if (coord) {
    for (int d = 0; d < numberDimensions_loading; ++d) {
      start[0] = d;
      fail = nc_get_vara_double(ncFile, coord, start, count, arrays[d]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting " << (char)('x' + d) << " coord array");
      }
    }
  }
  // Var for each coord
  else {
    char varname[] = "coord ";
    for (int d = 0; d < numberDimensions_loading; ++d) {
      varname[5] = 'x'+ (char)d;
      fail = nc_inq_varid(ncFile, varname, &coord);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting " << (char)('x' + d) << " coord variable");
      }

      fail = nc_get_vara_double(ncFile, coord, start, &count[1], arrays[d]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting " << (char)('x' + d) << " coord array");
      }
    }
  }

  // Zero out any coord values that are in database but not in file
  // (e.g. if MOAB has 3D coords but file is 2D then set Z coords to zero.)
  for (unsigned d = numberDimensions_loading; d < arrays.size(); ++d)
    std::fill(arrays[d], arrays[d]+numberNodes_loading, 0.0);

  if (file_id_tag) {
    Range nodes;
    nodes.insert(node_handle, node_handle + numberNodes_loading - 1);
    readMeshIface->assign_ids(*file_id_tag, nodes, vertexOffset);
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_block_headers(const int *blocks_to_load,
                                       const int num_blocks)
{
  // Get the element block ids; keep this in a separate list,
  // which is not offset by blockIdOffset; this list used later for
  // reading block connectivity

  // Get the ids of all the blocks of this file we're reading in
  std::vector<int> block_ids(numberElementBlocks_loading);
  int nc_block_ids = -1;
  GET_1D_INT_VAR("eb_prop1", nc_block_ids, block_ids);
  if (-1 == nc_block_ids) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting eb_prop1 variable");
  }

  int exodus_id = 1;

  // If the active_block_id_list is NULL all blocks are active.
  if (NULL == blocks_to_load || 0 == num_blocks) {
    blocks_to_load = &block_ids[0];
  }

  std::vector<int> new_blocks(blocks_to_load, blocks_to_load+numberElementBlocks_loading);

  std::vector<int>::iterator iter, end_iter;
  iter = block_ids.begin();
  end_iter = block_ids.end();

  // Read header information and initialize header-type block information
  int temp_dim;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  int block_seq_id = 1;
  
  for ( ; iter != end_iter; ++iter, block_seq_id++) {
    int num_elements;

    GET_DIMB(temp_dim, temp_string, "num_el_in_blk%d", block_seq_id, num_elements);

    // Don't read element type string for now, since it's an attrib
    // on the connectivity
    // Get the entity type corresponding to this ExoII element type
    //ExoIIElementType elem_type =
    //ExoIIUtil::static_element_name_to_type(element_type_string);

    // Tag each element block(mesh set) with enum for ElementType (SHELL, QUAD4, ....etc)
    ReadBlockData block_data;
    block_data.elemType = EXOII_MAX_ELEM_TYPE;
    block_data.blockId = *iter;
    block_data.startExoId = exodus_id; 
    block_data.numElements = num_elements;

    // If block is in 'blocks_to_load'----load it!
    if (std::find(new_blocks.begin(), new_blocks.end(), *iter) != new_blocks.end())
      block_data.reading_in = true;
    else
      block_data.reading_in = false;

    blocksLoading.push_back(block_data);
    exodus_id += num_elements;
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_face_blocks_headers()
{
  // Get the ids of all the blocks of this file we're reading in
  std::vector<int> fblock_ids(numberFaceBlocks_loading);
  int nc_fblock_ids = -1;
  GET_1D_INT_VAR("fa_prop1", nc_fblock_ids, fblock_ids);
  if (-1 == nc_fblock_ids) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting fa_prop1 variable");
  }

  int temp_dim;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  //int block_seq_id = 1;
  int exodus_id = 1;

  for (int i = 1; i <= numberFaceBlocks_loading; i++) {
    int num_elements;

    GET_DIMB(temp_dim, temp_string, "num_fa_in_blk%d", i, num_elements);

    // Don't read element type string for now, since it's an attrib
    // on the connectivity
    // Get the entity type corresponding to this ExoII element type
    //ExoIIElementType elem_type =
    //ExoIIUtil::static_element_name_to_type(element_type_string);

    // Tag each element block(mesh set) with enum for ElementType (SHELL, QUAD4, ....etc)
    ReadFaceBlockData fblock_data;
    fblock_data.faceBlockId = i;
    fblock_data.startExoId = exodus_id;
    fblock_data.numElements = num_elements;

    faceBlocksLoading.push_back(fblock_data);
    exodus_id += num_elements;
  }
  return MB_SUCCESS;
}
ErrorCode ReadNCDF::read_polyhedra_faces()
{
  int temp_dim;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  nodesInLoadedBlocks.resize(numberNodes_loading + 1);
  size_t start[1] = {0}, count[1]; // one dim arrays only here!
  std::vector<ReadFaceBlockData>::iterator this_it;
  this_it = faceBlocksLoading.begin();

  int fblock_seq_id = 1;
  std::vector<int> dims;
  int nc_var, fail;

  for ( ; this_it != faceBlocksLoading.end(); ++this_it, fblock_seq_id++) {
    int num_fa_in_blk;
    GET_DIMB(temp_dim, temp_string, "num_fa_in_blk%d", fblock_seq_id, num_fa_in_blk);
    int num_nod_per_fa;
    GET_DIMB(temp_dim, temp_string, "num_nod_per_fa%d", fblock_seq_id, num_nod_per_fa);
    // Get the ncdf connect variable and the element type
    INS_ID(temp_string, "fbconn%d", fblock_seq_id);
    GET_VAR(temp_string, nc_var, dims);
    std::vector<int> fbconn;
    fbconn.resize(num_nod_per_fa);
    count[0] = num_nod_per_fa;

    fail = nc_get_vara_int(ncFile, nc_var, start, count, &fbconn[0]);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting face polyhedra connectivity ");
    }
    std::vector<int> fbepecnt;
    fbepecnt.resize(num_fa_in_blk);
    INS_ID(temp_string, "fbepecnt%d", fblock_seq_id);
    GET_VAR(temp_string, nc_var, dims);
    count[0] = num_fa_in_blk;
    fail = nc_get_vara_int(ncFile, nc_var, start, count, &fbepecnt[0]);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting face polyhedra connectivity ");
    }
    // now we can create some polygons
    std::vector<EntityHandle> polyConn;
    int ix=0;
    for (int i=0; i<num_fa_in_blk; i++)
    {
      polyConn.resize(fbepecnt[i]);
      for (int j=0; j<fbepecnt[i]; j++)
      {
        nodesInLoadedBlocks[fbconn[ix]] = 1;
        polyConn[j]=vertexOffset + fbconn[ix++];
      }
      EntityHandle newp;
      /*
       *  ErrorCode create_element(const EntityType type,
                                   const EntityHandle *connectivity,
                                   const int num_vertices,
                                   EntityHandle &element_handle)
       */
      if(mdbImpl->create_element(MBPOLYGON, &polyConn[0], fbepecnt[i], newp) != MB_SUCCESS )
        return MB_FAILURE;

      // add the element in order
      polyfaces.push_back(newp);

    }
  }
  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_elements(const Tag* file_id_tag)
{
  // Read in elements

  int result = 0;

  // Initialize the nodeInLoadedBlocks vector
  if (nodesInLoadedBlocks.size() < (size_t) (numberNodes_loading + 1) )
    nodesInLoadedBlocks.resize(numberNodes_loading + 1); // this could be repeated?
  memset(&nodesInLoadedBlocks[0], 0, (numberNodes_loading + 1)*sizeof(char));

  std::vector<ReadBlockData>::iterator this_it;
    this_it = blocksLoading.begin();

  int temp_dim;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];

  int nc_var;
  int block_seq_id = 1;
  std::vector<int> dims;
  size_t start[2] = {0, 0}, count[2];

  for ( ; this_it != blocksLoading.end(); ++this_it, block_seq_id++) {
    // If this block isn't to be read in --- continue
    if (!(*this_it).reading_in)
      continue;

    // Get some information about this block
    int block_id = (*this_it).blockId;
    EntityHandle *conn = 0;

    // Get the ncdf connect variable and the element type
    INS_ID(temp_string, "connect%d", block_seq_id);
    GET_VAR(temp_string, nc_var, dims);
    if (-1 == nc_var || 0==nc_var) { // try other var, for polyhedra blocks
      // it could be polyhedra block, defined by fbconn and NFACED attribute
      INS_ID(temp_string, "facconn%d", block_seq_id);
      GET_VAR(temp_string, nc_var, dims);

      if (-1 == nc_var || 0 == nc_var) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting connec or faccon variable");
      }
    }
    nc_type att_type;
    size_t att_len;
    int fail = nc_inq_att(ncFile, nc_var, "elem_type", &att_type, &att_len);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting elem type attribute");
    }
    std::vector<char> dum_str(att_len + 1);
    fail = nc_get_att_text(ncFile, nc_var, "elem_type", &dum_str[0]);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting elem type");
    }
    ExoIIElementType elem_type = 
      ExoIIUtil::static_element_name_to_type(&dum_str[0]);
    (*this_it).elemType = elem_type;

    int verts_per_element = ExoIIUtil::VerticesPerElement[(*this_it).elemType];
    int number_nodes = (*this_it).numElements*verts_per_element;
    const EntityType mb_type = ExoIIUtil::ExoIIElementMBEntity[(*this_it).elemType];

    if (mb_type==MBPOLYGON)
    {
      // need to read another variable, num_nod_per_el, and
      int num_nodes_poly_conn;
      GET_DIMB(temp_dim, temp_string, "num_nod_per_el%d", block_seq_id, num_nodes_poly_conn);
      // read the connec1 array, which is of dimensions num_nodes_poly_conn (only one )
      std::vector<int> connec;
      connec.resize(num_nodes_poly_conn);
      count[0] = num_nodes_poly_conn;

      fail = nc_get_vara_int(ncFile, nc_var, start, count, &connec[0]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting polygon connectivity ");
      }
      // ebepecnt1
      std::vector <int> ebec;
      ebec.resize(this_it->numElements);
      // Get the ncdf connect variable and the element type
      INS_ID(temp_string, "ebepecnt%d", block_seq_id);
      GET_VAR(temp_string, nc_var, dims);
      count[0] = this_it->numElements;
      fail = nc_get_vara_int(ncFile, nc_var, start, count, &ebec[0]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting polygon nodes per elem ");
      }
      EntityHandle ms_handle;
      if (mdbImpl->create_meshset(MESHSET_SET | MESHSET_TRACK_OWNER, ms_handle) != MB_SUCCESS)
        return MB_FAILURE;
      // create polygons one by one, and put them in the list
      // also need to read the number of nodes for each polygon, then create
      std::vector<EntityHandle> polyConn;
      int ix=0;
      for (int i=0; i<this_it->numElements; i++)
      {
        polyConn.resize(ebec[i]);
        for (int j=0; j<ebec[i]; j++)
        {
          nodesInLoadedBlocks[connec[ix]] = 1;
          polyConn[j]=vertexOffset + connec[ix++];
        }
        EntityHandle newp;
        /*
         *  ErrorCode create_element(const EntityType type,
                                     const EntityHandle *connectivity,
                                     const int num_vertices,
                                     EntityHandle &element_handle)
         */
        if(mdbImpl->create_element(MBPOLYGON, &polyConn[0], ebec[i], newp) != MB_SUCCESS )
          return MB_FAILURE;

        // add the element in order
        this_it->polys.push_back(newp);
        if (mdbImpl->add_entities(ms_handle, &newp, 1) != MB_SUCCESS)
          return MB_FAILURE;
      }
      // Set the block id with an offset
      if (mdbImpl->tag_set_data(mMaterialSetTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;
      if (mdbImpl->tag_set_data(mGlobalIdTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;

    }
    else if (mb_type==MBPOLYHEDRON)
    {
      // need to read another variable, num_fac_per_el
      int num_fac_per_el;
      GET_DIMB(temp_dim, temp_string, "num_fac_per_el%d", block_seq_id, num_fac_per_el);
      // read the fbconn array, which is of dimensions num_nod_per_fa (only one dimension)
      std::vector<int> facconn;
      facconn.resize(num_fac_per_el);
      count[0] = num_fac_per_el;

      fail = nc_get_vara_int(ncFile, nc_var, start, count, &facconn[0]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting polygon connectivity ");
      }
      // ebepecnt1
      std::vector <int> ebepecnt;
      ebepecnt.resize(this_it->numElements);
      // Get the ncdf connect variable and the element type
      INS_ID(temp_string, "ebepecnt%d", block_seq_id);
      GET_VAR(temp_string, nc_var, dims);
      count[0] = this_it->numElements;
      fail = nc_get_vara_int(ncFile, nc_var, start, count, &ebepecnt[0]);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting polygon nodes per elem ");
      }
      EntityHandle ms_handle;
      if (mdbImpl->create_meshset(MESHSET_SET | MESHSET_TRACK_OWNER, ms_handle) != MB_SUCCESS)
        return MB_FAILURE;
      // create polygons one by one, and put them in the list
      // also need to read the number of nodes for each polygon, then create
      std::vector<EntityHandle> polyConn;
      int ix=0;
      for (int i=0; i<this_it->numElements; i++)
      {
        polyConn.resize(ebepecnt[i]);
        for (int j=0; j<ebepecnt[i]; j++)
        {
          polyConn[j]= polyfaces [ facconn[ix++]-1 ] ;
        }
        EntityHandle newp;
        /*
         *  ErrorCode create_element(const EntityType type,
                                     const EntityHandle *connectivity,
                                     const int num_vertices,
                                     EntityHandle &element_handle)
         */
        if(mdbImpl->create_element(MBPOLYHEDRON, &polyConn[0], ebepecnt[i], newp) != MB_SUCCESS )
          return MB_FAILURE;

        // add the element in order
        this_it->polys.push_back(newp);
        if (mdbImpl->add_entities(ms_handle, &newp, 1) != MB_SUCCESS)
          return MB_FAILURE;
      }
      // Set the block id with an offset
      if (mdbImpl->tag_set_data(mMaterialSetTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;
      if (mdbImpl->tag_set_data(mGlobalIdTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;

    }
    else // this is regular
    {
      // Allocate an array to read in connectivity data
      readMeshIface->get_element_connect(
          this_it->numElements,
          verts_per_element,
          mb_type,
          this_it->startExoId,
          this_it->startMBId,
          conn);

      // Create a range for this sequence of elements
      EntityHandle start_range, end_range;
      start_range = (*this_it).startMBId;
      end_range = start_range + (*this_it).numElements - 1;

      Range new_range(start_range, end_range);
      //Range<EntityHandle> new_range((*this_it).startMBId,
      //                              (*this_it).startMBId + (*this_it).numElements - 1);

      // Create a MBSet for this block and set the material tag

      EntityHandle ms_handle;
      if (mdbImpl->create_meshset(MESHSET_SET | MESHSET_TRACK_OWNER, ms_handle) != MB_SUCCESS)
        return MB_FAILURE;

      if (mdbImpl->add_entities(ms_handle, new_range) != MB_SUCCESS)
        return MB_FAILURE;

      int mid_nodes[4];
      CN::HasMidNodes(mb_type, verts_per_element, mid_nodes);
      if (mdbImpl->tag_set_data(mHasMidNodesTag, &ms_handle, 1, mid_nodes) != MB_SUCCESS)
        return MB_FAILURE;

      // Just a check because the following code won't work if this case fails
      assert(sizeof(EntityHandle) >= sizeof(int));

      // tmp_ptr is of type int* and points at the same place as conn
      int* tmp_ptr = reinterpret_cast<int*>(conn);

      // Read the connectivity into that memory,  this will take only part of the array
      // 1/2 if sizeof(EntityHandle) == 64 bits.
      count[0] = this_it->numElements;
      count[1] = verts_per_element;
      fail = nc_get_vara_int(ncFile, nc_var, start, count, tmp_ptr);
      if (NC_NOERR != fail) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting connectivity");
      }

      // Convert from exodus indices to vertex handles.
      // Iterate backwards in case handles are larger than ints.
      for (int i = number_nodes - 1; i >= 0; --i) {
        if ((unsigned)tmp_ptr[i] >= nodesInLoadedBlocks.size()) {
          MB_SET_ERR(MB_FAILURE, "Invalid node ID in block connectivity");
        }
        nodesInLoadedBlocks[tmp_ptr[i]] = 1;
        conn[i] = static_cast<EntityHandle>(tmp_ptr[i]) + vertexOffset;
      }


      // Adjust connectivity order if necessary
      const int* reorder = exodus_elem_order_map[mb_type][verts_per_element];
      if (reorder)
        ReadUtilIface::reorder(reorder, conn, this_it->numElements, verts_per_element);

      readMeshIface->update_adjacencies((*this_it).startMBId, (*this_it).numElements,
                                        ExoIIUtil::VerticesPerElement[(*this_it).elemType], conn);

      if (result == -1) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: error getting element connectivity for block " << block_id);
      }

      // Set the block id with an offset
      if (mdbImpl->tag_set_data(mMaterialSetTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;
      if (mdbImpl->tag_set_data(mGlobalIdTag, &ms_handle, 1, &block_id) != MB_SUCCESS)
        return MB_FAILURE;

      if (file_id_tag) {
        Range range;
        range.insert(this_it->startMBId, this_it->startMBId + this_it->numElements - 1);
        readMeshIface->assign_ids(*file_id_tag, range, this_it->startExoId);
      }
    } // end regular block (not polygon)
  }

  return MB_SUCCESS;
}
  
ErrorCode ReadNCDF::read_global_ids()
{
  // Read in the map from the exodus file
  std::vector<int> ids(std::max(numberElements_loading, numberNodes_loading));

  int varid = -1;
  GET_1D_INT_VAR("elem_map", varid, ids);
  if (-1 != varid) {
    std::vector<ReadBlockData>::iterator iter;
    int id_pos = 0;
    for (iter = blocksLoading.begin(); iter != blocksLoading.end(); ++iter) {
      if (iter->reading_in) {
        if (iter->elemType != EXOII_POLYGON &&
            iter->elemType != EXOII_POLYHEDRON )
        {
          if (iter->startMBId != 0) {
            Range range(iter->startMBId, iter->startMBId + iter->numElements - 1);
            ErrorCode error = mdbImpl->tag_set_data(mGlobalIdTag,
                                                    range, &ids[id_pos]);
            if (error != MB_SUCCESS)
              return error;
            id_pos += iter->numElements;
          }
          else
            return MB_FAILURE;
        }
        else // polygons or polyhedrons; use id from elements
        {
          for ( std::vector<EntityHandle>::iterator eit = iter->polys.begin();
              eit!=iter->polys.end(); eit++, id_pos++)
          {
            EntityHandle peh= *eit;
            if(mdbImpl->tag_set_data(mGlobalIdTag, &peh, 1, &ids[id_pos]) != MB_SUCCESS)
              return MB_FAILURE;
          }
        }
      }
    }
  }

  // Read in node map next
  varid = -1;
  GET_1D_INT_VAR("node_num_map", varid, ids);
  if (-1 != varid) {
    Range range(MB_START_ID + vertexOffset,
                MB_START_ID + vertexOffset + numberNodes_loading - 1);
    ErrorCode error = mdbImpl->tag_set_data(mGlobalIdTag,
                                            range, &ids[0]);
    if (MB_SUCCESS != error)
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem setting node global ids");
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_nodesets()
{
  // Read in the nodesets for the model

  if (0 == numberNodeSets_loading)
    return MB_SUCCESS;
  std::vector<int> id_array(numberNodeSets_loading);

  // Read in the nodeset ids
  int nc_var;
  GET_1D_INT_VAR("ns_prop1", nc_var, id_array);
  if (-1 == nc_var) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting ns_prop1 variable");
  }

  // Use a vector of ints to read node handles
  std::vector<int> node_handles;

  int i;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  int temp_dim;
  for (i = 0; i < numberNodeSets_loading; i++) {
    // Get nodeset parameters
    int number_nodes_in_set;
    int number_dist_factors_in_set;

    GET_DIMB(temp_dim, temp_string, "num_nod_ns%d", i + 1, number_nodes_in_set);
    GET_DIMB(temp_dim, temp_string, "num_df_ns%d", i + 1, number_dist_factors_in_set);

    // Need to new a vector to store dist. factors
    // This vector * gets stored as a tag on the sideset meshset
    std::vector<double> temp_dist_factor_vector(number_nodes_in_set);
    if (number_dist_factors_in_set != 0) {
      INS_ID(temp_string, "dist_fact_ns%d", i + 1);
      GET_1D_DBL_VAR(temp_string, temp_dim, temp_dist_factor_vector);
      if (-1 == temp_dim) {
        MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting dist fact variable");
      }
    }

    // Size new arrays and get ids and distribution factors
    if (node_handles.size() < (unsigned int)number_nodes_in_set) {
      node_handles.reserve(number_nodes_in_set);
      node_handles.resize(number_nodes_in_set);
    }

    INS_ID(temp_string, "node_ns%d", i + 1);
    int temp_var = -1;
    GET_1D_INT_VAR(temp_string, temp_var, node_handles);
    if (-1 == temp_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting nodeset node variable");
    }

    // Maybe there is already a nodesets meshset here we can append to
    Range child_meshsets;
    if (mdbImpl->get_entities_by_handle(0, child_meshsets) != MB_SUCCESS)
      return MB_FAILURE;

    child_meshsets = subtract(child_meshsets, initRange);

    Range::iterator iter, end_iter;
    iter = child_meshsets.begin();
    end_iter = child_meshsets.end();

    EntityHandle ns_handle = 0;
    for ( ; iter != end_iter; ++iter) {
      int nodeset_id;
      if (mdbImpl->tag_get_data(mDirichletSetTag, &(*iter), 1, &nodeset_id) != MB_SUCCESS)
        continue;

      if (id_array[i] == nodeset_id) {
        // Found the meshset
        ns_handle = *iter;
        break;
      }
    }

    std::vector< EntityHandle > nodes_of_nodeset;
    if (ns_handle)
      if (mdbImpl->get_entities_by_handle(ns_handle, nodes_of_nodeset, true) != MB_SUCCESS)
        return MB_FAILURE;

    // Make these into entity handles
    // TODO: could we have read it into EntityHandle sized array in the first place?
    int j, temp;
    std::vector<EntityHandle> nodes;
    std::vector<double> dist_factor_vector;
    for (j = 0; j < number_nodes_in_set; j++) {
      // See if this node is one we're currently reading in
      if (nodesInLoadedBlocks[node_handles[j]] == 1) {
        // Make sure that it already isn't in a nodeset
        unsigned int node_id = CREATE_HANDLE(MBVERTEX, node_handles[j] + vertexOffset, temp);
        if (!ns_handle ||
            std::find(nodes_of_nodeset.begin(), nodes_of_nodeset.end(), node_id) == nodes_of_nodeset.end()) {
          nodes.push_back(node_id);

          if (number_dist_factors_in_set != 0)
            dist_factor_vector.push_back(temp_dist_factor_vector[j]);
        }
      }
    }

    // No nodes to add
    if (nodes.empty())
      continue; 

    // If there was no meshset found --> create one
    if (ns_handle == 0) {
      if (mdbImpl->create_meshset(MESHSET_ORDERED | MESHSET_TRACK_OWNER, ns_handle) != MB_SUCCESS)
        return MB_FAILURE;

      // Set a tag signifying dirichlet bc
      // TODO: create this tag another way

      int nodeset_id = id_array[i];
      if (mdbImpl->tag_set_data(mDirichletSetTag, &ns_handle, 1, &nodeset_id) != MB_SUCCESS)
        return MB_FAILURE;
      if (mdbImpl->tag_set_data(mGlobalIdTag, &ns_handle, 1, &nodeset_id) != MB_SUCCESS)
        return MB_FAILURE;

      if (!dist_factor_vector.empty()) {
        int size = dist_factor_vector.size();
        const void* data = &dist_factor_vector[0];
        if (mdbImpl->tag_set_by_ptr(mDistFactorTag, &ns_handle, 1, &data, &size) != MB_SUCCESS)
          return MB_FAILURE;
      }
    }
    else if (!dist_factor_vector.empty()) {
      // Append dist factors to vector
      const void* ptr = 0;
      int size = 0;
      if (mdbImpl->tag_get_by_ptr(mDistFactorTag, &ns_handle, 1, &ptr, &size) != MB_SUCCESS)
        return MB_FAILURE;

      const double* data = reinterpret_cast<const double*>(ptr);
      dist_factor_vector.reserve(dist_factor_vector.size() + size);
      std::copy(data, data+size, std::back_inserter(dist_factor_vector));
      size = dist_factor_vector.size();
      ptr = &dist_factor_vector[0];
      if (mdbImpl->tag_set_by_ptr(mDistFactorTag, &ns_handle, 1, &ptr, &size) != MB_SUCCESS)
        return MB_FAILURE;
    }

    // Add the nodes to the meshset
    if (mdbImpl->add_entities(ns_handle, &nodes[0], nodes.size()) != MB_SUCCESS)
      return MB_FAILURE;
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_sidesets()
{
  // Uhhh if you read the same file (previously_read==true) then blow away the
  // sidesets pertaining to this file?  How do you do that? If you read a
  // new file make sure all the offsets are correct.

  // If not loading any sidesets -- exit
  if (0 == numberSideSets_loading)
    return MB_SUCCESS;

  // Read in the sideset ids
  std::vector<int> id_array(numberSideSets_loading);
  int temp_var = -1;
  GET_1D_INT_VAR("ss_prop1", temp_var, id_array);
  if (-1 == temp_var) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting ss_prop1 variable");
  }

  // Create a side set for each one
  int number_sides_in_set;
  int number_dist_factors_in_set;

  // Maybe there is already a sidesets meshset here we can append to
  Range child_meshsets;
  if (mdbImpl->get_entities_by_type(0, MBENTITYSET,
                                    child_meshsets) != MB_SUCCESS)
    return MB_FAILURE;

  child_meshsets = subtract(child_meshsets, initRange);

  Range::iterator iter, end_iter;

  int i;
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  int temp_dim;
  for (i = 0; i < numberSideSets_loading; i++) {
    // Get sideset parameters
    GET_DIMB(temp_dim, temp_string, "num_side_ss%d", i + 1, number_sides_in_set);
    GET_DIMB(temp_dim, temp_string, "num_df_ss%d", i + 1, number_dist_factors_in_set);

    // Size new arrays and get element and side lists
    std::vector<int> side_list(number_sides_in_set);
    std::vector<int> element_list(number_sides_in_set);
    INS_ID(temp_string, "side_ss%d", i + 1);
    temp_var = -1;
    GET_1D_INT_VAR(temp_string, temp_var, side_list);
    if (-1 == temp_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting sideset side variable");
    }

    INS_ID(temp_string, "elem_ss%d", i + 1);
    temp_var = -1;
    GET_1D_INT_VAR(temp_string, temp_var, element_list);
    if (-1 == temp_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting sideset elem variable");
    }

    std::vector<double> temp_dist_factor_vector;
    std::vector<EntityHandle> entities_to_add, reverse_entities;
    // Create the sideset entities
    if (create_ss_elements(&element_list[0], &side_list[0], number_sides_in_set, number_dist_factors_in_set,
                           entities_to_add, reverse_entities, temp_dist_factor_vector, i + 1) != MB_SUCCESS)
      return MB_FAILURE;

    // If there are elements to add
    if (!entities_to_add.empty() || !reverse_entities.empty()) {
      iter = child_meshsets.begin();
      end_iter = child_meshsets.end();

      EntityHandle ss_handle = 0;
      for ( ; iter != end_iter; ++iter) {
        int sideset_id;
        if (mdbImpl->tag_get_data(mNeumannSetTag, &(*iter), 1, &sideset_id) != MB_SUCCESS)
          continue; 

        if (id_array[i] == sideset_id) {
          // Found the meshset
          ss_handle = *iter;
          break;
        }
      } 

      // If we didn't find a sideset already
      if (ss_handle == 0) {
        if (mdbImpl->create_meshset(MESHSET_ORDERED | MESHSET_TRACK_OWNER, ss_handle) != MB_SUCCESS)
          return MB_FAILURE;

        if (ss_handle == 0) 
          return MB_FAILURE;

        int sideset_id = id_array[i];
        if (mdbImpl->tag_set_data(mNeumannSetTag, &ss_handle, 1, &sideset_id) != MB_SUCCESS)
          return MB_FAILURE;
        if (mdbImpl->tag_set_data(mGlobalIdTag, &ss_handle, 1, &sideset_id) != MB_SUCCESS)
          return MB_FAILURE;

        if (!reverse_entities.empty()) {
          // Also make a reverse set to put in this set
          EntityHandle reverse_set;
          if (mdbImpl->create_meshset(MESHSET_SET | MESHSET_TRACK_OWNER, reverse_set) != MB_SUCCESS)
            return MB_FAILURE;

          // Add the reverse set to the sideset set and the entities to the reverse set
          ErrorCode result = mdbImpl->add_entities(ss_handle, &reverse_set, 1);
          if (MB_SUCCESS != result)
            return result;

          result = mdbImpl->add_entities(reverse_set, &reverse_entities[0], reverse_entities.size());
          if (MB_SUCCESS != result)
            return result;

          // Set the reverse tag
          Tag sense_tag;
          int dum_sense = 0;
          result = mdbImpl->tag_get_handle("NEUSET_SENSE", 1, MB_TYPE_INTEGER, sense_tag, 
                                           MB_TAG_SPARSE | MB_TAG_CREAT, &dum_sense);
          if (result != MB_SUCCESS)
            return result;
          dum_sense = -1;
          result = mdbImpl->tag_set_data(sense_tag, &reverse_set, 1, &dum_sense);
          if (result != MB_SUCCESS)
            return result;
        }
      }

      if (mdbImpl->add_entities(ss_handle, (entities_to_add.empty())? NULL : &entities_to_add[0],
                                 entities_to_add.size()) != MB_SUCCESS)
        return MB_FAILURE;

      // Distribution factor stuff
      if (number_dist_factors_in_set) {
        // If this sideset does not already has a distribution factor array...set one
        const void* ptr = 0;
        int size = 0;
        if (MB_SUCCESS == mdbImpl->tag_get_by_ptr(mDistFactorTag, &ss_handle, 1, &ptr, &size)) {
          const double* data = reinterpret_cast<const double*>(ptr);
          std::copy(data, data + size, std::back_inserter(temp_dist_factor_vector));
        }

        ptr = &temp_dist_factor_vector[0];
        size =  temp_dist_factor_vector.size();
        if (mdbImpl->tag_set_by_ptr(mDistFactorTag, &ss_handle, 1, &ptr, &size) != MB_SUCCESS)
          return MB_FAILURE;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::create_ss_elements(int *element_ids,
                                       int *side_list,
                                       int num_sides,
                                       int num_dist_factors,
                                       std::vector<EntityHandle> &entities_to_add,
                                       std::vector<EntityHandle> &reverse_entities,
                                       std::vector<double> &dist_factor_vector,
                                       int ss_seq_id)
{
  // Determine entity type from element id
  int i, k;

  // If there are dist. factors, create a vector to hold the array
  // and place this array as a tag onto the sideset meshset

  std::vector<double> temp_dist_factor_vector(num_dist_factors);
  std::vector<char> temp_string_storage(max_str_length + 1);
  char *temp_string = &temp_string_storage[0];
  int temp_var;
  if (num_dist_factors) {
    INS_ID(temp_string, "dist_fact_ss%d", ss_seq_id);
    temp_var = -1;
    GET_1D_DBL_VAR(temp_string, temp_var, temp_dist_factor_vector);
    if (-1 == temp_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting dist fact variable");
    }
  }

  EntityType subtype;
  int num_side_nodes, num_elem_nodes;
  const EntityHandle* nodes;
  std::vector<EntityHandle> connectivity;
  int side_node_idx[32];

  int df_index = 0;
  int sense = 0;
  for (i = 0; i < num_sides; i++) {
    ExoIIElementType exoii_type;
    ReadBlockData block_data;
    block_data.elemType = EXOII_MAX_ELEM_TYPE;

    if (find_side_element_type(element_ids[i], exoii_type, block_data, df_index, side_list[i]) != MB_SUCCESS)
      continue; // Isn't being read in this time

    EntityType type = ExoIIUtil::ExoIIElementMBEntity[exoii_type];

    EntityHandle ent_handle = element_ids[i] - block_data.startExoId + block_data.startMBId;

    const int side_num = side_list[i] - 1;
    if (type == MBHEX) {
      // Get the nodes of the element
      if (mdbImpl->get_connectivity(ent_handle, nodes, num_elem_nodes) != MB_SUCCESS)
        return MB_FAILURE;

      CN::SubEntityNodeIndices(type, num_elem_nodes, 2, side_num, subtype, num_side_nodes, side_node_idx);
      if (num_side_nodes <= 0)
        return MB_FAILURE;

      connectivity.resize(num_side_nodes);
      for (k = 0; k < num_side_nodes; ++k)
        connectivity[k] = nodes[side_node_idx[k]];

      if (MB_SUCCESS != create_sideset_element(connectivity, subtype, ent_handle, sense))
        return MB_FAILURE;
      if (1 == sense)
        entities_to_add.push_back(ent_handle);
      else
        reverse_entities.push_back(ent_handle);

      // Read in distribution factor array
      if (num_dist_factors) {
        for (k = 0; k < 4; k++)
          dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
      }
    }

    // If it is a Tet
    else if (type == MBTET) {
      // Get the nodes of the element
      if (mdbImpl->get_connectivity(ent_handle, nodes, num_elem_nodes) != MB_SUCCESS)
        return MB_FAILURE;

      CN::SubEntityNodeIndices(type, num_elem_nodes, 2, side_num, subtype, num_side_nodes, side_node_idx);
      if (num_side_nodes <= 0)
        return MB_FAILURE;

      connectivity.resize(num_side_nodes);
      for (k = 0; k < num_side_nodes; ++k)
        connectivity[k] = nodes[side_node_idx[k]];

      if (MB_SUCCESS != create_sideset_element(connectivity, subtype, ent_handle, sense))
        return MB_FAILURE;
      if (1 == sense)
        entities_to_add.push_back(ent_handle);
      else
        reverse_entities.push_back(ent_handle);

      // Read in distribution factor array
      if (num_dist_factors) {
        for (k = 0; k < 3; k++)
          dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
      }
    }
    else if (type == MBQUAD &&
             exoii_type >= EXOII_SHELL && exoii_type <= EXOII_SHELL9) {
      //ent_handle = CREATE_HANDLE(MBQUAD, base_id, error);

      // Just use this quad
      if (side_list[i] == 1) {
        if (1 == sense)
          entities_to_add.push_back(ent_handle);
        else
          reverse_entities.push_back(ent_handle);

        if (num_dist_factors) {
          for (k = 0; k < 4; k++)
            dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
        }

        continue;
      }
      else if (side_list[i] == 2) {
        reverse_entities.push_back(ent_handle);

        if (num_dist_factors) {
          for (k = 0; k < 4; k++)
            dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
        }

        continue;
      }
      else {
        // Get the nodes of the element
        if (mdbImpl->get_connectivity(ent_handle, nodes, num_elem_nodes) != MB_SUCCESS)
          return MB_FAILURE;

        CN::SubEntityNodeIndices(type, num_elem_nodes, 1, side_num - 2, subtype, num_side_nodes, side_node_idx);
        if (num_side_nodes <= 0)
          return MB_FAILURE;

        connectivity.resize(num_side_nodes);
        for (k = 0; k < num_side_nodes; ++k)
          connectivity[k] = nodes[side_node_idx[k]];

        if (MB_SUCCESS != create_sideset_element(connectivity, subtype, ent_handle, sense))
          return MB_FAILURE;
        if (1 == sense)
          entities_to_add.push_back(ent_handle);
        else
          reverse_entities.push_back(ent_handle);

        if (num_dist_factors) {
          for (k = 0; k < 2; k++)
            dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
        }
      }
    }
    // If it is a Quad
    else if (type == MBQUAD) {
      // Get the nodes of the element
      if (mdbImpl->get_connectivity(ent_handle, nodes, num_elem_nodes) != MB_SUCCESS)
        return MB_FAILURE;

      CN::SubEntityNodeIndices(type, num_elem_nodes, 1, side_num, subtype, num_side_nodes, side_node_idx);
      if (num_side_nodes <= 0)
        return MB_FAILURE;
      
      connectivity.resize(num_side_nodes);
      for (k = 0; k < num_side_nodes; ++k)
        connectivity[k] = nodes[side_node_idx[k]];

      if (MB_SUCCESS != create_sideset_element(connectivity, subtype, ent_handle, sense))
        return MB_FAILURE;
      if (1 == sense)
        entities_to_add.push_back(ent_handle);
      else
        reverse_entities.push_back(ent_handle);

      // Read in distribution factor array
      if (num_dist_factors) {
        for (k = 0; k < 2; k++)
          dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
      }
    }
    else if (type == MBTRI) {
      int side_offset = 0;
      if (number_dimensions() == 3 && side_list[i] <= 2) {
        if (1 == sense)
          entities_to_add.push_back(ent_handle);
        else
          reverse_entities.push_back(ent_handle);
        if (num_dist_factors) {
          for (k = 0; k < 3; k++)
            dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
        }
      }
      else {
        if (number_dimensions() == 3) {
          if (side_list[i] > 2)
            side_offset = 2;
        }

        // Get the nodes of the element
        if (mdbImpl->get_connectivity(ent_handle, nodes, num_elem_nodes) != MB_SUCCESS)
          return MB_FAILURE;

        CN::SubEntityNodeIndices(type, num_elem_nodes, 1, side_num-side_offset, subtype, num_side_nodes, side_node_idx);
        if (num_side_nodes <= 0)
          return MB_FAILURE;

        connectivity.resize(num_side_nodes);
        for (k = 0; k < num_side_nodes; ++k)
          connectivity[k] = nodes[side_node_idx[k]];

        if (MB_SUCCESS != create_sideset_element(connectivity, subtype, ent_handle, sense))
          return MB_FAILURE;
        if (1 == sense)
          entities_to_add.push_back(ent_handle);
        else
          reverse_entities.push_back(ent_handle);

        if (num_dist_factors) {
          for (k = 0; k < 2; k++)
            dist_factor_vector.push_back(temp_dist_factor_vector[df_index++]);
        }
      }
    }
  }

  return MB_SUCCESS; 
}

ErrorCode ReadNCDF::create_sideset_element(const std::vector<EntityHandle>& connectivity,
                                           EntityType type, EntityHandle& handle, int &sense)
{
  // Get adjacent entities
  ErrorCode error = MB_SUCCESS;
  int to_dim = CN::Dimension(type);
  std::vector<EntityHandle> adj_ent;
  mdbImpl->get_adjacencies(&(connectivity[0]), 1, to_dim, false, adj_ent);

  // For each entity, see if we can find a match
  // If we find a match, return it
  bool match_found = false;
  std::vector<EntityHandle> match_conn;
  // By default, assume positive sense
  sense = 1;
  for (unsigned int i = 0; i < adj_ent.size() && match_found == false; i++) {
    // Get the connectivity
    error = mdbImpl->get_connectivity(&(adj_ent[i]), 1, match_conn);
    if (error != MB_SUCCESS)
      continue;

    // Make sure they have the same number of vertices (higher order elements ?)
    if (match_conn.size() != connectivity.size())
      continue;

    // Find a matching node
    std::vector<EntityHandle>::iterator iter;
    iter = std::find(match_conn.begin(), match_conn.end(), connectivity[0]);
    if (iter == match_conn.end())
      continue;

    // Rotate to match connectivity
    std::rotate(match_conn.begin(), iter, match_conn.end());

    bool they_match = true;
    unsigned int j;
    for (j = 1; j < connectivity.size(); j++) {
      if (connectivity[j] != match_conn[j]) {
        they_match = false;
        break;
      }
    }

    // If we didn't get a match
    if (!they_match) {
      // Try the opposite sense
      they_match = true;

      unsigned int k = connectivity.size() - 1;
      for (j = 1; j < connectivity.size(); ) {
        if (connectivity[j] != match_conn[k]) {
          they_match = false;
          break;
        }
        ++j;
        --k;
      }
      // If they matched here, sense is reversed
      if (they_match)
        sense = -1;
    }
    match_found = they_match;
    if (match_found)
      handle = adj_ent[i];
  }

  // If we didn't find a match, create an element
  if (!match_found)
    error = mdbImpl->create_element(type, &connectivity[0], connectivity.size(), handle);

  return error;
}

ErrorCode ReadNCDF::find_side_element_type(const int exodus_id, ExoIIElementType &elem_type,
                                           ReadBlockData &block_data, int &df_index, int side_id)
{
  std::vector<ReadBlockData>::iterator iter, end_iter;
  iter = blocksLoading.begin();
  end_iter = blocksLoading.end();
  elem_type = EXOII_MAX_ELEM_TYPE;

  for ( ; iter != end_iter; ++iter) {
    if (exodus_id >= (*iter).startExoId &&
        exodus_id < (*iter).startExoId + (*iter).numElements) {
      elem_type = (*iter).elemType;

      // If we're not reading this block in
      if (!(*iter).reading_in) {
        // Offset df_indes according to type
        if (elem_type >= EXOII_HEX && elem_type <= EXOII_HEX27)
          df_index += 4;
        else if (elem_type >= EXOII_TETRA && elem_type <= EXOII_TETRA14)
          df_index += 3;
        else if (elem_type >= EXOII_QUAD && elem_type <= EXOII_QUAD9)
          df_index += 2;
        else if (elem_type >= EXOII_SHELL && elem_type <= EXOII_SHELL9) {
          if (side_id == 1 || side_id == 2)
            df_index += 4;
          else
            df_index += 2;
        }
        else if (elem_type >= EXOII_TRI && elem_type <= EXOII_TRI7)
          df_index += 3;

        return MB_FAILURE;
      }

      block_data = *iter;

      return MB_SUCCESS;
    }
  }
  return MB_FAILURE;
}

ErrorCode ReadNCDF::read_qa_records(EntityHandle file_set)
{
  std::vector<std::string> qa_records;
  read_qa_information(qa_records);

  std::vector<char> tag_data;
  for (std::vector<std::string>::iterator i = qa_records.begin(); i != qa_records.end(); ++i) {
    std::copy(i->begin(), i->end(), std::back_inserter(tag_data));
    tag_data.push_back('\0');
  }

  // If there were qa_records...tag them to the mCurrentMeshHandle
  if (!tag_data.empty()) {
    const void* ptr = &tag_data[0];
    int size = tag_data.size();
    if (mdbImpl->tag_set_by_ptr(mQaRecordTag, &file_set, 1, &ptr, &size) != MB_SUCCESS) {
      return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_qa_information(std::vector<std::string> &qa_record_list)
{
  // Inquire on the genesis file to find the number of qa records

  int number_records = 0;

  int temp_dim;
  GET_DIM(temp_dim, "num_qa_rec", number_records);
  std::vector<char> data(max_str_length + 1);
  
  for (int i = 0; i < number_records; i++) {
    for (int j = 0; j < 4; j++) {
      data[max_str_length] = '\0';
      if (read_qa_string(&data[0], i, j) != MB_SUCCESS)
        return MB_FAILURE;

      qa_record_list.push_back(&data[0]);
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadNCDF::read_qa_string(char *temp_string,
                                   int record_number,
                                   int record_position)
{
  std::vector<int> dims;
  int temp_var = -1;
  GET_VAR("qa_records", temp_var, dims);
  if (-1 == temp_var) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting qa record variable");
    return MB_FAILURE;
  }
  size_t count[3], start[3];
  start[0] = record_number; start[1] = record_position; start[2] = 0;
  count[0] = 1; count[1] = 1; count[2] = max_str_length;
  int fail = nc_get_vara_text(ncFile, temp_var, start, count, temp_string);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem setting current record number variable position");
  }
  // Get the variable id in the exodus file

  return MB_SUCCESS;
}

// The cub_file_set contains the mesh to be updated. There could exist other
// file sets that should be kept separate, such as the geometry file set from
// ReadCGM.
ErrorCode ReadNCDF::update(const char *exodus_file_name,
                           const FileOptions& opts,
                           const int num_blocks,
                           const int *blocks_to_load,
                           const EntityHandle cub_file_set)
{
  // Function : updating current database from new exodus_file.
  // Creator:   Jane Hu
  // opts is currently designed as following
  // tdata = <var_name>[, time][,op][,destination]
  // where var_name show the tag name to be updated, this version just takes
  // coord.
  // time is the optional, and it gives time step of each of the mesh
  // info in exodus file. It start from 1.
  // op is the operation that is going to be performed on the var_name info.
  // currently support 'set'
  // destination shows where to store the updated info, currently assume it is
  // stored in the same database by replacing the old info if there's no input
  // for destination, or the destination data is given in exodus format and just
  // need to update the coordinates.
  // Assumptions:
  // 1. Assume the num_el_blk's in both DB and update exodus file are the same.
  // 2. Assume num_el_in_blk1...num_el_in_blk(num_el_blk) numbers are matching, may in
  // different order. example: num_el_in_blk11 = num_el_in_blk22 && num_el_in_blk12 =
  // num_el_in_blk21.
  // 3. In exodus file, get node_num_map
  // 4. loop through the node_num_map, use it to find the node in the cub file.
  // 5. Replace coord[0][n] with coordx[m] + vals_nod_var1(time_step, m) for all directions for matching nodes.
  //  Test: try to get hexes

  // *******************************************************************
  // Move nodes to their deformed locations.
  // *******************************************************************  
  ErrorCode rval;
  std::string s;
  rval = opts.get_str_option("tdata", s);
  if (MB_SUCCESS != rval) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem reading file options");
  }
  std::vector<std::string> tokens;
  tokenize(s, tokens, ",");

  // 1. Check for time step to find the match time
  int time_step = 1;
  if (tokens.size() > 1 && !tokens[1].empty()) {
    const char* time_s = tokens[1].c_str();
    char* endptr;
    long int pval = strtol(time_s, &endptr, 0);
    std::string end = endptr;
    if (!end.empty()) // Syntax error
      return MB_TYPE_OUT_OF_RANGE;

    // Check for overflow (parsing long int, returning int)
    time_step = pval;
    if (pval != (long int)time_step)
      return MB_TYPE_OUT_OF_RANGE;
    if (time_step <= 0)
      return MB_TYPE_OUT_OF_RANGE;
  }

  // 2. Check for the operations, currently support set.
  const char* op;
  if (tokens.size() < 3 || (tokens[2] != "set" && tokens[2] != "add")) {
    MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "ReadNCDF: invalid operation specified for update");
  }
  op = tokens[2].c_str();

  // Open netcdf/exodus file
  ncFile = 0;
  int fail = nc_open(exodus_file_name, 0, &ncFile);
  if (!ncFile) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, "ReadNCDF:: problem opening Netcdf/Exodus II file " << exodus_file_name);
  }

  rval = read_exodus_header();
  if (MB_SUCCESS != rval)
    return rval;

  // Check to make sure that the requested time step exists
  int ncdim = -1;
  int max_time_steps;
  GET_DIM(ncdim, "time_step", max_time_steps);
  if (-1 == ncdim) {
    std::cout << "ReadNCDF: could not get number of time steps" << std::endl;
    return MB_FAILURE;
  }
  std::cout << "  Maximum time step=" << max_time_steps << std::endl;
  if (max_time_steps < time_step) {
    std::cout << "ReadNCDF: time step is greater than max_time_steps" << std::endl;
    return MB_FAILURE;
  }

  // Get the time
  std::vector<double> times(max_time_steps);
  int nc_var = -1;
  GET_1D_DBL_VAR("time_whole", nc_var, times);
  if (-1 == nc_var) {
    std::cout << "ReadNCDF: unable to get time variable" << std::endl;
  }
  else {
    std::cout << "  Step " << time_step << " is at " << times[time_step - 1]
              << " seconds" << std::endl;
  }

  // Read in the node_num_map.
  std::vector<int> ptr(numberNodes_loading);

  int varid = -1;
  GET_1D_INT_VAR("node_num_map", varid, ptr);
  if (-1 == varid) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting node number map data");
  }

  // Read in the deformations.
  std::vector< std::vector<double> > deformed_arrays(3);
  std::vector< std::vector<double> > orig_coords(3);
  deformed_arrays[0].reserve(numberNodes_loading);
  deformed_arrays[1].reserve(numberNodes_loading);
  deformed_arrays[2].reserve(numberNodes_loading);
  orig_coords[0].reserve(numberNodes_loading);
  orig_coords[1].reserve(numberNodes_loading);
  orig_coords[2].reserve(numberNodes_loading);
  size_t start[2] = {static_cast<size_t>(time_step - 1), 0}, count[2] = {1, static_cast<size_t>(numberNodes_loading)};
  std::vector<int> dims;
  int coordx = -1, coordy = -1, coordz = -1;
  GET_VAR("vals_nod_var1", coordx, dims);
  GET_VAR("vals_nod_var2", coordy, dims);
  if (numberDimensions_loading == 3)
    GET_VAR("vals_nod_var3", coordz, dims);
  if (-1 == coordx || -1 == coordy ||
      (numberDimensions_loading == 3 && -1 == coordz)) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting coords variable");
  }

  fail = nc_get_vara_double(ncFile, coordx, start, count, &deformed_arrays[0][0]);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting x deformation array");
  }

  fail = nc_get_vara_double(ncFile, coordy, start, count, &deformed_arrays[1][0]);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting y deformation array");
  }
  if (numberDimensions_loading == 3) {
    fail = nc_get_vara_double(ncFile, coordz, start, count, &deformed_arrays[2][0]);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting z deformation array");
    }
  }

  int coord1 = -1, coord2 = -1, coord3 = -1;
  GET_1D_DBL_VAR("coordx", coord1, orig_coords[0]);
  if (-1 == coord1) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting x coord array");
  }
  GET_1D_DBL_VAR("coordy", coord2, orig_coords[1]);
  if (-1 == coord2) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting y coord array");
  }
  if (numberDimensions_loading == 3) {
    GET_1D_DBL_VAR("coordz", coord3, orig_coords[2]);
    if (-1 == coord3) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting z coord array");
    }
  }

  // b. Deal with DB file : get node info. according to node_num_map.
  if (tokens[0] != "coord" && tokens[0] != "COORD")
    return MB_NOT_IMPLEMENTED;

  if (strcmp(op, "set") && strcmp(op, " set"))
    return MB_NOT_IMPLEMENTED;

  // Two methods of matching nodes (id vs. proximity)
  const bool match_node_ids = true;

  // Get nodes in cubit file
  Range cub_verts;
  rval = mdbImpl->get_entities_by_type(cub_file_set, MBVERTEX, cub_verts);
  if (MB_SUCCESS != rval)
    return rval;
  std::cout << "  cub_file_set contains " << cub_verts.size() << " nodes."
            << std::endl;

  // Some accounting
  std::cout << "  exodus file contains " << numberNodes_loading << " nodes."
            << std::endl;
  double max_magnitude = 0;
  double average_magnitude = 0;
  int found = 0;
  int lost = 0;
  std::map<int, EntityHandle> cub_verts_id_map;
  AdaptiveKDTree kdtree(mdbImpl);
  EntityHandle root;

  // Should not use cub verts unless they have been matched. Place in a map
  // for fast handle_by_id lookup.
  std::map<int, EntityHandle> matched_cub_vert_id_map;

  // Place cub verts in a map for searching by id
  if (match_node_ids) {
    std::vector<int> cub_ids(cub_verts.size());
    rval = mdbImpl->tag_get_data(mGlobalIdTag, cub_verts, &cub_ids[0]);
    if (MB_SUCCESS != rval)
      return rval;
    for (unsigned i = 0; i != cub_verts.size(); ++i) {
      cub_verts_id_map.insert(std::pair<int, EntityHandle>(cub_ids[i], cub_verts[i]));
    }

  // Place cub verts in a kdtree for searching by proximity
  }
  else {
    FileOptions tree_opts("MAX_PER_LEAF=1;SPLITS_PER_DIR=1;CANDIDATE_PLANE_SET=0");
    rval = kdtree.build_tree(cub_verts, &root, &tree_opts);
    if (MB_SUCCESS != rval)
      return rval;
    AdaptiveKDTreeIter tree_iter;
    rval = kdtree.get_tree_iterator(root, tree_iter);
    if (MB_SUCCESS != rval)
      return rval;
  }

  // For each exo vert, find the matching cub vert
  for (int i = 0; i < numberNodes_loading; ++i) {
    int exo_id = ptr[i];
    CartVect exo_coords(orig_coords[0][i], orig_coords[1][i], orig_coords[2][i]);
    EntityHandle cub_vert = 0;
    bool found_match = false;

    // By id
    if (match_node_ids) {
      std::map<int, EntityHandle>::iterator i_iter;
      i_iter = cub_verts_id_map.find(exo_id);
      if (i_iter != cub_verts_id_map.end()) {
        found_match = true;
        cub_vert = i_iter->second;
      }

    // By proximity
    }
    else {
      // The MAX_NODE_DIST is the farthest distance to  search for a node.
      // For the 1/12th symmetry 85 pin model, the max node dist could not be less
      // than 1e-1 (March 26, 2010).
      const double MAX_NODE_DIST = 1e-1;

      std::vector<EntityHandle> leaves;
      double min_dist = MAX_NODE_DIST;
      rval = kdtree.distance_search(exo_coords.array(), MAX_NODE_DIST, leaves);
      if (MB_SUCCESS != rval)
        return rval;
      for (std::vector<EntityHandle>::const_iterator j = leaves.begin(); j != leaves.end(); ++j) {
        std::vector<EntityHandle> leaf_verts;
        rval = mdbImpl->get_entities_by_type(*j, MBVERTEX, leaf_verts);
        if (MB_SUCCESS != rval)
          return rval;
        for (std::vector<EntityHandle>::const_iterator k = leaf_verts.begin(); k != leaf_verts.end(); ++k) {
          CartVect orig_cub_coords, difference;
          rval = mdbImpl->get_coords(&(*k), 1, orig_cub_coords.array());
          if (MB_SUCCESS != rval)
            return rval;
          difference = orig_cub_coords - exo_coords;
          double dist = difference.length();
          if (dist < min_dist) {
            min_dist = dist;
            cub_vert = *k;
          }
        }
      }
      if (0 != cub_vert)
        found_match = true;
    }

    // If a match is found, update it with the deformed coords from the exo file.
    if (found_match) {
      CartVect updated_exo_coords;
      matched_cub_vert_id_map.insert(std::pair<int, EntityHandle>(exo_id, cub_vert));
      updated_exo_coords[0] = orig_coords[0][i] + deformed_arrays[0][i];
      updated_exo_coords[1] = orig_coords[1][i] + deformed_arrays[1][i];
      if (numberDimensions_loading == 3)
        updated_exo_coords[2] = orig_coords[2][i] + deformed_arrays[2][i];
      rval = mdbImpl->set_coords(&cub_vert, 1, updated_exo_coords.array());
      if (MB_SUCCESS != rval)
        return rval;
      ++found;
      double magnitude = sqrt(deformed_arrays[0][i]*deformed_arrays[0][i] +
                              deformed_arrays[1][i]*deformed_arrays[1][i] +
                              deformed_arrays[2][i]*deformed_arrays[2][i]);
      if (magnitude > max_magnitude)
        max_magnitude = magnitude;
      average_magnitude += magnitude;
    }
    else {
      ++lost;
      std::cout << "cannot match exo node " << exo_id << " " << exo_coords << std::endl;
    }
  }

  // Exo verts that could not be matched have already been printed. Now print the
  // cub verts that could not be matched.
  if (matched_cub_vert_id_map.size() < cub_verts.size()) {
    Range unmatched_cub_verts = cub_verts;
    for (std::map<int, EntityHandle>::const_iterator i = matched_cub_vert_id_map.begin();
        i != matched_cub_vert_id_map.end(); ++i) {
      unmatched_cub_verts.erase(i->second);
    }
    for (Range::const_iterator i = unmatched_cub_verts.begin(); i != unmatched_cub_verts.end(); ++i) {
      int cub_id;
      rval = mdbImpl->tag_get_data(mGlobalIdTag, &(*i), 1, &cub_id);
      if (MB_SUCCESS != rval)
        return rval;
      CartVect cub_coords;
      rval = mdbImpl->get_coords(&(*i), 1, cub_coords.array());
      if (MB_SUCCESS != rval)
        return rval;
      std::cout << "cannot match cub node " << cub_id << " " << cub_coords << std::endl;
    }
    std::cout << "  " << unmatched_cub_verts.size()
              << " nodes from the cub file could not be matched." << std::endl;
  }

  // Summarize statistics
  std::cout << "  " << found << " nodes from the exodus file were matched in the cub_file_set ";
  if (match_node_ids) {
    std::cout << "by id." << std::endl;
  }
  else {
    std::cout << "by proximity." << std::endl;
  }

  // Fail if all of the nodes could not be matched.
  if (0 != lost) {
    std::cout << "Error:  " << lost << " nodes from the exodus file could not be matched."
              << std::endl;
    //return MB_FAILURE;
  }
  std::cout << "  maximum node displacement magnitude: " << max_magnitude
            << " cm" << std::endl;
  std::cout << "  average node displacement magnitude: " << average_magnitude / found
            << " cm" << std::endl;

  // *******************************************************************
  // Remove dead elements from the MOAB instance.
  // *******************************************************************

  // How many element variables are in the file?
  int n_elem_var;
  GET_DIM(ncdim, "num_elem_var", n_elem_var);

  // Get element variable names
  varid = -1;
  int cstatus = nc_inq_varid(ncFile, "name_elem_var", &varid);
  std::vector<char> names_memory(n_elem_var * max_str_length);
  std::vector<char*> names(n_elem_var);
  for (int i = 0; i < n_elem_var; ++i)
    names[i] = &names_memory[i*max_str_length];
  if (cstatus!=NC_NOERR || varid == -1) {
    std::cout << "ReadNCDF: name_elem_var does not exist" << std::endl;
    return MB_FAILURE;
  }
  int status = nc_get_var_text(ncFile, varid, &names_memory[0]);
  if (NC_NOERR != status) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF: Problem getting element variable names");
  }

  // Is one of the element variable names "death_status"? If so, get its index
  // in the element variable array.
  int death_index;
  bool found_death_index = false;
  for (int i = 0; i < n_elem_var; ++i) {
    std::string temp(names[i]);
    std::string::size_type pos0 = temp.find("death_status");
    std::string::size_type pos1 = temp.find("Death_Status");
    std::string::size_type pos2 = temp.find("DEATH_STATUS");
    if (std::string::npos != pos0 || std::string::npos != pos1 || std::string::npos != pos2) {
      found_death_index = true;
      death_index = i + 1; // NetCDF variables start with 1
      break;
    }
  }
  if (!found_death_index) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF: Problem getting index of death_status variable");
  }

  // The exodus header has already been read. This contains the number of element
  // blocks.

  // Dead elements are listed by block. Read the block headers to determine how
  // many elements are in each block.
  rval = read_block_headers(blocks_to_load, num_blocks);
  if (MB_FAILURE == rval) {
    MB_SET_ERR(MB_FAILURE, "ReadNCDF: Problem reading block headers");
  }

  // Dead elements from the Exodus file can be located in the cub_file_set by id
  // or by connectivity. Currently, finding elements by id requires careful book
  // keeping when constructing the model in Cubit. To avoid this, one can match
  // elements by connectivity instead.
  const bool match_elems_by_connectivity = true;

  // Get the element id map. The ids in the map are from the elements in the blocks.
  // elem_num_map(blk1 elem ids, blk2 elem ids, blk3 elem ids, ...)
  std::vector<int> elem_ids(numberNodes_loading);
  if (!match_elems_by_connectivity) {
    GET_1D_INT_VAR("elem_num_map", varid, elem_ids);
    if (-1 == varid) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF: Problem getting element number map data");
    }
  }

  // For each block
  int first_elem_id_in_block = 0;
  int block_count = 1; // NetCDF variables start with 1
  int total_elems = 0;
  int total_dead_elems = 0;
  for (std::vector<ReadBlockData>::iterator i = blocksLoading.begin();
      i != blocksLoading.end(); ++i) {
    // Get the ncdf connect variable
    std::string temp_string("connect");
    std::stringstream temp_ss;
    temp_ss << block_count;
    temp_string += temp_ss.str();
    temp_string += "\0";
    std::vector<int> ldims;
    GET_VAR(temp_string.c_str(), nc_var, ldims);
    if (!nc_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting connect variable");
    }
    // The element type is an attribute of the connectivity variable
    nc_type att_type;
    size_t att_len;
    fail = nc_inq_att(ncFile, nc_var, "elem_type", &att_type, &att_len);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting elem type attribute");
    }
    std::vector<char> dum_str(att_len + 1);
    fail = nc_get_att_text(ncFile, nc_var, "elem_type", &dum_str[0]);
    if (NC_NOERR != fail) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting elem type");
    }
    ExoIIElementType elem_type = ExoIIUtil::static_element_name_to_type(&dum_str[0]);
    (*i).elemType = elem_type;
    const EntityType mb_type = ExoIIUtil::ExoIIElementMBEntity[(*i).elemType];

    // Get the number of nodes per element
    unsigned int nodes_per_element = ExoIIUtil::VerticesPerElement[(*i).elemType];
    
    // Read the connectivity into that memory.
    //int exo_conn[i->numElements][nodes_per_element];
    int *exo_conn = new int [i->numElements*nodes_per_element];
    //NcBool status = temp_var->get(&exo_conn[0][0], i->numElements, nodes_per_element);
    size_t lstart[2] = {0, 0}, lcount[2];
    lcount[0] = i->numElements;
    lcount[1] = nodes_per_element;
    fail = nc_get_vara_int(ncFile, nc_var, lstart, lcount, exo_conn);
    if (NC_NOERR != fail) {
      delete [] exo_conn;
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting connectivity");
    }

    // Get the death_status at the correct time step.
    std::vector<double> death_status(i->numElements); // It seems wrong, but it uses doubles
    std::string array_name("vals_elem_var");
    temp_ss.str(""); // stringstream won't clear by temp.clear() 
    temp_ss << death_index;
    array_name += temp_ss.str();
    array_name += "eb";
    temp_ss.str(""); // stringstream won't clear by temp.clear()
    temp_ss << block_count;
    array_name += temp_ss.str();
    array_name += "\0";
    GET_VAR(array_name.c_str(), nc_var, ldims);
    if (!nc_var) {
      MB_SET_ERR(MB_FAILURE, "ReadNCDF:: Problem getting death status variable");
    }
    lstart[0] = time_step - 1; lstart[1] = 0;
    lcount[0] = 1;
    lcount[1] = i->numElements;
    status = nc_get_vara_double(ncFile, nc_var, lstart, lcount, &death_status[0]);
    if (NC_NOERR != status) {
      delete[] exo_conn;
      MB_SET_ERR(MB_FAILURE, "ReadNCDF: Problem setting time step for death_status");
    }

    // Look for dead elements. If there is too many dead elements and this starts
    // to take too long, I should put the elems in a kd-tree for more efficient
    // searching. Alternatively I could get the exo connectivity and match nodes.
    int dead_elem_counter = 0, missing_elem_counter = 0;
    for (int j = 0; j < i->numElements; ++j) {
      if (1 != death_status[j]) {
        Range cub_elem, cub_nodes;
        if (match_elems_by_connectivity) {
          // Get exodus nodes for the element
          std::vector<int> elem_conn(nodes_per_element);
          for (unsigned int k = 0; k < nodes_per_element; ++k) {
            //elem_conn[k] = exo_conn[j][k];
            elem_conn[k] = exo_conn[j*nodes_per_element + k];
          }
          // Get the ids of the nodes (assume we are matching by id)
          // Remember that the exodus array locations start with 1 (not 0).
          std::vector<int> elem_conn_node_ids(nodes_per_element);
          for (unsigned int k = 0; k < nodes_per_element; ++k) {
            elem_conn_node_ids[k] = ptr[elem_conn[k] - 1];
          }
          // Get the cub_file_set nodes by id
          // The map is a log search and takes almost no time. 
          // MOAB's linear tag search takes 5-10 minutes.
          for (unsigned int k = 0; k < nodes_per_element; ++k) {
            std::map<int, EntityHandle>::iterator k_iter;
            k_iter = matched_cub_vert_id_map.find(elem_conn_node_ids[k]);

            if (k_iter == matched_cub_vert_id_map.end()) {
              std::cout << "ReadNCDF: Found no cub node with id=" << elem_conn_node_ids[k]
                        << ", but expected to find only 1." << std::endl;
              break;
            }
            cub_nodes.insert(k_iter->second);
          }

          if (nodes_per_element != cub_nodes.size()) {
            std::cout << "ReadNCDF: nodes_per_elemenet != cub_nodes.size()" << std::endl;
            delete[] exo_conn;
            return MB_INVALID_SIZE;
          }

          // Get the cub_file_set element with the same nodes
          int to_dim = CN::Dimension(mb_type);
          rval = mdbImpl->get_adjacencies(cub_nodes, to_dim, false, cub_elem);
          if (MB_SUCCESS != rval) {
            std::cout << "ReadNCDF: could not get dead cub element" << std::endl;
            delete[] exo_conn;
            return rval;
          }

          // Pronto/Presto renumbers elements, so matching cub and exo elements by
          // id is not possible at this time.
        }
        else {
          // Get dead element's id
          int elem_id = elem_ids[first_elem_id_in_block + j];
          void *id[] = {&elem_id};
          // Get the element by id
          rval = mdbImpl->get_entities_by_type_and_tag(cub_file_set, mb_type,
                                                       &mGlobalIdTag, id, 1, cub_elem,
                                                       Interface::INTERSECT);
          if (MB_SUCCESS != rval) {
            delete[] exo_conn;
            return rval;
          }
        }

        if (1 == cub_elem.size()) {
          // Delete the dead element from the cub file. It will be removed from sets
          // ONLY if they are tracking meshsets.
          rval = mdbImpl->remove_entities(cub_file_set, cub_elem);
          if (MB_SUCCESS != rval) {
            delete[] exo_conn;
            return rval;
          }
          rval = mdbImpl->delete_entities(cub_elem);
          if (MB_SUCCESS != rval) {
            delete[] exo_conn;
            return rval;
          }
        }
        else {
          std::cout << "ReadNCDF: Should have found 1 element with  type="
                    << mb_type << " in cub_file_set, but instead found "
                    << cub_elem.size() << std::endl;
          rval = mdbImpl->list_entities(cub_nodes);
          ++missing_elem_counter;
          delete[] exo_conn;
          return MB_FAILURE;
        }
        ++dead_elem_counter;
      }
    }
    // Print some statistics
    temp_ss.str("");
    temp_ss << i->blockId;
    total_dead_elems += dead_elem_counter;
    total_elems      += i->numElements;
    std::cout << "  Block " << temp_ss.str() << " has " << dead_elem_counter << "/"
              << i->numElements << " dead elements." << std::endl;
    if (0 != missing_elem_counter) {
      std::cout << "    " << missing_elem_counter 
                << " dead elements in this block were not found in the cub_file_set."
                << std::endl;
    }

    // Advance the pointers into element ids and block_count. Memory cleanup.
    first_elem_id_in_block += i->numElements;
    ++block_count;
    delete[] exo_conn;
  }

  std::cout << " Total: " << total_dead_elems << "/" << total_elems
            << " dead elements." << std::endl;

  // Close the file
  fail = nc_close(ncFile);
  if (NC_NOERR != fail) {
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");
  }

  ncFile = 0;
  return MB_SUCCESS;
}

void ReadNCDF::tokenize(const std::string& str,
                        std::vector<std::string>& tokens,
                        const char* delimiters)
{
  std::string::size_type last = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos  = str.find_first_of(delimiters, last);
  while (std::string::npos != pos && std::string::npos != last) {
    tokens.push_back(str.substr(last, pos - last));
    last = str.find_first_not_of(delimiters, pos);
    pos  = str.find_first_of(delimiters, last);
    if (std::string::npos == pos)
      pos = str.size();
  }
}

} // namespace moab
