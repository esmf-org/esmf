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

#include "Tqdcfr.hpp"
#include "moab/Core.hpp"
#include "moab/Range.hpp"
#include "moab/FileOptions.hpp"
#include <iostream>
#include <string>

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#endif

#include "moab/ReadUtilIface.hpp"
#include "SequenceManager.hpp"
#include "moab/GeomTopoTool.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"
#include "moab/HigherOrderFactory.hpp"
#include "exodus_order.h"

#include <sstream>
#include <assert.h>
#include <string.h>

namespace moab {

static bool debug = false;

const char Tqdcfr::geom_categories[][CATEGORY_TAG_SIZE] =
{"Vertex\0", "Curve\0", "Surface\0", "Volume\0"};

// Will be used in a static function, so declared outside class members :(
// major/minor cubit version that wrote this file
static int major = -1, minor = -1;
const EntityType Tqdcfr::group_type_to_mb_type[] = {
  MBENTITYSET, MBENTITYSET, MBENTITYSET, // group, body, volume
  MBENTITYSET, MBENTITYSET, MBENTITYSET, // surface, curve, vertex
  MBHEX, MBTET, MBPYRAMID, MBQUAD, MBTRI, MBEDGE, MBVERTEX
};

const EntityType Tqdcfr::block_type_to_mb_type[] = {
  MBVERTEX, // sphere
  MBEDGE, MBEDGE, MBEDGE, // bars
  MBEDGE, MBEDGE, MBEDGE, // beams
  MBEDGE, MBEDGE, MBEDGE, // truss
  MBEDGE, // spring
  MBTRI, MBTRI, MBTRI, MBTRI, // tri
  MBTRI, MBTRI, MBTRI, MBTRI, // trishell
  MBQUAD, MBQUAD, MBQUAD, MBQUAD, // shell
  MBQUAD, MBQUAD, MBQUAD, MBQUAD, MBQUAD, // quad
  MBTET, MBTET, MBTET, MBTET, MBTET, // tet
  MBPYRAMID, MBPYRAMID, MBPYRAMID, MBPYRAMID, MBPYRAMID, // pyramid
  MBHEX, MBHEX, MBHEX, MBHEX, MBHEX, // hex
  MBHEX, // hexshell
  MBMAXTYPE // last
};

// Mapping from mesh packet type to moab type
const EntityType Tqdcfr::mp_type_to_mb_type[] = {
  MBHEX, MBHEX, MBHEX, MBHEX, MBHEX, MBHEX, MBHEX, MBHEX, 
  MBTET, MBTET, MBTET, MBTET, MBTET, MBTET, MBTET, MBTET, 
  MBPYRAMID, MBPYRAMID, MBPYRAMID, MBPYRAMID, 
  MBQUAD, MBQUAD, MBQUAD, MBQUAD, 
  MBTRI, MBTRI, MBTRI, MBTRI, 
  MBEDGE, MBEDGE, MBVERTEX
};

const int Tqdcfr::cub_elem_num_verts[] = {
  1, // sphere
  2, 2, 3, // bars
  2, 2, 3, // beams
  2, 2, 3, // truss
  2, // spring
  3, 3, 6, 7, // tris
  3, 3, 6, 7, // trishells
  4, 4, 8, 9, // shells
  4, 4, 5, 8, 9, // quads
  4, 4, 8, 10, 14, // tets
  5, 5, 8, 13, 18, // pyramids
  8, 8, 9, 20, 27, 12, // hexes (incl. hexshell at end)
  0
};

const int Tqdcfr::cub_elem_num_verts_len =
  sizeof(cub_elem_num_verts) / sizeof(cub_elem_num_verts[0]);

// Define node-order map from Cubit to CN. Table is indexed
// by EntityType and number of nodes. Entries are NULL if Cubit order
// is the same as CN ordering. Non-null entries contain the
// index into the MOAB node order for the corresponding vertex
// in the Cubit connectivity list. Thus for a 27-node hex:
// moab_conn[ cub_hex27_order[i] ] = cubit_conn[ i ];
const int* const* const* const cub_elem_order_map = exodus_elem_order_map;

const char *const BLOCK_NODESET_OFFSET_TAG_NAME = "BLOCK_NODESET_OFFSET";
const char *const BLOCK_SIDESET_OFFSET_TAG_NAME = "BLOCK_SIDESET_OFFSET";

#define RR if (MB_SUCCESS != result) return result

// acis dimensions for each entity type, to match
// enum {BODY, LUMP, SHELL, FACE, LOOP, COEDGE, EDGE, VERTEX, ATTRIB, UNKNOWN} 

#define IO_ASSERT(C) INT_IO_ERROR(C, __LINE__)

static inline void INT_IO_ERROR(bool condition, unsigned line) {
  if (!condition) {
    char buffer[] = __FILE__ "             ";
    sprintf(buffer, "%s:%u", __FILE__, line);
    fflush(stderr);
    perror(buffer);
    abort();
  }
}

void Tqdcfr::FSEEK(unsigned int offset) {
  int rval = fseek(cubFile, offset, SEEK_SET);
  IO_ASSERT(!rval);
}

void Tqdcfr::FREADI(unsigned num_ents) {
  if (uint_buf.size() < num_ents) {
    uint_buf.resize(num_ents);
    int_buf = (int*)&uint_buf[0];
  }
  FREADIA(num_ents, &uint_buf[0]);
}

void Tqdcfr::FREADD(unsigned num_ents) {
  dbl_buf.resize(num_ents);
  FREADDA(num_ents, &dbl_buf[0]);
}

void Tqdcfr::FREADC(unsigned num_ents) {
  char_buf.resize(num_ents);
  FREADCA(num_ents, &char_buf[0]);
}

// Used for swapping
static void swap8_voff(long *data)
{
  unsigned char tmp, *cdat = (unsigned char *) data;
  tmp = cdat[0]; cdat[0] = cdat[7], cdat[7] = tmp;
  tmp = cdat[1]; cdat[1] = cdat[6], cdat[6] = tmp;
  tmp = cdat[2]; cdat[2] = cdat[5], cdat[5] = tmp;
  tmp = cdat[3]; cdat[3] = cdat[4], cdat[4] = tmp;
}

static void swap4_uint(unsigned int *data)
{
  unsigned char tmp, *cdat = (unsigned char *) data;
  tmp = cdat[0]; cdat[0] = cdat[3], cdat[3] = tmp;
  tmp = cdat[1]; cdat[1] = cdat[2], cdat[2] = tmp;
}

/*
static void swap2_ushort(unsigned short *data)
{
  unsigned char tmp, *cdat = (unsigned char *) data;
  tmp = cdat[0]; cdat[0] = cdat[1], cdat[1] = tmp;
}
*/

void Tqdcfr::FREADIA(unsigned num_ents, unsigned int* array) {
  unsigned rval = fread(array, sizeof(unsigned int), num_ents, cubFile);
  IO_ASSERT(rval == num_ents);
  if (swapForEndianness) {
    unsigned int* pt = array;
    for (unsigned int i = 0; i < num_ents; i++) {
      swap4_uint((unsigned int *)pt);
      pt++;
    }
  }
}

void Tqdcfr::FREADDA(unsigned num_ents, double* array) {
  unsigned rval = fread(array, sizeof(double), num_ents, cubFile);
  IO_ASSERT(rval == num_ents);
  if (swapForEndianness) {
    double* pt = array;
    for (unsigned int i = 0; i < num_ents; i++) {
      swap8_voff((long *)pt);
      pt++;
    }
  }
}

void Tqdcfr::FREADCA(unsigned num_ents, char* array) {
  unsigned rval = fread(array, sizeof(char), num_ents, cubFile);
  IO_ASSERT(rval == num_ents);
}

void Tqdcfr::CONVERT_TO_INTS(unsigned int num_ents)
{
  for (unsigned int i = 0; i < num_ents; i++)
    int_buf[i] = uint_buf[i];
}

ReaderIface* Tqdcfr::factory(Interface* iface)
{
  return new Tqdcfr(iface);
}

Tqdcfr::Tqdcfr(Interface *impl)
  :  cubFile(NULL), globalIdTag(0), cubIdTag(0), geomTag(0), uniqueIdTag(0),
    blockTag(0), nsTag(0), ssTag(0), attribVectorTag(0), entityNameTag(0), categoryTag(0),
    hasMidNodesTag(0), swapForEndianness(false), int_buf(NULL), mFileSet(0),
    printedSeqWarning(false), printedElemWarning(false), acisDumpFile(NULL)
{
  assert(NULL != impl);
  mdbImpl = impl;
  impl->query_interface(readUtilIface);
  assert(NULL != readUtilIface);

  currVHandleOffset = -1;
  for (EntityType this_type = MBVERTEX; this_type < MBMAXTYPE; this_type++)
    currElementIdOffset[this_type] = -1;

  ErrorCode rval;
  rval = mdbImpl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, blockTag);
  MB_CHK_SET_ERR_RET(rval, "Failed to tag_get_handle.");
  rval = mdbImpl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER, nsTag);
  MB_CHK_SET_ERR_RET(rval, "Failed to tag_get_handle.");
  rval = mdbImpl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER, ssTag);
  MB_CHK_SET_ERR_RET(rval, "Failed to tag_get_handle.");

  if (0 == entityNameTag) {
    rval = mdbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE,
                                   MB_TYPE_OPAQUE, entityNameTag,
                                   MB_TAG_SPARSE | MB_TAG_CREAT);
    MB_CHK_SET_ERR_RET(rval, "Failed to tag_get_handle.");
  }

  cubMOABVertexMap = NULL;
}

Tqdcfr::~Tqdcfr() 
{
  mdbImpl->release_interface(readUtilIface);

  if (NULL != cubMOABVertexMap)
    delete cubMOABVertexMap;
  if (attribVectorTag)
  {
    // get all sets, and release the string vectors
    Range allSets; // although only geom sets should have these attributes
    // can't error in a destructor
    ErrorCode rval = mdbImpl->get_entities_by_type(0, MBENTITYSET, allSets);
    if (rval != MB_SUCCESS)
      std::cerr << "WARNING: Could not get_entities_by_type" << std::endl;
    for (Range::iterator sit=allSets.begin(); sit!=allSets.end(); ++sit)
    {
      EntityHandle gset=*sit;
      std::vector<std::string> *dum_vec;
      // can't error in a destructor
      rval = mdbImpl->tag_get_data(attribVectorTag, &gset, 1, &dum_vec);
      if (rval != MB_SUCCESS)
        std::cerr << "WARNING: Could not tag_get_data" << std::endl;
      if(NULL!=dum_vec)
        delete dum_vec; //
    }
    mdbImpl->tag_delete(attribVectorTag);
    attribVectorTag = NULL;
  }
}

ErrorCode Tqdcfr::read_tag_values(const char* /* file_name */,
                                  const char* /* tag_name */,
                                  const FileOptions& /* opts */,
                                  std::vector<int>& /* tag_values_out */,
                                  const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode Tqdcfr::load_file(const char *file_name,
                            const EntityHandle* ,
                            const FileOptions& opts,
                            const ReaderIface::SubsetList* subset_list,
                            const Tag* file_id_tag)
{
  ErrorCode result;

  int tmpval;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_IO", 1, tmpval)) {
    if (0 < tmpval)
      debug = true;
  }

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for CUB files");
  }

  // Open file
  cubFile = fopen(file_name, "rb");
  if (NULL == cubFile) {
    MB_SET_ERR(MB_FAILURE, "File not found");
  }

  // Verify magic string
  FREADC(4);
  if (!(char_buf[0] == 'C' && char_buf[1] == 'U' && 
        char_buf[2] == 'B' && char_buf[3] == 'E')) {
    fclose(cubFile);
    MB_SET_ERR(MB_FAILURE, "This doesn't appear to be a .cub file");
  }

  // Get "before" entities
  result = mdbImpl->get_entities_by_handle(0, beforeEnts);MB_CHK_SET_ERR(result, "Couldn't get \"before\" entities");

  // ***********************
  // Read model header type information...
  // ***********************
  if (debug)
    std::cout << "Reading file header." << std::endl;
  result = read_file_header(); RR;

  if (debug)
    std::cout << "Reading model entries." << std::endl;
  result = read_model_entries(); RR;

  // Read model metadata
  if (debug)
    std::cout << "Reading model metadata." << std::endl;
  result = read_meta_data(fileTOC.modelMetaDataOffset, modelMetaData); RR;

  double data_version;
  int md_index = modelMetaData.get_md_entry(2, "DataVersion");
  if (-1 == md_index)
    data_version = 1.0;
  else
    data_version = modelMetaData.metadataEntries[md_index].mdDblValue;

  // Get the major/minor cubit version that wrote this file
  //int major = -1, minor = -1;
  md_index = modelMetaData.get_md_entry(2, "CubitVersion");
  if (md_index >= 0 && !modelMetaData.metadataEntries[md_index].mdStringValue.empty())
    sscanf(modelMetaData.metadataEntries[md_index].mdStringValue.c_str(), "%d.%d",
           &major, &minor);

  // ***********************
  // Read mesh...
  // ***********************
  int index = find_model(mesh); 
  if (-1 == index)
    return MB_FAILURE;
  ModelEntry *mesh_model = &modelEntries[index];

  // First the header & metadata info
  if (debug) std::cout << "Reading mesh model header and metadata." << std::endl;
  result = mesh_model->read_header_info(this, data_version);
  if (MB_SUCCESS != result)
    return result;
  result = mesh_model->read_metadata_info(this);
  if (MB_SUCCESS != result)
    return result;

  // Now read in mesh for each geometry entity; read in order of increasing dimension
  // so we have all the mesh we need
  for (int dim = 0; dim < 4; dim++) {
    for (unsigned int gindex = 0;
         gindex < mesh_model->feModelHeader.geomArray.numEntities;
         gindex++) {
      Tqdcfr::GeomHeader *geom_header = &mesh_model->feGeomH[gindex];

      if (geom_header->maxDim != dim)
        continue;

      // Read nodes
      if (debug) std::cout << "Reading geom index " << gindex << " mesh: nodes... ";
      result = read_nodes(gindex, mesh_model, geom_header); 
      if (MB_SUCCESS != result)
        return result;

      // Read elements
      if (debug) std::cout << "elements... ";
      result = read_elements(mesh_model, geom_header); 
      if (MB_SUCCESS != result)
        return result;
      if (debug)
        std::cout << std::endl;
    }
  }

  // ***********************
  // Read acis records...
  // ***********************
  std::string sat_file_name;
  if (MB_SUCCESS != opts.get_str_option("SAT_FILE", sat_file_name))
    sat_file_name.clear();
  result = read_acis_records(sat_file_name.empty() ? NULL : sat_file_name.c_str()); RR;

  // ***********************
  // Read groups...
  // ***********************
  if (debug) std::cout << "Reading groups... ";
  for (unsigned int grindex = 0; 
       grindex < mesh_model->feModelHeader.groupArray.numEntities;
       grindex++) {
    GroupHeader *group_header = &mesh_model->feGroupH[grindex];
    result = read_group(grindex, mesh_model, group_header); 
    if (MB_SUCCESS != result)
      return result;
  }
  if (debug)
    std::cout << mesh_model->feModelHeader.groupArray.numEntities
              << " read successfully." << std::endl;

  // ***********************
  // Read blocks...
  // ***********************
  if (debug) std::cout << "Reading blocks... ";
  Range ho_entities;
  for (unsigned int blindex = 0; 
       blindex < mesh_model->feModelHeader.blockArray.numEntities;
       blindex++) {
    BlockHeader *block_header = &mesh_model->feBlockH[blindex];
    result = read_block(blindex, data_version, mesh_model, block_header);
    if (MB_SUCCESS != result)
      return result;
  }

  if (debug) std::cout << mesh_model->feModelHeader.blockArray.numEntities
                       << " read successfully." << std::endl;

  // ***********************
  // Read nodesets...
  // ***********************
  if (debug) std::cout << "Reading nodesets... ";
  for (unsigned int nsindex = 0; 
       nsindex < mesh_model->feModelHeader.nodesetArray.numEntities;
       nsindex++) {
    NodesetHeader *nodeset_header = &mesh_model->feNodeSetH[nsindex];
    result = read_nodeset(nsindex, mesh_model, nodeset_header); 
    if (MB_SUCCESS != result)
      return result;
  }
  if (debug) std::cout << mesh_model->feModelHeader.nodesetArray.numEntities
                       << " read successfully." << std::endl;

  // ***********************
  // Read sidesets...
  // ***********************
  if (debug) std::cout << "Reading sidesets...";
  for (unsigned int ssindex = 0;
       ssindex < mesh_model->feModelHeader.sidesetArray.numEntities;
       ssindex++) {
    SidesetHeader *sideset_header = &mesh_model->feSideSetH[ssindex];
    result = read_sideset(ssindex, data_version, mesh_model, sideset_header);
    if (MB_SUCCESS != result)
      return result;
  }
  if (debug)std::cout << mesh_model->feModelHeader.sidesetArray.numEntities
                      << " read successfully." << std::endl;

  if (debug) {
    std::cout << "Read the following mesh:" << std::endl;
    mdbImpl->list_entities(0, 0);
  }

  // **************************
  // Restore geometric topology
  // **************************
  GeomTopoTool gtt(mdbImpl, true);
  result = gtt.restore_topology();
  if (MB_SUCCESS != result) {
    std::cout << "Failed to restore topology " << std::endl;
  }

  // Convert blocks to nodesets/sidesets if tag is set
  result = convert_nodesets_sidesets();
  if (MB_SUCCESS != result)
    return result;

  Range after_ents;
  result = mdbImpl->get_entities_by_handle(0, after_ents);
  if (MB_SUCCESS != result) 
    return result;

  after_ents = subtract(after_ents, beforeEnts);

  if (file_id_tag)
    readUtilIface->assign_ids(*file_id_tag, after_ents);

  // done with the cubit file
  fclose(cubFile); 
  return result;
}

ErrorCode Tqdcfr::convert_nodesets_sidesets() 
{
  // Look first for the nodeset and sideset offset flags; if they're not
  // set, we don't need to convert
  const EntityHandle msh = 0;
  unsigned int nodeset_offset, sideset_offset;
  Tag tmp_tag;
  ErrorCode result = mdbImpl->tag_get_handle(BLOCK_NODESET_OFFSET_TAG_NAME,
                                             1, MB_TYPE_INTEGER, tmp_tag);
  if (MB_SUCCESS != result) nodeset_offset = 0;
  else {
    result = mdbImpl->tag_get_data(tmp_tag, &msh, 1, &nodeset_offset);
    if (MB_SUCCESS != result)
      return result;
  }

  result = mdbImpl->tag_get_handle(BLOCK_SIDESET_OFFSET_TAG_NAME,
                                   1, MB_TYPE_INTEGER, tmp_tag);
  if (MB_SUCCESS != result) sideset_offset = 0;
  else {
    result = mdbImpl->tag_get_data(tmp_tag, &msh, 1, &sideset_offset);
    if (MB_SUCCESS != result)
      return result;
  }

  if (0 == nodeset_offset && 0 == sideset_offset)
    return MB_SUCCESS;

  // Look for all blocks
  Range blocks;
  result = mdbImpl->get_entities_by_type_and_tag(0, MBENTITYSET,
                                                 &blockTag, NULL, 1,
                                                 blocks);
  if (MB_SUCCESS != result || blocks.empty())
    return result;

  // Get the id tag for them
  std::vector<int> block_ids(blocks.size());
  result = mdbImpl->tag_get_data(globalIdTag, blocks, &block_ids[0]);
  if (MB_SUCCESS != result)
    return result;

  unsigned int i = 0;
  Range::iterator rit = blocks.begin();
  Range new_nodesets, new_sidesets;
  std::vector<int> new_nodeset_ids, new_sideset_ids;
  for ( ; rit != blocks.end(); i++, ++rit) {
    if (0 != nodeset_offset && block_ids[i] >= (int) nodeset_offset &&
        (nodeset_offset > sideset_offset || block_ids[i] < (int) sideset_offset)) {
      // This is a nodeset
      new_nodesets.insert(*rit);
      new_nodeset_ids.push_back(block_ids[i]);
    }
    else if (0 != sideset_offset && block_ids[i] >= (int) sideset_offset &&
             (sideset_offset > nodeset_offset || block_ids[i] < (int) nodeset_offset)) {
      // This is a sideset
      new_sidesets.insert(*rit);
      new_sideset_ids.push_back(block_ids[i]);
    }
  }

  // OK, have the new nodesets and sidesets; now remove the block tags, and
  // add nodeset and sideset tags
  ErrorCode tmp_result = MB_SUCCESS;
  if (0 != nodeset_offset) {
    if (0 == nsTag) {
      int default_val = 0;
      tmp_result = mdbImpl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                           nsTag, MB_TAG_SPARSE | MB_TAG_CREAT, &default_val);
      if (MB_SUCCESS != tmp_result)
        result = tmp_result;
    }
    if (MB_SUCCESS == tmp_result)
      tmp_result = mdbImpl->tag_set_data(nsTag, new_nodesets, 
                                         &new_nodeset_ids[0]);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
    tmp_result = mdbImpl->tag_delete_data(blockTag, new_nodesets);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
  }
  if (0 != sideset_offset) {
    if (0 == ssTag) {
      int default_val = 0;
      tmp_result = mdbImpl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                           ssTag, MB_TAG_SPARSE | MB_TAG_CREAT, &default_val);
      if (MB_SUCCESS != tmp_result)
        result = tmp_result;
    }
    if (MB_SUCCESS == tmp_result) 
      tmp_result = mdbImpl->tag_set_data(ssTag, new_sidesets,
                                         &new_sideset_ids[0]);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
    tmp_result = mdbImpl->tag_delete_data(blockTag, new_sidesets);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
  }

  return result;
}

ErrorCode Tqdcfr::read_nodeset(const unsigned int nsindex,
                               Tqdcfr::ModelEntry *model,
                               Tqdcfr::NodesetHeader *nodeseth)
{
  if (nodeseth->memTypeCt == 0)
    return MB_SUCCESS;

  // Position file
  FSEEK(model->modelOffset + nodeseth->memOffset);

  // Read ids for each entity type
  unsigned int this_type, num_ents; //, uid;
  std::vector<char> bc_data;
  unsigned int num_read = 0;
  std::vector<EntityHandle> ns_entities, excl_entities;
  for (unsigned int i = 0; i < nodeseth->memTypeCt; i++) {
    // Get how many and what type
    FREADI(2); num_read += 2*sizeof(int);
    this_type = uint_buf[0];
    num_ents = uint_buf[1];

    // Now get the ids
    FREADI(num_ents); num_read += sizeof(int);
    CONVERT_TO_INTS(num_ents);

    ErrorCode result = get_entities(this_type + 2, &int_buf[0], num_ents,
                                    ns_entities, excl_entities);
    if (MB_SUCCESS != result)
      return result;
  }
  // Check for more data
  if (num_read < nodeseth->nsLength) {
    FREADC(2); //num_read += 2;
    if (char_buf[0] == 'i' && char_buf[1] == 'd') {
      FREADI(1); //num_read += sizeof(int);
      //uid = int_buf[0];
    }
    else {
      if (char_buf[0] == 'b' && char_buf[1] == 'c') {
        FREADI(1); //num_read += sizeof(int);
        int num_bcs = uint_buf[0];
        bc_data.resize(num_bcs);
        FREADCA(num_bcs, &bc_data[0]); //num_read += num_bcs;
      }
    }
  }

  if (debug) {
    nodeseth->print();
    if (!bc_data.empty()) {
      std::cout << "bc_data = ";
      std::vector<char>::iterator vit = bc_data.begin();
      for ( ; vit != bc_data.end(); ++vit) {
        std::cout << std::hex << (int)((unsigned char)*vit) << " ";
      }
      std::cout << ": ";
      vit = bc_data.begin();
      for ( ; vit != bc_data.end(); ++vit) {
        std::cout << *vit;
      }
      std::cout << std::endl;
    }
  }

  // And put entities into this nodeset's set
  ErrorCode result = put_into_set(nodeseth->setHandle, ns_entities, excl_entities);
  if (MB_SUCCESS != result)
    return result;

  result = get_names(model->nodesetMD, nsindex, nodeseth->setHandle);
  if (MB_SUCCESS != result)
    return result;

  const int def_bc_data_len = 0;
  std::string tag_name = std::string(DIRICHLET_SET_TAG_NAME)+"__BC_DATA";
  Tag nbc_data;
  result = mdbImpl->tag_get_handle(tag_name.c_str(), def_bc_data_len, MB_TYPE_OPAQUE,
                                   nbc_data, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_BYTES | MB_TAG_VARLEN, NULL);
  if (MB_SUCCESS != result)
    return result;
  void const* tag_data[] = { (bc_data.empty())? NULL : &(bc_data[0]) };
  int tag_size = bc_data.size();
  result = mdbImpl->tag_set_by_ptr(nbc_data, &nodeseth->setHandle, 1, tag_data, &tag_size);
  if (MB_SUCCESS != result)
    return result;

  return result;
}

ErrorCode Tqdcfr::read_sideset(const unsigned int ssindex,
                               const double data_version,
                               Tqdcfr::ModelEntry *model,
                               Tqdcfr::SidesetHeader *sideseth)
{
  if (sideseth->memCt == 0)
    return MB_SUCCESS;

  ErrorCode result;

  // Position file
  FSEEK(model->modelOffset + sideseth->memOffset);

  // Read ids for each entity type
  unsigned int this_type, num_ents, sense_size;

  std::vector<char> bc_data;
  unsigned int num_read = 0; //, uid;
  std::vector<EntityHandle> ss_entities, excl_entities;
  if (data_version <= 1.0) {
    for (unsigned int i = 0; i < sideseth->memTypeCt; i++) {
      // Get how many and what type
      FREADI(3); num_read += 3*sizeof(int);
      this_type = uint_buf[0];
      num_ents = uint_buf[1];
      sense_size = uint_buf[2];

      // Now get the ids
      FREADI(num_ents); num_read += sizeof(int);
      CONVERT_TO_INTS(num_ents);
    
      result = get_entities(this_type + 2, &int_buf[0], num_ents,
                            ss_entities, excl_entities);
      if (MB_SUCCESS != result)
        return result;

      if (sense_size == 1) {
        // Byte-size sense flags; make sure read ends aligned...
        unsigned int read_length = (num_ents / 8) * 8;
        if (read_length < num_ents)
          read_length += 8;
        FREADC(read_length); num_read += read_length;
      
      }
      else if (sense_size == 2) {
        // Int-size sense flags
        FREADI(num_ents); num_read += sizeof(int);
      }

      // Now do something with them...
      process_sideset_10(this_type, num_ents, sense_size, ss_entities, sideseth);
    }
  }
  else {
    for (unsigned int i = 0; i < sideseth->memTypeCt; i++) {
      // Get how many and what type
      FREADI(1); num_read += sizeof(int);
      num_ents = uint_buf[0];

      // Get the types, and ids
      std::vector<unsigned int> mem_types(num_ents), mem_ids(num_ents);
      FREADIA(num_ents, &mem_types[0]); num_read += num_ents*sizeof(int);
      FREADI(num_ents); num_read += sizeof(int);

      result = get_entities(&mem_types[0], &int_buf[0], num_ents, false,
                            ss_entities);
      if (MB_SUCCESS != result)
        return result;

      // Byte-size sense flags; make sure read ends aligned...
      unsigned int read_length = (num_ents / 8) * 8;
      if (read_length < num_ents) read_length += 8;
      FREADC(read_length); num_read += read_length;

      // wrt entities
      FREADI(1); num_read += sizeof(int);
      int num_wrts = uint_buf[0];
      FREADI(num_wrts); num_read += num_wrts*sizeof(int);

      result = process_sideset_11(ss_entities, num_wrts, sideseth);
      if (MB_SUCCESS != result)
        return result;
    }
  }

  // Now set the dist factors
  if (sideseth->numDF > 0) {
    // Have to read dist factors
    FREADD(sideseth->numDF); num_read += sideseth->numDF*sizeof(double);
    Tag distFactorTag;
    result = mdbImpl->tag_get_handle("distFactor", 0, MB_TYPE_DOUBLE,
                                     distFactorTag,
                                     MB_TAG_SPARSE | MB_TAG_VARLEN | MB_TAG_CREAT);
    if (MB_SUCCESS != result)
      return result;
    const void* dist_data = &dbl_buf[0];
    const int dist_size = sideseth->numDF;
    result = mdbImpl->tag_set_by_ptr(distFactorTag, &sideseth->setHandle, 1, &dist_data, &dist_size);
    if (MB_SUCCESS != result)
      return result;
  }

  // Check for more data
  if (data_version > 1.0 && num_read < sideseth->ssLength) {
    FREADC(2); //num_read += 2;
    if (char_buf[0] == 'i' && char_buf[1] == 'd') {
      FREADI(1); //num_read += sizeof(int);
      //uid = int_buf[0];
    }
    else {
      // Check for bc_data
      if (char_buf[0] == 'b' && char_buf[1] == 'c') {
        FREADI(1); //num_read += sizeof(int);
        int num_bcs = uint_buf[0];
        bc_data.resize(num_bcs);
        FREADCA(num_bcs, &bc_data[0]); //num_read += num_bcs;
      }
    }
  }

  if (debug) {
    sideseth->print(); 
    if (!bc_data.empty()) {
      std::cout << "bc_data = ";
      std::vector<char>::iterator vit = bc_data.begin();
      for ( ; vit != bc_data.end(); ++vit) {
        std::cout << std::hex << (int)((unsigned char)*vit) << " ";
      }
      std::cout << ": ";
      vit = bc_data.begin();
      for ( ; vit != bc_data.end(); ++vit) {
        std::cout << *vit;
      }
      std::cout << std::endl;
    }
  }

  result = get_names(model->sidesetMD, ssindex, sideseth->setHandle);
  if (MB_SUCCESS != result)
    return result;

  const int def_bc_data_len = 0;
  std::string tag_name = std::string(NEUMANN_SET_TAG_NAME) + "__BC_DATA";
  Tag nbc_data;
  result = mdbImpl->tag_get_handle(tag_name.c_str(), def_bc_data_len, MB_TYPE_OPAQUE,
                                   nbc_data, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_BYTES | MB_TAG_VARLEN, NULL);
  if (MB_SUCCESS != result)
    return result;
  void const* tag_data[] = { (bc_data.empty())? NULL : &(bc_data[0]) };
  int tag_size = bc_data.size();
  result = mdbImpl->tag_set_by_ptr(nbc_data, &sideseth->setHandle, 1, tag_data, &tag_size);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::process_sideset_10(const int this_type, const int num_ents,
                                     const int sense_size,
                                     std::vector<EntityHandle> &ss_entities,
                                     Tqdcfr::SidesetHeader *sideseth)
{
  std::vector<EntityHandle> forward, reverse;
  if (this_type == 3 // Surface
      && sense_size == 1 // Byte size
      ) {
    // Interpret sense flags w/o reference to anything
    for (int i = 0; i < num_ents; i++) {
      if ((int) char_buf[i] == 0)
        forward.push_back(ss_entities[i]);
      else if ((int) char_buf[i] == 1)
        reverse.push_back(ss_entities[i]);
      else if ((int) char_buf[i] == -1) { // -1 means "unknown", which means both
        forward.push_back(ss_entities[i]);
        reverse.push_back(ss_entities[i]);
      }
    }
  }
  else if (this_type == 4 // Curve
           && sense_size == 2 // int32 size
           ) {
    for (int i = 0; i < num_ents; i++) {
      if (uint_buf[i] == 0)
        forward.push_back(ss_entities[i]);
      else if (uint_buf[i] == 1)
        reverse.push_back(ss_entities[i]);
      else if (*((int*)&uint_buf[i]) == -1) { // -1 means "unknown", which means both
        forward.push_back(ss_entities[i]);
        reverse.push_back(ss_entities[i]);
      }
    }
  }

  // Now actually put them in the set
  ErrorCode result = MB_SUCCESS;
  if (!forward.empty()) {
    ErrorCode tmp_result = mdbImpl->add_entities(sideseth->setHandle, &forward[0], forward.size());
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
  }
  if (!reverse.empty()) {
    // Need to make a new set, add a reverse sense tag, then add to the sideset
    EntityHandle reverse_set;
    ErrorCode tmp_result = create_set(reverse_set);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
    tmp_result = mdbImpl->add_entities(reverse_set, &reverse[0], reverse.size());
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
    int def_val = 1;
    Tag sense_tag;
    tmp_result = mdbImpl->tag_get_handle("NEUSET_SENSE", 1, MB_TYPE_INTEGER, sense_tag,
                                         MB_TAG_SPARSE | MB_TAG_CREAT, &def_val);
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
    def_val = -1;
    tmp_result = mdbImpl->tag_set_data(sense_tag, &reverse_set, 1, &def_val);
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
    tmp_result = mdbImpl->add_entities(sideseth->setHandle, &reverse_set, 1);
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
  }

  return result;
}

ErrorCode Tqdcfr::process_sideset_11(std::vector<EntityHandle> &ss_entities,
                                     int num_wrts,
                                     Tqdcfr::SidesetHeader *sideseth)
{
  std::vector<EntityHandle> forward, reverse;

  unsigned int num_ents = ss_entities.size();
  unsigned int *wrt_it = &uint_buf[0];

  for (unsigned int i = 0; i < num_ents; i++) {
    unsigned int num_wrt = 0;
    if (0 != num_wrts)
      num_wrt = *wrt_it++;
    for (unsigned int j = 0; j < num_wrt; j++)
      wrt_it += 2;
    // Assume here that if it's in the list twice, we get both senses
    if (num_wrt > 1) {
      forward.push_back(ss_entities[i]);
      reverse.push_back(ss_entities[i]);
    }
    else {
      // Else interpret the sense flag
      if ((int) char_buf[i] == 0)
        forward.push_back(ss_entities[i]);
      else if ((int) char_buf[i] == 1)
        reverse.push_back(ss_entities[i]);
      else if ((int) char_buf[i] == -1) { // -1 means "unknown", which means both
        forward.push_back(ss_entities[i]);
        reverse.push_back(ss_entities[i]);
      }
    }
  }

  // Now actually put them in the set
  ErrorCode result = MB_SUCCESS;
  if (!forward.empty()) {
    ErrorCode tmp_result = mdbImpl->add_entities(sideseth->setHandle, &forward[0], forward.size());
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
  }
  if (!reverse.empty()) {
    // Need to make a new set, add a reverse sense tag, then add to the sideset
    EntityHandle reverse_set;
    ErrorCode tmp_result = create_set(reverse_set);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
    tmp_result = mdbImpl->add_entities(reverse_set, &reverse[0], reverse.size());
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
    int def_val = 1;
    Tag sense_tag;
    tmp_result = mdbImpl->tag_get_handle("NEUSET_SENSE", 1, MB_TYPE_INTEGER, sense_tag,
                                         MB_TAG_SPARSE | MB_TAG_CREAT, &def_val);
    if (tmp_result != MB_SUCCESS && tmp_result != MB_ALREADY_ALLOCATED)
      result = tmp_result;
    def_val = -1;
    tmp_result = mdbImpl->tag_set_data(sense_tag, &reverse_set, 1, &def_val);
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
    tmp_result = mdbImpl->add_entities(sideseth->setHandle, &reverse_set, 1);
    if (tmp_result != MB_SUCCESS)
      result = tmp_result;
  }

  return result;
}

ErrorCode Tqdcfr::read_block(const unsigned int blindex,
                             const double /*data_version*/,
                             Tqdcfr::ModelEntry *model,
                             Tqdcfr::BlockHeader *blockh)
{
  if (blockh->memCt == 0)
    return MB_SUCCESS;

  // Position file
  FSEEK(model->modelOffset + blockh->memOffset);

  // Read ids for each entity type
  unsigned int num_read = 0;
  int this_type, num_ents; //, uid;
  std::vector<EntityHandle> block_entities, excl_entities;
  for (unsigned int i = 0; i < blockh->memTypeCt; i++) {
    // Get how many and what type
    FREADI(2); num_read += 2*sizeof(int);
    this_type = uint_buf[0];
    num_ents = uint_buf[1];

    // Now get the ids
    FREADI(num_ents); num_read += num_ents*sizeof(int);
    CONVERT_TO_INTS(num_ents);

    ErrorCode result = get_entities(this_type + 2, &int_buf[0], num_ents,
                                    block_entities, excl_entities);
    if (MB_SUCCESS != result)
      return result;
  }

  // And put entities into this block's set
  ErrorCode result = put_into_set(blockh->setHandle, block_entities, excl_entities);
  if (MB_SUCCESS != result)
    return result;

  // Read attribs if there are any
  Tag block_attribs;
  {
    int def_block_attributes_length = 0;
    result = mdbImpl->tag_get_handle(BLOCK_ATTRIBUTES, def_block_attributes_length, MB_TYPE_DOUBLE,
                                     block_attribs, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN, NULL);
    if (MB_SUCCESS != result && MB_ALREADY_ALLOCATED != result)
      return result;
  }

  if (blockh->attribOrder > 0) {
    FREADD(blockh->attribOrder); num_read += sizeof(double);
    void const* tag_data[] = { &dbl_buf[0] };
    int tag_sizes[] = { static_cast<int>(blockh->attribOrder) };
    result = mdbImpl->tag_set_by_ptr(block_attribs, &(blockh->setHandle), 1, tag_data, tag_sizes);
    if (MB_SUCCESS != result)
      return result;
  }

  // Check for more data
  if (num_read < blockh->blockLength) {
    FREADC(2); //num_read += 2;
    if (char_buf[0] == 'i' && char_buf[1] == 'd') {
      FREADI(1); //num_read += sizeof(int);
      //uid = int_buf[0];
    }
  }

  result = get_names(model->blockMD, blindex, blockh->setHandle);
  if (MB_SUCCESS != result)
    return result;
  
  // Put additional higher-order nodes into element connectivity list.
  // Cubit saves full connectivity list only for NodeHex and NodeTet
  // elements. Other element types will only have the corners and
  // the mid-element node if there is one. Need to reconstruct additional
  // connectivity entries from mid-nodes of adjacent lower-order entities.
  int node_per_elem = cub_elem_num_verts[blockh->blockElemType];
  if (blockh->blockEntityType==MBMAXTYPE)
    return MB_SUCCESS;
  if ((14 == major && 2 < minor) || 15 <= major )
  {
    if (55 == blockh->blockElemType ||
        CN::VerticesPerEntity(blockh->blockEntityType) == node_per_elem)
      return MB_SUCCESS;
  }
  else
  {
    if (52 == blockh->blockElemType ||
      CN::VerticesPerEntity(blockh->blockEntityType) == node_per_elem)
      return MB_SUCCESS;
  }

  // Can't use Interface::convert_entities because block could contain
  // both entity sets and entities. convert_entities will fail if block
  // contains an entity set, but we still need to call it on any elements
  // directly in the block (rather than a geometry subset). So bypass
  // Interface and call HOFactory directly with an Range of entities.
  Range ho_entities, entities;
  mdbImpl->get_entities_by_type(blockh->setHandle, blockh->blockEntityType, entities, true);
  if (CN::Dimension(blockh->blockEntityType) > 2) {
    result = mdbImpl->get_adjacencies(entities, 2, false, ho_entities, Interface::UNION);
    if (MB_SUCCESS != result)
      return result;
  }
  if (CN::Dimension(blockh->blockEntityType) > 1) {
    result = mdbImpl->get_adjacencies(entities, 1, false, ho_entities, Interface::UNION);
    if (MB_SUCCESS != result)
      return result;
  }
  entities.merge(ho_entities);

  Core *mbcore = dynamic_cast<Core*>(mdbImpl);
  assert(mbcore != NULL);
  HigherOrderFactory ho_fact(mbcore, 0);
  return ho_fact.convert(entities, !!blockh->hasMidNodes[1], !!blockh->hasMidNodes[2],
                         !!blockh->hasMidNodes[3]);
}

ErrorCode Tqdcfr::get_names(MetaDataContainer &md, unsigned int set_index, EntityHandle seth)
{
  ErrorCode result = MB_SUCCESS;

  // Now get block names, if any
  int md_index = md.get_md_entry(set_index, "Name");
  if (-1 == md_index)
    return result;
  MetaDataContainer::MetaDataEntry *md_entry = &(md.metadataEntries[md_index]);
  //assert(md_entry->mdStringValue.length() + 1 <= NAME_TAG_SIZE);
  char name_tag_data[NAME_TAG_SIZE];
  memset(name_tag_data, 0, NAME_TAG_SIZE); // Make sure any extra bytes zeroed
  strncpy(name_tag_data, md_entry->mdStringValue.c_str(), NAME_TAG_SIZE - 1);
  result = mdbImpl->tag_set_data(entityNameTag, &seth, 1, name_tag_data);
  if (MB_SUCCESS != result)
    return result;

  // Look for extra names
  md_index = md.get_md_entry(set_index, "NumExtraNames");
  if (-1 == md_index)
    return result;
  int num_names = md.metadataEntries[md_index].mdIntValue;
  for (int i = 0; i < num_names; i++) {
    std::ostringstream extra_name_label("ExtraName");
    extra_name_label << i;
    std::ostringstream moab_extra_name("EXTRA_");
    moab_extra_name << NAME_TAG_NAME << i;
    md_index = md.get_md_entry(set_index, extra_name_label.str().c_str());
    if (-1 != md_index) {
      md_entry = &(md.metadataEntries[md_index]);
      Tag extra_name_tag;
      ErrorCode rval;
      rval = mdbImpl->tag_get_handle(
          moab_extra_name.str().c_str(),
          NAME_TAG_SIZE,
          MB_TYPE_OPAQUE,
          extra_name_tag,
          MB_TAG_SPARSE | MB_TAG_CREAT
          ); MB_CHK_ERR(rval);
      memset(name_tag_data, 0, NAME_TAG_SIZE); // Make sure any extra bytes zeroed
      strncpy(name_tag_data, md_entry->mdStringValue.c_str(), NAME_TAG_SIZE - 1);
      result = mdbImpl->tag_set_data(extra_name_tag, &seth, 1, name_tag_data);
    }
  }

  return result;
}

ErrorCode Tqdcfr::read_group(const unsigned int group_index,
                             Tqdcfr::ModelEntry *model,
                             Tqdcfr::GroupHeader *grouph)
{
  // Position file
  FSEEK(model->modelOffset + grouph->memOffset);
  char name_tag_data[NAME_TAG_SIZE];

  // Read ids for each entity type
  int this_type, num_ents;
  std::vector<EntityHandle> grp_entities, excl_entities;
  for (unsigned int i = 0; i < grouph->memTypeCt; i++) {
    // Get how many and what type
    FREADI(2);
    this_type = uint_buf[0];
    num_ents = uint_buf[1];

    // Now get the ids
    FREADI(num_ents);
    CONVERT_TO_INTS(num_ents);

    // Get the entities in this group
    ErrorCode result = get_entities(this_type, &int_buf[0], num_ents, grp_entities, excl_entities);
    if (MB_SUCCESS != result)
      return result;
  }

  // Now add the entities
  ErrorCode result = put_into_set(grouph->setHandle, grp_entities, excl_entities);
  if (MB_SUCCESS != result)
    return result;

  // Now get group names, if any
  int md_index = model->groupMD.get_md_entry(grouph->grpID, "NAME");
  if (-1 != md_index) {
    MetaDataContainer::MetaDataEntry *md_entry = &(model->groupMD.metadataEntries[md_index]);
    if (0 == entityNameTag) {
       memset(name_tag_data, 0, NAME_TAG_SIZE);
       result = mdbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
                                        entityNameTag, MB_TAG_SPARSE | MB_TAG_CREAT,
                                        name_tag_data);
      if (MB_SUCCESS != result)
        return result;
    }
    //assert(md_entry->mdStringValue.length() + 1 <= NAME_TAG_SIZE);
    memset(name_tag_data, 0, NAME_TAG_SIZE); // Make sure any extra bytes zeroed
    strncpy(name_tag_data, md_entry->mdStringValue.c_str(), NAME_TAG_SIZE - 1);
    result = mdbImpl->tag_set_data(entityNameTag, &grouph->setHandle, 1,
                                   name_tag_data);
    if (MB_SUCCESS != result)
      return result;

    // Look for extra names
    md_index = model->groupMD.get_md_entry(group_index, "NumExtraNames");
    if (-1 != md_index) {
      int num_names = model->groupMD.metadataEntries[md_index].mdIntValue;
      for (int i = 0; i < num_names; i++) {
        std::ostringstream extra_name_label("ExtraName");
        extra_name_label << i;
        std::ostringstream moab_extra_name("EXTRA_");
        moab_extra_name << NAME_TAG_NAME << i;
        md_index = model->groupMD.get_md_entry(group_index, extra_name_label.str().c_str());
        if (-1 != md_index) {
          md_entry = &(model->groupMD.metadataEntries[md_index]);
          Tag extra_name_tag;
          memset(name_tag_data, 0, NAME_TAG_SIZE);
          result = mdbImpl->tag_get_handle(moab_extra_name.str().c_str(),
                                           NAME_TAG_SIZE, MB_TYPE_OPAQUE,
                                           extra_name_tag, MB_TAG_SPARSE | MB_TAG_CREAT,
                                           name_tag_data);
          if (MB_SUCCESS != result)
            return result;
          //assert(md_entry->mdStringValue.length() + 1 <= NAME_TAG_SIZE);
          memset(name_tag_data, 0, NAME_TAG_SIZE); // Make sure any extra bytes zeroed
          strncpy(name_tag_data, md_entry->mdStringValue.c_str(), NAME_TAG_SIZE - 1);
          result = mdbImpl->tag_set_data(extra_name_tag, &grouph->setHandle, 1,
                                         name_tag_data);
        }
      }
    }
  }

  return result;
}

ErrorCode Tqdcfr::put_into_set(EntityHandle set_handle,
                               std::vector<EntityHandle> &entities,
                               std::vector<EntityHandle> &excl_entities)
{
  // And put entities into this block's set
  ErrorCode result = mdbImpl->add_entities(set_handle, &entities[0], entities.size());
  if (MB_SUCCESS != result)
    return result;

  // Check for excluded entities, and add them to a vector hung off the block if there
  Tag excl_tag;
  if (!excl_entities.empty()) {
    result = mdbImpl->tag_get_handle("Exclude_Entities",
                                     sizeof(std::vector<EntityHandle>*),
                                     MB_TYPE_OPAQUE, excl_tag,
                                     MB_TAG_SPARSE | MB_TAG_CREAT);
    if (MB_SUCCESS != result)
      return result;
    std::vector<EntityHandle> *new_vector = new std::vector<EntityHandle>;
    new_vector->swap(excl_entities);
    result = mdbImpl->tag_set_data(excl_tag, &set_handle, 1, &new_vector);
    if (MB_SUCCESS != result) {
      delete new_vector;
      return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::get_entities(const unsigned int *mem_types,
                               int *id_buf, const unsigned int id_buf_size,
                               const bool is_group,
                               std::vector<EntityHandle> &entities)
{
  ErrorCode tmp_result, result = MB_SUCCESS;

  for (unsigned int i = 0; i < id_buf_size; i++) {
    if (is_group)
      tmp_result = get_entities(mem_types[i], id_buf + i, 1, entities, entities);
    else
      // For blocks/nodesets/sidesets, use CSOEntityType, which is 2 greater than
      // group entity types
      tmp_result = get_entities(mem_types[i] + 2, id_buf + i, 1, entities, entities);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
  }

  return result;
}

ErrorCode Tqdcfr::get_entities(const unsigned int this_type,
                               int *id_buf, const unsigned int id_buf_size,
                               std::vector<EntityHandle> &entities,
                               std::vector<EntityHandle> &excl_entities)
{
  ErrorCode result = MB_FAILURE;

  if (this_type <= VERTEX)
    result = get_ref_entities(this_type, id_buf, id_buf_size, entities);
  else if (this_type >= HEX && this_type <= NODE)
    result = get_mesh_entities(this_type, id_buf, id_buf_size, entities, excl_entities);

  return result;
}

ErrorCode Tqdcfr::get_ref_entities(const unsigned int this_type,
                                   int *id_buf, const unsigned int id_buf_size,
                                   std::vector<EntityHandle> &entities)
{
  for (unsigned int i = 0; i < id_buf_size; i++)
    entities.push_back((gidSetMap[5 - this_type])[id_buf[i]]);

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::get_mesh_entities(const unsigned int this_type,
                                    int *id_buf, const unsigned int id_buf_size,
                                    std::vector<EntityHandle> &entities,
                                    std::vector<EntityHandle> &excl_entities)
{
  ErrorCode result = MB_SUCCESS;
  std::vector<EntityHandle> *ent_list = NULL;
  EntityType this_ent_type = MBVERTEX;
  const unsigned int arr_len = sizeof(group_type_to_mb_type) / sizeof(group_type_to_mb_type[0]);
  if (this_type > 1000) {
    if (this_type - 1000 < arr_len) {
      this_ent_type = group_type_to_mb_type[this_type - 1000];
      ent_list = &excl_entities;
    }
  }
  else {
    if (this_type < arr_len) {
      this_ent_type = group_type_to_mb_type[this_type];
      ent_list = &entities;
    }
  }
  if (NULL == ent_list) {
    MB_SET_ERR(MB_FAILURE, "Entities list is NULL");
  }

  // Get entities with this type, and get their cub id tags
  if (MBVERTEX == this_ent_type) {
    // Use either vertex offset or cubMOABVertexMap
    if (NULL == cubMOABVertexMap) {
      for (unsigned int i = 0; i < id_buf_size; i++)
        ent_list->push_back((EntityHandle)(id_buf[i] + currVHandleOffset));
    }
    else {
      for (unsigned int i = 0; i < id_buf_size; i++) {
        assert(0 != (*cubMOABVertexMap)[id_buf[i]]);
        ent_list->push_back((*cubMOABVertexMap)[id_buf[i]]);
      }
    }
  }
  else {
    Range tmp_ents;
    result = mdbImpl->get_entities_by_type(0, this_ent_type, tmp_ents);
    if (MB_SUCCESS != result)
      return result;
    if (tmp_ents.empty() && 0 != id_buf_size)
      return MB_FAILURE;

    std::vector<int> cub_ids(tmp_ents.size());
    result = mdbImpl->tag_get_data(globalIdTag, tmp_ents, &cub_ids[0]);
    if (MB_SUCCESS != result && MB_TAG_NOT_FOUND != result)
      return result;

    // Now go through id list, finding each entity by id
    for (unsigned int i = 0; i < id_buf_size; i++) {
      std::vector<int>::iterator vit = 
        std::find(cub_ids.begin(), cub_ids.end(), id_buf[i]);
      if (vit != cub_ids.end()) {
        EntityHandle this_ent = tmp_ents[vit - cub_ids.begin()];
        if (mdbImpl->type_from_handle(this_ent) != MBMAXTYPE) ent_list->push_back(this_ent);
      }
      else {
        std::cout << "Warning: didn't find " << CN::EntityTypeName(this_ent_type)
                  << " " << id_buf[i] << std::endl;
      }
    }
  }

  return result;
}

ErrorCode Tqdcfr::read_nodes(const unsigned int gindex,
                             Tqdcfr::ModelEntry *model,
                             Tqdcfr::GeomHeader *entity)
{
  if (entity->nodeCt == 0) {
    if (debug) std::cout << "(no nodes) ";
    return MB_SUCCESS;
  }

  // Get the ids & coords in separate calls to minimize memory usage
  // Position the file
  FSEEK(model->modelOffset + entity->nodeOffset);
  // Get node ids in uint_buf
  FREADI(entity->nodeCt);

  if (debug) {
    std::cout << "(";
    for (unsigned int i = 0; i < entity->nodeCt; i++) {
      std::cout << uint_buf[i];
      if (i != entity->nodeCt - 1) std::cout << ", ";
    }
    std::cout << ")...";
  }

  // Get a space for reading nodal data directly into MB, and read that data
  EntityHandle vhandle = 0;
  std::vector<double*> arrays;
  readUtilIface->get_node_coords(3, entity->nodeCt,
                                 uint_buf[0],
                                 vhandle, arrays,
                                 SequenceManager::DEFAULT_VERTEX_SEQUENCE_SIZE);

  // Get node x's in arrays[0]
  FREADDA(entity->nodeCt, arrays[0]);
  // Get node y's in arrays[1]
  FREADDA(entity->nodeCt, arrays[1]);
  // Get node z's in arrays[2]
  FREADDA(entity->nodeCt, arrays[2]);

  // Add these nodes into the entity's set
  Range dum_range(vhandle, vhandle + entity->nodeCt - 1);
  ErrorCode result = mdbImpl->add_entities(entity->setHandle, dum_range);
  if (MB_SUCCESS != result)
    return result;

  // Check for id contiguity; know that cid's will never be > 32bit, so
  // ids can be unsigned int
  unsigned int max_cid, min_cid;
  int contig;
  check_contiguous(entity->nodeCt, contig, min_cid, max_cid);

   // Compute the offset we get in this batch and compare to any previous one
  long vhandle_offset = vhandle - min_cid;
  if (-1 == currVHandleOffset)
    currVHandleOffset = vhandle_offset;

  // In 2 situations we'll need to add/modify a cubit_id -> vhandle map:
  // case A: no map yet, and either this offset different from
  // previous or not contiguous
  if (!cubMOABVertexMap &&
      (currVHandleOffset != vhandle_offset || !contig)) {
    // Get all vertices, removing ones in this batch
    Range vrange, tmp_range(dum_range);
    result = mdbImpl->get_entities_by_type(0, MBVERTEX, vrange); RR;
    if (!beforeEnts.empty())
      tmp_range.merge(beforeEnts.subset_by_type(MBVERTEX));
    vrange = subtract(vrange, tmp_range);
    // Compute the max cid; map is indexed by cid, so size is max_cid + 1
#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)
    // Sanity check that max vhandle is larger than offset
    long new_max = *vrange.rbegin() - currVHandleOffset;
    assert(new_max >= 0 && ((long)*vrange.begin()) - currVHandleOffset >= 0);
    max_cid = MAX(max_cid, ((unsigned int) new_max));
    cubMOABVertexMap = new std::vector<EntityHandle>(max_cid + 1);
    // Initialize to zero then put previous vertices into the map
    std::fill(cubMOABVertexMap->begin(), cubMOABVertexMap->end(), 0);
    Range::iterator rit;
    for (rit = vrange.begin(); rit != vrange.end(); ++rit) {
      assert(((long)*rit) - currVHandleOffset >= 0 &&
             ((long)*rit) - currVHandleOffset <= max_cid);
      (*cubMOABVertexMap)[*rit - currVHandleOffset] = *rit;
    }
  }
  // case B: there is a map and we need to resize it
  else if (cubMOABVertexMap && max_cid + 1 > cubMOABVertexMap->size()) {
    unsigned int old_size = cubMOABVertexMap->size();
    cubMOABVertexMap->resize(max_cid + 1);
    std::fill(&(*cubMOABVertexMap)[old_size],
              &(*cubMOABVertexMap)[0] + cubMOABVertexMap->size(), 0);
  }

  // OK, we have a map or don't need one
  if (NULL == cubMOABVertexMap) {
    // If we're not forward-contiguous (i.e. we're reverse or
    // out-of-order contiguous), re-order coordinates for handles
    // so that they are
    if (-1 == contig || -2 == contig) {
      // In case the arrays are large, do each coord separately
      std::vector<double> tmp_coords(entity->nodeCt);
      for (unsigned int j = 0; j < 3; j++) {
        // Permute the coords into new order
        for (unsigned int i = 0; i < entity->nodeCt; i++) {
          assert(uint_buf[i] >= min_cid && 
                 max_cid-uint_buf[i] < entity->nodeCt);
          tmp_coords[uint_buf[i]-min_cid] = arrays[j][i];
        }
        // Copy the permuted to storage
        std::copy(&tmp_coords[0], &tmp_coords[0] + entity->nodeCt, arrays[j]);
      }
      // Now re-order the ids; either way just go off min, max cid
      for (unsigned int i = 0; i < entity->nodeCt; i++)
        uint_buf[i] = min_cid + i;
    }
    else if (!contig)
      // Shouldn't get here, since in non-contiguous case map should be there
      assert(false);
  }
  else {
    // Put new vertices into the map
    // Now set the new values
    unsigned int *vit = &uint_buf[0];
    Range::iterator rit = dum_range.begin();
    for ( ; rit != dum_range.end(); vit++, ++rit) {
      assert(*vit < cubMOABVertexMap->size());
      (*cubMOABVertexMap)[*vit] = *rit;
    }
  }

  // No longer need to use uint_buf; convert in-place to ints, so we
  // can assign gid tag
  CONVERT_TO_INTS(entity->nodeCt);
  result = mdbImpl->tag_set_data(globalIdTag, dum_range, &int_buf[0]);
  if (MB_SUCCESS != result)
    return result;

  // Set the dimension to at least zero (entity has at least nodes) on the geom tag
  int max_dim = 0;
  result = mdbImpl->tag_set_data(geomTag, &(entity->setHandle), 1, &max_dim);
  if (MB_SUCCESS != result)
    return result;
  // Set the category tag just in case there're only vertices in this set
  result = mdbImpl->tag_set_data(categoryTag, &entity->setHandle, 1,
                                 &geom_categories[0]);
  if (MB_SUCCESS != result)
    return result;

  // Get fixed node data and assign
  int md_index = model->nodeMD.get_md_entry(gindex, "FixedNodes");
  if (-1 == md_index)
    return MB_SUCCESS;
  MetaDataContainer::MetaDataEntry *md_entry = &(model->nodeMD.metadataEntries[md_index]);

  std::vector<int> fixed_flags(entity->nodeCt);
  std::fill(fixed_flags.begin(), fixed_flags.end(), 0);
  if (md_entry->mdDataType != 3)
    return MB_FAILURE;

  for (std::vector<unsigned int>::iterator vit = md_entry->mdIntArrayValue.begin();
       vit != md_entry->mdIntArrayValue.end(); ++vit) {
#ifndef NDEBUG
    EntityHandle fixed_v = (cubMOABVertexMap ?
                            (*cubMOABVertexMap)[*vit] :
                            (EntityHandle) currVHandleOffset + *vit);
    assert(fixed_v >= *dum_range.begin() && fixed_v <= *dum_range.rbegin());
#endif
    fixed_flags[*vit - *dum_range.begin()] = 1;
  }

  Tag fixedFlagTag;
  int dum_val = 0;
  result = mdbImpl->tag_get_handle("NodeFixed", 1, MB_TYPE_INTEGER, fixedFlagTag,
                                   MB_TAG_SPARSE | MB_TAG_CREAT, &dum_val);
  if (MB_SUCCESS != result)
    return result;
  result = mdbImpl->tag_set_data(fixedFlagTag, dum_range, &fixed_flags[0]);

  return result;
}

ErrorCode Tqdcfr::read_elements(Tqdcfr::ModelEntry *model,
                                Tqdcfr::GeomHeader *entity)
{
  if (entity->elemTypeCt == 0)
    return MB_SUCCESS;
  const int in_order_map[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                              11, 12, 13, 14, 15, 16, 17, 18, 19,
                              20, 21, 22, 23, 24, 25, 26, 27};

  // Get data in separate calls to minimize memory usage
  // Position the file
  FSEEK(model->modelOffset + entity->elemOffset);

  int int_type, nodes_per_elem, num_elem;
  int max_dim = -1;
  ErrorCode result;
  for (unsigned int i = 0; i < entity->elemTypeCt; i++) {
    // For this elem type, get the type, nodes per elem, num elems
    FREADI(3);
    int_type = uint_buf[0];
    nodes_per_elem = uint_buf[1];
    num_elem = uint_buf[2];

    // Get MB element type from cub file's
    EntityType elem_type = mp_type_to_mb_type[int_type];
    max_dim = (max_dim < CN::Dimension(elem_type) ? CN::Dimension(elem_type) : max_dim);

    if (debug)
      std::cout << "type " << CN::EntityTypeName(elem_type) << ":";

    const int* node_order = cub_elem_order_map[elem_type][nodes_per_elem];
    if (!node_order)
      node_order = in_order_map;

    // Get element ids
    FREADI(num_elem);

    // Check to see if ids are contiguous...
    int contig;
    unsigned int max_id, min_id;
    check_contiguous(num_elem, contig, min_id, max_id);
    if (0 == contig && !printedElemWarning) {
      std::cout << "Element ids are not contiguous!" << std::endl;
      printedElemWarning = true;
    }

    // Get a space for reading connectivity data directly into MB
    EntityHandle *conn, start_handle;

    result = readUtilIface->get_element_connect(num_elem, nodes_per_elem,
                                                elem_type, int_buf[0],
                                                start_handle, conn,
                                                SequenceManager::DEFAULT_ELEMENT_SEQUENCE_SIZE);
    if (MB_SUCCESS != result)
      return result;

    Range dum_range(start_handle, start_handle + num_elem - 1);

    long elem_offset;
    elem_offset = (1 == contig ? start_handle - int_buf[0] : int_buf[num_elem - 1]);
    if (-1 == currElementIdOffset[elem_type])
      currElementIdOffset[elem_type] = elem_offset;

    // Set the gids on elements
    CONVERT_TO_INTS(num_elem);
    result = mdbImpl->tag_set_data(globalIdTag, dum_range, &int_buf[0]);
    if (MB_SUCCESS != result)
      return result;

    // Get the connectivity array
    unsigned int total_conn = num_elem * nodes_per_elem;
    if (major >=14)
      FREADI(num_elem); // We need to skip num_elem in advance, it looks like
    FREADI(total_conn);

    // Post-process connectivity into handles
    EntityHandle new_handle;
    int j = 0;
    for (int e = 0; e < num_elem; ++e) {
      for (int k = 0; k < nodes_per_elem; ++k, ++j) {
        if (debug) {
          if (0 == j) std::cout << "Conn=";
          std::cout << ", " << uint_buf[j];
        }
        if (NULL == cubMOABVertexMap)
          new_handle = (EntityHandle) currVHandleOffset + uint_buf[j];
        else {
          assert(uint_buf[j] < cubMOABVertexMap->size() &&
                 0 != (*cubMOABVertexMap)[uint_buf[j]]);
          new_handle = (*cubMOABVertexMap)[uint_buf[j]];
        }
  #ifndef NDEBUG
        EntityHandle dum_handle;
        assert(MB_SUCCESS ==
               mdbImpl->handle_from_id(MBVERTEX, mdbImpl->id_from_handle(new_handle),
                                       dum_handle));
  #endif
        conn[e*nodes_per_elem + node_order[k]] = new_handle;
      }
    }

    // Add these elements into the entity's set
    result = mdbImpl->add_entities(entity->setHandle, dum_range);
    if (MB_SUCCESS != result)
      return result;

    // Notify MOAB of the new elements
    result = readUtilIface->update_adjacencies(start_handle, num_elem,
                                               nodes_per_elem, conn);
    if (MB_SUCCESS != result)
      return result;
  }

  // Set the dimension on the geom tag
  result = mdbImpl->tag_set_data(geomTag, &entity->setHandle, 1, &max_dim);
  if (MB_SUCCESS != result)
    return result;
  if (max_dim != -1) {
    result = mdbImpl->tag_set_data(categoryTag, &entity->setHandle, 1,
                                   &geom_categories[max_dim]);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

void Tqdcfr::check_contiguous(const unsigned int num_ents, int &contig,
                              unsigned int &min_id, unsigned int &max_id)
{
  unsigned int *id_it, curr_id, i;

  // Check in forward-contiguous direction
  id_it = &uint_buf[0];
  curr_id = *id_it++ + 1;
  contig = 1;
  min_id = uint_buf[0];
  max_id = uint_buf[0];
  for (i = 1; i < num_ents; id_it++, i++, curr_id++) {
    if (*id_it != curr_id) {
      contig = 0;
    }
    min_id = MIN(min_id, uint_buf[i]);
    max_id = MAX(max_id, uint_buf[i]);
  }

  // If we got here and we're at the end of the loop, it's forward-contiguous
  if (1 == contig)
    return;

  // Check in reverse-contiguous direction
  contig = -1;
  id_it = &uint_buf[0];
  curr_id = *id_it++ - 1;
  for (i = 1; i < num_ents; id_it++, i++, curr_id--) {
    if (*id_it != curr_id) {
      contig = 0;
      break;
    }
  }

  // If we got here and we're at the end of the loop, it's reverse-contiguous
  if (-1 == contig)
    return;

  // One final check, for contiguous but out of order
  if (max_id - min_id + 1 == num_ents)
    contig = -2;

  // Else it's not contiguous at all
  contig = 0;
}

void Tqdcfr::FEModelHeader::init(const unsigned int offset, Tqdcfr* instance)
{
  instance->FSEEK(offset);
  instance->FREADI(4);
  feEndian = instance->uint_buf[0];
  feSchema = instance->uint_buf[1];
  feCompressFlag = instance->uint_buf[2];
  feLength = instance->uint_buf[3];
  instance->FREADI(3); geomArray.init(instance->uint_buf);
  instance->FREADI(2);
  nodeArray.metaDataOffset = instance->uint_buf[0];
  elementArray.metaDataOffset = instance->uint_buf[1];
  instance->FREADI(3); groupArray.init(instance->uint_buf);
  instance->FREADI(3); blockArray.init(instance->uint_buf);
  instance->FREADI(3); nodesetArray.init(instance->uint_buf);
  instance->FREADI(3); sidesetArray.init(instance->uint_buf);
  instance->FREADI(1);
}

ErrorCode Tqdcfr::read_file_header() 
{
  // Read file header
  FSEEK(4);
  // Read the first int from the file
  // If it is 0, it is littleEndian
  unsigned rval = fread(&fileTOC.fileEndian, sizeof(unsigned int), 1, cubFile);
  IO_ASSERT(rval == 1);
#ifdef WORDS_BIGENDIAN
  if (fileTOC.fileEndian == 0)
    swapForEndianness = true;
#else
  if (fileTOC.fileEndian != 0)
    swapForEndianness = true;
#endif
  if (debug)
    std::cout << " swapping ? " << swapForEndianness << "\n";
  FREADI(5);
  //fileTOC.fileEndian = uint_buf[0];
  fileTOC.fileSchema = uint_buf[0];
  fileTOC.numModels = uint_buf[1];
  fileTOC.modelTableOffset = uint_buf[2];
  fileTOC.modelMetaDataOffset = uint_buf[3];
  fileTOC.activeFEModel = uint_buf[4];
  if (debug)
    fileTOC.print();

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::read_model_entries() 
{
  // Read model entries
  FSEEK(fileTOC.modelTableOffset);
  FREADI(fileTOC.numModels*6);
  modelEntries.resize(fileTOC.numModels);
  if (modelEntries.empty())
    return MB_FAILURE;
  std::vector<unsigned int>::iterator int_it = uint_buf.begin();
  for (unsigned int i = 0; i < fileTOC.numModels; i++) {
    modelEntries[i].modelHandle = *int_it++;
    modelEntries[i].modelOffset = *int_it++;
    modelEntries[i].modelLength = *int_it++;
    modelEntries[i].modelType = *int_it++;
    modelEntries[i].modelOwner = *int_it++;
    modelEntries[i].modelPad = *int_it++;
    if (int_it == uint_buf.end() && i != fileTOC.numModels - 1)
      return MB_FAILURE;
    if (debug)
      modelEntries[i].print();
  }

  return MB_SUCCESS;
}

int Tqdcfr::find_model(const unsigned int model_type)
{
  for (unsigned int i = 0; i < fileTOC.numModels; i++) {
    if (modelEntries[i].modelType == model_type)
      return i;
  }

  return -1;
}

ErrorCode Tqdcfr::read_meta_data(const unsigned int metadata_offset,
                                 Tqdcfr::MetaDataContainer &mc)
{
  // Read the metadata header
  FSEEK(metadata_offset);
  FREADI(3);
  mc.mdSchema = uint_buf[0];
  mc.compressFlag = uint_buf[1];

  // Allocate space for the entries
  mc.metadataEntries.resize(uint_buf[2]);

  // Now read the metadata values
  for (unsigned int i = 0; i < mc.metadataEntries.size(); i++) {
    FREADI(2);
    mc.metadataEntries[i].mdOwner = uint_buf[0];
    mc.metadataEntries[i].mdDataType = uint_buf[1];

    // Read the name string
    read_md_string(mc.metadataEntries[i].mdName);

    if (mc.metadataEntries[i].mdDataType == 0) {
      // integer
      FREADI(1);
      mc.metadataEntries[i].mdIntValue = uint_buf[0];
    }
    else if (mc.metadataEntries[i].mdDataType == 1) {
      // string
      read_md_string(mc.metadataEntries[i].mdStringValue);
    }
    else if (mc.metadataEntries[i].mdDataType == 2) {
      // double
      FREADD(1);
      mc.metadataEntries[i].mdDblValue = dbl_buf[0];
    }
    else if (mc.metadataEntries[i].mdDataType == 3) {
      // int array
      FREADI(1);
      mc.metadataEntries[i].mdIntArrayValue.resize(uint_buf[0]);
      FREADI(mc.metadataEntries[i].mdIntArrayValue.size());
      std::copy(uint_buf.begin(), 
                uint_buf.begin() + mc.metadataEntries[i].mdIntArrayValue.size(),
                mc.metadataEntries[i].mdIntArrayValue.begin());
    }
    else if (mc.metadataEntries[i].mdDataType == 4) {
      // double array
      FREADI(1);
      mc.metadataEntries[i].mdDblArrayValue.resize(uint_buf[0]);
      FREADD(mc.metadataEntries[i].mdDblArrayValue.size());
      std::copy(dbl_buf.begin(),
                dbl_buf.begin() + mc.metadataEntries[i].mdDblArrayValue.size(),
                mc.metadataEntries[i].mdDblArrayValue.begin());
    }
    else
      return MB_FAILURE;
  }
  if (debug)
    mc.print();

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::read_md_string(std::string &name)
{
  FREADI(1);
  int str_size = uint_buf[0];
  if (str_size > 0) {
    FREADC(str_size);
    if (char_buf.size() <= (unsigned int) str_size)
      char_buf.resize(str_size + 1);
    char_buf[str_size] = '\0';
    name = (char *) &char_buf[0];
    // Read pad if any
    int extra = str_size % sizeof(int);
    if (extra) {
      // Read extra chars to end of pad
      str_size = sizeof(int) - extra;
      FREADC(str_size);
    }
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::GeomHeader::read_info_header(const unsigned int model_offset,
                                               const Tqdcfr::FEModelHeader::ArrayInfo &info,
                                               Tqdcfr* instance,
                                               Tqdcfr::GeomHeader *&geom_headers)
{
  geom_headers = new GeomHeader[info.numEntities];
  instance->FSEEK(model_offset + info.tableOffset);
  int dum_int;
  ErrorCode result;

  if (0 == instance->categoryTag) {
    static const char val[CATEGORY_TAG_SIZE] = {0};
    result = instance->mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
                                               MB_TYPE_OPAQUE, instance->categoryTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, val);
    if (MB_SUCCESS != result)
      return result;
  }

  for (unsigned int i = 0; i < info.numEntities; i++) {
    instance->FREADI(8);
    geom_headers[i].nodeCt = instance->uint_buf[0];
    geom_headers[i].nodeOffset = instance->uint_buf[1];
    geom_headers[i].elemCt = instance->uint_buf[2];
    geom_headers[i].elemOffset = instance->uint_buf[3];
    geom_headers[i].elemTypeCt = instance->uint_buf[4];
    geom_headers[i].elemLength = instance->uint_buf[5];
    geom_headers[i].geomID = instance->uint_buf[6];

    // Don't represent in MOAB if no mesh
    if (geom_headers[i].nodeCt == 0 && geom_headers[i].elemCt == 0)
      continue;

    // Create an entity set for this entity
    result = instance->create_set(geom_headers[i].setHandle);
    if (MB_SUCCESS != result)
      return result;

    // Set the dimension to -1; will have to reset later, after elements are read
    dum_int = -1;
    result = instance->mdbImpl->tag_set_data(instance->geomTag,
                                             &(geom_headers[i].setHandle), 1, &dum_int);
    if (MB_SUCCESS != result)
      return result;

    // Set a unique id tag
    result = instance->mdbImpl->tag_set_data(instance->uniqueIdTag,
                                             &(geom_headers[i].setHandle), 1,
                                             &(geom_headers[i].geomID));
    if (MB_SUCCESS != result)
      return result;

    // Put the set and uid into a map for later
    instance->uidSetMap[geom_headers[i].geomID] = geom_headers[i].setHandle;
  }

  // Now get the dimensions of elements for each geom entity
  for (unsigned int i = 0; i < info.numEntities; i++) {
    if (geom_headers[i].elemTypeCt == 0) continue;
    instance->FSEEK(model_offset + geom_headers[i].elemOffset);
    for (unsigned int j = 0; j < geom_headers[i].elemTypeCt; j++) {
      // For this elem type, get the type, nodes per elem, num elems
      instance->FREADI(3);
      int int_type = instance->uint_buf[0];
      int nodes_per_elem = instance->uint_buf[1];
      int num_elem = instance->uint_buf[2];
      EntityType elem_type = mp_type_to_mb_type[int_type];
      geom_headers[i].maxDim = std::max(geom_headers[i].maxDim,
                                        (int)CN::Dimension(elem_type));
      if (j < geom_headers[i].elemTypeCt - 1) {
        int num_skipped_ints = num_elem + num_elem*nodes_per_elem;
        if (major >= 14)
          num_skipped_ints += num_elem;
        instance->FREADI(num_skipped_ints);
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::GroupHeader::read_info_header(const unsigned int model_offset,
                                                const Tqdcfr::FEModelHeader::ArrayInfo &info,
                                                Tqdcfr* instance,
                                                Tqdcfr::GroupHeader *&group_headers)
{
  group_headers = new GroupHeader[info.numEntities];
  instance->FSEEK(model_offset + info.tableOffset);
  ErrorCode result;

  if (0 == instance->categoryTag) {
    static const char val[CATEGORY_TAG_SIZE] = {0};
    result = instance->mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
                                               MB_TYPE_OPAQUE, instance->categoryTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, val);
    if (MB_SUCCESS != result)
      return result;
  }

  for (unsigned int i = 0; i < info.numEntities; i++) {
    // Create an entity set for this entity
    result = instance->create_set(group_headers[i].setHandle);
    if (MB_SUCCESS != result)
      return result;
    static const char group_category[CATEGORY_TAG_SIZE] = "Group\0";

    instance->FREADI(6);
    group_headers[i].grpID = instance->uint_buf[0];
    group_headers[i].grpType = instance->uint_buf[1];
    group_headers[i].memCt = instance->uint_buf[2];
    group_headers[i].memOffset = instance->uint_buf[3];
    group_headers[i].memTypeCt = instance->uint_buf[4];
    group_headers[i].grpLength = instance->uint_buf[5];

    // Set the category tag to signify this is a group
    result = instance->mdbImpl->tag_set_data(instance->categoryTag,
                                             &(group_headers[i].setHandle), 1,
                                             group_category);
    if (MB_SUCCESS != result)
      return result;

    // Set a global id tag
    result = instance->mdbImpl->tag_set_data(instance->globalIdTag,
                                             &(group_headers[i].setHandle), 1,
                                             &(group_headers[i].grpID));
    if (MB_SUCCESS != result)
      return result;

    instance->gidSetMap[5][group_headers[i].grpID] = group_headers[i].setHandle;
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::BlockHeader::read_info_header(const double data_version,
                                                const unsigned int model_offset,
                                                const Tqdcfr::FEModelHeader::ArrayInfo &info,
                                                Tqdcfr* instance,
                                                Tqdcfr::BlockHeader *&block_headers)
{
  block_headers = new BlockHeader[info.numEntities];
  instance->FSEEK(model_offset + info.tableOffset);
  ErrorCode result;

  if (0 == instance->categoryTag) {
    static const char val[CATEGORY_TAG_SIZE] = {0};
    result = instance->mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
                                               MB_TYPE_OPAQUE, instance->categoryTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, val);
    if (MB_SUCCESS != result && MB_ALREADY_ALLOCATED != result)
      return result;
  }

  for (unsigned int i = 0; i < info.numEntities; i++) {
    // Create an entity set for this entity
    result = instance->create_set(block_headers[i].setHandle);
    if (MB_SUCCESS != result)
      return result;
    static const char material_category[CATEGORY_TAG_SIZE] = "Material Set\0";

    instance->FREADI(12);
    block_headers[i].blockID = instance->uint_buf[0];
    block_headers[i].blockElemType = instance->uint_buf[1];
    block_headers[i].memCt = instance->uint_buf[2];
    block_headers[i].memOffset = instance->uint_buf[3];
    block_headers[i].memTypeCt = instance->uint_buf[4];
    block_headers[i].attribOrder = instance->uint_buf[5]; // Attrib order
    block_headers[i].blockCol = instance->uint_buf[6];
    block_headers[i].blockMixElemType = instance->uint_buf[7]; // Mixed elem type
    block_headers[i].blockPyrType = instance->uint_buf[8];
    block_headers[i].blockMat = instance->uint_buf[9];
    block_headers[i].blockLength = instance->uint_buf[10];
    block_headers[i].blockDim = instance->uint_buf[11];

    Tag bhTag_header;
    {
      std::vector<int> def_uint_zero(3,0);
      result = instance->mdbImpl->tag_get_handle(BLOCK_HEADER, 3*sizeof(unsigned int), MB_TYPE_INTEGER,
                                                 bhTag_header, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_BYTES,
                                                 &def_uint_zero[0]);
      if (MB_SUCCESS != result)
        return result;
      int block_header_data[] = { static_cast<int>(block_headers[i].blockCol), static_cast<int>(block_headers[i].blockMat), 
                                  static_cast<int>(block_headers[i].blockDim) };
      result = instance->mdbImpl->tag_set_data(bhTag_header, &(block_headers[i].setHandle), 1,
                                               block_header_data);
    }

    if (MB_SUCCESS != result)
      return result;

    // Adjust element type for data version; older element types didn't include
    // 4 new trishell element types
    if (data_version <= 1.0 && block_headers[i].blockElemType >= 15)
      block_headers[i].blockElemType += 4;
    
    if (block_headers[i].blockElemType >= (unsigned)cub_elem_num_verts_len) {
      // Block element type unassigned, will have to infer from verts/element; make sure it's
      // the expected value of 52
      if ((14 == major && 2 < minor) || 15 <= major )
      {
        if(55 != block_headers[i].blockElemType)
           MB_SET_ERR(MB_FAILURE, "Invalid block element type: " << block_headers[i].blockElemType);
      }
      else
      {
        if(52 != block_headers[i].blockElemType)
           MB_SET_ERR(MB_FAILURE, "Invalid block element type: " << block_headers[i].blockElemType);
      }
    }

    // Set the material set tag and id tag both to id
    result = instance->mdbImpl->tag_set_data(instance->blockTag, &(block_headers[i].setHandle), 1,
                                             &(block_headers[i].blockID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->globalIdTag, &(block_headers[i].setHandle), 1,
                                             &(block_headers[i].blockID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->categoryTag,
                                             &(block_headers[i].setHandle), 1,
                                             material_category);
    if (MB_SUCCESS != result)
      return result;

    // If this block is empty, continue
    if (!block_headers[i].memCt)
      continue;

    // Check the number of vertices in the element type, and set the has mid nodes tag
    // accordingly; if element type wasn't set, they're unlikely to have mid nodes
    // 52 is for CUBIT versions below 14.1, 55 for CUBIT version 14.9 and above 
    if (52 != block_headers[i].blockElemType && 55 != block_headers[i].blockElemType) {
      int num_verts = cub_elem_num_verts[block_headers[i].blockElemType];
      block_headers[i].blockEntityType = block_type_to_mb_type[block_headers[i].blockElemType];
      if ((block_headers[i].blockEntityType < MBMAXTYPE) &&
          (num_verts != CN::VerticesPerEntity(block_headers[i].blockEntityType))) {
        // Not a linear element; try to find hasMidNodes values
        for (int j = 0; j < 4; j++) block_headers[i].hasMidNodes[j] = 0;
        if (0 == instance->hasMidNodesTag) {
          result = instance->mdbImpl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                                                     instance->hasMidNodesTag, MB_TAG_SPARSE | MB_TAG_CREAT,
                                                     block_headers[i].hasMidNodes);
          if (MB_SUCCESS != result)
            return result;
        }

        CN::HasMidNodes(block_headers[i].blockEntityType, num_verts,
                        block_headers[i].hasMidNodes);

        // Now set the tag on this set
        result = instance->mdbImpl->tag_set_data(instance->hasMidNodesTag, &block_headers[i].setHandle, 1,
                                                 block_headers[i].hasMidNodes);
        if (MB_SUCCESS != result)
          return result;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::NodesetHeader::read_info_header(const unsigned int model_offset,
                                                  const Tqdcfr::FEModelHeader::ArrayInfo &info,
                                                  Tqdcfr* instance,
                                                  Tqdcfr::NodesetHeader *&nodeset_headers)
{
  nodeset_headers = new NodesetHeader[info.numEntities];
  instance->FSEEK(model_offset + info.tableOffset);
  ErrorCode result;

  if (0 == instance->categoryTag) {
    static const char val[CATEGORY_TAG_SIZE] = {0};
    result = instance->mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
                                               MB_TYPE_OPAQUE, instance->categoryTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, val);
    if (MB_SUCCESS != result)
      return result;
  }

  for (unsigned int i = 0; i < info.numEntities; i++) {
    // Create an entity set for this entity
    result = instance->create_set(nodeset_headers[i].setHandle);
    if (MB_SUCCESS != result)
      return result;
    static const char dirichlet_category[CATEGORY_TAG_SIZE] = "Dirichlet Set\0";

    instance->FREADI(8);
    nodeset_headers[i].nsID = instance->uint_buf[0];
    nodeset_headers[i].memCt = instance->uint_buf[1];
    nodeset_headers[i].memOffset = instance->uint_buf[2];
    nodeset_headers[i].memTypeCt = instance->uint_buf[3];
    nodeset_headers[i].pointSym = instance->uint_buf[4]; // Point sym
    nodeset_headers[i].nsCol = instance->uint_buf[5];
    nodeset_headers[i].nsLength = instance->uint_buf[6];
    // Pad

    // Set the dirichlet set tag and id tag both to id
    result = instance->mdbImpl->tag_set_data(instance->nsTag, &(nodeset_headers[i].setHandle), 1,
                                             &(nodeset_headers[i].nsID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->globalIdTag, &(nodeset_headers[i].setHandle), 1,
                                             &(nodeset_headers[i].nsID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->categoryTag,
                                             &(nodeset_headers[i].setHandle), 1,
                                             dirichlet_category);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::SidesetHeader::read_info_header(const unsigned int model_offset,
                                                  const Tqdcfr::FEModelHeader::ArrayInfo &info,
                                                  Tqdcfr* instance,
                                                  Tqdcfr::SidesetHeader *&sideset_headers)
{
  sideset_headers = new SidesetHeader[info.numEntities];
  instance->FSEEK(model_offset + info.tableOffset);
  ErrorCode result;

  if (0 == instance->categoryTag) {
    static const char val[CATEGORY_TAG_SIZE] = {0};
    result = instance->mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
                                               MB_TYPE_OPAQUE, instance->categoryTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, val);
    if (MB_SUCCESS != result)
      return result;
  }

  for (unsigned int i = 0; i < info.numEntities; i++) {
    // Create an entity set for this entity
    result = instance->create_set(sideset_headers[i].setHandle);
    if (MB_SUCCESS != result)
      return result;
    static const char neumann_category[CATEGORY_TAG_SIZE] = "Neumann Set\0";

    instance->FREADI(8);
    sideset_headers[i].ssID = instance->uint_buf[0];
    sideset_headers[i].memCt = instance->uint_buf[1];
    sideset_headers[i].memOffset = instance->uint_buf[2];
    sideset_headers[i].memTypeCt = instance->uint_buf[3];
    sideset_headers[i].numDF = instance->uint_buf[4]; // Num dist factors
    sideset_headers[i].ssCol = instance->uint_buf[5];
    sideset_headers[i].useShell = instance->uint_buf[6];
    sideset_headers[i].ssLength = instance->uint_buf[7];

    // Set the neumann set tag and id tag both to id
    result = instance->mdbImpl->tag_set_data(instance->ssTag, &(sideset_headers[i].setHandle), 1,
                                             &(sideset_headers[i].ssID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->globalIdTag, &(sideset_headers[i].setHandle), 1,
                                             &(sideset_headers[i].ssID));
    if (MB_SUCCESS != result)
      return result;
    result = instance->mdbImpl->tag_set_data(instance->categoryTag,
                                             &(sideset_headers[i].setHandle), 1,
                                             neumann_category);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

void Tqdcfr::ModelEntry::print_geom_headers(const char *prefix,
                                            GeomHeader *header,
                                            const unsigned int num_headers)
{
  if (!debug)
    return;
  std::cout << prefix << std::endl;
  if (NULL != header) {
    for (unsigned int i = 0; i < num_headers; i++) {
      std::cout << "Index " << i << std::endl;
      header[i].print();
    }
  }
}

void Tqdcfr::ModelEntry::print_group_headers(const char *prefix,
                                             GroupHeader *header,
                                             const unsigned int num_headers)
{
  if (!debug)
    return;
  std::cout << prefix << std::endl;
  if (NULL != header) {
    for (unsigned int i = 0; i < num_headers; i++)
      header[i].print();
  }
}

void Tqdcfr::ModelEntry::print_block_headers(const char *prefix,
                                             BlockHeader *header,
                                             const unsigned int num_headers)
{
  if (!debug)
    return;
  std::cout << prefix << std::endl;
  if (NULL != header) {
    for (unsigned int i = 0; i < num_headers; i++)
      header[i].print();
  }
}

void Tqdcfr::ModelEntry::print_nodeset_headers(const char *prefix,
                                               NodesetHeader *header,
                                               const unsigned int num_headers)
{
  if (!debug)
    return;
  std::cout << prefix << std::endl;
  if (NULL != header) {
    for (unsigned int i = 0; i < num_headers; i++)
      header[i].print();
  }
}

void Tqdcfr::ModelEntry::print_sideset_headers(const char *prefix,
                                               SidesetHeader *header,
                                               const unsigned int num_headers)
{
  if (!debug)
    return;
  std::cout << prefix << std::endl;
  if (NULL != header) {
    for (unsigned int i = 0; i < num_headers; i++)
      header[i].print();
  }
}

ErrorCode Tqdcfr::ModelEntry::read_header_info(Tqdcfr* instance, const double data_version)
{
  feModelHeader.init(modelOffset, instance);
  int negone = -1;
  ErrorCode result;

  int zero = 0;
  result = instance->mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                             instance->globalIdTag, MB_TAG_DENSE | MB_TAG_CREAT, &zero);
  if (MB_SUCCESS != result)
    return result;

  if (feModelHeader.geomArray.numEntities > 0) {
    result = instance->mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
                                               instance->geomTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
    if (MB_SUCCESS != result)
      return result;

    result = instance->mdbImpl->tag_get_handle("UNIQUE_ID", 1, MB_TYPE_INTEGER,
                                               instance->uniqueIdTag,
                                               MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
    if (MB_SUCCESS != result)
      return result;
    
    result = Tqdcfr::GeomHeader::read_info_header(modelOffset,
                                                  feModelHeader.geomArray,
                                                  instance,
                                                  feGeomH);
    print_geom_headers("Geom headers:", feGeomH, feModelHeader.geomArray.numEntities);
    if (MB_SUCCESS != result)
      return result;
  }

  if (feModelHeader.groupArray.numEntities > 0) {
    result = Tqdcfr::GroupHeader::read_info_header(modelOffset,
                                                   feModelHeader.groupArray,
                                                   instance,
                                                   feGroupH);
    print_group_headers("Group headers:", feGroupH, feModelHeader.groupArray.numEntities);
    if (MB_SUCCESS != result)
      return result;
  }

  if (feModelHeader.blockArray.numEntities > 0) {
    result = instance->mdbImpl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                               instance->blockTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
    if (MB_SUCCESS != result)
      return result;

    result = Tqdcfr::BlockHeader::read_info_header(data_version, modelOffset,
                                                   feModelHeader.blockArray,
                                                   instance,
                                                   feBlockH);
    print_block_headers("Block headers:", feBlockH, feModelHeader.blockArray.numEntities);
    if (MB_SUCCESS != result)
      return result;
  }
  if (feModelHeader.nodesetArray.numEntities > 0) {
    result = instance->mdbImpl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                               instance->nsTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
    if (MB_SUCCESS != result)
      return result;

    result = Tqdcfr::NodesetHeader::read_info_header(modelOffset,
                                                     feModelHeader.nodesetArray,
                                                     instance,
                                                     feNodeSetH);
    if (MB_SUCCESS != result)
      return result;
    print_nodeset_headers("Nodeset headers:", feNodeSetH, feModelHeader.nodesetArray.numEntities);
  }
  if (feModelHeader.sidesetArray.numEntities > 0) {
    result = instance->mdbImpl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                               instance->ssTag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
    if (MB_SUCCESS != result)
      return result;

    result = Tqdcfr::SidesetHeader::read_info_header(modelOffset,
                                                     feModelHeader.sidesetArray,
                                                     instance,
                                                     feSideSetH);
    print_sideset_headers("SideSet headers:", feSideSetH, feModelHeader.sidesetArray.numEntities);
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::ModelEntry::read_metadata_info(Tqdcfr *tqd)
{
  if (debug)
    std::cout << "Geom metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.geomArray.metaDataOffset,
                      geomMD);
  if (debug)
    std::cout << "Node metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.nodeArray.metaDataOffset,
                      nodeMD);
  if (debug)
    std::cout << "Elem metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.elementArray.metaDataOffset,
                      elementMD);
  if (debug)
    std::cout << "Group metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.groupArray.metaDataOffset,
                      groupMD);
  if (debug)
    std::cout << "Block metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.blockArray.metaDataOffset,
                      blockMD);
  if (debug)
    std::cout << "Nodeset metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.nodesetArray.metaDataOffset,
                      nodesetMD);
  if (debug)
    std::cout << "Sideset metadata:" << std::endl;
  tqd->read_meta_data(modelOffset + feModelHeader.sidesetArray.metaDataOffset,
                      sidesetMD);

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::read_acis_records(const char* sat_filename)
{
  // Get the acis model location
  unsigned int acis_model_offset = 0, acis_model_length = 0, acis_model_handle = 1,
               acis_sat_type = 1;
  for (unsigned int i = 0; i < fileTOC.numModels; i++) {
    if (modelEntries[i].modelHandle == acis_model_handle &&
        modelEntries[i].modelType == acis_sat_type) {
      acis_model_offset = modelEntries[i].modelOffset;
      acis_model_length = modelEntries[i].modelLength;
      break;
    }
  }

  if (acis_model_length == 0)
    return MB_SUCCESS;

  std::vector<AcisRecord> records;

  acisDumpFile = NULL;
  if (sat_filename) {
    acisDumpFile = fopen(sat_filename, "w+");
    if (NULL == acisDumpFile)
      return MB_FAILURE;
  }

  // Position the file at the start of the acis model
  FSEEK(acis_model_offset);

  unsigned int bytes_left = acis_model_length;

  struct AcisRecord this_record;
  reset_record(this_record);
  char *ret;

  // Make the char buffer at least buf_size + 1 long, to fit null char
  const unsigned int buf_size = 1023;

  //CHECK_SIZE(char_buf, buf_size + 1);
  char_buf.resize(buf_size + 1);

  while (0 != bytes_left) {
    // Read the next buff characters, or bytes_left if smaller
    unsigned int next_buf = (bytes_left > buf_size ? buf_size : bytes_left);
    FREADC(next_buf);

    if (NULL != acisDumpFile)
      fwrite(&char_buf[0], sizeof(char), next_buf, acisDumpFile);

    // Put null at end of string to stop searches
    char_buf.resize(next_buf + 1);
    char_buf[next_buf] = '\0';
    unsigned int buf_pos = 0;

    // Check for first read, and if so, get rid of the header
    if (bytes_left == acis_model_length) {
      // Look for 3 newlines
      ret = strchr(&(char_buf[0]), '\n'); ret = strchr(ret + 1, '\n'); ret = strchr(ret + 1, '\n');
      if (NULL == ret)
        return MB_FAILURE;
      buf_pos += ret - &(char_buf[0]) + 1;
    }

    bytes_left -= next_buf;

    // Now start grabbing records
    do {
      // Get next occurrence of '#' (record terminator)
      ret = strchr(&(char_buf[buf_pos]), '#');
      while (ret && (unsigned int)(ret + 1 - &char_buf[0]) < bytes_left
          && *(ret + 1) != '\n'  && *(ret + 1) != '\r'  && *(ret + 1) != 0) // CR added for windows
        ret = strchr(ret + 1, '#');
      if (NULL != ret) {
        // Grab the string (inclusive of the record terminator and the line feed) and complete the record
        int num_chars = ret - &(char_buf[buf_pos]) + 2;
        if (*(ret + 1) == '\r')
          num_chars++; // add more one character for Windows CR
        this_record.att_string.append(&(char_buf[buf_pos]), num_chars);
        buf_pos += num_chars;
        process_record(this_record);

        // Put the record in the list...
        records.push_back(this_record);

        // And reset the record
        reset_record(this_record);
      }
      else {
        // Reached end of buffer; cache string then go get another; discard last character,
        // which will be the null character
        this_record.att_string.append(&(char_buf[buf_pos]), next_buf - buf_pos);
        buf_pos = next_buf;
      }
    } while (buf_pos < next_buf);
  }

  if (NULL != acisDumpFile)
    fwrite("\n======================\nSorted acis records:\n======================\n", 1, 68, acisDumpFile);

  // Now interpret the records
  interpret_acis_records(records);

  if (NULL != acisDumpFile)
    fclose(acisDumpFile);

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::interpret_acis_records(std::vector<AcisRecord> &records)
{
  // Make a tag for the vector holding unrecognized attributes
  void *default_val = NULL;
  ErrorCode result = 
    mdbImpl->tag_get_handle("ATTRIB_VECTOR", sizeof(void*), MB_TYPE_OPAQUE,
                            attribVectorTag, MB_TAG_CREAT | MB_TAG_SPARSE, &default_val);
  if (MB_SUCCESS != result)
    return result;

  unsigned int current_record = 0;

#define REC records[current_record]

  while (current_record != records.size()) {
    // If this record's been processed, or if it's an attribute, continue
    if (REC.processed || REC.rec_type == Tqdcfr::ATTRIB) {
      current_record++;
      continue;
    }

    if (REC.rec_type == Tqdcfr::UNKNOWN) {
      REC.processed = true;
      current_record++;
      continue;
    }

    // It's a known, non-attrib rec type; parse for any attribs
    parse_acis_attribs(current_record, records);

    REC.processed = true;

    current_record++;
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::parse_acis_attribs(const unsigned int entity_rec_num,
                                     std::vector<AcisRecord> &records)
{
  unsigned int num_read;
  std::vector<std::string> attrib_vec;
  char temp_name[1024];
  char name_tag_val[NAME_TAG_SIZE];
  std::string name_tag;
  int id = -1;
  int uid = -1;
  int next_attrib = -1;
  ErrorCode result;

  int current_attrib = records[entity_rec_num].first_attrib;
  if (-1 == current_attrib)
    return MB_SUCCESS;

  if (NULL != acisDumpFile) {
    fwrite("-----------------------------------------------------------------------\n", 1, 72, acisDumpFile);
    fwrite(records[entity_rec_num].att_string.c_str(), sizeof(char), 
           records[entity_rec_num].att_string.length(), acisDumpFile);
  }

  while (-1 != current_attrib) {
    if (records[current_attrib].rec_type != Tqdcfr::UNKNOWN &&
       (records[current_attrib].att_next != next_attrib ||
        records[current_attrib].att_ent_num != (int)entity_rec_num))
      return MB_FAILURE;

    if (NULL != acisDumpFile)
      fwrite(records[current_attrib].att_string.c_str(), sizeof(char),
             records[current_attrib].att_string.length(), acisDumpFile);

    // Is the attrib one we already recognize?
    if (strncmp(records[current_attrib].att_string.c_str(), "ENTITY_NAME", 11) == 0) {
      // Parse name
      int num_chars;
      num_read = sscanf(records[current_attrib].att_string.c_str(), "ENTITY_NAME @%d %s", &num_chars, temp_name);
      if (num_read != 2)
        num_read = sscanf(records[current_attrib].att_string.c_str(), "ENTITY_NAME %d %s", &num_chars, temp_name);
      if (num_read != 2)
        return MB_FAILURE;

      // Put the name on the entity
      name_tag = std::string(temp_name, num_chars);
    }
    else if (strncmp(records[current_attrib].att_string.c_str(), "ENTITY_ID", 9) == 0) {
      // Parse id
      int bounding_uid, bounding_sense;
      num_read = sscanf(records[current_attrib].att_string.c_str(), "ENTITY_ID 0 3 %d %d %d",
                        &id, &bounding_uid, &bounding_sense);
      if (3 != num_read) {
        // Try reading updated entity_id format, which has coordinate triple embedded in it too
        float dumx, dumy, dumz;
        num_read = sscanf(records[current_attrib].att_string.c_str(), 
                          "ENTITY_ID 3 %f %f %f 3 %d %d %d", 
                          &dumx, &dumy, &dumz, &id, &bounding_uid, &bounding_sense);
        num_read -= 3;
      }

      if (3 != num_read)
        std::cout << "Warning: bad ENTITY_ID attribute in .sat file, record number " << entity_rec_num
                  << ", record follows:" << std::endl
                  << records[current_attrib].att_string.c_str() << std::endl;
    }
    else if (strncmp(records[current_attrib].att_string.c_str(), "UNIQUE_ID", 9) == 0) {
      // Parse uid
      if (major >=14) // Change of format for cubit 14:
        num_read =sscanf(records[current_attrib].att_string.c_str(), "UNIQUE_ID 0 1 %d", &uid);
      else
        num_read = sscanf(records[current_attrib].att_string.c_str(), "UNIQUE_ID 1 0 1 %d", &uid);
      if (1 != num_read)
        return MB_FAILURE;
    }
    else if (strncmp(records[current_attrib].att_string.c_str(), "COMPOSITE_ATTRIB @9 UNIQUE_ID", 29) == 0) {
      // Parse uid
      int dum1, dum2, dum3, dum4;
      num_read = sscanf(records[current_attrib].att_string.c_str(), "COMPOSITE_ATTRIB @9 UNIQUE_ID %d %d %d %d %d",
          &dum1, &dum2, &dum3, &dum4, &uid);
      if (5 != num_read)
        return MB_FAILURE;
    }
    else if (strncmp(records[current_attrib].att_string.c_str(), "COMPOSITE_ATTRIB @9 ENTITY_ID", 29) == 0) {
      // Parse id
      int dum1, dum2, dum3;
      num_read = sscanf(records[current_attrib].att_string.c_str(), "COMPOSITE_ATTRIB @9 ENTITY_ID %d %d %d %d",
          &dum1, &dum2, &dum3, &id);
      if (4 != num_read)
        return MB_FAILURE;
    }
    else {
      attrib_vec.push_back(records[current_attrib].att_string);
    }

    records[current_attrib].processed = true;
    next_attrib = current_attrib;
    current_attrib = records[current_attrib].att_prev;
  }

  // At this point, there aren't entity sets for entity types which don't contain mesh
  // in this case, just return
  if (records[entity_rec_num].rec_type == aBODY ||
      (records[entity_rec_num].entity == 0 && uid == -1)) {
    return MB_SUCCESS;
    // Warning: couldn't resolve entity of type 1 because no uid was found.
    // ddriv: GeomTopoTool.cpp:172: ErrorCode GeomTopoTool::separate_by_dimension(const Range&, Range*, void**): Assertion `false' failed.
    // xxx
  }

  // Parsed the data; now put on mdb entities; first we need to find the entity
  if (records[entity_rec_num].entity == 0) {
    records[entity_rec_num].entity = uidSetMap[uid];
  }

  if (0 == records[entity_rec_num].entity)
    return MB_SUCCESS; // We do not have a MOAB entity for this, skip

  //assert(records[entity_rec_num].entity);

  // Set the id
  if (id != -1) {
    result = mdbImpl->tag_set_data(globalIdTag, &(records[entity_rec_num].entity), 1, &id);
    if (MB_SUCCESS != result)
      return result;

    int ent_dim = -1;
    if (records[entity_rec_num].rec_type == aBODY)
      ent_dim = 4;
    else if (records[entity_rec_num].rec_type == LUMP)
      ent_dim = 3;
    else if (records[entity_rec_num].rec_type == FACE)
      ent_dim = 2;
    else if (records[entity_rec_num].rec_type == aEDGE)
      ent_dim = 1;
    else if (records[entity_rec_num].rec_type == aVERTEX)
      ent_dim = 0;
    if (-1 != ent_dim)
      gidSetMap[ent_dim][id] = records[entity_rec_num].entity;
  }

  // Set the name
  if (!name_tag.empty()) {
    if (0 == entityNameTag) {
      char dum_val[NAME_TAG_SIZE] = {0};
      result = mdbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
                                       entityNameTag, MB_TAG_SPARSE | MB_TAG_CREAT, dum_val);
      if (MB_SUCCESS != result)
        return result;
    }

    size_t len = name_tag.size();
    if (len >= NAME_TAG_SIZE)
      len = NAME_TAG_SIZE - 1; // Truncate a name that is too big
    memcpy(name_tag_val, name_tag.c_str(), len);
    memset(name_tag_val + len, '\0', NAME_TAG_SIZE - len);
    result = mdbImpl->tag_set_data(entityNameTag, &(records[entity_rec_num].entity), 1, name_tag_val);
    if (MB_SUCCESS != result)
      return result;
  }

  if (!attrib_vec.empty()) {
    // Put the attrib vector in a tag on the entity
    std::vector<std::string> *dum_vec;
    result = mdbImpl->tag_get_data(attribVectorTag, &(records[entity_rec_num].entity), 1, &dum_vec);
    if (MB_SUCCESS != result && MB_TAG_NOT_FOUND != result)
      return result;
    if (MB_TAG_NOT_FOUND == result || dum_vec == NULL) {
      // Put this list directly on the entity
      dum_vec = new std::vector<std::string>;
      dum_vec->swap(attrib_vec);
      result = mdbImpl->tag_set_data(attribVectorTag, &(records[entity_rec_num].entity), 1, &dum_vec);
      if (MB_SUCCESS != result) {
        delete dum_vec;
        return result;
      }
    }
    else {
      // Copy this list over, and delete this list
      std::copy(attrib_vec.begin(), attrib_vec.end(),
                std::back_inserter(*dum_vec));
    }
  }

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::reset_record(AcisRecord &this_record)
{
  this_record.rec_type = Tqdcfr::UNKNOWN;
  this_record.att_string.clear();
  this_record.first_attrib = this_record.att_prev =
    this_record.att_next = this_record.att_ent_num = -1;
  this_record.processed = false;
  this_record.entity = 0;

  return MB_SUCCESS;
}

ErrorCode Tqdcfr::process_record(AcisRecord &this_record)
{
  // Get the entity type
  const char *type_substr;

  // Try attribs first, since the others have some common processing between them
  if ((type_substr = strstr(this_record.att_string.c_str(), "attrib")) != NULL &&
      type_substr-this_record.att_string.c_str() < 20) {
    this_record.rec_type = Tqdcfr::ATTRIB;
    bool simple_attrib = false;
    bool generic_attrib = false;
    if ((type_substr = strstr(this_record.att_string.c_str(), "simple-snl-attrib")) != NULL)
      simple_attrib = true;
    else if ((type_substr = strstr(this_record.att_string.c_str(), "integer_attrib-name_attrib-gen-attrib")) != NULL)
      generic_attrib = true;
    else {
      this_record.rec_type = Tqdcfr::UNKNOWN;
      return MB_SUCCESS;
    }

    // Find next space
    type_substr = strchr(type_substr, ' ');
    if (NULL == type_substr)
      return MB_FAILURE;

    // Read the numbers from there
    int num_converted = sscanf(type_substr, " $-1 -1 $%d $%d $%d -1", &(this_record.att_prev),
                               &(this_record.att_next), &(this_record.att_ent_num));
    if (num_converted != 3)
      return MB_FAILURE;

    // Trim the string to the attribute, if it's a simple attrib
    if (simple_attrib) {
      type_substr = strstr(this_record.att_string.c_str(), "NEW_SIMPLE_ATTRIB");
      if (NULL == type_substr)
        return MB_FAILURE;
      type_substr = strstr(type_substr, "@");
      if (NULL == type_substr)
        return MB_FAILURE;
      type_substr = strstr(type_substr, " ") + 1;
      // Copy the rest of the string to a dummy string
      std::string dum_str(type_substr);
      this_record.att_string = dum_str;
    }
    else if (generic_attrib) {
      type_substr = strstr(this_record.att_string.c_str(), "CUBIT_ID");
      if (NULL == type_substr)
        return MB_FAILURE;
      // Copy the rest of the string to a dummy string
      std::string dum_str(type_substr);
      this_record.att_string = dum_str;
    }
  }
  else {
    // Else it's a topological entity, I think
    if ((type_substr = strstr(this_record.att_string.c_str(), "body")) != NULL
        && type_substr-this_record.att_string.c_str() < 20) {
      this_record.rec_type = Tqdcfr::aBODY;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "lump")) != NULL  &&
             type_substr-this_record.att_string.c_str() < 20) {
      this_record.rec_type = Tqdcfr::LUMP;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "shell")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      // Don't care about shells
      this_record.rec_type = Tqdcfr::UNKNOWN;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "surface")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      // Don't care about surfaces
      this_record.rec_type = Tqdcfr::UNKNOWN;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "face")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      this_record.rec_type = Tqdcfr::FACE;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "loop")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      // Don't care about loops
      this_record.rec_type = Tqdcfr::UNKNOWN;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "coedge")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      // Don't care about coedges
      this_record.rec_type = Tqdcfr::UNKNOWN;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "edge")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      this_record.rec_type = Tqdcfr::aEDGE;
    }
    else if ((type_substr = strstr(this_record.att_string.c_str(), "vertex")) != NULL &&
             type_substr-this_record.att_string.c_str() < 20) {
      this_record.rec_type = Tqdcfr::aVERTEX;
    }
    else 
      this_record.rec_type = Tqdcfr::UNKNOWN;

    if (this_record.rec_type != Tqdcfr::UNKNOWN) {
      // Print a warning if it looks like there are sequence numbers
      if (type_substr != this_record.att_string.c_str() && !printedSeqWarning) {
        std::cout << "Warning: acis file has sequence numbers!" << std::endl;
        printedSeqWarning = true;
      }

      // Scan ahead to the next white space
      type_substr = strchr(type_substr, ' ');
      if (NULL == type_substr)
        return MB_FAILURE;

      // Get the id of the first attrib
      int num_converted = sscanf(type_substr, " $%d", &(this_record.first_attrib));
      if (num_converted != 1)
        return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

Tqdcfr::FileTOC::FileTOC()
  : fileEndian(0), fileSchema(0), numModels(0), modelTableOffset(0),
    modelMetaDataOffset(0), activeFEModel(0) {}

void Tqdcfr::FileTOC::print()
{
  std::cout << "FileTOC:End, Sch, #Mdl, TabOff, "
            << "MdlMDOff, actFEMdl = ";
  std::cout << fileEndian << ", " << fileSchema << ", " << numModels
            << ", " << modelTableOffset << ", " 
            << modelMetaDataOffset << ", " << activeFEModel << std::endl;
}

Tqdcfr::FEModelHeader::ArrayInfo::ArrayInfo()
  : numEntities(0), tableOffset(0), metaDataOffset(0)
{}

void Tqdcfr::FEModelHeader::ArrayInfo::print()
{
  std::cout << "ArrayInfo:numEntities, tableOffset, metaDataOffset = "
            << numEntities << ", " << tableOffset << ", " << metaDataOffset << std::endl;
}

void Tqdcfr::FEModelHeader::ArrayInfo::init(const std::vector<unsigned int>& uint_buf_in)
{
  numEntities = uint_buf_in[0]; tableOffset = uint_buf_in[1]; metaDataOffset = uint_buf_in[2];
}

void Tqdcfr::FEModelHeader::print()
{
  std::cout << "FEModelHeader:feEndian, feSchema, feCompressFlag, feLength = "
            << feEndian << ", " << feSchema << ", " << feCompressFlag << ", " << feLength << std::endl;
  std::cout << "geomArray: "; geomArray.print();
  std::cout << "nodeArray: "; nodeArray.print();
  std::cout << "elementArray: "; elementArray.print();
  std::cout << "groupArray: "; groupArray.print();
  std::cout << "blockArray: "; blockArray.print();
  std::cout << "nodesetArray: "; nodesetArray.print();
  std::cout << "sidesetArray: "; sidesetArray.print();
}

Tqdcfr::GeomHeader::GeomHeader()
  : geomID(0), nodeCt(0), nodeOffset(0), elemCt(0), elemOffset(0),
    elemTypeCt(0), elemLength(0), maxDim(0), setHandle(0)
{}

void Tqdcfr::GeomHeader::print() 
{
  std::cout << "geomID = " << geomID << std::endl;
  std::cout << "nodeCt = " << nodeCt << std::endl;
  std::cout << "nodeOffset = " << nodeOffset << std::endl;
  std::cout << "elemCt = " << elemCt << std::endl;
  std::cout << "elemOffset = " << elemOffset << std::endl;
  std::cout << "elemTypeCt = " << elemTypeCt << std::endl;
  std::cout << "elemLength = " << elemLength << std::endl;
  std::cout << "setHandle = " << setHandle << std::endl;
}

Tqdcfr::GroupHeader::GroupHeader()
  : grpID(0), grpType(0), memCt(0), memOffset(0), memTypeCt(0), grpLength(0),
    setHandle(0)
{}

void Tqdcfr::GroupHeader::print() 
{
  std::cout << "grpID = " << grpID << std::endl;
  std::cout << "grpType = " << grpType << std::endl;
  std::cout << "memCt = " << memCt << std::endl;
  std::cout << "memOffset = " << memOffset << std::endl;
  std::cout << "memTypeCt = " << memTypeCt << std::endl;
  std::cout << "grpLength = " << grpLength << std::endl;
  std::cout << "setHandle = " << setHandle << std::endl;
}

Tqdcfr::BlockHeader::BlockHeader()
  : blockID(0), blockElemType(0), memCt(0), memOffset(0), memTypeCt(0), attribOrder(0), blockCol(0),
    blockMixElemType(0), blockPyrType(0), blockMat(0), blockLength(0), blockDim(0),
    setHandle(0), blockEntityType(MBMAXTYPE)
{}

void Tqdcfr::BlockHeader::print() 
{
  std::cout << "blockID = " << blockID << std::endl;
  std::cout << "blockElemType = " << blockElemType << std::endl;
  std::cout << "memCt = " << memCt << std::endl;
  std::cout << "memOffset = " << memOffset << std::endl;
  std::cout << "memTypeCt = " << memTypeCt << std::endl;
  std::cout << "attribOrder = " << attribOrder << std::endl;
  std::cout << "blockCol = " << blockCol << std::endl;
  std::cout << "blockMixElemType = " << blockMixElemType << std::endl;
  std::cout << "blockPyrType = " << blockPyrType << std::endl;
  std::cout << "blockMat = " << blockMat << std::endl;
  std::cout << "blockLength = " << blockLength << std::endl;
  std::cout << "blockDim = " << blockDim << std::endl;
  std::cout << "setHandle = " << setHandle << std::endl;
  std::cout << "blockEntityType = " << blockEntityType << std::endl;
}

Tqdcfr::NodesetHeader::NodesetHeader()
  : nsID(0), memCt(0), memOffset(0), memTypeCt(0), pointSym(0), nsCol(0), nsLength(0),
    setHandle(0)
{}

void Tqdcfr::NodesetHeader::print() 
{
  std::cout << "nsID = " << nsID << std::endl;
  std::cout << "memCt = " << memCt << std::endl;
  std::cout << "memOffset = " << memOffset << std::endl;
  std::cout << "memTypeCt = " << memTypeCt << std::endl;
  std::cout << "pointSym = " << pointSym << std::endl;
  std::cout << "nsCol = " << nsCol << std::endl;
  std::cout << "nsLength = " << nsLength << std::endl;
  std::cout << "setHandle = " << setHandle << std::endl;
}

Tqdcfr::SidesetHeader::SidesetHeader()
  : ssID(0), memCt(0), memOffset(0), memTypeCt(0), numDF(0), ssCol(0), useShell(0), ssLength(0),
    setHandle(0)
{}

void Tqdcfr::SidesetHeader::print() 
{
  std::cout << "ssID = " << ssID << std::endl;
  std::cout << "memCt = " << memCt << std::endl;
  std::cout << "memOffset = " << memOffset << std::endl;
  std::cout << "memTypeCt = " << memTypeCt << std::endl;
  std::cout << "numDF = " << numDF << std::endl;
  std::cout << "ssCol = " << ssCol << std::endl;
  std::cout << "useShell = " << useShell << std::endl;
  std::cout << "ssLength = " << ssLength << std::endl;
  std::cout << "setHandle = " << setHandle << std::endl;
}

Tqdcfr::MetaDataContainer::MetaDataEntry::MetaDataEntry()
  : mdOwner(0), mdDataType(0), mdIntValue(0),
    mdName("(uninit)"), mdStringValue("(uninit)"), mdDblValue(0)
{}

void Tqdcfr::MetaDataContainer::MetaDataEntry::print()
{
  std::cout << "MetaDataEntry:own, typ, name, I, D, S = "
            << mdOwner << ", " << mdDataType << ", " << mdName << ", " << mdIntValue << ", "
            << mdDblValue << ", " << mdStringValue;
  unsigned int i;
  if (mdIntArrayValue.size()) {
    std::cout << std::endl << "IArray = " << mdIntArrayValue[0];
    for (i = 1; i < mdIntArrayValue.size(); i++)
      std::cout << ", " << mdIntArrayValue[i];
  }
  if (mdDblArrayValue.size()) {
    std::cout << std::endl << "DArray = " << mdDblArrayValue[0];
    for (i = 1; i < mdDblArrayValue.size(); i++)
      std::cout << ", " << mdDblArrayValue[i];
  }
  std::cout << std::endl;
}

void Tqdcfr::MetaDataContainer::print()
{
  std::cout << "MetaDataContainer:mdSchema, compressFlag, numDatums = "
            << mdSchema << ", " << compressFlag << ", " << metadataEntries.size() << std::endl;

  for (unsigned int i = 0; i < metadataEntries.size(); i++)
    metadataEntries[i].print();
}

Tqdcfr::MetaDataContainer::MetaDataContainer()
  : mdSchema(0), compressFlag(0)
{}

int Tqdcfr::MetaDataContainer::get_md_entry(const unsigned int owner, const std::string &name)
{
  for (unsigned int i = 0; i < metadataEntries.size(); i++) {
    if (owner == metadataEntries[i].mdOwner && name == metadataEntries[i].mdName)
      return i;
  }

  return -1;
}

Tqdcfr::ModelEntry::ModelEntry()
  : modelHandle(0), modelOffset(0),
    modelLength(0), modelType(0), modelOwner(0), modelPad(0),
    feGeomH(NULL), feGroupH(NULL), feBlockH(NULL),
    feNodeSetH(NULL), feSideSetH(NULL)
{}

Tqdcfr::ModelEntry::~ModelEntry()
{
  delete [] feGeomH;
  delete [] feGroupH;
  delete [] feBlockH;
  delete [] feNodeSetH;
  delete [] feSideSetH;
}

void Tqdcfr::ModelEntry::print()
{
  std::cout << "ModelEntry: Han, Of, Len, Tp, Own, Pd = "
            << modelHandle << ", " << modelOffset << ", " << modelLength
            << ", " << modelType << ", " << modelOwner << ", " << modelPad
            << std::endl;
}

ErrorCode Tqdcfr::create_set(EntityHandle& h, unsigned int flags)
{
  return mdbImpl->create_meshset(flags, h);
}

} // namespace moab
