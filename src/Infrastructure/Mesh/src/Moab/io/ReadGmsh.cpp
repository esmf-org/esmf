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
 * \class ReadGmsh
 * \brief Gmsh (http://www.geuz.org/gmsh) file reader
 *
 * See: http://geuz.org/gmsh/doc/texinfo/gmsh.html#MSH-ASCII-file-format
 *
 * \author Jason Kraftcheck
 */

#include "ReadGmsh.hpp"
#include "FileTokenizer.hpp" // for file tokenizer
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/CN.hpp"
#include "GmshUtil.hpp"

#include <errno.h>
#include <string.h>
#include <map>
#include <set>

namespace moab {

ReaderIface* ReadGmsh::factory(Interface* iface)
{
  return new ReadGmsh(iface);
}

ReadGmsh::ReadGmsh(Interface* impl)
  : mdbImpl(impl), globalId(0)
{
  mdbImpl->query_interface(readMeshIface);
}

ReadGmsh::~ReadGmsh()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadGmsh::read_tag_values(const char* /* file_name */,
                                    const char* /* tag_name */,
                                    const FileOptions& /* opts */,
                                    std::vector<int>& /* tag_values_out */,
                                    const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadGmsh::load_file(const char* filename,
                              const EntityHandle*,
                              const FileOptions&,
                              const ReaderIface::SubsetList* subset_list,
                              const Tag* file_id_tag)
{
  int num_material_sets = 0;
  const int* material_set_list = 0;
  int zero = 0;
  if (subset_list) {
    if (subset_list->tag_list_length > 1 && 
        !strcmp(subset_list->tag_list[0].tag_name, MATERIAL_SET_TAG_NAME)) {
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "GMsh supports subset read only by material ID");
    }
    material_set_list = subset_list->tag_list[0].tag_values;
    num_material_sets = subset_list->tag_list[0].num_tag_values;
  }

  geomSets.clear();
  ErrorCode result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                             globalId, MB_TAG_DENSE | MB_TAG_CREAT, &zero);
  if (MB_SUCCESS != result)
    return result;

  // Create set for more convenient check for material set ids
  std::set<int> blocks;
  for (const int* mat_set_end = material_set_list + num_material_sets;
       material_set_list != mat_set_end; ++material_set_list)
    blocks.insert(*material_set_list);
  
  // Map of ID->handle for nodes
  std::map<long, EntityHandle> node_id_map;
  int data_size = 8;

  // Open file and hand off pointer to tokenizer
  FILE* file_ptr = fopen(filename, "r");
  if (!file_ptr) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, filename << ": " << strerror(errno));
  }
  FileTokenizer tokens(file_ptr, readMeshIface);

  // Determine file format version
  const char* const start_tokens[] = {"$NOD", "$MeshFormat", 0};
  int format_version = tokens.match_token(start_tokens);
  if (!format_version)
    return MB_FILE_DOES_NOT_EXIST;

  // If version 2.0, read additional header info
  if (2 == format_version) {
    double version;
    if (!tokens.get_doubles(1, &version))
      return MB_FILE_WRITE_ERROR;

    if (version != 2.0 && version != 2.1 && version != 2.2) {
      MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, filename << ": unknown format version: " << version);
      return MB_FILE_DOES_NOT_EXIST;
    }

    int file_format;
    if (!tokens.get_integers(1, &file_format) || !tokens.get_integers(1,
        &data_size) || !tokens.match_token("$EndMeshFormat"))
           return MB_FILE_WRITE_ERROR;
           // If physical entities in the gmsh file -> discard this
    const char* const phys_tokens[] = { "$Nodes", "$PhysicalNames", 0 };
    int hasPhys = tokens.match_token(phys_tokens);

    if (hasPhys == 2) {
      long num_phys;
      if (!tokens.get_long_ints(1, &num_phys))
        return MB_FILE_WRITE_ERROR;
      for (long loop_phys = 0; loop_phys < num_phys; loop_phys++) {
        long physDim;
        long physGroupNum;
        //char const * physName;
        if (!tokens.get_long_ints(1, &physDim))
          return MB_FILE_WRITE_ERROR;
        if (!tokens.get_long_ints(1, &physGroupNum))
          return MB_FILE_WRITE_ERROR;
        const char * ptc = tokens.get_string();
        if (!ptc)
          return MB_FILE_WRITE_ERROR;
        // try to get to the end of the line, without reporting errors
        // really, we need to skip this
        while(!tokens.get_newline(false))
          ptc = tokens.get_string();
      }
      if (!tokens.match_token("$EndPhysicalNames") || !tokens.match_token(
          "$Nodes"))
        return MB_FILE_WRITE_ERROR;
    }
  }

  // Read number of nodes
  long num_nodes;
  if (!tokens.get_long_ints(1, &num_nodes))
    return MB_FILE_WRITE_ERROR;

  // Allocate nodes
  std::vector<double*> coord_arrays;
  EntityHandle handle = 0;
  result = readMeshIface->get_node_coords(3, num_nodes, MB_START_ID,
                                          handle, coord_arrays);
  if (MB_SUCCESS != result)
    return result;

  // Read nodes
  double *x = coord_arrays[0],
         *y = coord_arrays[1],
         *z = coord_arrays[2];
  for (long i = 0; i < num_nodes; ++i, ++handle) {
    long id;
    if (!tokens.get_long_ints(1, &id) ||
        !tokens.get_doubles(1, x++) ||
        !tokens.get_doubles(1, y++) ||
        !tokens.get_doubles(1, z++))
      return MB_FILE_WRITE_ERROR;

    if (!node_id_map.insert(std::pair<long, EntityHandle>(id, handle)).second) {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Duplicate node ID at line " << tokens.line_number());
    }
  }

  // Create reverse map from handle to id
  std::vector<int> ids(num_nodes);
  std::vector<int>::iterator id_iter = ids.begin();
  std::vector<EntityHandle> handles(num_nodes);
  std::vector<EntityHandle>::iterator h_iter = handles.begin();
  for (std::map<long, EntityHandle>::iterator i = node_id_map.begin();
      i != node_id_map.end(); ++i, ++id_iter, ++h_iter) {
    *id_iter = i->first;
    * h_iter = i->second;
  }
  // Store IDs in tags
  result = mdbImpl->tag_set_data(globalId, &handles[0], num_nodes, &ids[0]);
  if (MB_SUCCESS != result)
    return result;
  if (file_id_tag) {
    result = mdbImpl->tag_set_data(*file_id_tag, &handles[0], num_nodes, &ids[0]);
    if (MB_SUCCESS != result) 
      return result;
  }
  ids.clear();
  handles.clear();

  // Get tokens signifying end of node data and start of elements
  if (!tokens.match_token(format_version == 1 ? "$ENDNOD" : "$EndNodes") ||
      !tokens.match_token(format_version == 1 ? "$ELM" : "$Elements"))
    return MB_FILE_WRITE_ERROR;

  // Get element count
  long num_elem;
  if (!tokens.get_long_ints(1, &num_elem))
    return MB_FILE_WRITE_ERROR;

  // Lists of data accumulated for elements
  std::vector<EntityHandle> connectivity;
  std::vector<int> mat_set_list, geom_set_list, part_set_list, id_list;
  // Temporary, per-element data
  std::vector<int> int_data(5), tag_data(2);
  std::vector<long> tmp_conn;
  int curr_elem_type = -1;
  for (long i = 0; i < num_elem; ++i) {
    // Read element description
    // File format 1.0
    if (1 == format_version) {
      if (!tokens.get_integers(5, &int_data[0]))
        return MB_FILE_WRITE_ERROR;
      tag_data[0] = int_data[2];
      tag_data[1] = int_data[3];
      if ((unsigned)tag_data[1] < GmshUtil::numGmshElemType &&
           GmshUtil::gmshElemTypes[tag_data[1]].num_nodes != (unsigned)int_data[4]) {
        MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid node count for element type at line " << tokens.line_number());
      }
    }
    // File format 2.0
    else {
      if (!tokens.get_integers(3, &int_data[0]))
        return MB_FILE_WRITE_ERROR;
      tag_data.resize(int_data[2]);
      if (!tokens.get_integers(tag_data.size(), &tag_data[0]))
        return MB_FILE_WRITE_ERROR;
    }

    // If a list of material sets was specified in the
    // argument list, skip any elements for which the
    // material set is not specified or is not in the
    // passed list.
    if (!blocks.empty() && (tag_data.empty() ||
        blocks.find(tag_data[0]) != blocks.end()))
      continue;

    // If the next element is not the same type as the last one,
    // create a sequence for the block of elements we've read
    // to this point (all of the same type), and clear accumulated
    // data.
    if (int_data[1] != curr_elem_type) {
      if (!id_list.empty()) { // First iteration
        result = create_elements(GmshUtil::gmshElemTypes[curr_elem_type],
                                 id_list,
                                 mat_set_list,
                                 geom_set_list,
                                 part_set_list,
                                 connectivity,
                                 file_id_tag);
        if (MB_SUCCESS != result)
          return result;
      }

      id_list.clear();
      mat_set_list.clear();
      geom_set_list.clear();
      part_set_list.clear();
      connectivity.clear();
      curr_elem_type = int_data[1];
      if ((unsigned)curr_elem_type >= GmshUtil::numGmshElemType ||
          GmshUtil::gmshElemTypes[curr_elem_type].mb_type == MBMAXTYPE) {
        MB_SET_ERR(MB_FILE_WRITE_ERROR, "Unsupported element type " << curr_elem_type << " at line " << tokens.line_number());
      }
      tmp_conn.resize(GmshUtil::gmshElemTypes[curr_elem_type].num_nodes);
    }

    // Store data from element description
    id_list.push_back(int_data[0]);
    if (tag_data.size() > 3)
      part_set_list.push_back(tag_data[3]); // it must be new format for gmsh, >= 2.5
                                            // it could have negative partition ids, for ghost elements
    else if (tag_data.size() > 2)
      part_set_list.push_back(tag_data[2]); // old format, partition id saved in 3rd tag field
    else
      part_set_list.push_back(0);
    geom_set_list.push_back(tag_data.size() > 1 ? tag_data[1] : 0);
     mat_set_list.push_back(tag_data.size() > 0 ? tag_data[0] : 0);

    // Get element connectivity
    if (!tokens.get_long_ints(tmp_conn.size(), &tmp_conn[0]))
      return MB_FILE_WRITE_ERROR;

    // Convert connectivity from IDs to handles
    for (unsigned j = 0; j < tmp_conn.size(); ++j) {
      std::map<long, EntityHandle>::iterator k = node_id_map.find(tmp_conn[j]);
      if (k == node_id_map.end()) {
        MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid node ID at line " << tokens.line_number());
      }
      connectivity.push_back(k->second);
    }
  } // for (num_nodes)

  // Create entity sequence for last element(s).
  if (!id_list.empty()) {
    result = create_elements(GmshUtil::gmshElemTypes[curr_elem_type],
                             id_list,
                             mat_set_list,
                             geom_set_list,
                             part_set_list,
                             connectivity,
                             file_id_tag);
    if (MB_SUCCESS != result)
      return result;
  }

  // Construct parent-child relations for geometric sets.
  // Note:  At the time this comment was written, the following
  //        function was not implemented.
  result = create_geometric_topology();
  geomSets.clear();
  return result;
}

//! Create an element sequence
ErrorCode ReadGmsh::create_elements(const GmshElemType& type,
                                    const std::vector<int>& elem_ids,
                                    const std::vector<int>& matl_ids,
                                    const std::vector<int>& geom_ids,
                                    const std::vector<int>& prtn_ids,
                                    const std::vector<EntityHandle>& connectivity,
                                    const Tag* file_id_tag)
{
  ErrorCode result;

  // Make sure input is consistent
  const unsigned long num_elem = elem_ids.size();
  const int node_per_elem = type.num_nodes;
  if (matl_ids.size() != num_elem ||
      geom_ids.size() != num_elem ||
      prtn_ids.size() != num_elem ||
      connectivity.size() != num_elem*node_per_elem)
    return MB_FAILURE;

  // Create the element sequence
  // do not do anything for point type
  if (type.mb_type==MBVERTEX)
    return MB_SUCCESS; // do not create anything
  EntityHandle handle = 0;
  EntityHandle* conn_array;
  result = readMeshIface->get_element_connect(num_elem, node_per_elem, type.mb_type,
                                              MB_START_ID,
                                              handle, conn_array);
  if (MB_SUCCESS != result)
    return result;

  // Copy passed element connectivity into entity sequence data.
  if (type.node_order) {
    for (unsigned long i = 0; i < num_elem; ++i)
      for (int j = 0; j < node_per_elem; ++j)
        conn_array[i*node_per_elem + type.node_order[j]] = connectivity[i*node_per_elem + j];
  }
  else {
    memcpy(conn_array, &connectivity[0], connectivity.size() * sizeof(EntityHandle));
  }

  // Notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(handle, num_elem, node_per_elem, conn_array);
  if (MB_SUCCESS != result)
    return result;

  // Store element IDs
  Range elements(handle, handle + num_elem - 1);
  result = mdbImpl->tag_set_data(globalId, elements, &elem_ids[0]);
  if (MB_SUCCESS != result)
    return result;
  if (file_id_tag) {
    result = mdbImpl->tag_set_data(*file_id_tag, elements, &elem_ids[0]);
    if (MB_SUCCESS != result) 
      return result;
  }

  // Add elements to material sets
  result = create_sets(type.mb_type, elements, matl_ids, 0);
  if (MB_SUCCESS != result)
    return result;
  // Add elements to geometric sets
  result = create_sets(type.mb_type, elements, geom_ids, 1);
  if (MB_SUCCESS != result)
    return result;
  // Add elements to parallel partitions
  result = create_sets(type.mb_type, elements, prtn_ids, 2);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

//! Add elements to sets as dictated by grouping ID in file.
ErrorCode ReadGmsh::create_sets(EntityType type,
                                const Range& elements,
                                const std::vector<int>& set_ids,
                                int set_type)
{
  ErrorCode result;

  // Get a unique list of set IDs
  std::set<int> ids;
  for (std::vector<int>::const_iterator i = set_ids.begin(); i != set_ids.end(); ++i)
    ids.insert(*i);

  // No Sets?
  if (ids.empty() || (ids.size() == 1 && *ids.begin() == 0))
    return MB_SUCCESS; // no sets (all ids are zero)

  // Get/create tag handles
  int num_tags;
  Tag tag_handles[2];
  int tag_val;
  const void* tag_values[2] = {&tag_val, NULL};

  switch (set_type) {
    default:
      return MB_FAILURE;
    case 0:
    case 2: {
      const char* name = set_type ? PARALLEL_PARTITION_TAG_NAME : MATERIAL_SET_TAG_NAME;
      result = mdbImpl->tag_get_handle(name, 1, MB_TYPE_INTEGER, tag_handles[0],
                                       MB_TAG_SPARSE | MB_TAG_CREAT);
      if (MB_SUCCESS != result)
        return result;
      num_tags = 1;
      break;
    }
    case 1: {
      result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
                                       tag_handles[1], MB_TAG_SPARSE | MB_TAG_CREAT);
      if (MB_SUCCESS != result)
        return result;
      tag_values[1] = NULL;
      tag_handles[0]= globalId;
      num_tags = 2;
      break;
    }
  } // switch

  // For each unique set ID...
  for (std::set<int>::iterator i = ids.begin(); i != ids.end(); ++i) {
    // Skip "null" set ID
    if (*i == 0)
      continue;

    // Get all entities with the current set ID
    Range entities, sets;
    std::vector<int>::const_iterator j = set_ids.begin();
    for (Range::iterator k = elements.begin(); k != elements.end(); ++j, ++k)
      if (*i == *j)
        entities.insert(*k);

    // Get set by ID
    // Cppcheck warning (false positive): variable tag_val is assigned a value that is never used
    tag_val = *i;
    result = mdbImpl->get_entities_by_type_and_tag(0, MBENTITYSET,
                                                   tag_handles, tag_values, num_tags,
                                                   sets);
    if (MB_SUCCESS != result && MB_ENTITY_NOT_FOUND != result)
      return result;

    // Don't use existing geometry sets (from some other file)
    if (1 == set_type) // Geometry
      sets = intersect(sets,  geomSets);

    // Get set handle
    EntityHandle set;
    // If no sets with ID, create one
    if (sets.empty()) {
      result = mdbImpl->create_meshset(MESHSET_SET, set);
      if (MB_SUCCESS != result)
        return result;

      result = mdbImpl->tag_set_data(tag_handles[0], &set, 1, &*i);
      if (MB_SUCCESS != result)
        return result;

      if (1 == set_type) { // Geometry
        int dim = CN::Dimension(type);
        result = mdbImpl->tag_set_data(tag_handles[1], &set, 1, &dim);
        if (MB_SUCCESS != result)
          return result;
        geomSets.insert(set);
      }
    }
    else {
      set = *sets.begin();
      if (1 == set_type) { // Geometry
        int dim = CN::Dimension(type);
        // Get dimension of set
        int dim2;
        result = mdbImpl->tag_get_data(tag_handles[1], &set, 1, &dim2);
        if (MB_SUCCESS != result) 
          return result;
        // If we're putting geometry of a higher dimension into the
        // set, increase the dimension of the set.
        if (dim > dim2) {
          result = mdbImpl->tag_set_data(tag_handles[1], &set, 1, &dim);
          if (MB_SUCCESS != result)
            return result;
        }
      }
    }

    // Put the mesh entities into the set
    result = mdbImpl->add_entities(set, entities);
    if (MB_SUCCESS != result)
      return result;
  } // for (ids)

  return MB_SUCCESS;
}

//! NOT IMPLEMENTED
//! Reconstruct parent-child relations for geometry sets from
//! mesh connectivity.
ErrorCode ReadGmsh::create_geometric_topology()
{
  if (geomSets.empty())
    return MB_SUCCESS;

  // Not implemented yet
  geomSets.clear();
  return MB_SUCCESS;
}

} // namespace moab
