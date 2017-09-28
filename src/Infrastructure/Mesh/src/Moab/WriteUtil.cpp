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
#pragma warning(disable : 4786)
#endif

#include "WriteUtil.hpp"
#include "moab/Core.hpp"
#include "moab/Error.hpp"
#include "SequenceManager.hpp"
#include "ElementSequence.hpp"
#include "VertexSequence.hpp"
#include "AEntityFactory.hpp"
#include "MBTagConventions.hpp"
#include "RangeSeqIntersectIter.hpp"
#include "MeshSetSequence.hpp"

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>
#include <iostream>

#ifdef WIN32
  #define stat _stat
#else
  #include <unistd.h>
#endif

namespace moab {

WriteUtil::WriteUtil(Core* mdb, Error* error_handler)
  : WriteUtilIface(), mMB(mdb), mError(error_handler)
{
}

//! Check if the specified file already exists.
//! Returns MB_SUCCESS if file does not exist, MB_ALREADY_ALLOCATED
//! if file does exist, or MB_FAILURE for some other error condition.
ErrorCode WriteUtil::check_doesnt_exist(const char* file_name)
{
  struct stat s;
  if (0 == stat(file_name, &s)) {
    MB_SET_ERR(MB_ALREADY_ALLOCATED, file_name << ": file already exists");
  }
  else if (errno == ENOENT)
    return MB_SUCCESS;
  else
    return MB_FAILURE;
}

//! Gather all entities in the mesh, or in the sets specified
ErrorCode WriteUtil::gather_entities(Range &all_ents,
                                       /**< range in which entities are returned */
                                     const EntityHandle *ent_sets,
                                       /**< entity sets whose contents are to be gathered */
                                     const int num_sets
                                       /**< number of sets in list */
                                     )
{
  ErrorCode rval = MB_SUCCESS;
  if (!ent_sets || num_sets == 0) {
    rval = mMB->get_entities_by_handle(0, all_ents);
  }
  else {
    for (int i = 0; i < num_sets; i++) {
      ErrorCode tmp_rval = mMB->get_entities_by_handle(ent_sets[i], all_ents);
      if (MB_SUCCESS != tmp_rval)
        rval = tmp_rval;
    }
  }

  return rval;
}

ErrorCode WriteUtil::get_node_coords(const int num_arrays,
                                     const int num_nodes,
                                     const Range& entities,
                                     Tag node_id_tag,
                                     const int start_node_id,
                                     std::vector<double*>& arrays)
{
  // Check the data coming into the function
  // Dimension should be proper
  if (num_arrays < 1 || num_arrays > 3)
    return MB_FAILURE;

  // There should be some entities
  //if (entities.empty())
  //  return MB_FAILURE;
  // The above necessitates annoying special cases for files 
  // w/out vertices (e.g. a kD-tree).  Return NULL array
  // pointers instead. - kraftcheck, 3-14-08
  if (entities.empty()) {
    arrays.clear();
    arrays.resize(num_arrays, NULL);
    return MB_SUCCESS;
  }

  // Memory should already be allocated for us
  int tmp_num_arrays = 0;
  for (unsigned int i = 0; i < 3; i++)
    if (i + 1 <= arrays.size() && NULL != arrays[i])
      tmp_num_arrays++;
  if (0 == tmp_num_arrays)
    return MB_FAILURE;

  // Get coordinate data
  ErrorCode result = mMB->get_coords(entities,
                   num_arrays < 1 || arrays.size() < 1 ? NULL : arrays[0],
                   num_arrays < 2 || arrays.size() < 2 ? NULL : arrays[1],
                   num_arrays < 3 || arrays.size() < 3 ? NULL : arrays[2]);

  if (0 == node_id_tag || MB_SUCCESS != result)
    return result;

  // Now assign tags
  std::vector<int> ids(num_nodes);
  int node_id = start_node_id;
  for (int i = 0; i < num_nodes; i++)
    ids[i] = node_id++;
  result = mMB->tag_set_data(node_id_tag, entities, &ids[0]);

  return result;
}

ErrorCode WriteUtil::get_node_coords(const int which_array, /* 0->X, 1->Y, 2->Z, -1->all */
                                     Range::const_iterator iter,
                                     const Range::const_iterator& end,
                                     const size_t output_array_len,
                                     double* const output_array)
{
  // Check the data coming into the function
  // Dimension should be proper
  if (which_array < -1 || which_array > 2)
    return MB_FAILURE;

  // There should be some entities
  if (iter == end)
    return MB_FAILURE;

  // Memory should already be allocated for us
  if (NULL == output_array || 0 == output_array_len)
    return MB_FAILURE;

  // Sequence iterators
  TypeSequenceManager::iterator seq_iter, seq_end;
  seq_iter = mMB->sequence_manager()->entity_map(MBVERTEX).begin();
  seq_end = mMB->sequence_manager()->entity_map(MBVERTEX).end();

  // Loop over range, getting coordinate value
  double* output_iter = output_array;
  double* const output_end = output_array + output_array_len;
  while (iter != end) {
    // Find the sequence containing the current handle
    while (seq_iter != seq_end && (*seq_iter)->end_handle() < *iter)
      ++seq_iter;
    if (seq_iter == seq_end || *iter < (*seq_iter)->start_handle())
      return MB_FAILURE;

    // Determine how much of the sequence we want.
    Range::pair_iterator pair(iter);
    Range::const_iterator prev(end);
    --prev;
    EntityHandle range_end = pair->second;
    EntityHandle sequence_end = (*seq_iter)->end_handle();
    EntityHandle end_handle = range_end > sequence_end ? sequence_end : range_end;
    if (end_handle > *prev)
      end_handle = *prev;
    EntityHandle count = end_handle - *iter + 1;

    // Get offset in sequence to start at
    assert(*iter >= (*seq_iter)->start_handle());
    EntityHandle offset = *iter - (*seq_iter)->start_handle();

    // Get coordinate arrays from sequence
    double* coord_array[3];
    static_cast<VertexSequence*>(*seq_iter)
      ->get_coordinate_arrays(coord_array[0], coord_array[1], coord_array[2]);

    // Copy data to output buffer
    if (-1 != which_array) {
      if (output_iter + count > output_end)
        return MB_FAILURE;
      memcpy(output_iter, coord_array[which_array] + offset, count * sizeof(double));
      output_iter += count;
    }
    else {
      if (output_iter + 3*count > output_end)
        return MB_FAILURE;
      for (unsigned int i = 0; i < count; i++) {
        *output_iter = coord_array[0][i + offset]; output_iter++;
        *output_iter = coord_array[1][i + offset]; output_iter++;
        *output_iter = coord_array[2][i + offset]; output_iter++;
      }
    }

    // Iterate
    iter += count;
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_element_connect(const int num_elements,
                                         const int verts_per_element,
                                         Tag node_id_tag,
                                         const Range& elements,
                                         Tag element_id_tag,
                                         int start_element_id,
                                         int* element_array,
                                         bool add_sizes)
{
  // Check the data we got
  if (num_elements < 1)
    return MB_FAILURE;
  if (verts_per_element < 1)
    return MB_FAILURE;
  if (elements.empty())
    return MB_FAILURE;
  if (!element_array)
    return MB_FAILURE;

  Range::const_iterator range_iter = elements.begin();
  Range::const_iterator range_iter_end = elements.end();

  TypeSequenceManager::iterator seq_iter, seq_iter_end;
  EntityType current_type = TYPE_FROM_HANDLE(*range_iter);

  seq_iter = mMB->sequence_manager()->entity_map(current_type).begin();
  seq_iter_end = mMB->sequence_manager()->entity_map(current_type).end();

  // Let's find the entity sequence which holds the first entity
  TypeSequenceManager::iterator seq_iter_lookahead = seq_iter;
  ++seq_iter_lookahead;
  for ( ; seq_iter_lookahead != seq_iter_end &&
       (*seq_iter_lookahead)->start_handle() < *range_iter; ) {
    ++seq_iter;
    ++seq_iter_lookahead;
  }

  // A look ahead iterator
  Range::const_iterator range_iter_lookahead = range_iter;

  // Our main loop
  for ( ; range_iter != range_iter_end && seq_iter != seq_iter_end; /* ++ is handled in loop*/ ) {
    // Find a range that fits in the current entity sequence
    for ( ; range_iter_lookahead != range_iter_end &&
         *range_iter_lookahead <= (*seq_iter)->end_handle();
         ++range_iter_lookahead) {
    }

    if (current_type != TYPE_FROM_HANDLE(*range_iter)) {
      current_type = TYPE_FROM_HANDLE(*range_iter);
      seq_iter = mMB->sequence_manager()->entity_map(current_type).begin();
      seq_iter_end = mMB->sequence_manager()->entity_map(current_type).end();

      // Let's find the entity sequence which holds the first entity of this type
      TypeSequenceManager::const_iterator seq_iter_lookahead2 = seq_iter;
      ++seq_iter_lookahead2;
      for ( ; seq_iter_lookahead2 != seq_iter_end &&
           (*seq_iter_lookahead2)->start_handle() < *range_iter; ) {
        ++seq_iter;
        ++seq_iter_lookahead2;
      }
    }

    int i = static_cast<ElementSequence*>(*seq_iter)->nodes_per_element();

    // Get the connectivity array
    EntityHandle* conn_array =
      static_cast<ElementSequence*>(*seq_iter)->get_connectivity_array();

    EntityHandle start_handle = (*seq_iter)->start_handle();

    for (Range::const_iterator tmp_iter = range_iter;
         tmp_iter != range_iter_lookahead;
         ++tmp_iter) {
      // Set the element id tag
      mMB->tag_set_data(element_id_tag, &*tmp_iter, 1, &start_element_id);
      ++start_element_id;

      if (add_sizes)
        *element_array++ = i;

      // For each node
      for (int j = 0; j < i; j++) {
        EntityHandle node = *(conn_array + j + i*(*tmp_iter - start_handle));
        mMB->tag_get_data(node_id_tag, &node, 1, element_array);
        element_array++;
      }
    }

    // Go to the next entity sequence
    ++seq_iter;
    // Start with the next entities
    range_iter = range_iter_lookahead;
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_element_connect(Range::const_iterator iter,
                                         const Range::const_iterator& end,
                                         const int vertices_per_elem,
                                         Tag node_id_tag,
                                         const size_t elem_array_size,
                                         int *const element_array,
                                         bool add_sizes)
{
  // Check the data we got
  if (iter == end)
    return MB_FAILURE;
  if (vertices_per_elem < 1)
    return MB_FAILURE;
  if (!element_array || elem_array_size < (unsigned)vertices_per_elem)
    return MB_FAILURE;

  // Sequence iterators
  TypeSequenceManager::const_iterator seq_iter, seq_end;
  
  // loop over range, getting coordinate value
  EntityType current_type = MBMAXTYPE;
  int* output_iter = element_array;
  int*const output_end = element_array + elem_array_size;
  while (iter != end) {
    // Make sure we have the right sequence list (and get the sequence
    // list for the first iteration.)
    EntityType type = TYPE_FROM_HANDLE(*iter);
    if (type != current_type) {
      if (type >= MBENTITYSET || type < MBEDGE)
        return MB_FAILURE;
      seq_iter = mMB->sequence_manager()->entity_map(type).begin();
      seq_end  = mMB->sequence_manager()->entity_map(type).end();
      current_type = type;
    }

    // Find the sequence containing the current handle
    while (seq_iter != seq_end && (*seq_iter)->end_handle() < *iter)
      ++seq_iter;
    if (seq_iter == seq_end || *iter < (*seq_iter)->start_handle())
      return MB_FAILURE;

    // Get the connectivity array
    EntityHandle* conn_array = NULL;
    int conn_size = static_cast<ElementSequence*>(*seq_iter)->nodes_per_element();
    conn_array = static_cast<ElementSequence*>(*seq_iter)->get_connectivity_array();

    // Determine how much of the sequence we want.
    Range::pair_iterator pair(iter);
    Range::const_iterator prev(end);
    --prev;
    EntityHandle range_end = pair->second;
    EntityHandle sequence_end = (*seq_iter)->end_handle();
    EntityHandle end_handle = range_end > sequence_end ? sequence_end : range_end;
    if (end_handle > *prev)
      end_handle = *prev;
    EntityHandle count = end_handle - *iter + 1;

    // Get offset in sequence to start at
    assert(*iter >= (*seq_iter)->start_handle());
    EntityHandle offset = *iter - (*seq_iter)->start_handle();

    // Make sure sufficient space in output array
    if ((!add_sizes && output_iter + (count * conn_size) > output_end) ||
        (add_sizes && output_iter + (count * (conn_size + 1)) > output_end))
      return MB_FAILURE;

    // If the nodes per element match, do in one call
    conn_array += (conn_size * offset);
    if (vertices_per_elem == conn_size && !add_sizes) {
      ErrorCode rval = mMB->tag_get_data(node_id_tag,
                                         conn_array,
                                         count * conn_size,
                                         output_iter);
      if (MB_SUCCESS != rval)
        return rval;

      output_iter += count * conn_size;
    }
    // Otherwise need to do one at a time
    else {
      int min = vertices_per_elem > conn_size ? conn_size : vertices_per_elem;
      for (EntityHandle i = 0; i < count; ++i) {
        *output_iter++ = min;
        ErrorCode rval = mMB->tag_get_data(node_id_tag,
                                           conn_array,
                                           min,
                                           output_iter);
        if (MB_SUCCESS != rval)
          return rval;

        output_iter += min;
        conn_array += conn_size;

        if (vertices_per_elem > conn_size) { // Need to pad
          memset(output_iter, 0, sizeof(int) * (vertices_per_elem - conn_size));
          output_iter += (vertices_per_elem - conn_size);
        }
      }
    }

    iter += count;
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_element_connect(Range::const_iterator iter,
                                         const Range::const_iterator& end,
                                         const int vertices_per_elem,
                                         const size_t elem_array_size,
                                         EntityHandle *const element_array)
{
  // Check the data we got
  if (iter == end)
    return MB_FAILURE;
  if (vertices_per_elem < 1)
    return MB_FAILURE;
  if (!element_array || elem_array_size < (unsigned)vertices_per_elem)
    return MB_FAILURE;

  // Sequence iterators
  TypeSequenceManager::const_iterator seq_iter, seq_end;

  // Loop over range, getting coordinate value
  EntityType current_type = MBMAXTYPE;
  EntityHandle* output_iter = element_array;
  EntityHandle*const output_end = element_array + elem_array_size;
  while (iter != end) {
    // Make sure we have the right sequence list (and get the sequence
    // list for the first iteration.)
    EntityType type = TYPE_FROM_HANDLE(*iter);
    if (type != current_type) {
      if (type >= MBENTITYSET || type < MBEDGE)
        return MB_FAILURE;
      seq_iter = mMB->sequence_manager()->entity_map(type).begin();
      seq_end  = mMB->sequence_manager()->entity_map(type).end();
      current_type = type;
    }

    // Find the sequence containing the current handle
    while (seq_iter != seq_end && (*seq_iter)->end_handle() < *iter)
      ++seq_iter;
    if (seq_iter == seq_end || *iter < (*seq_iter)->start_handle())
      return MB_FAILURE;

    // Get the connectivity array
    EntityHandle* conn_array = NULL;
    int conn_size = static_cast<ElementSequence*>(*seq_iter)->nodes_per_element();
    if (conn_size != vertices_per_elem)
      return MB_FAILURE;
    conn_array = static_cast<ElementSequence*>(*seq_iter)->get_connectivity_array();

    // Determine how much of the sequence we want.
    Range::pair_iterator pair(iter);
    Range::const_iterator prev(end);
    --prev;
    EntityHandle range_end = pair->second;
    EntityHandle sequence_end = (*seq_iter)->end_handle();
    EntityHandle end_handle = range_end > sequence_end ? sequence_end : range_end;
    if (end_handle > *prev)
      end_handle = *prev;
    EntityHandle count = end_handle - *iter + 1;

    // Get offset in sequence to start at
    assert(*iter >= (*seq_iter)->start_handle());
    EntityHandle offset = *iter - (*seq_iter)->start_handle();

    // Make sure sufficient space in output array
    if (output_iter + (count * conn_size) > output_end)
      return MB_FAILURE;

    if (conn_array == NULL) { // If it is structured mesh
      ErrorCode rval;
      int temp_buff_size = conn_size * sizeof(EntityHandle);
      for (unsigned i = 0; i < count; i++) { // Copy connectivity element by element
        std::vector<EntityHandle> connect;
        rval = static_cast<ElementSequence*>(*seq_iter)->get_connectivity(*iter, connect);
        if (MB_SUCCESS != rval) {
          return rval;
        }
        memcpy(output_iter, &connect[0], temp_buff_size);
        output_iter += conn_size;
        ++iter;
      }
    }
    else {
      // Copy connectivity into output array
      conn_array += (conn_size * offset);
      memcpy(output_iter, conn_array, count * conn_size * sizeof(EntityHandle));
      output_iter += count * conn_size;
      iter += count;
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_poly_connect_size(Range::const_iterator /* begin */,
                                           const Range::const_iterator& /* end */ ,
                                           int& /* connectivity_size */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode WriteUtil::get_poly_connect(Range::const_iterator& /* iter */,
                                      const Range::const_iterator& /* end */ ,
                                      const Tag /* node_id_tag */ ,
                                      size_t& /* handle_array_len */,
                                      int *const /* handle_array */,
                                      size_t& /* index_array_len */,
                                      int*const /* index_array */,
                                      int& /* index_offset */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode WriteUtil::gather_nodes_from_elements(const Range& elements,
                                                const Tag node_bit_mark_tag,
                                                Range& nodes)
{
  bool printed_warning = false;

  if (elements.empty())
    return MB_SUCCESS;

  if (TYPE_FROM_HANDLE(elements.front()) <= MBVERTEX ||
      TYPE_FROM_HANDLE(elements.back()) >= MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  // See if we need to use our own marking tag
  Tag exporting_nodes_tag = 0;
  if (node_bit_mark_tag)
    exporting_nodes_tag = node_bit_mark_tag;
  else {
    mMB->tag_get_handle("__MBWriteUtil::exporting_nodes", 1, MB_TYPE_BIT, 
                     exporting_nodes_tag, MB_TAG_CREAT);
  }

  // The x,y,z tag handles we need
  EntityHandle lower_bound = ~0, upper_bound = 0;

  std::vector<EntityHandle> tmp_conn;

  RangeSeqIntersectIter iter(mMB->sequence_manager());
  for (ErrorCode rval = iter.init(elements.begin(), elements.end());
       MB_FAILURE != rval; rval = iter.step()) {
    if (MB_ENTITY_NOT_FOUND == rval) {
      if (!printed_warning) {
        std::cerr << "Warning: ignoring invalid element handle(s) in gather_nodes_from_elements" << std::endl;
        printed_warning = true;
      }
      continue;
    }

    ElementSequence* seq = static_cast<ElementSequence*>(iter.get_sequence());

    // Get the connectivity array
    const EntityHandle* conn_array = seq->get_connectivity_array();

    // If unstructured mesh
    if (conn_array && mMB->type_from_handle(iter.get_start_handle()) != MBPOLYHEDRON) {
      assert(iter.get_start_handle() >= seq->start_handle());
      assert(iter.get_end_handle() <= seq->end_handle());
      const EntityHandle offset = iter.get_start_handle() - seq->start_handle();
      const EntityHandle num_elem = iter.get_end_handle() - iter.get_start_handle() + 1;

      conn_array += offset * seq->nodes_per_element();
      const EntityHandle num_node = num_elem * seq->nodes_per_element();

      // For each node
      for (EntityHandle j = 0; j < num_node; j++) {
        EntityHandle node = conn_array[j];
        if (node < lower_bound)
          lower_bound = node;
        if (node > upper_bound)
          upper_bound = node;
        unsigned char bit = 0x1;
        rval = mMB->tag_set_data(exporting_nodes_tag, &node, 1, &bit);
        assert(MB_SUCCESS == rval);
        if (MB_SUCCESS != rval)
          return rval;
      }
    }
    // Polyhedra
    else if (conn_array && mMB->type_from_handle(iter.get_start_handle()) == MBPOLYHEDRON) {
      assert(iter.get_start_handle() >= seq->start_handle());
      assert(iter.get_end_handle() <= seq->end_handle());
      const EntityHandle offset = iter.get_start_handle() - seq->start_handle();
      const EntityHandle num_elem = iter.get_end_handle() - iter.get_start_handle() + 1;

      conn_array += offset * seq->nodes_per_element();
      int num_face = num_elem * seq->nodes_per_element();

      // For each node
      for (int j = 0; j < num_face; j++) {
        const EntityHandle *face_conn = NULL;
        int face_num_conn = 0;
        rval = mMB->get_connectivity(conn_array[j], face_conn, face_num_conn, false);
        if (MB_SUCCESS != rval)
          return rval;
        for (int k = 0; k < face_num_conn; k++) {
          EntityHandle node = face_conn[k];
          if (node < lower_bound)
            lower_bound = node;
          if (node > upper_bound)
            upper_bound = node;
          unsigned char bit = 0x1;
          rval = mMB->tag_set_data(exporting_nodes_tag, &node, 1, &bit);
          assert(MB_SUCCESS == rval);
          if (MB_SUCCESS != rval)
            return rval;
        }
      }
    }
    // Structured mesh
    else {
      EntityHandle end_h = iter.get_end_handle() + 1;
      for (EntityHandle h = iter.get_start_handle();
           h < end_h; ++h) {
        tmp_conn.clear();
        rval = seq->get_connectivity(h, tmp_conn, false);
        if (MB_SUCCESS != rval) {
          if (node_bit_mark_tag == 0)
            mMB->tag_delete(exporting_nodes_tag);
          return rval;
        }

        // For each node
        for (size_t j = 0; j < tmp_conn.size(); j++) {
          EntityHandle node = tmp_conn[j];
          if (node < lower_bound)
            lower_bound = node;
          if (node > upper_bound)
            upper_bound = node;
          unsigned char bit = 0x1;
          mMB->tag_set_data(exporting_nodes_tag, &node, 1, &bit);
        }
      }
    }
  }

  // We can get a REALLY long loop if lower_bound is zero
  assert(lower_bound != 0);
  // Gather up all the nodes
  for ( ; upper_bound >= lower_bound; --upper_bound) {
    unsigned char node_marked=0;
    mMB->tag_get_data(exporting_nodes_tag, &upper_bound, 1, &node_marked);
    if (node_marked == 0x1)
      nodes.insert(upper_bound);
  }

  // Clean up our own marking tag
  if (node_bit_mark_tag == 0)
    mMB->tag_delete(exporting_nodes_tag);

  return MB_SUCCESS;
}

//! Assign ids to input elements starting with start_id, written to id_tag
//! if zero, assigns to GLOBAL_ID_TAG_NAME
ErrorCode WriteUtil::assign_ids(Range &elements,
                                Tag id_tag,
                                const int start_id)
{
  ErrorCode result;
  if (0 == id_tag) {
    // Get the global id tag
    int def_val = 0;
    result = mMB->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, id_tag, MB_TAG_DENSE|MB_TAG_CREAT, &def_val);
    if (MB_SUCCESS != result) return result;
  }

  // Now assign the ids
  int i;
  Range::iterator rit;
  ErrorCode tmp_result;
  result = MB_SUCCESS;
  for (i = start_id, rit = elements.begin(); rit != elements.end(); ++rit, i++) {
    tmp_result = mMB->tag_set_data(id_tag, &(*rit), 1, &i);
    if (MB_SUCCESS != tmp_result)
      result = tmp_result;
  }

  return result;
}

ErrorCode WriteUtil::get_adjacencies(EntityHandle entity,
                                     Tag id_tag,
                                     std::vector<int>& adj)
{
  ErrorCode rval;
  const EntityHandle* adj_array;
  int num_adj, id;

  // Get handles of adjacent entities
  rval = mMB->a_entity_factory()->get_adjacencies(entity, adj_array, num_adj);
  if (MB_SUCCESS != rval) {
    adj.clear();
    return rval;
  }

  // Append IDs of adjacent entities -- skip meshsets
  adj.resize(num_adj); // Pre-allocate space
  adj.clear(); // Clear used space

  const EntityHandle* const end = adj_array + num_adj;
  for (const EntityHandle* iter = adj_array; iter != end; ++iter) {
    if (TYPE_FROM_HANDLE(*iter) != MBENTITYSET) {
      rval = mMB->tag_get_data(id_tag, iter, 1, &id);
      if (MB_SUCCESS != rval)
        return rval;
      adj.push_back(id);
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_adjacencies(EntityHandle entity,
                                     const EntityHandle*& adj_array,
                                     int& num_adj)
{
  return mMB->a_entity_factory()->get_adjacencies(entity, adj_array, num_adj);
}

ErrorCode WriteUtil::get_tag_list(std::vector<Tag>& result_list,
                                  const Tag* user_tag_list,
                                  int user_tag_list_length,
                                  bool include_variable_length_tags)
{
  ErrorCode rval;

  if (user_tag_list) {
    result_list.clear();
    result_list.reserve(user_tag_list_length);
    for (int i = 0; i < user_tag_list_length; ++i) {
      std::string name;
      rval = mMB->tag_get_name(user_tag_list[i], name);MB_CHK_SET_ERR(rval, "Error " << (int)rval << " getting name for tag (Invalid input tag handle?)");

      if (name.empty()) {
        MB_SET_ERR(MB_TAG_NOT_FOUND, "Explicit request to save anonymous tag");
      }

      int size;
      if (!include_variable_length_tags &&
          MB_VARIABLE_DATA_LENGTH == mMB->tag_get_length(user_tag_list[i], size)) {
        MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "File format cannot store variable-length tag: \"" << name << "\"");
      }

      result_list.push_back(user_tag_list[i]);
    }
  }
  else {
    std::vector<Tag> temp_list;
    rval = mMB->tag_get_tags(temp_list);MB_CHK_SET_ERR(rval, "Interface::tag_get_tags failed");

    result_list.clear();
    result_list.reserve(temp_list.size());

    std::vector<Tag>::iterator i;
    for (i = temp_list.begin(); i != temp_list.end(); ++i) {
      std::string name;
      rval = mMB->tag_get_name(*i, name);MB_CHK_SET_ERR(rval, "Error " << (int)rval << " getting name for tag (Stale tag handle?)");

      // Skip anonymous tags
      if (name.empty())
        continue;

      // Skip private/internal tags
      if (name.size() >= 2 && name[0] == '_' && name[1] == '_')
        continue;

      // If requested, skip variable-length tags
      int size;
      if (!include_variable_length_tags &&
          MB_VARIABLE_DATA_LENGTH == mMB->tag_get_length(*i, size))
        continue;

       result_list.push_back(*i);
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteUtil::get_entity_list_pointers(Range::const_iterator begin,
                                              Range::const_iterator end,
                                              EntityHandle const* * pointers,
                                              EntityListType relation,
                                              int* lengths,
                                              unsigned char* flags)
{
  RangeSeqIntersectIter iter(mMB->sequence_manager());
  ErrorCode rval = iter.init(begin, end);
  while (MB_SUCCESS == rval) {
    EntityType type = TYPE_FROM_HANDLE(iter.get_start_handle());

    if (MBENTITYSET == type) {
      const MeshSetSequence* seq = reinterpret_cast<MeshSetSequence*>(iter.get_sequence());
      const MeshSet* set;
      int len = 0; size_t clen;
      for (EntityHandle h = iter.get_start_handle(); h <= iter.get_end_handle(); ++h) {
        set = seq->get_set(h);
        switch (relation) {
          case CONTENTS: *pointers = set->get_contents(clen); len = clen; break;
          case CHILDREN: *pointers = set->get_children(len); break;
          case PARENTS:  *pointers = set->get_parents(len); break;
        }
        if (lengths) {
          *lengths = len;
          ++lengths;
        }
        if (flags) {
          *flags = (unsigned char)set->flags();
          ++flags;
        }
        ++pointers;
      }
    }

    else if (MBVERTEX != type) {
      const bool topological = (relation == TOPOLOGICAL);
      int len;
      const ElementSequence* seq = reinterpret_cast<ElementSequence*>(iter.get_sequence());
      for (EntityHandle h = iter.get_start_handle(); h <= iter.get_end_handle(); ++h) {
        rval = seq->get_connectivity(h, *pointers, len, topological);
        if (MB_SUCCESS != rval)
          return rval;
        if (lengths) {
          *lengths = len;
          ++lengths;
        }
        if (flags) {
          *flags = 0;
          ++flags;
        }
        ++pointers;
      }
    }
    else {
      return MB_TYPE_OUT_OF_RANGE;
    }

    rval = iter.step();
  }
  if (MB_FAILURE == rval)
    return MB_SUCCESS; // At end of list
  else
    return rval;
}

ErrorCode WriteUtil::get_entity_list_pointers(EntityHandle const* entities,
                                              int num_entities,
                                              EntityHandle const* * pointers,
                                              EntityListType relation,
                                              int* lengths,
                                              unsigned char* flags)
{
  SequenceManager *sm = mMB->sequence_manager();
  const EntitySequence *tmp_seq;
  ErrorCode rval = MB_SUCCESS;
  for (int i = 0; i < num_entities; i++) {
    rval = sm->find(entities[i], tmp_seq);
    if (MB_SUCCESS != rval)
      return rval;

    EntityType type = TYPE_FROM_HANDLE(entities[i]);

    if (MBENTITYSET == type) {
      const MeshSetSequence* seq = reinterpret_cast<const MeshSetSequence*>(tmp_seq);
      const MeshSet* set;
      int len = 0; size_t clen;
      set = seq->get_set(entities[i]);
      switch (relation) {
        case CONTENTS: *pointers = set->get_contents(clen); len = clen; break;
        case CHILDREN: *pointers = set->get_children(len); break;
        case PARENTS:  *pointers = set->get_parents(len); break;
      }
      if (lengths) {
        *lengths = len;
        ++lengths;
      }
      if (flags) {
        *flags = (unsigned char)set->flags();
        ++flags;
      }
      ++pointers;
    }
    else if (MBVERTEX != type) {
      const bool topological = (relation == TOPOLOGICAL);
      int len;
      const ElementSequence* seq = reinterpret_cast<const ElementSequence*>(tmp_seq);
      rval = seq->get_connectivity(entities[i], *pointers, len, topological);
      if (MB_SUCCESS != rval)
        return rval;
      if (lengths) {
        *lengths = len;
        ++lengths;
      }
      if (flags) {
        *flags = 0;
        ++flags;
      }
      ++pointers;
    }
    else {
      return MB_TYPE_OUT_OF_RANGE;
    }
  }

  return MB_SUCCESS;
}

} // namespace moab
