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

#include "ReadUtil.hpp"
#include "moab/Core.hpp"
#include "AEntityFactory.hpp"
#include "moab/Error.hpp"
#include "SequenceManager.hpp"
#include "VertexSequence.hpp"
#include "ElementSequence.hpp"

namespace moab {

#define RR if (MB_SUCCESS != result) return result

ReadUtil::ReadUtil(Core* mdb, Error* /*error_handler*/)
  : ReadUtilIface(), mMB(mdb)
{
}

ErrorCode ReadUtil::get_node_coords(const int /*num_arrays*/,
                                    const int num_nodes,
                                    const int preferred_start_id,
                                    EntityHandle& actual_start_handle,
                                    std::vector<double*>& arrays,
                                    int sequence_size)
{
  ErrorCode error;
  EntitySequence* seq = 0;

  if (num_nodes < 1) {
    actual_start_handle = 0;
    arrays.clear();
    return MB_INDEX_OUT_OF_RANGE;
  }

  // Create an entity sequence for these nodes
  error = mMB->sequence_manager()->create_entity_sequence(
    MBVERTEX, num_nodes, 0, preferred_start_id,
    actual_start_handle,
    seq, sequence_size);

  if (error != MB_SUCCESS)
    return error;

  if (seq->start_handle() > actual_start_handle ||
      seq->end_handle() < actual_start_handle ||
      seq->end_handle() - actual_start_handle + 1 < (unsigned)num_nodes)
    return MB_FAILURE;

  arrays.resize(3);

  error = static_cast<VertexSequence*>(seq)->get_coordinate_arrays(arrays[0], arrays[1], arrays[2]);
  for (unsigned i = 0; i< arrays.size(); ++i)
    if (arrays[i])
      arrays[i] += (actual_start_handle - seq->start_handle());

  return error;
}

ErrorCode ReadUtil::get_element_connect(const int num_elements,
                                        const int verts_per_element,
                                        const EntityType mdb_type,
                                        const int preferred_start_id,
                                        EntityHandle& actual_start_handle,
                                        EntityHandle*& array,
                                        int sequence_size)
{
  ErrorCode error;
  EntitySequence* seq;

  if (num_elements < 1) {
    actual_start_handle = 0;
    array = 0;
    return MB_INDEX_OUT_OF_RANGE;
  }

  //if (mdb_type <= MBVERTEX || mdb_type >= MBPOLYHEDRON || mdb_type == MBPOLYGON)
    //return MB_TYPE_OUT_OF_RANGE;

  // Make an entity sequence to hold these elements
  error = mMB->sequence_manager()->create_entity_sequence(
      mdb_type, num_elements, verts_per_element, preferred_start_id, 
      actual_start_handle, seq, sequence_size);
  if (MB_SUCCESS != error)
    return error;

  if (seq->start_handle() > actual_start_handle ||
      seq->end_handle() < actual_start_handle ||
      seq->end_handle() - actual_start_handle + 1 < (unsigned)num_elements)
    return MB_FAILURE;

  // Get an array for the connectivity
  array = static_cast<ElementSequence*>(seq)->get_connectivity_array();
  if (!array)
    return MB_FAILURE;
  array += (actual_start_handle - seq->start_handle()) 
         * static_cast<ElementSequence*>(seq)->nodes_per_element();

  return error;
}

ErrorCode ReadUtil::create_entity_sets(EntityID num_sets,
                                       const unsigned* flags,
                                       EntityID start_id,
                                       EntityHandle& start_handle)
{
  if (num_sets < 1) {
    start_handle = 0;
    return MB_INDEX_OUT_OF_RANGE;
  }

  ErrorCode error;
  EntitySequence* seq;
  error = mMB->sequence_manager()->create_meshset_sequence(num_sets,
                                                           start_id,
                                                           flags,
                                                           start_handle,
                                                           seq);
  if (MB_SUCCESS != error)
    return error;

  if (seq->start_handle() > start_handle ||
      seq->end_handle() < start_handle ||
      seq->end_handle() - start_handle + 1 < (EntityHandle)num_sets)
    return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode ReadUtil::update_adjacencies(const EntityHandle start_handle,
                                       const int number_elements,
                                       const int number_vertices_per_element,
                                       const EntityHandle* conn_array)
{
  EntityHandle tmp_hndl = start_handle;
  AEntityFactory* adj_fact = mMB->a_entity_factory();

  // Iterate over the elements and update adjacency information
  if (adj_fact != NULL && adj_fact->vert_elem_adjacencies()) {
    int j = 0;
    for (int i = 0; i < number_elements; i++) {
      adj_fact->notify_create_entity(tmp_hndl, (conn_array + j), number_vertices_per_element);
      tmp_hndl++;
      j += number_vertices_per_element;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadUtil::gather_related_ents(Range &partition,
                                        Range &related_ents,
                                        EntityHandle *file_set)
{
  // Loop over any sets, getting contained ents
  std::pair<Range::const_iterator, Range::const_iterator> pair_it =
    partition.equal_range(MBENTITYSET);

  ErrorCode result = MB_SUCCESS;
  for (Range::const_iterator rit = pair_it.first;
       rit != pair_it.second; ++rit) {
    ErrorCode tmp_result = 
      mMB->get_entities_by_handle(*rit, related_ents,
                                  Interface::UNION);
    if (MB_SUCCESS != tmp_result) result = tmp_result;
  }
  RR;

  // Gather adjacent ents of other dimensions
  Range tmp_ents;
  for (int dim = 3; dim >= 0; dim--) {
    tmp_ents.clear();
    ErrorCode tmp_result = mMB->get_adjacencies(related_ents, dim, false,
                                                tmp_ents,
                                                Interface::UNION);
    if (MB_SUCCESS != tmp_result) result = tmp_result;
    else related_ents.merge(tmp_ents);
  }
  RR;

  // Related ents includes the partition itself
  related_ents.merge(partition);

  // Get contains-related sets
  Range tmp_ents3, last_related;
  if (file_set)
    result = mMB->get_entities_by_type(*file_set, MBENTITYSET, tmp_ents3);
  else
    result = mMB->get_entities_by_type(0, MBENTITYSET, tmp_ents3);
  RR;

  while (related_ents.size() != last_related.size()) {
    last_related = related_ents;
    for (Range::iterator rit = tmp_ents3.begin(); 
         rit != tmp_ents3.end(); ++rit) {
      if (related_ents.find(*rit) != related_ents.end()) continue;

      tmp_ents.clear();
      result = mMB->get_entities_by_handle(*rit, tmp_ents, true); RR;
      Range tmp_ents2 = intersect(tmp_ents, related_ents);

      // If the intersection is not empty, set is related
      if (!tmp_ents2.empty()) related_ents.insert(*rit);
    }
  }

  // Get child-related sets
  last_related.clear();
  while (related_ents.size() != last_related.size()) {
    last_related = related_ents;
    std::pair<Range::const_iterator, Range::const_iterator> it_pair = 
      last_related.equal_range(MBENTITYSET);

    for (Range::const_iterator rit = it_pair.first;
         rit != it_pair.second; ++rit) {
      // Get all children and add to related ents
      tmp_ents.clear();
      result = mMB->get_child_meshsets(*rit, tmp_ents, 0); RR;
      related_ents.merge(tmp_ents);
    }
  }

  // Get parent-related sets
  last_related.clear();
  while (related_ents.size() != last_related.size()) {
    last_related = related_ents;
    std::pair<Range::const_iterator, Range::const_iterator> it_pair =
      last_related.equal_range(MBENTITYSET);

    for (Range::const_iterator rit = it_pair.first;
         rit != it_pair.second; ++rit) {
      // Get all parents and add to related ents
      tmp_ents.clear();
      result = mMB->get_parent_meshsets(*rit, tmp_ents, 0); RR;
      related_ents.merge(tmp_ents);
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadUtil::get_ordered_vertices(EntityHandle *bound_ents,
                                         int *sense,
                                         int bound_size,
                                         int dim,
                                         EntityHandle *bound_verts,
                                         EntityType &etype)
{
  // Get dimension of bounding entities
  int bound_dim = CN::Dimension(TYPE_FROM_HANDLE(bound_ents[0]));
  int indices[MAX_SUB_ENTITY_VERTICES];
  const EntityHandle *connect = NULL;
  std::vector<EntityHandle> tmp_connect;

  // Find the right entity type based on # bounding ents
  int numv = 0, num_connect = 0;
  ErrorCode result;
  for (EntityType t = MBEDGE; t < MBENTITYSET; t++) {
    int nindex = CN::NumSubEntities(t, bound_dim);
    if (CN::Dimension(t) != dim || nindex != bound_size)
      continue;

    // Fill in vertices from bounding entity vertices
    int nverts = CN::VerticesPerEntity(t);
    std::fill(bound_verts, bound_verts+nverts, 0);
    for (int index = 0; index < nindex; index++) {
      result = mMB->get_connectivity(bound_ents[index], connect, num_connect,
                                     false, &tmp_connect);
      if (MB_SUCCESS != result)
        return result;
      
      CN::SubEntityVertexIndices(t, bound_dim, index, indices);

      for (int c = 0; c < num_connect; c++) {
        if (!bound_verts[indices[c]]) {
          bound_verts[indices[c]] = (sense[index] > 0) ?
              connect[c] : connect[num_connect - c - 1];
          numv++;
        }
      }
      if (numv == nverts) {
        etype = t;
        return MB_SUCCESS;
      }
    }
  }

  // If we get here, we didn't get full connectivity
  etype = MBMAXTYPE;
  return MB_FAILURE;
}

static ErrorCode check_int_tag(Interface* mb, Tag tag)
{
  int size;
  DataType type;
  ErrorCode rval = mb->tag_get_bytes(tag, size);
  if (MB_SUCCESS != rval)
    return rval;
  if (size != sizeof(int))
    return MB_TYPE_OUT_OF_RANGE;
  rval = mb->tag_get_data_type(tag, type);
  if (type != MB_TYPE_OPAQUE && type != MB_TYPE_INTEGER)
    return MB_TYPE_OUT_OF_RANGE;

  return MB_SUCCESS;
}

ErrorCode ReadUtil::assign_ids(Tag id_tag, const Range& ents, int start)
{
  ErrorCode rval = check_int_tag(mMB, id_tag);
  if (MB_SUCCESS != rval)
    return rval;

  Range tmp_range;
  std::vector<int> data;
  for (Range::const_pair_iterator i = ents.pair_begin();
       i != ents.pair_end(); ++i) {
    data.resize(i->second + 1 - i->first);
    for (std::vector<int>::iterator j = data.begin(); j != data.end(); ++j)
      *j = start++;
    tmp_range.clear();
    tmp_range.insert(i->first, i->second);
    rval = mMB->tag_set_data(id_tag, tmp_range, &data[0]);
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}

ErrorCode ReadUtil::assign_ids(Tag id_tag,
                               const EntityHandle* ents,
                               size_t num_ents,
                               int start)
{
  ErrorCode rval = check_int_tag(mMB, id_tag);
  if (MB_SUCCESS != rval)
    return rval;

  std::vector<int> data;
  const EntityHandle* const end = ents + num_ents;
  const EntityHandle* i = ents;
  while (i != end) {
    const EntityHandle* next = std::find(i, end, 0u);
    size_t size = next - i;
    if (!size) {
      ++i;
      continue;
    }

    int id = start + (i - ents);
    data.resize(size);
    for (std::vector<int>::iterator j = data.begin(); j != data.end(); ++j)
      *j = id++;

    rval = mMB->tag_set_data(id_tag, i, size, &data[0]);
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}

ErrorCode ReadUtil::create_gather_set(EntityHandle& gather_set)
{
  ErrorCode rval = mMB->create_meshset(MESHSET_SET, gather_set);
  if (MB_SUCCESS != rval)
    return rval;

  Tag gather_set_tag;
  rval = mMB->tag_get_handle("GATHER_SET", 1, MB_TYPE_INTEGER, gather_set_tag, MB_TAG_CREAT | MB_TAG_SPARSE);
  if (MB_SUCCESS != rval)
    return rval;

  int gather_val = 1;
  rval = mMB->tag_set_data(gather_set_tag, &gather_set, 1, &gather_val);
  if (MB_SUCCESS != rval)
    return rval;

  return MB_SUCCESS;
}

ErrorCode ReadUtil::get_gather_set(EntityHandle& gather_set)
{
  Tag gather_set_tag;
  ErrorCode rval = mMB->tag_get_handle("GATHER_SET", 1, MB_TYPE_INTEGER, gather_set_tag, MB_TAG_SPARSE);
  if (MB_SUCCESS != rval)
    return rval;

  int gather_val = 1;
  void* vals[] = {&gather_val};
  Range gather_sets;
  rval = mMB->get_entities_by_type_and_tag(0, MBENTITYSET, &gather_set_tag, vals, 1, gather_sets);
  if (MB_SUCCESS != rval)
    return rval;

  if (gather_sets.empty())
    return MB_ENTITY_NOT_FOUND;

  gather_set = gather_sets[0];

  return MB_SUCCESS;
}

} // namespace moab
