#include "SequenceManager.hpp"
#include "VertexSequence.hpp"
#include "UnstructuredElemSeq.hpp"
#include "ScdVertexData.hpp"
#include "MeshSetSequence.hpp"
#include "SweptElementSeq.hpp"
#include "StructuredElementSeq.hpp"
#include "moab/HomXform.hpp"
#include "PolyElementSeq.hpp"
#include "SysUtil.hpp"
#include "moab/Error.hpp"

#include <assert.h>
#include <new>
#include <algorithm>

#ifndef NDEBUG
#include <iostream>
#endif

namespace moab {

const EntityID SequenceManager::DEFAULT_VERTEX_SEQUENCE_SIZE = 512 * 1024;
const EntityID SequenceManager::DEFAULT_ELEMENT_SEQUENCE_SIZE = DEFAULT_VERTEX_SEQUENCE_SIZE;
const EntityID SequenceManager::DEFAULT_POLY_SEQUENCE_SIZE = 16 * 1024;
const EntityID SequenceManager::DEFAULT_MESHSET_SEQUENCE_SIZE = DEFAULT_VERTEX_SEQUENCE_SIZE;

const int UNUSED_SIZE = 0;

EntityID SequenceManager::default_poly_sequence_size(int conn_len)
{
  return std::max(DEFAULT_POLY_SEQUENCE_SIZE / conn_len, (EntityID)1);
}

SequenceManager::~SequenceManager()
{
  // Release variable-length tag data
  for (unsigned i = 0; i < tagSizes.size(); ++i)
    if (tagSizes[i] == MB_VARIABLE_LENGTH)
      release_tag_array(0, i, false);
}

void SequenceManager::clear()
{
  // reset sequence multiplier
  sequence_multiplier = 1.0;

  // Destroy all TypeSequenceManager instances
  for (EntityType t = MBVERTEX; t < MBMAXTYPE; ++t)
    typeData[t].~TypeSequenceManager();

  // Now re-create TypeSequenceManager instances
  for (EntityType t = MBVERTEX; t < MBMAXTYPE; ++t)
    new (typeData + t)TypeSequenceManager();
}

void SequenceManager::get_entities(Range& entities_out) const
{
  for (EntityType t = MBENTITYSET; t >= MBVERTEX; --t)
    typeData[t].get_entities(entities_out);
}

void SequenceManager::get_entities(std::vector<EntityHandle>& entities_out) const
{
  for (EntityType t = MBVERTEX; t != MBMAXTYPE; ++t)
    typeData[t].get_entities(entities_out);
}

EntityID SequenceManager::get_number_entities() const
{
  EntityID sum = 0;
  for (EntityType t = MBVERTEX; t != MBMAXTYPE; ++t)
    sum += typeData[t].get_number_entities();

  return sum;
}

ErrorCode SequenceManager::check_valid_entities(Error* /* error */,
                                                const Range& entities) const
{
  ErrorCode rval;
  Range::const_pair_iterator i;
  for (i = entities.const_pair_begin(); i != entities.const_pair_end(); ++i) {
    const EntityType type1 = TYPE_FROM_HANDLE(i->first);
    const EntityType type2 = TYPE_FROM_HANDLE(i->second);
    if (type1 == type2) {
      rval = typeData[type1].check_valid_handles(NULL, i->first, i->second);
      if (MB_SUCCESS != rval)
        return rval;
    }
    else {
      int junk;
      EntityHandle split = CREATE_HANDLE(type2, 0, junk);
      rval = typeData[type1].check_valid_handles(NULL, i->first, split - 1);
      if (MB_SUCCESS != rval)
        return rval;
      rval = typeData[type2].check_valid_handles(NULL, split, i->second);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::check_valid_entities(Error* /* error_handler */,
                                                const EntityHandle* entities,
                                                size_t num_entities,
                                                bool root_set_okay) const
{
  ErrorCode rval;
  const EntitySequence* ptr = 0;

  const EntityHandle* const end = entities + num_entities;
  for (; entities < end; ++entities) {
    rval = find(*entities, ptr);
    if (MB_SUCCESS != rval && !(root_set_okay && !*entities)) {
      if (MB_ENTITY_NOT_FOUND == rval) {
        // MB_ENTITY_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
#       if 0
        fprintf(stderr, "[Warning]: Invalid entity handle: 0x%lx\n", (unsigned long)*entities);
#       endif
      }
      return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::delete_entity(Error* /* error */, EntityHandle entity)
{
  return typeData[TYPE_FROM_HANDLE(entity)].erase(NULL, entity);
}

ErrorCode SequenceManager::delete_entities(Error* /* error */, const Range& entities)
{
  ErrorCode rval = check_valid_entities(NULL, entities);
  if (MB_SUCCESS != rval)
    return rval;

  ErrorCode result = MB_SUCCESS;
  Range::const_pair_iterator i;
  for (i = entities.const_pair_begin(); i != entities.const_pair_end(); ++i) {
    const EntityType type1 = TYPE_FROM_HANDLE(i->first);
    const EntityType type2 = TYPE_FROM_HANDLE(i->second);
    if (type1 == type2) {
      rval = typeData[type1].erase(NULL, i->first, i->second);
      if (MB_SUCCESS != rval)
        return result = rval;
    }
    else {
      int junk;
      EntityHandle split = CREATE_HANDLE(type2, 0, junk);
      rval = typeData[type1].erase(NULL, i->first, split - 1);
      if (MB_SUCCESS != rval)
        return result = rval;
      rval = typeData[type2].erase(NULL, split, i->second);
      if (MB_SUCCESS != rval)
        return result = rval;
    }
  }

  return result;
}

ErrorCode SequenceManager::create_vertex(const double coords[3],
                                         EntityHandle& handle)
{
  const EntityHandle start = CREATE_HANDLE(MBVERTEX, MB_START_ID);
  const EntityHandle   end = CREATE_HANDLE(MBVERTEX,   MB_END_ID);
  bool append;
  TypeSequenceManager::iterator seq = typeData[MBVERTEX].find_free_handle(start, end, append);
  VertexSequence* vseq;

  if (seq == typeData[MBVERTEX].end()) {
    SequenceData* seq_data = 0;
    EntityID seq_data_size = 0;
    handle = typeData[MBVERTEX].find_free_sequence(DEFAULT_VERTEX_SEQUENCE_SIZE, start, end, seq_data, seq_data_size);
    if (!handle) 
      return MB_FAILURE;

    if (seq_data) 
      vseq = new VertexSequence(handle, 1, seq_data);
    else
      vseq = new VertexSequence(handle, 1, DEFAULT_VERTEX_SEQUENCE_SIZE);

    ErrorCode rval = typeData[MBVERTEX].insert_sequence(vseq);
    if (MB_SUCCESS != rval) {
      SequenceData* vdata = vseq->data();
      delete vseq;
      if (!seq_data)
        delete vdata;

      return rval;
    }
  }
  else {
    vseq = reinterpret_cast<VertexSequence*>(*seq);
    if (append) {
      vseq->push_back(1);
      handle = vseq->end_handle();
      typeData[MBVERTEX].notify_appended(seq);
    }
    else {
      vseq->push_front(1);
      handle = vseq->start_handle();
      typeData[MBVERTEX].notify_prepended(seq);
    }
  }

  return vseq->set_coordinates(handle, coords);
}

ErrorCode SequenceManager::create_element(EntityType type,
                                          const EntityHandle* conn,
                                          unsigned conn_len,
                                          EntityHandle& handle)
{
  if (type <= MBVERTEX || type >= MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  const EntityHandle start = CREATE_HANDLE(type, MB_START_ID);
  const EntityHandle   end = CREATE_HANDLE(type,   MB_END_ID);
  bool append;
  TypeSequenceManager::iterator seq = typeData[type].find_free_handle(start, end, append, conn_len);
  UnstructuredElemSeq* eseq;

  if (seq == typeData[type].end()) {
    SequenceData* seq_data = 0;
    unsigned size = DEFAULT_ELEMENT_SEQUENCE_SIZE;
    if (type == MBPOLYGON || type == MBPOLYHEDRON) {
      size = default_poly_sequence_size(conn_len);
    }
    EntityID seq_data_size = 0;
    handle = typeData[type].find_free_sequence(size, start, end, seq_data, seq_data_size, conn_len);
    if (!handle) 
      return MB_FAILURE;

    if (MBPOLYGON == type || MBPOLYHEDRON == type) {
      if (seq_data) 
        eseq = new PolyElementSeq(handle, 1, conn_len, seq_data);
      else
        eseq = new PolyElementSeq(handle, 1, conn_len, size);
    }
    else {
      if (seq_data) 
        eseq = new UnstructuredElemSeq(handle, 1, conn_len, seq_data);
      else
        eseq = new UnstructuredElemSeq(handle, 1, conn_len, size);
    }

    ErrorCode rval = typeData[type].insert_sequence(eseq);
    if (MB_SUCCESS != rval) {
      SequenceData* vdata = eseq->data();
      delete eseq;
      if (!seq_data)
        delete vdata;

      return rval;
    }
  }
  else {
    eseq = reinterpret_cast<UnstructuredElemSeq*>(*seq);
    if (append) {
      eseq->push_back(1);
      handle = eseq->end_handle();
      typeData[type].notify_appended(seq);
    }
    else {
      eseq->push_front(1);
      handle = eseq->start_handle();
      typeData[type].notify_prepended(seq);
    }
  }

  return eseq->set_connectivity(handle, conn, conn_len);
}

ErrorCode SequenceManager::create_mesh_set(unsigned flags,
                                           EntityHandle& handle)
{
  const EntityHandle start = CREATE_HANDLE(MBENTITYSET, MB_START_ID);
  const EntityHandle   end = CREATE_HANDLE(MBENTITYSET,   MB_END_ID);
  bool append;
  TypeSequenceManager::iterator seq = typeData[MBENTITYSET].find_free_handle(start, end, append);
  MeshSetSequence* msseq;

  if (seq == typeData[MBENTITYSET].end()) {
    SequenceData* seq_data = 0;
    EntityID seq_data_size = 0;
    handle = typeData[MBENTITYSET].find_free_sequence(DEFAULT_MESHSET_SEQUENCE_SIZE, start, end, seq_data, seq_data_size);
    if (!handle) 
      return MB_FAILURE;

    if (seq_data) 
      msseq = new MeshSetSequence(handle, 1, flags, seq_data);
    else
      msseq = new MeshSetSequence(handle, 1, flags, DEFAULT_MESHSET_SEQUENCE_SIZE);
      
    ErrorCode rval = typeData[MBENTITYSET].insert_sequence(msseq);
    if (MB_SUCCESS != rval) {
      SequenceData* vdata = msseq->data();
      delete msseq;
      if (!seq_data)
        delete vdata;

      return rval;
    }
  }
  else {
    msseq = reinterpret_cast<MeshSetSequence*>(*seq);
    if (append) {
      msseq->push_back(1, &flags);
      handle = msseq->end_handle();
      typeData[MBENTITYSET].notify_appended(seq);
    }
    else {
      msseq->push_front(1, &flags);
      handle = msseq->start_handle();
      typeData[MBENTITYSET].notify_prepended(seq);
    }
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::allocate_mesh_set(EntityHandle handle,
                                             unsigned flags)
{
  SequenceData* data = 0;
  TypeSequenceManager::iterator seqptr;
  EntityHandle block_start = 1, block_end = 0;
  ErrorCode rval = typeData[MBENTITYSET].is_free_handle(handle, seqptr, data, block_start, block_end);
  if (MB_SUCCESS != rval)
    return rval;

  MeshSetSequence* seq;
  if (seqptr != typeData[MBENTITYSET].end()) {
    seq = static_cast<MeshSetSequence*>(*seqptr);
    if (seq->start_handle() - 1 == handle) {
      rval = seq->push_front(1, &flags);
      if (MB_SUCCESS == rval) {
        rval = typeData[MBENTITYSET].notify_prepended(seqptr);
        if (MB_SUCCESS != rval)
          seq->pop_front(1);
      }
      return rval;
    }
    else if (seq->end_handle() + 1 == handle) {
      rval = seq->push_back(1, &flags);
      if (MB_SUCCESS == rval) {
        rval = typeData[MBENTITYSET].notify_appended(seqptr);
        if (MB_SUCCESS != rval)
          seq->pop_back(1);
      }
      return rval;
    }
    else
      return MB_FAILURE; // Should be unreachable
  }
  else {
    if (data) {
      seq = new MeshSetSequence(handle, 1, flags, data);
    }
    else {
      assert(handle >= block_start && handle <= block_end);
      trim_sequence_block(handle, block_end, DEFAULT_MESHSET_SEQUENCE_SIZE);
      seq = new MeshSetSequence(handle, 1, flags, block_end - handle + 1);
    }

    rval = typeData[MBENTITYSET].insert_sequence(seq);
    if (MB_SUCCESS != rval) {
      SequenceData* vdata = seq->data();
      delete seq;
      if (!data)
        delete vdata;
      return rval;
    }

    return MB_SUCCESS;
  }
}

void SequenceManager::trim_sequence_block(EntityHandle start_handle,
                                          EntityHandle& end_handle,
                                          unsigned max_size)
{
  assert(end_handle >= start_handle);
  assert((int)max_size > 0); // Cast to int also prohibits some ridiculously large values

  // If input range is larger than preferred size, trim it
  if (end_handle - start_handle >= max_size)
    end_handle = start_handle + max_size - 1;
}

EntityHandle SequenceManager::sequence_start_handle(EntityType type,
                                                    EntityID count,
                                                    int size,
                                                    EntityID start,
                                                    SequenceData*& data,
                                                    EntityID &data_size)
{
  TypeSequenceManager &tsm = typeData[type];
  data = 0;
  EntityHandle handle = CREATE_HANDLE(type, start);
  if (start < MB_START_ID ||
      !tsm.is_free_sequence(handle, count, data, size)) {
    EntityHandle pstart = CREATE_HANDLE(type, MB_START_ID);
    EntityHandle pend   = CREATE_HANDLE(type,   MB_END_ID);
    handle = tsm.find_free_sequence(count, pstart, pend, data, data_size, size);
  }

  return handle;
}

EntityID SequenceManager::new_sequence_size(EntityHandle start,
                                            EntityID requested_size,
                                            int sequence_size) const
{

  requested_size = (EntityID) (this->sequence_multiplier*requested_size);

  if (sequence_size < (int)requested_size)
    return requested_size;

  EntityHandle last = typeData[TYPE_FROM_HANDLE(start)].last_free_handle(start);
  if (!last) {
    assert(false);
    return 0;
  }

  EntityID available_size = last - start + 1;
  if (sequence_size < available_size)
    return sequence_size;
  else
    return available_size;
}

ErrorCode SequenceManager::create_entity_sequence(EntityType type,
                                                  EntityID count,
                                                  int size,
                                                  EntityID start,
                                                  EntityHandle& handle,
                                                  EntitySequence*& sequence,
                                                  int sequence_size)
{
  SequenceData* data = NULL;
  EntityID data_size = 0;
  handle = sequence_start_handle(type, count, size, start, data, data_size);

  if (!handle)
    return MB_MEMORY_ALLOCATION_FAILED;

  switch (type) {
  case MBENTITYSET:
  case MBMAXTYPE:
    return MB_TYPE_OUT_OF_RANGE;

  case MBVERTEX:
    if (size != 0)
      return MB_INDEX_OUT_OF_RANGE;

    if (data)
      sequence = new VertexSequence(handle, count, data);
    else {
      if (!data_size)
        data_size = new_sequence_size(handle, count, sequence_size);
      sequence = new VertexSequence(handle, count, data_size);
    }
    break;

  case MBPOLYGON:
  case MBPOLYHEDRON:
    if (size == 0)
      return MB_INDEX_OUT_OF_RANGE;

    if (data)
      sequence = new PolyElementSeq(handle, count, size, data);
    else {
      if (!data_size)
        data_size = new_sequence_size(handle, count,
                                      (-1 == sequence_size ? default_poly_sequence_size(size) :
                                       sequence_size));
      sequence = new PolyElementSeq(handle, count, size, data_size);
    }
    break;

  default:
    if (size == 0)
      return MB_INDEX_OUT_OF_RANGE;

    if (data)
      sequence = new UnstructuredElemSeq(handle, count, size, data);
    else {
      if (!data_size)
        data_size = new_sequence_size(handle, count, sequence_size);
      sequence = new UnstructuredElemSeq(handle, count, size, data_size);
    }
    // tjt calling new_sequence_size 'cuz don't have a sequence data;
    // start 41467, count 246
    break;
  }

  ErrorCode result = typeData[type].insert_sequence(sequence);
  if (MB_SUCCESS != result) {
    // Change to NULL if had an existing data or if no existing data,
    // change to the new data created
    data = data ? 0 : sequence->data();
    delete sequence;
    delete data;
    return result;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::create_meshset_sequence(EntityID count,
                                                   EntityID start,
                                                   const unsigned* flags,
                                                   EntityHandle& handle,
                                                   EntitySequence*& sequence)
{
  SequenceData* data = 0;
  EntityID data_size = 0;
  handle = sequence_start_handle(MBENTITYSET, count, 0, start, data, data_size);

  if (!handle)
    return MB_MEMORY_ALLOCATION_FAILED;

  if (data)
    sequence = new MeshSetSequence(handle, count, flags, data);
  else
    sequence = new MeshSetSequence(handle, count, flags, count);

  ErrorCode result = typeData[MBENTITYSET].insert_sequence(sequence);
  if (MB_SUCCESS != result) {
    // Change to NULL if had an existing data or if no existing data,
    // change to the new data created
    data = data ? 0 : sequence->data();
    delete sequence;
    delete data;
    return result;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::create_meshset_sequence(EntityID count,
                                                   EntityID start,
                                                   unsigned flags,
                                                   EntityHandle& handle,
                                                   EntitySequence*& sequence)
{
  SequenceData* data = 0;
  EntityID data_size = 0;
  handle = sequence_start_handle(MBENTITYSET, count, 0, start, data, data_size);
  if (!handle)
    return MB_MEMORY_ALLOCATION_FAILED;

  if (data)
    sequence = new MeshSetSequence(handle, count, flags, data);
  else
    sequence = new MeshSetSequence(handle, count, flags, count);

  ErrorCode result = typeData[MBENTITYSET].insert_sequence(sequence);
  if (MB_SUCCESS != result) {
    // Change to NULL if had an existing data or if no existing data,
    // change to the new data created
    data = data ? 0 : sequence->data();
    delete sequence;
    delete data;
    return result;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::create_scd_sequence(int imin, int jmin, int kmin,
                                               int imax, int jmax, int kmax,
                                               EntityType type,
                                               EntityID start_id_hint,
                                               EntityHandle& handle,
                                               EntitySequence*& sequence,
                                               int *is_periodic)
{
  int this_dim = CN::Dimension(type);

  // Use > instead of != in the following assert to also catch cases where imin > imax, etc.
  assert((this_dim < 3 || kmax > kmin) &&
         (this_dim < 2 || jmax > jmin) &&
         (this_dim < 1 || imax > imin));

  // Compute # entities; not as easy as it would appear...
  EntityID num_ent;
  if (MBVERTEX == type)
    num_ent = (EntityID)(imax - imin + 1) * (EntityID)(jmax - jmin + 1) * (EntityID)(kmax - kmin + 1);
  else {
    num_ent = (imax-imin + (is_periodic && is_periodic[0] ? 1 : 0)) *
      (this_dim >= 2 ? (jmax - jmin + (is_periodic && is_periodic[1] ? 1 : 0)) : 1) *
      (this_dim >= 3 ? (kmax - kmin) : 1);
  }

  if (MBVERTEX == type && (is_periodic && (is_periodic[0] || is_periodic[1])))
    return MB_FAILURE;

  // Get a start handle
  SequenceData* data = 0;
  EntityID data_size = 0;
  handle = sequence_start_handle(type, num_ent, -1, start_id_hint, data, data_size);

  if (!handle)
    return MB_MEMORY_ALLOCATION_FAILED;
  assert(!data);

  switch (type) {
  case MBVERTEX:
    data = new ScdVertexData(handle, imin, jmin, kmin, imax, jmax, kmax);
    sequence = new VertexSequence(handle, data->size(), data);
    break;
  case MBEDGE:
  case MBQUAD:
  case MBHEX:
      sequence = new StructuredElementSeq(handle, imin, jmin, kmin, imax, jmax, kmax,
                                          is_periodic);
    break;
  default:
    return MB_TYPE_OUT_OF_RANGE;
  }

  ErrorCode result = typeData[type].insert_sequence(sequence);
  if (MB_SUCCESS != result) {
    data = sequence->data();
    delete sequence;
    delete data;
    return result;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::create_scd_sequence(const HomCoord& coord_min,
                                               const HomCoord& coord_max,
                                               EntityType type,
                                               EntityID start_id_hint,
                                               EntityHandle& first_handle_out,
                                               EntitySequence*& sequence_out,
                                               int *is_periodic)
{
  return create_scd_sequence(coord_min.i(), coord_min.j(), coord_min.k(),
                             coord_max.i(), coord_max.j(), coord_max.k(),
                             type, start_id_hint,
                             first_handle_out, sequence_out, is_periodic);
}

ErrorCode SequenceManager::create_sweep_sequence(int imin, int jmin, int kmin,
                                                 int imax, int jmax, int kmax,
                                                 int* Cq,
                                                 EntityType type,
                                                 EntityID start_id_hint,
                                                 EntityHandle& handle,
                                                 EntitySequence*& sequence)
{
  int this_dim = CN::Dimension(type);

  assert((this_dim < 3 || kmax > kmin) &&
         (this_dim < 2 || jmax > jmin) &&
         (this_dim < 1 || imax > imin));

  EntityID num_ent;
  if (MBVERTEX == type)
    num_ent = (EntityID)(imax - imin + 1) * (EntityID)(jmax - jmin + 1) * (EntityID)(kmax - kmin + 1);
  else {
    num_ent = (imax-imin) *
      (this_dim >= 2 ? (jmax - jmin) : 1) *
      (this_dim >= 3 ? (kmax - kmin) : 1);
  }

  // Get a start handle
  SequenceData* data = 0;
  EntityID data_size = 0;
  handle = sequence_start_handle(type, num_ent, -1, start_id_hint, data, data_size);

  if (!handle)
    return MB_MEMORY_ALLOCATION_FAILED;
  assert(!data);

  switch (type) {
  case MBVERTEX:
    data = new ScdVertexData(handle, imin, jmin, kmin, imax, jmax, kmax);
    sequence = new VertexSequence(handle, data->size(), data);
    break;
  case MBEDGE:
  case MBQUAD:
  case MBHEX:
    sequence = new SweptElementSeq(handle, imin, jmin, kmin, imax, jmax, kmax, Cq);
    break;
  default:
    return MB_TYPE_OUT_OF_RANGE;
  }

  ErrorCode result = typeData[type].insert_sequence(sequence);
  if (MB_SUCCESS != result) {
    data = sequence->data();
    delete sequence;
    delete data;
    return result;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::create_sweep_sequence(const HomCoord& coord_min,
                                                 const HomCoord& coord_max,
                                                 int* Cq,
                                                 EntityType type,
                                                 EntityID start_id_hint,
                                                 EntityHandle& first_handle_out,
                                                 EntitySequence*& sequence_out)
{
  return create_sweep_sequence(coord_min.i(), coord_min.j(), coord_min.k(),
                               coord_max.i(), coord_max.j(), coord_max.k(),
                               Cq,
                               type, start_id_hint,
                               first_handle_out, sequence_out);
}

ErrorCode SequenceManager::add_vsequence(EntitySequence *vert_seq,
                                         EntitySequence *elem_seq,
                                         const HomCoord &p1, const HomCoord &q1,
                                         const HomCoord &p2, const HomCoord &q2,
                                         const HomCoord &p3, const HomCoord &q3,
                                         bool bb_input,
                                         const HomCoord *bb_min,
                                         const HomCoord *bb_max)
{
  // Check first that they're structured vtx/elem sequences
  ScdVertexData *scd_vd = dynamic_cast<ScdVertexData*>(vert_seq->data());
  if (!scd_vd)
    return MB_FAILURE;

  ScdElementData *scd_ed = dynamic_cast<ScdElementData*>(elem_seq->data());
  if (!scd_ed)
    return MB_FAILURE;

  if (bb_min && bb_max)
    return scd_ed->add_vsequence(scd_vd, p1, q1, p2, q2, p3, q3,
                                 bb_input, *bb_min, *bb_max);
  else
    return scd_ed->add_vsequence(scd_vd, p1, q1, p2, q2, p3, q3,
                                 bb_input, HomCoord::unitv[0], HomCoord::unitv[0]);
}

ErrorCode SequenceManager::replace_subsequence(EntitySequence* new_seq)
{
  const EntityType type = TYPE_FROM_HANDLE(new_seq->start_handle());
  return typeData[type].replace_subsequence(new_seq, &tagSizes[0], tagSizes.size());
}

void SequenceManager::get_memory_use(unsigned long long& total_entity_storage,
                                     unsigned long long& total_storage) const

{
  total_entity_storage = 0;
  total_storage = 0;
  unsigned long long temp_entity, temp_total;
  for (EntityType i = MBVERTEX; i < MBMAXTYPE; ++i) {
    temp_entity = temp_total = 0;
    get_memory_use(i, temp_entity, temp_total);
    total_entity_storage += temp_entity;
    total_storage        += temp_total;
  }
}

void SequenceManager::get_memory_use(EntityType type,
                                     unsigned long long& total_entity_storage,
                                     unsigned long long& total_storage) const
{
  typeData[type].get_memory_use(total_entity_storage, total_storage);
}

void SequenceManager::get_memory_use(const Range& entities,
                                     unsigned long long& total_entity_storage,
                                     unsigned long long& total_amortized_storage) const
{
  total_entity_storage = 0;
  total_amortized_storage = 0;
  unsigned long long temp_entity, temp_total;
  Range::const_pair_iterator i;
  for (i = entities.const_pair_begin(); i != entities.const_pair_end(); ++i) {
    const EntityType t1 = TYPE_FROM_HANDLE(i->first);
    const EntityType t2 = TYPE_FROM_HANDLE(i->second);
    if (t1 == t2) {
      temp_entity = temp_total = 0;
      typeData[t1].get_memory_use(i->first, i->second, temp_entity, temp_total);
      total_entity_storage += temp_entity;
      total_amortized_storage += temp_total;
    }
    else {
      int junk;

      temp_entity = temp_total = 0;
      typeData[t1].get_memory_use(i->first, CREATE_HANDLE(t1, MB_END_ID, junk), temp_entity, temp_total);
      total_entity_storage += temp_entity;
      total_amortized_storage += temp_total;

      temp_entity = temp_total = 0;
      typeData[t2].get_memory_use(CREATE_HANDLE(t2, MB_START_ID, junk), i->second, temp_entity, temp_total);
      total_entity_storage += temp_entity;
      total_amortized_storage += temp_total;
    }
  }
}

ErrorCode SequenceManager::reserve_tag_array(Error* /* error_handler */,
                                             int size, int& index)
{
  if (size < 1 && size != MB_VARIABLE_LENGTH) {
    MB_SET_ERR(MB_INVALID_SIZE, "Invalid tag size: " << size);
  }

  std::vector<int>::iterator i = std::find(tagSizes.begin(), tagSizes.end(), UNUSED_SIZE);
  if (i == tagSizes.end()) {
    index = tagSizes.size();
    tagSizes.push_back(size);
  }
  else {
    index = i - tagSizes.begin();
    *i = size;
  }

  return MB_SUCCESS;
}

ErrorCode SequenceManager::release_tag_array(Error* /* error_handler */,
                                             int index, bool release_id)
{
  if ((unsigned)index >= tagSizes.size() || UNUSED_SIZE == tagSizes[index]) {
    // MB_TAG_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
#if 0
    fprintf(stderr, "[Warning]: Invalid dense tag index: %d\n", index);
#endif
    return MB_TAG_NOT_FOUND;
  }

  for (EntityType t = MBVERTEX; t <= MBENTITYSET; ++t) {
    TypeSequenceManager& seqs = entity_map(t);
    for (TypeSequenceManager::iterator i = seqs.begin(); i != seqs.end(); ++i)
      (*i)->data()->release_tag_data(index, tagSizes[index]);
  }

  if (release_id)
    tagSizes[index] = UNUSED_SIZE;

  return MB_SUCCESS;
}

// These are meant to be called from the debugger (not declared in any header)
// so leave them out of release builds (-DNDEBUG).
#ifndef NDEBUG

std::ostream& operator<<(std::ostream& s, const TypeSequenceManager& seq_man)
{
  const SequenceData* prev_data = 0;
  for (TypeSequenceManager::const_iterator i = seq_man.begin(); i != seq_man.end(); ++i) {
    const EntitySequence* seq = *i;
    if (seq->data() != prev_data) {
      prev_data = seq->data();
      s << "SequenceData ["
        << ID_FROM_HANDLE(seq->data()->start_handle())
        << ","
        << ID_FROM_HANDLE(seq->data()->end_handle())
        << "]"
        << std::endl;
    }
    s << "  Sequence ["
      << ID_FROM_HANDLE(seq->start_handle())
      << ","
      << ID_FROM_HANDLE(seq->end_handle())
      << "]"
      << std::endl;
  }

  return s;
}

std::ostream& operator<<(std::ostream& s, const SequenceManager& seq_man)
{
  for (EntityType t = MBVERTEX; t < MBMAXTYPE; ++t) {
    if (!seq_man.entity_map(t).empty())
      s << std::endl 
        << "****************** " << CN::EntityTypeName(t) << " ******************"
        << std::endl << seq_man.entity_map(t) << std::endl;
  }

  return s;
}

void print_sequences(const SequenceManager& seqman)
{
  std::cout << seqman << std::endl;
}

void print_sequences(const TypeSequenceManager& seqman)
{
  std::cout << seqman << std::endl;
}

#endif

} // namespace moab
