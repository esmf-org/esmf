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

#include <memory.h>
#include <algorithm>

#include "SparseTag.hpp"
#include "moab/Range.hpp"
#include "TagCompare.hpp"
#include "SysUtil.hpp"
#include "SequenceManager.hpp"
#include "moab/Error.hpp"
#include "moab/ErrorHandler.hpp"
#include "moab/CN.hpp"

namespace moab {

SparseTag::SparseTag(const char* name,
                     int size,
                     DataType type,
                     const void* default_value)
  : TagInfo(name, size, type, default_value, size)
  {}

SparseTag::~SparseTag()
{
  release_all_data(0, 0, true);
}

TagType SparseTag::get_storage_type() const
{
  return MB_TAG_SPARSE;
}

ErrorCode SparseTag::release_all_data(SequenceManager*, Error*, bool)
{
  for(MapType::iterator i = mData.begin(); i != mData.end(); ++i)
    mAllocator.destroy(i->second);
  mData.clear();
  return MB_SUCCESS;
}

ErrorCode SparseTag::set_data(Error*, EntityHandle entity_handle, const void* data)
{
#ifdef MOAB_HAVE_UNORDERED_MAP
  MapType::iterator iter = mData.find(entity_handle);
#else
  MapType::iterator iter = mData.lower_bound(entity_handle);
#endif

  // Data space already exists
  if (iter!= mData.end() && iter->first == entity_handle)
    memcpy(iter->second, data, get_size());
  // We need to make some data space
  else {
    void *new_data = allocate_data(entity_handle, iter, false);
    memcpy(new_data, data, get_size());
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::get_data_ptr(EntityHandle entity_handle, const void*& ptr, bool allocate) const
{
  MapType::const_iterator iter = mData.find(entity_handle);

  if (iter != mData.end())
    ptr = iter->second;
  else if (get_default_value() && allocate)
    ptr = const_cast<SparseTag*>(this)->allocate_data(entity_handle, iter, allocate);
  else 
    return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode SparseTag::get_data(Error* /* error */, EntityHandle entity_handle, void* data) const
{
  const void* ptr = 0;
  ErrorCode rval = get_data_ptr(entity_handle, ptr, false);
  if (MB_SUCCESS == rval) {
    memcpy(data, ptr, get_size());
    return rval;
  }
  else if (get_default_value()) {
    memcpy(data, get_default_value(), get_size());
    return MB_SUCCESS;
  }
  else
    return MB_TAG_NOT_FOUND;
}

ErrorCode SparseTag::remove_data(Error* /* error */, EntityHandle entity_handle)
{
  MapType::iterator i = mData.find(entity_handle);
  if (i == mData.end())
    return MB_TAG_NOT_FOUND;

  mAllocator.destroy(i->second);
  mData.erase(i);

  return MB_SUCCESS;
}

ErrorCode SparseTag::get_data(const SequenceManager*,
                              Error* /* error */,
                              const EntityHandle* entities,
                              size_t num_entities,
                              void* data) const
{
  ErrorCode rval;
  unsigned char* ptr = reinterpret_cast<unsigned char*>(data);
  for (size_t i = 0; i < num_entities; ++i, ptr += get_size()) {
    rval = get_data(NULL, entities[i], ptr);
    if (MB_SUCCESS != rval) return rval;
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::get_data(const SequenceManager*,
                              Error* /* error */,
                              const Range& entities,
                              void* data) const
{
  ErrorCode rval;
  unsigned char* ptr = reinterpret_cast<unsigned char*>(data);
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ptr += get_size()) {
    rval = get_data(NULL, *i, ptr);
    if (MB_SUCCESS != rval) return rval;
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::get_data(const SequenceManager*,
                              Error* /* error */,
                              const EntityHandle* entities,
                              size_t num_entities,
                              const void** pointers,
                              int* data_lengths) const
{
  if (data_lengths) {
    int len = get_size();
    SysUtil::setmem(data_lengths, &len, sizeof(int), num_entities);
  }

  ErrorCode rval = MB_SUCCESS, rval_tmp;
  for (size_t i = 0; i < num_entities; ++i, ++pointers) {
    rval_tmp = get_data_ptr(entities[i], *pointers);
    if (MB_SUCCESS != rval_tmp && get_default_value()) {
      *pointers = get_default_value();
    }
    else if (MB_SUCCESS != rval_tmp) {
      return MB_TAG_NOT_FOUND;
    }
  }

  return rval;
}

ErrorCode SparseTag::get_data(const SequenceManager*,
                              Error* /* error */,
                              const Range& entities,
                              const void** pointers,
                              int* data_lengths) const
{
  if (data_lengths) {
    int len = get_size();
    SysUtil::setmem(data_lengths, &len, sizeof(int), entities.size());
  }

  ErrorCode rval = MB_SUCCESS, rval_tmp;
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ++pointers) {
    rval_tmp = get_data_ptr(*i, *pointers);
    if (MB_SUCCESS != rval_tmp && get_default_value()) {
      *pointers = get_default_value();
    }
    else if (MB_SUCCESS != rval_tmp) {
      return MB_TAG_NOT_FOUND;
    }
  }

  return rval;
}

ErrorCode SparseTag::set_data(SequenceManager* seqman,
                              Error* /* error */,
                              const EntityHandle* entities,
                              size_t num_entities,
                              const void* data)
{
  ErrorCode rval = seqman->check_valid_entities(NULL, entities, num_entities, true);MB_CHK_ERR(rval);

  const unsigned char* ptr = reinterpret_cast<const unsigned char*>(data);
  for (size_t i = 0; i < num_entities; ++i, ptr += get_size()) {
    rval = set_data(NULL, entities[i], ptr);MB_CHK_ERR(rval);
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::set_data(SequenceManager* seqman,
                              Error* /* error */,
                              const Range& entities,
                              const void* data)
{
  ErrorCode rval = seqman->check_valid_entities(NULL, entities);MB_CHK_ERR(rval);

  const unsigned char* ptr = reinterpret_cast<const unsigned char*>(data);
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ptr += get_size())
    if (MB_SUCCESS != (rval = set_data(NULL, *i, ptr)))
      return rval;

  return MB_SUCCESS;
}

ErrorCode SparseTag::set_data(SequenceManager* seqman,
                              Error* /* error */,
                              const EntityHandle* entities,
                              size_t num_entities,
                              void const* const* pointers,
                              const int* lengths)
{
  ErrorCode rval = validate_lengths(NULL, lengths, num_entities);MB_CHK_ERR(rval);

  rval = seqman->check_valid_entities(NULL, entities, num_entities, true);MB_CHK_ERR(rval);

  for (size_t i = 0; i < num_entities; ++i, ++pointers) {
    rval = set_data(NULL, entities[i], *pointers);MB_CHK_ERR(rval);
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::set_data(SequenceManager* seqman,
                              Error* /* error */,
                              const Range& entities,
                              void const* const* pointers,
                              const int* lengths)
{
  ErrorCode rval = validate_lengths(NULL, lengths, entities.size());MB_CHK_ERR(rval);

  rval = seqman->check_valid_entities(NULL, entities);MB_CHK_ERR(rval);

  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ++pointers) {
    rval = set_data(NULL, *i, *pointers);MB_CHK_ERR(rval);
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::clear_data(SequenceManager* seqman,
                                Error* /* error */,
                                const EntityHandle* entities,
                                size_t num_entities,
                                const void* value_ptr,
                                int value_len)
{
  if (value_len && value_len != get_size()) {
    MB_SET_ERR(MB_INVALID_SIZE, "Invalid data size " << get_size() << " specified for sparse tag " << get_name() << " of size " << value_len);
  }

  ErrorCode rval = seqman->check_valid_entities(NULL, entities, num_entities, true);MB_CHK_ERR(rval);

  for (size_t i = 0; i < num_entities; ++i) {
    rval = set_data(NULL, entities[i], value_ptr);MB_CHK_ERR(rval);
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::clear_data(SequenceManager* seqman,
                                Error* /* error */,
                                const Range& entities,
                                const void* value_ptr,
                                int value_len)
{
  if (value_len && value_len != get_size()) {
    MB_SET_ERR(MB_INVALID_SIZE, "Invalid data size " << get_size() << " specified for sparse tag " << get_name() << " of size " << value_len);
  }

  ErrorCode rval = seqman->check_valid_entities(NULL, entities);MB_CHK_ERR(rval);

  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i) {
    rval = set_data(NULL, *i, value_ptr);MB_CHK_ERR(rval);
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::remove_data(SequenceManager*,
                                 Error* /* error */,
                                 const EntityHandle* entities,
                                 size_t num_entities)
{
  ErrorCode rval;
  for (size_t i = 0; i < num_entities; ++i) {
    rval = remove_data(NULL, entities[i]);
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}

ErrorCode SparseTag::remove_data(SequenceManager*,
                                 Error* /* error */,
                                 const Range& entities)
{
  for (Range::const_iterator i = entities.begin(); i != entities.end(); ++i)
    if (MB_SUCCESS != remove_data(NULL, *i))
      return MB_TAG_NOT_FOUND;

  return MB_SUCCESS;
}

ErrorCode SparseTag::tag_iterate(SequenceManager* seqman,
                                 Error* /* error */,
                                 Range::iterator& iter,
                                 const Range::iterator& end,
                                 void*& data_ptr,
                                 bool allocate)
{
  // Note: We are asked to returning a block of contiguous storage
  //       for some block of contiguous handles for which the tag
  //       storage is also contiguous. As sparse tag storage is
  //       never contiguous, all we can do is return a pointer to the
  //       data for the first entity.

  // If asked for nothing, successfully return nothing.
  if (iter == end)
    return MB_SUCCESS;

  // Note: get_data_ptr will return the default value if the
  //       handle is not found, so test to make sure that the
  //       handle is valid.
  ErrorCode rval = seqman->check_valid_entities(NULL, &*iter, 1);MB_CHK_ERR(rval);

  // Get pointer to tag storage for entity pointed to by iter
  const void* ptr = NULL;
  rval = get_data_ptr(*iter, ptr);
  if (MB_SUCCESS == rval) 
    data_ptr = const_cast<void*>(ptr);
  else if (get_default_value() && allocate) {
    ptr = allocate_data(*iter, mData.end());
    data_ptr = const_cast<void*>(ptr);
  }
  else {
    // If allocation was not requested, need to increment the iterator so that
    // the count can be computed properly
    if (get_default_value() && !allocate)
      ++iter;
    // return not_found(get_name(), *iter);
  }

  // Increment iterator and return
  ++iter;
  return MB_SUCCESS;
}

template <class Container> static inline
void get_tagged(const SparseTag::MapType& mData,
                EntityType type,
                Container& output_range)
{
  SparseTag::MapType::const_iterator iter;
  typename Container::iterator hint = output_range.begin();
  if (MBMAXTYPE == type) {
    for (iter = mData.begin(); iter != mData.end(); ++iter)
      hint = output_range.insert(hint, iter->first);
  }
  else {
#ifdef MOAB_HAVE_UNORDERED_MAP
    for (iter = mData.begin(); iter != mData.end(); ++iter)
      if (TYPE_FROM_HANDLE(iter->first) == type)
        hint = output_range.insert(hint, iter->first);
#else
    iter = mData.lower_bound(FIRST_HANDLE(type));
    SparseTag::MapType::const_iterator end = mData.lower_bound(LAST_HANDLE(type) + 1);
    for (; iter != end; ++iter)
      hint = output_range.insert(hint, iter->first);
#endif
  }
}

template <class Container> static inline
void get_tagged(const SparseTag::MapType& mData,
                Range::const_iterator begin,
                Range::const_iterator end,
                Container& output_range)
{
  typename Container::iterator hint = output_range.begin();
  for (Range::const_iterator i = begin; i != end; ++i)
    if (mData.find(*i) != mData.end())
      hint = output_range.insert(hint, *i);
}

template <class Container> static inline
void get_tagged(const SparseTag::MapType& mData,
                Container& entities,
                EntityType type,
                const Range* intersect)
{
  if (!intersect)
    get_tagged<Container>(mData, type, entities);
  else if (MBMAXTYPE == type)
    get_tagged<Container>(mData, intersect->begin(), intersect->end(), entities);
  else {
    std::pair<Range::iterator,Range::iterator> r = intersect->equal_range(type);
    get_tagged<Container>(mData, r.first, r.second, entities);
  }
}

//! Gets all entity handles that match a type and tag
ErrorCode SparseTag::get_tagged_entities(const SequenceManager*,
                                         Range& output_range,
                                         EntityType type,
                                         const Range* intersect) const
{
  get_tagged(mData, output_range, type, intersect);
  return MB_SUCCESS;
}

//! Gets all entity handles that match a type and tag
ErrorCode SparseTag::num_tagged_entities(const SequenceManager*,
                                         size_t& output_count,
                                         EntityType type,
                                         const Range* intersect) const
{
  InsertCount counter(output_count);
  get_tagged(mData, counter, type, intersect);
  output_count = counter.end();
  return MB_SUCCESS;
}

ErrorCode SparseTag::find_entities_with_value(
#ifdef MOAB_HAVE_UNORDERED_MAP
                                              const SequenceManager* seqman,
#else
                                              const SequenceManager*,
#endif
                                              Error* /* error */,
                                              Range& output_entities,
                                              const void* value,
                                              int value_bytes,
                                              EntityType type,
                                              const Range* intersect_entities) const
{
  if (value_bytes && value_bytes != get_size()) {
    MB_SET_ERR(MB_INVALID_SIZE, "Invalid data size " << get_size() << " specified for sparse tag " << get_name() << " of size " << value_bytes);
  }

  MapType::const_iterator iter, end;
#ifdef MOAB_HAVE_UNORDERED_MAP
  if (intersect_entities) {
    std::pair<Range::iterator,Range::iterator> r;
    if (type == MBMAXTYPE) {
      r.first = intersect_entities->begin();
      r.second = intersect_entities->end();
    }
    else {
      r = intersect_entities->equal_range(type);
    }

    find_map_values_equal(*this, value, get_size(),
                          r.first, r.second,
                          mData, output_entities);
  }
  else if (type == MBMAXTYPE) {
    find_tag_values_equal(*this, value, get_size(),
                          mData.begin(), mData.end(),
                          output_entities);
  }
  else {
    Range tmp;
    seqman->get_entities(type, tmp);
    find_map_values_equal(*this, value, get_size(),
                          tmp.begin(), tmp.end(),
                          mData, output_entities);
  }
#else
  if (intersect_entities) {
    for (Range::const_pair_iterator p = intersect_entities->begin();
         p != intersect_entities->end(); ++p) {
      iter = mData.lower_bound(p->first);
      end = mData.upper_bound(p->second);
      find_tag_values_equal(*this, value, get_size(), iter, end,
                            output_entities);
    }
  }
  else {
    if (type == MBMAXTYPE) {
      iter = mData.begin();
      end = mData.end();
    }
    else {
      iter = mData.lower_bound(CREATE_HANDLE(type, MB_START_ID));
      end = mData.upper_bound(CREATE_HANDLE(type, MB_END_ID));
    }
    find_tag_values_equal(*this, value, get_size(), iter, end,
                          output_entities);
  }
#endif

  return MB_SUCCESS;
}

bool SparseTag::is_tagged(const SequenceManager*, EntityHandle h) const
{
  return mData.find(h) != mData.end();
}

ErrorCode SparseTag::get_memory_use(const SequenceManager*,
                                    unsigned long& total,
                                    unsigned long& per_entity) const

{
  per_entity = get_size() + 4*sizeof(void*);
  total = (mData.size() * per_entity)
        + sizeof(*this) + TagInfo::get_memory_use();

  return MB_SUCCESS;
}

} // namespace moab
