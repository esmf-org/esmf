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


/**********************************************
 * Filename   :     SparseTagSuperCollection.cpp
 *
 * Purpose    :     To store any size data with
 *                  any entity handle
 *
 * Creator    :     Clinton Stimpson
 *
 * Date       :     3 April 2002
 *
 * ********************************************/


#include <memory.h>
#include <algorithm>

#include "SparseTagSuperCollection.hpp"
#include "SparseTagCollection.hpp"
#include "moab/Range.hpp"

namespace moab {

/*
  SparseTagSuperCollection functions -----------------------------
*/

SparseTagSuperCollection::~SparseTagSuperCollection()
{
  std::vector<SparseTagCollection*>::iterator tag_iterator;
  for(tag_iterator = mDataTags.begin(); tag_iterator != mDataTags.end(); ++tag_iterator)
    delete *tag_iterator;
  mDataTags.clear();
}

void SparseTagSuperCollection::reset_data()
{
  std::vector<SparseTagCollection*>::iterator tag_iterator;
  for(tag_iterator = mDataTags.begin(); tag_iterator != mDataTags.end(); ++tag_iterator)
  {
    if (*tag_iterator) {
      int data_size = (*tag_iterator)->tag_size();
      delete *tag_iterator;
      *tag_iterator = new SparseTagCollection(data_size);
    }
  }
  
}

ErrorCode SparseTagSuperCollection::reserve_tag_id(int data_size, TagId tag_id)
{
  if(data_size<=0 && data_size != MB_VARIABLE_LENGTH)
    return MB_FAILURE;

  if (tag_id >= mDataTags.size())
    mDataTags.resize( tag_id+1, 0 );
    
  if (mDataTags[tag_id])
    return MB_FAILURE;
    
  mDataTags[tag_id] = new SparseTagCollection(data_size);
  return MB_SUCCESS;
}

ErrorCode SparseTagSuperCollection::release_tag_id(TagId tag_id)
{
  if (tag_id >= mDataTags.size() || !mDataTags[tag_id])
    return MB_TAG_NOT_FOUND;
  
  delete mDataTags[tag_id];
  mDataTags[tag_id] = 0;
  return MB_SUCCESS;
}

int SparseTagSuperCollection::tag_size(const TagId tag_id) const
{
  SparseTagCollection* coll = get_collection(tag_id);
  return coll ? coll->tag_size() : 0;
}


ErrorCode SparseTagSuperCollection::set_data( TagId tag_handle,
                                                const EntityHandle* handles,
                                                int num_handles,
                                                const void* data )
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
    
  const int length = coll->tag_size();
  if (length == MB_VARIABLE_LENGTH)
    return MB_VARIABLE_DATA_LENGTH;

  ErrorCode rval, result = MB_SUCCESS;
  const unsigned char* ptr = reinterpret_cast<const unsigned char*>(data);
  const EntityHandle *const end = handles + num_handles;
  for (const EntityHandle* i = handles; i != end; ++i, ptr += length) {
    rval = coll->set_data( *i, ptr );
    if (MB_SUCCESS != rval)
      result = rval;
  }
    
  return result;
}

ErrorCode SparseTagSuperCollection::set_data( TagId tag_handle,
                                              const EntityHandle* handles,
                                              int num_handles,
                                              void const* const* data_ptrs,
                                              const int* lengths,
                                              bool one_value )
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  const bool step = !one_value;
  const int length = coll->tag_size();
  int length_step;
  if (length == MB_VARIABLE_LENGTH) {
    if (!lengths)
      return MB_VARIABLE_DATA_LENGTH;
    length_step = step;
  }
  else {
    lengths = &length;
    length_step = 0;
  }

  ErrorCode rval, result = MB_SUCCESS;
  const EntityHandle *const end = handles + num_handles;
  void const* const* ptr = data_ptrs;
  for (const EntityHandle* i = handles; i != end; ++i, ptr += step, lengths += length_step) {
    rval = coll->set_data( *i, *ptr, *lengths );
    if (MB_SUCCESS != rval)
      result = rval;
  }
    
  return result;
}


ErrorCode SparseTagSuperCollection::set_data( TagId tag_handle,
                                                const Range& handles,
                                                const void* data )
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
    
  const int length = coll->tag_size();
  if (length == MB_VARIABLE_LENGTH)
    return MB_VARIABLE_DATA_LENGTH;

  ErrorCode rval, result = MB_SUCCESS;
  const unsigned char* ptr = reinterpret_cast<const unsigned char*>(data);
  for (Range::const_iterator i = handles.begin(); i != handles.end(); ++i, ptr += length) {
    rval = coll->set_data( *i, ptr );
    if (MB_SUCCESS != rval)
      result = rval;
  }
    
  return result;
}

ErrorCode SparseTagSuperCollection::set_data( TagId tag_handle,
                                              const Range& handles,
                                              void const* const* data_ptrs,
                                              const int* lengths,
                                              bool one_value )
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  const bool step = !one_value;
  const int length = coll->tag_size();
  int length_step;
  if (length == MB_VARIABLE_LENGTH) {
    if (!lengths)
      return MB_VARIABLE_DATA_LENGTH;
    length_step = step;
  }
  else {
    lengths = &length;
    length_step = 0;
  }

  ErrorCode rval, result = MB_SUCCESS;
  void const* const* ptr = data_ptrs;
  for (Range::const_iterator i = handles.begin(); i != handles.end(); ++i, ptr += step, lengths += length_step) {
    rval = coll->set_data( *i, *ptr, *lengths );
    if (MB_SUCCESS != rval)
      result = rval;
  }
    
  return result;
}

ErrorCode SparseTagSuperCollection::tag_iterate( TagId tag_handle,
                                              Range::iterator& iter,
                                              const Range::iterator& end,
                                              void*& data_ptr_out,
                                              const void* default_value )

{
    // Note: We are asked to returning a block of contiguous storage
    //       for some block of contiguous handles for which the tag 
    //       storage is also contiguous.  As sparse tag storage is
    //       never contigous, all we can do is return a pointer to the
    //       data for the first entity.

    // If asked for nothing, successfully return nothing.
  if (iter == end)
    return MB_SUCCESS;

  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
    
    // not supported for variable-length tags
  const int length = coll->tag_size();
  if (length == MB_VARIABLE_LENGTH)
    return MB_VARIABLE_DATA_LENGTH;

    // get pointer to tag storage for entity pointed to by iter
  int junk;
  ErrorCode rval = coll->get_data( *iter, data_ptr_out, junk );
  if (MB_SUCCESS != rval) {
      // if no tag value but default_value, then set tag to
      // default_value and return the new tag storage.  Note:
      // it is not sufficient to return the passed default_value
      // pointer because we are returning an non-const pointer that
      // could be modified by the caller.
    if (MB_TAG_NOT_FOUND == rval && default_value) {
      rval = coll->set_data( *iter, default_value );
      if (MB_SUCCESS != rval)
        return rval;
      rval = coll->get_data( *iter, data_ptr_out, junk );
      if (MB_SUCCESS != rval)
        return rval;
    }
    else {
      return rval;
    }
  }
    
    // increment iterator and return
  ++iter;
  return MB_SUCCESS;
}

ErrorCode SparseTagSuperCollection::get_data( TagId tag_handle,
                                                const EntityHandle* handles,
                                                int num_handles,
                                                void* data,
                                                const void* default_value ) const
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
    
  const int length = coll->tag_size();
  if (length == MB_VARIABLE_LENGTH)
    return MB_VARIABLE_DATA_LENGTH;

  ErrorCode rval;
  unsigned char* ptr = reinterpret_cast<unsigned char*>(data);
  const EntityHandle *const end = handles + num_handles;
  for (const EntityHandle* i = handles; i != end; ++i, ptr += length) {
    rval = coll->get_data( *i, ptr );
    if (MB_SUCCESS != rval) {
      if (MB_TAG_NOT_FOUND == rval && default_value) 
        memcpy( ptr, default_value, length );
      else
        return rval;
    }
  }
    
  return MB_SUCCESS;
}

ErrorCode SparseTagSuperCollection::get_data( TagId tag_handle,
                                                const EntityHandle* handles,
                                                int num_handles,
                                                const void** data,
                                                int* lengths,
                                                const void* default_value,
                                                int default_val_length ) const
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  int junk_length;
  int length_step = 1;
  const int length = coll->tag_size();
  if (!lengths) {
    if (length == MB_VARIABLE_LENGTH)
      return MB_VARIABLE_DATA_LENGTH;
    lengths = &junk_length;
    length_step = 0;
  }
  
  
  ErrorCode rval, result = MB_SUCCESS;
  const EntityHandle *const end = handles + num_handles;
  for (const EntityHandle* i = handles; i != end; ++i, ++data, lengths += length_step) {
    void* ptr;
    rval = coll->get_data( *i, ptr, *lengths );
    if (MB_SUCCESS == rval)
      *data = ptr;
    else if (MB_TAG_NOT_FOUND == rval && default_value) {
      *data = default_value;
      *lengths = default_val_length;
    }
    else {
      *data = 0;
      *lengths = 0;
      result = rval;
    }
  }
    
  return result;
}


ErrorCode SparseTagSuperCollection::get_data( TagId tag_handle,
                                                const Range& handles,
                                                void* data,
                                                const void* default_value ) const
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
    
  const int length = coll->tag_size();
  if (length == MB_VARIABLE_LENGTH)
    return MB_VARIABLE_DATA_LENGTH;

  ErrorCode rval;
  unsigned char* ptr = reinterpret_cast<unsigned char*>(data);
  for (Range::const_iterator i = handles.begin(); i != handles.end(); ++i, ptr += length) {
    rval = coll->get_data( *i, ptr );
    if (MB_SUCCESS != rval) {
      if (MB_TAG_NOT_FOUND == rval && default_value) 
        memcpy( ptr, default_value, length );
      else
        return rval;
    }
  }
    
  return MB_SUCCESS;
}

ErrorCode SparseTagSuperCollection::get_data( TagId tag_handle,
                                                const Range& handles,
                                                const void** data,
                                                int* lengths,
                                                const void* default_value,
                                                int default_val_length ) const
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  int junk_length;
  int length_step = 1;
  const int length = coll->tag_size();
  if (!lengths) {
    if (length == MB_VARIABLE_LENGTH)
      return MB_VARIABLE_DATA_LENGTH;
    lengths = &junk_length;
    length_step = 0;
  }
  
  
  ErrorCode rval, result = MB_SUCCESS;
  for (Range::const_iterator i = handles.begin(); i != handles.end(); ++i, ++data, lengths += length_step) {
    void* ptr;
    rval = coll->get_data( *i, ptr, *lengths );
    if (MB_SUCCESS == rval) {
      *data = ptr;
    }
    else if (MB_TAG_NOT_FOUND == rval && default_value) {
      *data = default_value;
      *lengths = default_val_length;
    }
    else {
      *data = 0;
      *lengths = 0;
      result = rval;
    }
  }
    
  return result;
}

ErrorCode SparseTagSuperCollection::remove_data( const TagId tag_handle, 
    const EntityHandle entity_handle )
{
  SparseTagCollection* coll = get_collection(tag_handle);
  return coll ? coll->remove_data( entity_handle ) : MB_TAG_NOT_FOUND;
}


//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_entities(const TagId tag_handle, 
                                                   Range &entities)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  return coll ? coll->get_entities(entities) : MB_TAG_NOT_FOUND;
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_entities(const TagId tag_handle, const EntityType type,
                                                    Range &entities)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  return coll ? coll->get_entities(type, entities) : MB_TAG_NOT_FOUND;
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_entities(const Range &range,
                                                    const TagId tag_handle, const EntityType type,
                                                    Range &entities)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  Range dum_range;
  ErrorCode result = coll->get_entities(type, dum_range);

  std::set_intersection(dum_range.begin(), dum_range.end(),
                        range.begin(), range.end(),
                        range_inserter(entities));
  
  return result;
}

ErrorCode SparseTagSuperCollection::get_tags(const EntityHandle entity,
                                                std::vector<Tag> &all_tags)
{
  for (TagId id = 0; id < mDataTags.size(); ++id)
    if (mDataTags[id] && mDataTags[id]->contains(entity))
      all_tags.push_back( TAG_HANDLE_FROM_ID( id, MB_TAG_SPARSE ) );
  
  return MB_SUCCESS;
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_entities_with_tag_value(
                           const TagId tag_handle, 
                           const TagInfo& tag_info,
                           const EntityType type,
                           Range &entities, 
                           const void* tag_value,
                           int value_size)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;
  
  return coll->get_entities_with_tag_value(tag_info, type, entities, tag_value, value_size);
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_entities_with_tag_value(
                          const Range &range,
                          const TagId tag_handle, 
                          const TagInfo& tag_info,
                          const EntityType type,
                          Range &entities, 
                          const void* tag_value,
                          int value_size)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  if (!coll)
    return MB_TAG_NOT_FOUND;

  Range dum_range;
  ErrorCode result = coll->get_entities_with_tag_value(
                           tag_info, type, dum_range, tag_value, value_size);

    // do this the hard way to preserve order in the vector
  std::set_intersection(range.begin(), range.end(),
                        dum_range.begin(), dum_range.end(),
                        range_inserter(entities));
  
  return result;
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_number_entities(const TagId tag_handle, const EntityType type,
                                                           int& num_entities)
{
  SparseTagCollection* coll = get_collection(tag_handle);
  return coll ? coll->get_number_entities( type, num_entities ) : MB_TAG_NOT_FOUND;
}

//! gets all entity handles that match a type and tag
ErrorCode SparseTagSuperCollection::get_number_entities(const Range &range,
                                                           const TagId tag_handle, const EntityType type,
                                                           int& num_entities)
{
  Range dum_range;
  ErrorCode result = get_entities(range, tag_handle, type, dum_range);
  num_entities = dum_range.size();
  return result;
}


ErrorCode SparseTagSuperCollection::get_memory_use( TagId tag_id,
                                              unsigned long& total,
                                              unsigned long& per_entity )
{
  SparseTagCollection* coll = get_collection(tag_id);
  if (!coll)
    return MB_TAG_NOT_FOUND;

    // 3*sizeof(void*)                      - std::map RB tree node
    // sizeof(void*)*sizeof(EntityHandle) - data in std::map node
    // coll->tag_size()                     - the actual tag data
  per_entity = 4*sizeof(void*)+sizeof(EntityHandle)+coll->tag_size();
    
    // Count number of occupied slots in mDataTags vector
  unsigned num_coll =0;
  for (unsigned i = 0; i < mDataTags.size(); ++i)
    if (mDataTags[i])
      ++num_coll;

    // amortized space in mDataTags vector
  total = sizeof(SparseTagCollection*) * mDataTags.capacity() / num_coll;
    // SparseTagCollection object for this tag
  total += sizeof(SparseTagCollection);
    // Per-entity data in SparseTagCollection
  total += per_entity * coll->get_number_entities();
  return MB_SUCCESS;
}

} // namespace moab




