/*
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**\file MeshSetSequence.cpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2007-04-30
 */

#include "MeshSetSequence.hpp"
#include "SequenceManager.hpp"

namespace moab {

MeshSetSequence::MeshSetSequence( EntityHandle start,
                                  EntityID count,
                                  const unsigned* flags,
                                  SequenceData* dat )
  : EntitySequence( start, count, dat )
{
  initialize( flags );
}

MeshSetSequence::MeshSetSequence( EntityHandle start,
                                  EntityID count,
                                  unsigned flags,
                                  SequenceData* dat )
  : EntitySequence( start, count, dat )
{
  std::vector<unsigned> vect( count, flags );
  initialize( &vect[0] );
}

MeshSetSequence::MeshSetSequence( EntityHandle start,
                                  EntityID count,
                                  const unsigned* flags,
                                  EntityID data_size )
  : EntitySequence( start, count, new SequenceData( 1, start, start+data_size-1) )
{
  initialize( flags );
}

MeshSetSequence::MeshSetSequence( EntityHandle start,
                                  EntityID count,
                                  unsigned flags,
                                  EntityID data_size )
  : EntitySequence( start, count, new SequenceData( 1, start, start+data_size-1) )
{
  std::vector<unsigned> vect( count, flags );
  initialize( &vect[0] );
}

void MeshSetSequence::initialize( const unsigned* flags )
{
  if (!data()->get_sequence_data(0))
    data()->create_sequence_data( 0, SET_SIZE );
 
  EntityID offset = start_handle() - data()->start_handle();
  for (EntityID i = 0; i < size(); ++i)
    allocate_set( flags[i], i+offset );
}

MeshSetSequence::~MeshSetSequence()
{
  EntityID offset = start_handle() - data()->start_handle();
  EntityID count = size();
  for (EntityID i = 0; i < count; ++i)
    deallocate_set( i + offset );
}

EntitySequence* MeshSetSequence::split( EntityHandle here )
{
  return new MeshSetSequence( *this, here );
}

ErrorCode MeshSetSequence::pop_back( EntityID count )
{
  EntityID offset = end_handle() + 1 - count - data()->start_handle();
  ErrorCode rval = EntitySequence::pop_back(count);
  if (MB_SUCCESS == rval)
    for (EntityID i = 0; i < count; ++i)
      deallocate_set( i + offset );
  return rval;
}

ErrorCode MeshSetSequence::pop_front( EntityID count )
{
  EntityID offset = start_handle() - data()->start_handle();
  ErrorCode rval = EntitySequence::pop_front(count);
  if (MB_SUCCESS == rval)
    for (EntityID i = 0; i < count; ++i)
      deallocate_set( i + offset );
  return rval;
}

ErrorCode MeshSetSequence::push_back( EntityID count, const unsigned* flags )
{
  EntityID offset = end_handle() + 1 - data()->start_handle();
  ErrorCode rval = EntitySequence::append_entities( count );
  if (MB_SUCCESS == rval)
    for (EntityID i = 0; i < count; ++i)
      allocate_set( flags[i], i + offset );
  return rval;
}

ErrorCode MeshSetSequence::push_front( EntityID count, const unsigned* flags )
{
  EntityID offset = start_handle() - data()->start_handle() - count;
  ErrorCode rval = EntitySequence::prepend_entities( count );
  if (MB_SUCCESS == rval)
    for (EntityID i = 0; i < count; ++i)
      allocate_set( flags[i], i + offset );
  return rval;
}

void MeshSetSequence::get_const_memory_use( unsigned long& per_ent,
                                            unsigned long& seq_size ) const
{
  per_ent = SET_SIZE;
  seq_size = sizeof(*this);
}

unsigned long MeshSetSequence::get_per_entity_memory_use( EntityHandle first,
                                                          EntityHandle last
                                                        ) const
{
  if (first < start_handle())
    first = start_handle();
  if (last > end_handle())
    last = end_handle();
  
  unsigned long sum = 0;
  for (EntityHandle h = first; h <= last; ++h) 
    sum += get_set(h)->get_memory_use();
  return sum;
}

ErrorCode MeshSetSequence::get_entities( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           Range& entities,
                                           bool recursive ) const
{
  if (!recursive) {
    get_set(handle)->get_entities( entities );
    return MB_SUCCESS;
  }
  else {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_non_set_entities( entities );
    return rval;
  }
}

ErrorCode MeshSetSequence::get_entities( EntityHandle handle,
                                std::vector<EntityHandle>& entities ) const
{
  get_set(handle)->get_entities( entities );
  return MB_SUCCESS;
}

ErrorCode MeshSetSequence::get_dimension( const SequenceManager* seqman,
                                            EntityHandle handle,
                                            int dimension,
                                            std::vector<EntityHandle>& entities,
                                            bool recursive ) const
{
  if (!recursive) {
    get_set(handle)->get_entities_by_dimension( dimension, entities );
    return MB_SUCCESS;
  }
  else {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_entities_by_dimension( dimension, entities );
    return rval;
  }
}

ErrorCode MeshSetSequence::get_dimension( const SequenceManager* seqman,
                                            EntityHandle handle,
                                            int dimension,
                                            Range& entities,
                                            bool recursive ) const
{
  if (!recursive) {
    get_set(handle)->get_entities_by_dimension( dimension, entities );
    return MB_SUCCESS;
  }
  else {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_entities_by_dimension( dimension, entities );
    return rval;
  }
}

ErrorCode MeshSetSequence::get_type( const SequenceManager* seqman,
                                       EntityHandle handle,
                                       EntityType tp,
                                       std::vector<EntityHandle>& entities,
                                       bool recursive ) const
{
  if (!recursive) {
    get_set(handle)->get_entities_by_type( tp, entities );
    return MB_SUCCESS;
  }
  else if (tp == MBENTITYSET) {
    return recursive_get_sets( handle, seqman, 0, 0, &entities );
  }
  else if (tp == MBMAXTYPE) {
    Range tmp;
    ErrorCode rval = get_entities( seqman, handle, tmp, recursive );
    if (MB_SUCCESS == rval) {
#ifdef NO_VECTOR_TEMPLATE_INSERT
      std::copy( tmp.begin(), tmp.end(), std::back_inserter(entities) );
#else
      entities.insert( entities.end(), tmp.begin(), tmp.end() );
#endif
    }
    return rval;
  }
  else {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_entities_by_type( tp, entities );
    return rval;
  }
}

ErrorCode MeshSetSequence::get_type( const SequenceManager* seqman,
                                       EntityHandle handle,
                                       EntityType tp,
                                       Range& entities,
                                       bool recursive ) const
{
  if (!recursive) {
    get_set(handle)->get_entities_by_type( tp, entities );
    return MB_SUCCESS;
  }
  else if (tp == MBENTITYSET) {
    return recursive_get_sets( handle, seqman, 0, &entities );
  }
  else if (tp == MBMAXTYPE) {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_non_set_entities( entities );
    return rval;
  }
  else {
    std::vector<const MeshSet*> list;
    ErrorCode rval = recursive_get_sets( handle, seqman, &list );
    for (std::vector<const MeshSet*>::iterator i = list.begin(); i != list.end(); ++i)
      (*i)->get_entities_by_type( tp, entities );
    return rval;
  }
}

ErrorCode MeshSetSequence::num_entities( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           int& number,
                                           bool recursive ) const
{
  if (!recursive) {
    number = get_set(handle)->num_entities();
    return MB_SUCCESS;
  }
  else {
    Range range;
    ErrorCode result = get_entities( seqman, handle, range, true );
    number = range.size();
    return result;
  }
}

ErrorCode MeshSetSequence::num_dimension( const SequenceManager* seqman,
                                            EntityHandle handle,
                                            int dimension,
                                            int& number,
                                            bool recursive ) const
{
  if (!recursive) {
    number = get_set(handle)->num_entities_by_dimension(dimension);
    return MB_SUCCESS;
  }
  else {
    Range range;
    ErrorCode result = get_dimension( seqman, handle, dimension, range, true );
    number = range.size();
    return result;
  }
}
 
ErrorCode MeshSetSequence::num_type( const SequenceManager* seqman,
                                       EntityHandle handle,
                                       EntityType tp,
                                       int& number,
                                       bool recursive ) const
{
  if (!recursive) {
    number = get_set(handle)->num_entities_by_type(tp);
    return MB_SUCCESS;
  }
  else {
    Range range;
    ErrorCode result = get_type( seqman, handle, tp, range, true );
    number = range.size();
    return result;
  }
}

ErrorCode MeshSetSequence::recursive_get_sets( EntityHandle start_set,
                              const SequenceManager* seq_sets,
                              std::vector<const MeshSet*>* sets,
                              Range* set_handles,
                              std::vector<EntityHandle>* set_vector )
{
  std::set<EntityHandle> visited;
  std::vector<EntityHandle> stack;
  stack.push_back( start_set );
  bool remove_start_set = true;
  while (!stack.empty()) {
    EntityHandle handle = stack.back();
    stack.pop_back();
    
    if (!visited.insert(handle).second) {
      if (handle == start_set)
        remove_start_set = false;
      continue;
    }
    
    const EntitySequence* seq;
    ErrorCode rval = seq_sets->find( handle, seq );
    if (MB_SUCCESS != rval)
      return rval;
    
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    const MeshSet *ms_ptr = mseq->get_set( handle );
    if (sets)
      sets->push_back( ms_ptr );
    
    Range tmp_range;
    ms_ptr->get_entities_by_type( MBENTITYSET, tmp_range );
    std::copy( tmp_range.begin(), tmp_range.end(), std::back_inserter( stack ) );
  }
  
  if (set_handles) {
    if (remove_start_set)
      visited.erase( start_set );
    Range::iterator hint = set_handles->begin();
    std::set<EntityHandle>::iterator it;
    for (it = visited.begin(); it != visited.end(); ++it)
      hint = set_handles->insert( hint, *it, *it );
  }
  
  if (set_vector) {
    if (remove_start_set)
      visited.erase( start_set );
    std::copy( visited.begin(), visited.end(), std::back_inserter(*set_vector) );
  }
    
  return MB_SUCCESS;
}

ErrorCode MeshSetSequence::recursive_get_sets( EntityHandle start_set,
                              SequenceManager* seq_sets,
                              std::vector<MeshSet*>& sets )
{
  std::set<EntityHandle> visited;
  std::vector<EntityHandle> stack;
  stack.push_back( start_set );
  while (!stack.empty()) {
    EntityHandle handle = stack.back();
    stack.pop_back();
    
    if (!visited.insert(handle).second)
      continue;
    
    EntitySequence* seq;
    ErrorCode rval = seq_sets->find( handle, seq );
    if (MB_SUCCESS != rval)
      return rval;
    
    MeshSetSequence* mseq = reinterpret_cast<MeshSetSequence*>(seq);
    MeshSet *ms_ptr = mseq->get_set( handle );
    sets.push_back( ms_ptr );
    
    Range tmp_range;
    ms_ptr->get_entities_by_type( MBENTITYSET, tmp_range );
    std::copy( tmp_range.begin(), tmp_range.end(), std::back_inserter( stack ) );
  }
    
  return MB_SUCCESS;
}

ErrorCode MeshSetSequence::get_parent_child_meshsets( EntityHandle meshset,
                                    const SequenceManager* seq_sets,
                                    std::vector<EntityHandle>& results,
                                    int num_hops, SearchType link_type ) const
{
  ErrorCode result = MB_SUCCESS;
  std::vector<EntityHandle>::iterator i;
  const EntityHandle *tmp_array = 0, *end;
  EntityHandle s, e;
  int count = 0;
  size_t n;
  
    // Skip any meshsets already in input vector (yes, don't
    // get their children either even if num_hops would indicate
    // that we should.)  There is an exception to that if the
    // input meshset is in the list, which is handled by the order
    // of checks in the main loop below.
  std::set<EntityHandle> visited;
  for (i = results.begin(); i != results.end(); ++i)
    visited.insert( *i );
    
    // Two lists for breadth-first search
  std::vector<EntityHandle> lists[2];
  int index = 0;  // which list to read from (write to lists[1-index])
  lists[index].push_back( meshset ); // begin with input set
    // loop for num_hops (or until no more sets)
  for( ; num_hops && !lists[index].empty(); --num_hops) {
      // for each set at the current num_hops
    for (i = lists[index].begin(); i != lists[index].end(); ++i) {
        // get meshset from handle
      const EntitySequence* seq;
      ErrorCode rval = seq_sets->find( *i, seq );
      if (MB_SUCCESS != rval)
        return rval;
      const MeshSet *ms_ptr = reinterpret_cast<const MeshSetSequence*>(seq)->get_set( *i );
      
      
      switch (link_type) {
      case CONTAINED:
        tmp_array = ms_ptr->get_contents(n);
        end = tmp_array + n;
        if (ms_ptr->vector_based()) {
          for (; tmp_array != end; ++tmp_array) 
            if (MBENTITYSET == TYPE_FROM_HANDLE(*tmp_array) &&
                visited.insert(*tmp_array).second) 
              lists[1-index].push_back(*tmp_array);
        }
        else {
          assert(n%2 == 0);
          tmp_array = std::lower_bound( tmp_array, tmp_array+n, FIRST_HANDLE(MBENTITYSET) );
            // only part of first block is of type
          if ((end - tmp_array)%2) {
            ++tmp_array;
            s = FIRST_HANDLE(MBENTITYSET);
            e = *tmp_array;
            for (; s <= e; ++s) 
              if (visited.insert(s).second)
                lists[1-index].push_back(s);
          }
          while (tmp_array < end) {
            s = *tmp_array++;
            e = *tmp_array++;
            for (; s <= e; ++s) 
              if (visited.insert(s).second)
                lists[1-index].push_back(s);
          }
        }
        continue;
      case PARENTS:
        tmp_array = ms_ptr->get_parents(count);
        break;
      case CHILDREN:
        tmp_array = ms_ptr->get_children(count);
        break;
      }
      
        // copy any parents/children we haven't visited yet into list
      for (end = tmp_array+count; tmp_array != end; ++tmp_array) 
        if (visited.insert(*tmp_array).second) 
          lists[1-index].push_back(*tmp_array);
    }
    
      // iterate
    lists[index].clear();
    index = 1 - index;
      // append each level of sets to the output list.
      // note: to make a more useful search (e.g. get entities 3 hops away, 
      // rather than entities up to and including 3 hops) move this outside
      // the loop, but then need to handle the get all (num_hops < 0) case
      // specially.
    std::copy( lists[index].begin(), lists[index].end(), std::back_inserter(results) );
  }
  
  return result;
}

ErrorCode MeshSetSequence::get_parents( const SequenceManager* seqman,
                                          EntityHandle handle,
                                          std::vector<EntityHandle>& parents,
                                          int num_hops ) const
{
  if (num_hops == 1) {
    int count;
    const EntityHandle* tmp_array = get_set( handle )->get_parents(count);  
    if (parents.empty()) {
      parents.resize(count);
      std::copy( tmp_array, tmp_array + count, parents.begin() );
      return MB_SUCCESS;
    }
    else if (!count) {
      return MB_SUCCESS;
    }
  }
  
  if (num_hops > 0)
    return get_parent_child_meshsets( handle, seqman, parents, num_hops, PARENTS );
  else
    return get_parent_child_meshsets( handle, seqman, parents, -1, PARENTS );
}

ErrorCode MeshSetSequence::get_children( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           std::vector<EntityHandle>& children,
                                           int num_hops ) const
{
  if (num_hops == 1) {
    int count;
    const EntityHandle* tmp_array = get_set( handle )->get_children(count);  
    if (children.empty()) {
      children.resize(count);
      std::copy( tmp_array, tmp_array + count, children.begin() );
      return MB_SUCCESS;
    }
    else if (!count) {
      return MB_SUCCESS;
    }
  }

  if (num_hops > 0) 
    return get_parent_child_meshsets( handle, seqman, children, num_hops, CHILDREN );
  else 
    return get_parent_child_meshsets( handle, seqman, children, -1, CHILDREN );
}

ErrorCode MeshSetSequence::get_contained_sets( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           std::vector<EntityHandle>& contained,
                                           int num_hops ) const
{
  if (num_hops == 1 && contained.empty()) {
    return get_set(handle)->get_entities_by_type( MBENTITYSET, contained );
  }

  if (num_hops > 0) 
    return get_parent_child_meshsets( handle, seqman, contained, num_hops, CONTAINED );
  else 
    return get_parent_child_meshsets( handle, seqman, contained, -1, CONTAINED );
}

ErrorCode MeshSetSequence::num_parents( const SequenceManager* seqman,
                                          EntityHandle handle,
                                          int& number,
                                          int num_hops ) const
{
  if (num_hops == 1) {
    number = get_set( handle )->num_parents();
    return MB_SUCCESS;
  }
  
  std::vector<EntityHandle> parents;
  ErrorCode result = get_parents( seqman, handle, parents, num_hops );
  number = parents.size();
  return result;
}


ErrorCode MeshSetSequence::num_children( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           int& number,
                                           int num_hops ) const
{
  if (num_hops == 1) {
    number = get_set( handle )->num_children();
    return MB_SUCCESS;
  }
  
  std::vector<EntityHandle> children;
  ErrorCode result = get_children( seqman, handle, children, num_hops );
  number = children.size();
  return result;
}

ErrorCode MeshSetSequence::num_contained_sets( const SequenceManager* seqman,
                                           EntityHandle handle,
                                           int& number,
                                           int num_hops ) const
{
  if (num_hops == 1) {
    number = get_set( handle )->num_entities_by_type(MBENTITYSET);
    return MB_SUCCESS;
  }
  
  std::vector<EntityHandle> contained;
  ErrorCode result = get_contained_sets( seqman, handle, contained, num_hops );
  number = contained.size();
  return result;
}
  
} // namespace moab
