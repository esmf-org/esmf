#include "TypeSequenceManager.hpp"
#include "SequenceData.hpp"
#include "moab/Error.hpp"
#include <assert.h>
#include <limits>

namespace moab {

TypeSequenceManager::~TypeSequenceManager()
{
  // We assume that for there to be multiple sequences referencing
  // the same SequenceData, there must be some portion of the
  // SequenceData that is unused. Otherwise the sequences should
  // have been merged. Given that assumption, it is the case that
  // either a) a SequenceData is in availableList or b) the
  // SequenceData is referenced by exactly one sequence.

  // Delete every entity sequence
  for (iterator i = begin(); i != end(); ++i) {
    EntitySequence* seq = *i;
    // Check for case b) above
    if (seq->using_entire_data()) {
      // Delete sequence before data, because sequence
      // has a pointer to data and may try to dereference
      // that pointer during its destruction.
      SequenceData* data = seq->data();
      delete seq;
      delete data;
    }
    else {
      delete seq;
    }
  }
  sequenceSet.clear();

  // Case a) above
  for (data_iterator i = availableList.begin(); i != availableList.end(); ++i)
    delete *i;
  availableList.clear();
}

ErrorCode TypeSequenceManager::merge_internal(iterator i, iterator j)
{
  EntitySequence* dead = *j;
  sequenceSet.erase(j);
  ErrorCode rval = (*i)->merge(*dead);
  if (MB_SUCCESS != rval) {
    sequenceSet.insert(dead);
    return rval;
  }

  if (lastReferenced == dead)
    lastReferenced = *i;
  delete dead;

  // If merging results in no unused portions of the SequenceData,
  // remove it from the available list.
  if ((*i)->using_entire_data())
    availableList.erase((*i)->data());

  return MB_SUCCESS;
}

ErrorCode TypeSequenceManager::check_merge_next(iterator i)
{
  iterator j = i; ++j;
  if (j == end() || (*j)->data() != (*i)->data() || 
     (*j)->start_handle() > (*i)->end_handle() + 1)
    return MB_SUCCESS;

  assert((*i)->end_handle() + 1 == (*j)->start_handle());
  return merge_internal(i, j);
}

ErrorCode TypeSequenceManager::check_merge_prev(iterator i)
{
  if (i == begin())
    return MB_SUCCESS;

  iterator j = i; --j;
  if ((*j)->data() != (*i)->data() ||
     (*j)->end_handle() + 1 < (*i)->start_handle())
    return MB_SUCCESS;

  assert((*j)->end_handle() + 1 == (*i)->start_handle());
  return merge_internal(i, j);
}

ErrorCode TypeSequenceManager::insert_sequence(EntitySequence* seq_ptr)
{
  if (!seq_ptr->data())
    return MB_FAILURE;

  if (seq_ptr->data()->start_handle() > seq_ptr->start_handle() ||
      seq_ptr->data()->end_handle() < seq_ptr->end_handle() ||
      seq_ptr->end_handle() < seq_ptr->start_handle())
    return MB_FAILURE;

  iterator i = lower_bound(seq_ptr->start_handle());
  if (i != end()) {
    if ((*i)->start_handle() <= seq_ptr->end_handle())
      return MB_ALREADY_ALLOCATED;
    if (seq_ptr->data() != (*i)->data() &&
        (*i)->data()->start_handle() <= seq_ptr->data()->end_handle())
      return MB_ALREADY_ALLOCATED;
  }

  if (i != begin()) {
    iterator j = i; --j;
    if (seq_ptr->data() != (*j)->data() &&
        (*j)->data()->end_handle() >= seq_ptr->data()->start_handle())
      return MB_ALREADY_ALLOCATED;
  }

  i = sequenceSet.insert(i, seq_ptr);

  // Merge with previous sequence ?
  if (seq_ptr->start_handle() > seq_ptr->data()->start_handle() && i != begin()) {
    if (MB_SUCCESS != check_merge_prev(i)) {
      sequenceSet.erase(i);
      return MB_FAILURE;
    }
  }

  // Merge with next sequence ?
  if ((*i)->end_handle() < (*i)->data()->end_handle()) {
    if (MB_SUCCESS != check_merge_next(i)) {
      sequenceSet.erase(i);
      return MB_FAILURE;
    }
  }

  // We merged adjacent sequences sharing a SequenceData, so
  // we can safely assume that unless this EntitySequence is
  // using the entire SequenceData, there are unused portions.
  if (!seq_ptr->using_entire_data()) 
    availableList.insert(seq_ptr->data());

  // lastReferenced is only allowed to be NULL if there are
  // no sequences (avoids unnecessary if's in fast path).
  if (!lastReferenced)
    lastReferenced = seq_ptr;

  // Each SequenceData has a pointer to the first EntitySequence
  // referencing it. Update that pointer if the new sequence is
  // the first one.
  if ((*i)->start_handle() == (*i)->data()->start_handle() ||
      lower_bound((*i)->data()->start_handle()) == i)
    (*i)->data()->seqManData.firstSequence = i;

  assert(check_valid_data(seq_ptr));
  return MB_SUCCESS;
}

ErrorCode TypeSequenceManager::replace_subsequence(EntitySequence* seq_ptr,
                                                   const int* tag_sizes,
                                                   int num_tag_sizes)
{
  // Find the sequence of interest
  iterator i = lower_bound(seq_ptr->start_handle());
  if (i == end() || (*i)->data() == seq_ptr->data())
    return MB_FAILURE;
  // New sequence must be a subset of an existing one
  if (seq_ptr->start_handle() < (*i)->start_handle() ||
      seq_ptr->end_handle() > (*i)->end_handle())
    return MB_FAILURE;
  // New sequence's data must be new also, and cannot intersect
  // any existing sequence (just require that the data range
  // matches the sequence range for now)
  if (!seq_ptr->using_entire_data())
    return MB_FAILURE;
  // Copy tag data (move ownership of var-len data)
  SequenceData* const dead_data = (*i)->data();
  dead_data->move_tag_data(seq_ptr->data(), tag_sizes, num_tag_sizes);

  // Split sequences sharing old data into two groups:
  // p->i : first sequence to i
  // i->n : i to one past last sequence
  iterator p, n = i;
  p = (*i)->data()->seqManData.firstSequence;
  for (++n; n != end() && (*n)->data() == (*i)->data(); ++n);

  // First subdivide EntitySequence as necessary
  // Move i to be the first sequence past the insertion point
  // such that the new order will be:
  // [p,i-1] seq_ptr [i,n]
  // where p == i if no previous sequence

  // Four possible cases:
  // 0. All entities in sequence are in new sequence
  // 1. Old entities in sequence before and after new sequence,
  //    requiring sequence to be split.
  // 2. Old entities after new sequence
  // 3. Old entities before new sequence
  const bool some_before = ((*i)->start_handle() < seq_ptr->start_handle());
  const bool some_after  = ((*i)->  end_handle() > seq_ptr->  end_handle());
  // Case 0
  if (!(some_before || some_after)) {
    // Remove dead sequence from internal lists
    EntitySequence* seq = *i;
    iterator dead = i; ++i;
    if (p == dead)
      p = i;
    sequenceSet.erase(dead);

    // Delete old sequence
    delete seq;
    // Make sure lastReferenced isn't stale
    if (lastReferenced == seq)
      lastReferenced = seq_ptr;
  }
  // Case 1
  else if (some_before && some_after) {
    i = split_sequence(i, seq_ptr->start_handle());
    (*i)->pop_front(seq_ptr->size());
  }
  // Case 2
  else if (some_after) {
    (*i)->pop_front(seq_ptr->size());
  }
  // Case 3
  else { // some_before
    (*i)->pop_back(seq_ptr->size());
    ++i;
  }

  // Now subdivide the underlying sequence data as necessary
  availableList.erase(dead_data);
  if (p != i) {
    iterator last = i; --last;
    SequenceData* new_data = (*p)->create_data_subset((*p)->start_handle(), (*last)->end_handle());
    new_data->seqManData.firstSequence = p;

    for (; p != i; ++p)
      (*p)->data(new_data);
    // Copy tag data (move ownership of var-len data)
    dead_data->move_tag_data(new_data, tag_sizes, num_tag_sizes);
    if (!(*new_data->seqManData.firstSequence)->using_entire_data())
      availableList.insert(new_data);
  }
  if (i != n) {
    iterator last = n; --last;
    SequenceData* new_data = (*i)->create_data_subset((*i)->start_handle(), (*last)->end_handle());
    new_data->seqManData.firstSequence = i;
    for (; i != n; ++i)
      (*i)->data(new_data);
    // Copy tag data (move ownership of var-len data)
    dead_data->move_tag_data(new_data, tag_sizes, num_tag_sizes);
    if (!(*new_data->seqManData.firstSequence)->using_entire_data())
      availableList.insert(new_data);
  }
  delete dead_data;

  // Put new sequence in lists
  return insert_sequence(seq_ptr);
}

TypeSequenceManager::iterator TypeSequenceManager::erase(iterator i)
{
  EntitySequence* seq = *i;
  SequenceData* data = seq->data();
  iterator j;

  // Check if we need to delete the referenced SequenceData also
  bool delete_data;
  if (seq->using_entire_data()) // Only sequence
    delete_data = true;
  else if (data->seqManData.firstSequence != i) { // Earlier sequence?
    delete_data = false;
    availableList.insert(data);
  }
  else { // Later sequence ?
    j = i; ++j;
    delete_data = (j == end() || (*j)->data() != data);
    if (delete_data)
      availableList.erase(data);
    else {
      availableList.insert(data);
      data->seqManData.firstSequence = j;
    }
  }

  // Remove sequence, updating i to be next sequence
  j = i++;
  sequenceSet.erase(j);

  // Make sure lastReferenced isn't stale. It can only be NULL if
  // no sequences.
  if (lastReferenced == seq)
    lastReferenced = sequenceSet.empty() ? 0 : *sequenceSet.begin();

  // Always delete sequence before the SequenceData it references.
  assert(0 == find(seq->start_handle()));
  delete seq;
  if (delete_data)
    delete data;
  else {
    assert(check_valid_data(*data->seqManData.firstSequence));
    assert(lastReferenced != seq);
  }
  return i;
}

ErrorCode TypeSequenceManager::remove_sequence(const EntitySequence* seq_ptr,
                                               bool& unreferenced_data)
{
  // Remove sequence from set
  iterator i = lower_bound(seq_ptr->start_handle());
  if (i == end() || *i != seq_ptr)
    return MB_ENTITY_NOT_FOUND;
  sequenceSet.erase(i);

  // Check if this is the only sequence referencing its data
  if (seq_ptr->using_entire_data()) 
    unreferenced_data = true;
  else {
    i = lower_bound(seq_ptr->data()->start_handle());
    unreferenced_data = i == end() || (*i)->data() != seq_ptr->data();
    if (unreferenced_data)
      availableList.erase(seq_ptr->data());
    else
      seq_ptr->data()->seqManData.firstSequence = i; // Might be 'i' already
  }

  if (lastReferenced == seq_ptr)
    lastReferenced = sequenceSet.empty() ? 0 : *sequenceSet.begin();

  return MB_SUCCESS;
}

TypeSequenceManager::iterator
TypeSequenceManager::find_free_handle(EntityHandle min_start_handle,
                                      EntityHandle max_end_handle,
                                      bool& append_out,
                                      int values_per_ent)
{
  for (data_iterator i = availableList.begin(); i != availableList.end(); ++i) {
    if ((*(*i)->seqManData.firstSequence)->values_per_entity() != values_per_ent)
      continue;

    if ((*i)->start_handle() > max_end_handle || (*i)->end_handle() < min_start_handle)
      continue;

    for (iterator j = (*i)->seqManData.firstSequence;
         j != end() && (*j)->start_handle() <= (max_end_handle + 1) && (*j)->data() == *i;
         ++j) {
      if ((*j)->end_handle() + 1 < min_start_handle)
        continue;
      if ((*j)->start_handle() > (*i)->start_handle() &&
          (*j)->start_handle() > min_start_handle) {
        append_out = false;
        return j;
      }
      if ((*j)->end_handle() < (*i)->end_handle() &&
          (*j)->end_handle() < max_end_handle) {
        append_out = true;
        return j;
      }
    }
  }

  return end();
}

bool TypeSequenceManager::is_free_sequence(EntityHandle start,
                                           EntityID num_entities,
                                           SequenceData*& data_out,
                                           int values_per_ent)
{
  data_out = 0;
  if (empty())
    return true;

  const_iterator i = lower_bound(start);
  if (i == end()) {
    --i; // Safe because already tested empty()
    // If we don't overlap the last data object...
    if ((*i)->data()->end_handle() < start)
      return true;
    data_out = (*i)->data();
    if ((*i)->values_per_entity() != values_per_ent)
      return false;
    // If we overlap a data object, we must be entirely inside of it
    return start + num_entities - 1 <= (*i)->data()->end_handle();
  }

#ifndef NDEBUG
  if (i != begin()) {
    const_iterator j = i;
    --j;
    assert((*j)->end_handle() < start);
  }
#endif

  // Check if we fit in the block of free handles
  if (start + num_entities > (*i)->start_handle()) // start + num + 1 >= i->start
    return false;

  // Check if we overlap the data for the next sequence
  if (start + num_entities > (*i)->data()->start_handle()) {
    data_out = (*i)->data();
    if ((*i)->values_per_entity() != values_per_ent)
      return false;
    // If overlap, must be entirely contained
    return start >= data_out->start_handle() &&
           start + num_entities - 1 <= data_out->end_handle();
  }

  // Check if we overlap the data for the previous sequence
  if (i != begin()) {
    --i;
    if ((*i)->data()->end_handle() >= start) {
      data_out = (*i)->data();
      if ((*i)->values_per_entity() != values_per_ent)
        return false;
      return start + num_entities - 1 <= (*i)->data()->end_handle();
    }
  }

  // Unused handle block that overlaps no SequenceData
  return true;
}

EntityHandle TypeSequenceManager::find_free_block(EntityID num_entities,
                                                  EntityHandle min_start_handle,
                                                  EntityHandle max_end_handle)
{
  const_iterator i = lower_bound(min_start_handle);
  if (i == end())
    return min_start_handle;

  if ((*i)->start_handle() < min_start_handle + num_entities)
    return min_start_handle;

  EntityHandle prev_end = (*i)->end_handle(); ++i;
  for (; i != end(); prev_end = (*i)->end_handle(), ++i) {
    EntityID len = (*i)->start_handle() - prev_end - 1;
    if (len >= num_entities)
      break;
  }

  if (prev_end + num_entities > max_end_handle)
    return 0;
  else
    return prev_end + 1;
}

struct range_data {
  EntityID num_entities;
  EntityHandle min_start_handle, max_end_handle;
  EntityHandle first, last;
};

static bool check_range(const range_data& d,
                        bool prefer_end,
                        EntityHandle& result)
{
  EntityHandle first = std::max(d.min_start_handle, d.first);
  EntityHandle  last = std::min(d.max_end_handle, d.last);
  if (last < first + d.num_entities - 1) {
    result = 0;
    return false;
  }

  result = prefer_end ? last + 1 - d.num_entities : first;
  return true;
}

EntityHandle TypeSequenceManager::find_free_sequence(EntityID num_entities,
                                                     EntityHandle min_start_handle,
                                                     EntityHandle max_end_handle,
                                                     SequenceData*& data_out,
                                                     EntityID &data_size,
                                                     int num_verts)
{
  if (max_end_handle < min_start_handle + num_entities - 1)
    return 0;

  EntityHandle result;
  iterator p, i = lower_bound(min_start_handle);
  range_data d = {num_entities, min_start_handle, max_end_handle, 0, 0};

  if (i == end()) {
    data_out = 0;
    return min_start_handle;
  }
  else if (i == begin()) {
    if ((*i)->values_per_entity() == num_verts) {
      d.first = (*i)->data()->start_handle();
      d.last  = (*i)->start_handle() - 1;
      if (check_range(d, true, result)) {
        data_out = (*i)->data();
        return result;
      }
    }
    d.first = min_start_handle;
    d.last = (*i)->data()->start_handle() - 1;
    if (check_range(d, true, result)) {
      data_out = 0;
      // This will back up against the end of the seq data, so
      // size the data that way
      data_size = num_entities;
      return result;
    }
    p = i++;
  }
  else {
    p = i;
    --p;
  }

  for (; i != end() && (*i)->start_handle() < max_end_handle; p = i++) {
    if ((*p)->data() == (*i)->data()) {
      if ((*p)->values_per_entity() == num_verts) {
        d.first = (*p)->end_handle() + 1;
        d.last = (*i)->start_handle() - 1;
        if (check_range(d, false, result)) {
          data_out = (*p)->data();
          return result;
        }
      }
    }
    else {
      if ((*p)->values_per_entity() == num_verts) {
        d.first = (*p)->end_handle() + 1;
        d.last = (*p)->data()->end_handle();
        if (check_range(d, false, result)) {
          data_out = (*p)->data();
          return result;
        }
      }
      if ((*i)->values_per_entity() == num_verts) {
        d.first = (*i)->data()->start_handle();
        d.last = (*i)->start_handle() - 1;
        if (check_range(d, true, result)) {
          data_out = (*i)->data();
          return result;
        }
      }
      d.first = (*p)->data()->end_handle() + 1;
      d.last  = (*i)->data()->start_handle() - 1;
      if (check_range(d, false, result)) {
        data_out = 0;
        data_size = d.last - d.first + 1;
        return result;
      }
    }
  }

  if ((*p)->values_per_entity() == num_verts) {
    d.first = (*p)->end_handle() + 1;
    d.last = (*p)->data()->end_handle();
    if (check_range(d, false, result)) {
      data_out = (*p)->data();
      return result;
    }
  }

  d.first = (*p)->data()->end_handle() + 1;
  d.last = max_end_handle;
  if (check_range(d, false, result)) {
    data_out = 0;
    return result;
  }

  data_out = 0;
  return 0;
}

EntityHandle TypeSequenceManager::last_free_handle(EntityHandle after_this) const
{
  int junk;
  const_iterator it = lower_bound(after_this);
  if (it == end())
    return CREATE_HANDLE(TYPE_FROM_HANDLE(after_this), MB_END_ID, junk);
  else if ((*it)->start_handle() > after_this) {
    // Need to check against the sequence data first
    EntityHandle rhandle = (*it)->data()->start_handle();
    return rhandle - 1;
  }
  else
    return 0;
}

ErrorCode TypeSequenceManager::check_valid_handles(Error* /* error_handler */,
                                                   EntityHandle first,
                                                   EntityHandle last) const
{
  const_iterator i = lower_bound(first);
  if (i == end() || (*i)->start_handle() > first) {
#if 0
    // MB_ENTITY_NOT_FOUND could be a non-error condition, do not call
    // MB_SET_ERR on it
    fprintf(
      stderr,
      "[Warning]: Invalid entity handle: 0x%lx\n", (unsigned long)first
      );
#endif
    return MB_ENTITY_NOT_FOUND;
  }

  while ((*i)->end_handle() < last) {
    EntityHandle prev_end = (*i)->end_handle();
    ++i;
    if (i == end() || prev_end + 1 != (*i)->start_handle())
      return MB_ENTITY_NOT_FOUND;
  }

  return MB_SUCCESS;
}

ErrorCode TypeSequenceManager::erase(Error* /* error_handler */, EntityHandle h)
{
  EntitySequence* seq = find(h);
  if (!seq) {
#if 0
    // MB_ENTITY_NOT_FOUND could be a non-error condition, do not call
    // MB_SET_ERR on it
    fprintf(
      stderr,
      "[Warning]: Invalid entity handle: 0x%lx\n", (unsigned long)h
      );
#endif
    return MB_ENTITY_NOT_FOUND;
  }

  if (seq->start_handle() == h) {
    if (seq->end_handle() != h) {
      if (seq->using_entire_data())
        availableList.insert(seq->data());
      seq->pop_front(1);
      return MB_SUCCESS;
    }
    SequenceData* data = seq->data();
    bool delete_data;
    ErrorCode rval = remove_sequence(seq, delete_data);
    if (MB_SUCCESS != rval)
      return rval;
    delete seq;
    if (delete_data)
      delete data;
  }
  else if (seq->end_handle() == h) {
    if (seq->using_entire_data())
      availableList.insert(seq->data());
    seq->pop_back(1);
  }
  else {
    iterator i = lower_bound(h);
    if ((*i)->using_entire_data())
      availableList.insert((*i)->data());
    i = split_sequence(i, h);
    seq = *i;
    assert(seq->start_handle() == h);
    seq->pop_front(1);
  }

  return MB_SUCCESS;
}

ErrorCode TypeSequenceManager::erase(Error* /* error */, EntityHandle first, EntityHandle last)
{
  // First check that all entities in range are valid

  ErrorCode rval = check_valid_handles(NULL, first, last);
  if (MB_SUCCESS != rval)
    return rval;

  // Now remove entities

  // Get first sequence intersecting range
  iterator i = lower_bound(first);
  if (i == end()) // Shouldn't be possible given check_valid_handles call above.
    return MB_ENTITY_NOT_FOUND;

  // If range is entirely in interior of sequence, need to split sequence.
  if ((*i)->start_handle() < first && (*i)->end_handle() > last) {
    if ((*i)->using_entire_data())
      availableList.insert((*i)->data());
    i = split_sequence(i, first);
    (*i)->pop_front(last - first + 1);
    assert(check_valid_data(*i));
    return MB_SUCCESS;
  }

  // If range doesn't entirely contain first sequence, remove some
  // handles from the end of the sequence and advance to the next
  // sequence.
  if ((*i)->start_handle() < first) {
    if ((*i)->using_entire_data())
      availableList.insert((*i)->data());
    (*i)->pop_back((*i)->end_handle() - first + 1);
    ++i;
  }

  // Destroy all sequences contained entirely within the range
  while (i != end() && (*i)->end_handle() <= last)
    i = erase(i);

  // If necessary, remove entities from the beginning of the
  // last sequence.
  if (i != end() && (*i)->start_handle() <= last) {
    if ((*i)->using_entire_data())
      availableList.insert((*i)->data());
    (*i)->pop_front(last - (*i)->start_handle() + 1);
    assert(check_valid_data(*i));
  }

  return MB_SUCCESS;
}

TypeSequenceManager::iterator TypeSequenceManager::split_sequence(iterator i, EntityHandle h)
{
  EntitySequence* seq = (*i)->split(h);
  if (!seq)
    return end();

  i = sequenceSet.insert(i, seq);
  assert(check_valid_data(*i));

  return i;
}

ErrorCode TypeSequenceManager::is_free_handle(EntityHandle handle,
                                              iterator& seq_iter_out,
                                              SequenceData*& data_ptr_out,
                                              EntityHandle& block_start,
                                              EntityHandle& block_end,
                                              int values_per_ent)
{
  int junk;
  block_start = CREATE_HANDLE(TYPE_FROM_HANDLE(handle), MB_START_ID, junk);
  block_end   = CREATE_HANDLE(TYPE_FROM_HANDLE(handle), MB_END_ID,   junk);

  iterator i = lower_bound(handle);
  if (i != end()) {
    block_end = (*i)->start_handle() - 1;

    // If sequence contains handle, then already allocated
    if ((*i)->start_handle() <= handle)
      return MB_ALREADY_ALLOCATED;

    // Handle is not within an existing sequence, but is
    // within an existing SequenceData...
    if ((*i)->data()->start_handle() <= handle) {
      // If values_per_entity don't match, can't put new entity
      // in existing SequenceData
      if ((*i)->values_per_entity() != values_per_ent)
        return MB_ALREADY_ALLOCATED;

      data_ptr_out = (*i)->data();
      if (block_end == handle) {
        // Prepend to existing sequence
        seq_iter_out = i;
        block_start = handle;
      }
      else {
        // Add new sequence to existing SequenceData
        seq_iter_out = end();
        if (i == begin() || (*--i)->data() != data_ptr_out)
          block_start = data_ptr_out->start_handle();
        else 
          block_start = (*i)->end_handle() + 1;
      }
      return MB_SUCCESS;
    }
  }

  if (i != begin()) {
    --i;
    block_start = (*i)->end_handle() + 1;

    // Handle is within previous sequence data...
    if ((*i)->data()->end_handle() >= handle) {
      // If values_per_entity don't match, can't put new entity
      // in existing SequenceData
      if ((*i)->values_per_entity() != values_per_ent)
        return MB_ALREADY_ALLOCATED;

      data_ptr_out = (*i)->data();
      if (block_start == handle) {
        // Append to existing sequence
        seq_iter_out = i;
        block_end = handle;
      }
      else {
        // Add new sequence to existing SequenceData
        seq_iter_out = end();
        if (++i == end() || (*i)->data() != data_ptr_out)
          block_end = data_ptr_out->end_handle();
        else
          block_end = (*i)->start_handle() - 1;
      }
      return MB_SUCCESS;
    }
  }

  seq_iter_out = end();
  data_ptr_out = 0;

  return MB_SUCCESS;
}

ErrorCode TypeSequenceManager::notify_appended(iterator seq)
{
  ErrorCode rval = check_merge_next(seq);
  if ((*seq)->using_entire_data())
    availableList.erase((*seq)->data());

  return rval;
}

ErrorCode TypeSequenceManager::notify_prepended(iterator seq)
{
  ErrorCode rval = check_merge_prev(seq);
  if ((*seq)->using_entire_data())
    availableList.erase((*seq)->data());

  return rval;
}

void TypeSequenceManager::get_memory_use(unsigned long long& entity_storage,
                                         unsigned long long& total_storage) const
{
  entity_storage = total_storage = 0;
  if (empty())
    return;

  EntityType mytype = TYPE_FROM_HANDLE(lastReferenced->start_handle());
  int junk;
  get_memory_use(CREATE_HANDLE(mytype, MB_START_ID, junk),
                 CREATE_HANDLE(mytype, MB_END_ID,   junk),
                 entity_storage,
                 total_storage);
}

void TypeSequenceManager::append_memory_use(EntityHandle first,
                                            EntityHandle last,
                                            const SequenceData* data,
                                            unsigned long long& entity_storage,
                                            unsigned long long& total_storage) const
{
  const unsigned long allocated_count = data->size();

  unsigned long bytes_per_ent, seq_size;
  const_iterator i = data->seqManData.firstSequence;
  (*i)->get_const_memory_use(bytes_per_ent, seq_size);

  unsigned long other_ent_mem = 0;
  unsigned long occupied_count = 0, entity_count = 0, sequence_count = 0;
  for (; i != end() && (*i)->data() == data; ++i) {
    occupied_count += (*i)->size();
    ++sequence_count;

    EntityHandle start = std::max(first, (*i)->start_handle());
    EntityHandle  stop = std::min(last, (*i)->end_handle());
    if (stop < start)
      continue;

    entity_count += stop - start + 1;
    other_ent_mem += (*i)->get_per_entity_memory_use(start, stop);
  }

  unsigned long sum = sequence_count * seq_size +
                      allocated_count * bytes_per_ent;

  // Watch for overflow
  assert(entity_count > 0 && occupied_count > 0 && allocated_count > 0);
  if (std::numeric_limits<unsigned long>::max() / entity_count <= sum) {
    total_storage  += sum * (entity_count /  occupied_count) + other_ent_mem;
    entity_storage += sum * (entity_count / allocated_count) + other_ent_mem; 
  }
  else {
    total_storage  += sum * entity_count /  occupied_count + other_ent_mem;
    entity_storage += sum * entity_count / allocated_count + other_ent_mem;
  }
}

void TypeSequenceManager::get_memory_use(EntityHandle first,
                                         EntityHandle last,
                                         unsigned long long& entity_storage,
                                         unsigned long long& total_storage) const
{
  entity_storage = total_storage = 0;

  while (first <= last) {
    const_iterator i = lower_bound(first);
    if (i == end())
      return;

    SequenceData* data = (*i)->data();
    if (first < data->end_handle()) {
      append_memory_use(first, last, data, entity_storage, total_storage);
    }
    first  = data->end_handle() + 1;
  }
}

EntityID TypeSequenceManager::get_occupied_size(const SequenceData* data) const
{
  EntityID result = 0;
  for (const_iterator i = data->seqManData.firstSequence; i != end() && (*i)->data() == data; ++i)
    result += (*i)->size();

  return result;
}

#ifndef NDEBUG
bool TypeSequenceManager::check_valid_data(const EntitySequence* seq) const
{
  // Caller passed a sequence that should be contained, so cannot be empty
  if (empty())
    return false;

  // Make sure lastReferenced points to something
  if (!lastReferenced)
    return false;

  const_iterator seqi = sequenceSet.lower_bound(lastReferenced);
  if (seqi == sequenceSet.end() || *seqi != lastReferenced)
    return false;

  // Make sure passed sequence is in list
  const EntitySequence* seq2 = find(seq->start_handle());
  if (seq2 != seq)
    return false;

  // Check all sequences referencing the same SequenceData
  const SequenceData* data = seq->data();
  const_iterator i = lower_bound(data->start_handle());
  if (i != data->seqManData.firstSequence)
    return false;

  if (i != begin()) {
    const_iterator j = i;
    --j;
    if ((*j)->end_handle() >= data->start_handle())
      return false;
    if ((*j)->data()->end_handle() >= data->start_handle())
      return false;
  }

  for (;;) {
    seq2 = *i;
    ++i;
    if (i == end())
      return true;
    if ((*i)->data() != data)
      break;

    if (seq2->end_handle() >= (*i)->start_handle())
      return false;
  }

  if ((*i)->start_handle() <= data->end_handle())
    return false;
  if ((*i)->data()->start_handle() <= data->end_handle())
    return false;

  return true;
}

#endif

} // namespace moab
