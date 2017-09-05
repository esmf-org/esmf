/** \file   DenseTag.cpp
 *  \author Jason Kraftcheck 
 *  \date   2010-12-14
 */

#include "DenseTag.hpp"
#include "moab/Range.hpp"
#include "TagCompare.hpp"
#include "SysUtil.hpp"
#include "SequenceManager.hpp"
#include "SequenceData.hpp"
#include "moab/Error.hpp"
#include "moab/ErrorHandler.hpp"
#include "moab/CN.hpp"
#include <utility>

namespace moab {

inline
static ErrorCode not_found(const std::string& /*name*/, EntityHandle /*h*/)
{
  // MB_TAG_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
#if 0
  if (h)
    fprintf(stderr, "[Warning]: No dense tag %s value for %s %ld\n",
        name.c_str(),
        CN::EntityTypeName(TYPE_FROM_HANDLE(h)),
        (unsigned long)ID_FROM_HANDLE(h));
  else
    fprintf(stderr, "[Warning]: No dense tag %s value for root set\n", name.c_str());
#endif

  return MB_TAG_NOT_FOUND;
}

inline
static ErrorCode ent_not_found(const std::string& /*name*/, EntityHandle /*h*/)
{
  // MB_ENTITY_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
#if 0
  fprintf(
      stderr,
      "[Warning]: Invalid entity handle setting tag %s: %s %ld\n",
      name.c_str(),
      CN::EntityTypeName(TYPE_FROM_HANDLE(h)),
      (unsigned long)ID_FROM_HANDLE(h)
      );
#endif

  return MB_ENTITY_NOT_FOUND;
}

DenseTag::DenseTag(int index,
                   const char* name,
                   int size,
                   DataType type,
                   const void* default_value)
  : TagInfo(name, size, type, default_value, size),
    mySequenceArray(index),
    meshValue(0)
  {}

TagType DenseTag::get_storage_type() const
{
  return MB_TAG_DENSE;
}

DenseTag* DenseTag::create_tag(SequenceManager* seqman,
                               Error* /* error */,
                               const char* name,
                               int bytes,
                               DataType type,
                               const void* default_value)
{
  if (bytes < 1)
    return 0;

  int index;
  if (MB_SUCCESS != seqman->reserve_tag_array(NULL, bytes, index))
    return 0;

  return new DenseTag(index, name, bytes, type, default_value);
}

DenseTag::~DenseTag()
{
  assert(mySequenceArray < 0);
  delete [] meshValue;
}

ErrorCode DenseTag::release_all_data(SequenceManager* seqman,
                                     Error* /* error */,
                                     bool delete_pending)
{
  ErrorCode result = seqman->release_tag_array(NULL, mySequenceArray, delete_pending);
  if (MB_SUCCESS == result && delete_pending)
    mySequenceArray = -1;
  return result;
}

ErrorCode DenseTag::get_array(const SequenceManager* seqman,
                              Error* /* error */,
                              EntityHandle h,
                              const unsigned char* const& ptr,
                              size_t& count) const
{
  return get_array(seqman, NULL, h, ptr, count);
}

ErrorCode DenseTag::get_array(const SequenceManager* seqman,
                              Error* /* error */,
                              EntityHandle h,
                              const unsigned char*& ptr,
                              size_t& count) const
{
  const EntitySequence* seq = 0;
  ErrorCode rval = seqman->find(h, seq);
  if (MB_SUCCESS != rval) {
    if (!h) { // Root set
      ptr = meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else { // Not root set
      ptr = 0;
      count = 0;
      return ent_not_found(get_name(), h);
    }
  }

  const void* mem = seq->data()->get_tag_data(mySequenceArray);
  ptr = reinterpret_cast<const unsigned char*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += get_size() * (h - seq->data()->start_handle());

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_array(const EntitySequence* seq,
                              const unsigned char* const & ptr) const
{
  return get_array(seq, ptr);
}

ErrorCode DenseTag::get_array(const EntitySequence* seq,
                              const unsigned char* & ptr) const
{
  ptr = reinterpret_cast<const unsigned char*>(seq->data()->get_tag_data(mySequenceArray));
  if (ptr)
    ptr += get_size() * (seq->start_handle() - seq->data()->start_handle());

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_array(SequenceManager* seqman,
                              Error* /* error */,
                              EntityHandle h,
                              unsigned char*& ptr,
                              size_t& count,
                              bool allocate)
{
  EntitySequence* seq = 0;
  ErrorCode rval = seqman->find(h, seq);
  if (MB_SUCCESS != rval) {
    if (!h) { // Root set
      if (!meshValue && allocate) 
        meshValue = new unsigned char[get_size()];
      ptr = meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else { // Not root set
      ptr = 0;
      count = 0;
      return ent_not_found(get_name(), h);
    }
  }

  void* mem = seq->data()->get_tag_data(mySequenceArray);
  if (!mem && allocate) {
    mem = seq->data()->allocate_tag_array(mySequenceArray, get_size(), get_default_value());
    if (!mem) {
      MB_SET_ERR(MB_MEMORY_ALLOCATION_FAILED, "Memory allocation for dense tag data failed");
    }

    if (!get_default_value())
      memset(mem, 0, get_size() * seq->data()->size());
  }

  ptr = reinterpret_cast<unsigned char*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += get_size() * (h - seq->data()->start_handle());
  return MB_SUCCESS;
}

ErrorCode DenseTag::get_data(const SequenceManager* seqman,
                             Error* /* error */,
                             const EntityHandle* entities,
                             size_t num_entities,
                             void* adata) const
{
  size_t junk = 0;
  unsigned char* ptr = reinterpret_cast<unsigned char*>(adata);
  const EntityHandle *const end = entities + num_entities;
  for (const EntityHandle* i = entities; i != end; ++i, ptr += get_size()) {
    const unsigned char* data = 0;
    ErrorCode rval = get_array(seqman, NULL, *i, data, junk);MB_CHK_ERR(rval);

    if (data)
      memcpy(ptr, data, get_size());
    else if (get_default_value())
      memcpy(ptr, get_default_value(), get_size());
    else
     return not_found(get_name(), *i);
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_data(const SequenceManager* seqman,
                             Error* /* error */,
                             const Range& entities,
                             void* values) const
{
  ErrorCode rval;
  size_t avail = 0;
  const unsigned char* array = NULL; // Initialize to get rid of warning
  unsigned char* data = reinterpret_cast<unsigned char*>(values);

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail);MB_CHK_ERR(rval);

      const size_t count = std::min<size_t>(p->second - start + 1, avail);
      if (array)
        memcpy(data, array, get_size() * count);
      else if (get_default_value())
        SysUtil::setmem(data, get_default_value(), get_size(), count);
      else
        return not_found(get_name(), start);

      data += get_size() * count;
      start += count;
    }
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_data(const SequenceManager* seqman,
                             Error* /* error */,
                             const EntityHandle* entities,
                             size_t num_entities,
                             const void** pointers,
                             int* data_lengths) const
{
  ErrorCode result;
  const EntityHandle *const end = entities + num_entities;
  size_t junk = 0;
  const unsigned char* ptr = NULL; // Initialize to get rid of warning

  if (data_lengths) {
    const int len = get_size();
    SysUtil::setmem(data_lengths, &len, sizeof(int), num_entities);
  }

  for (const EntityHandle* i = entities; i != end; ++i, ++pointers) {
    result = get_array(seqman, NULL, *i, ptr, junk);MB_CHK_ERR(result);

    if (ptr)
      *pointers = ptr;
    else if (get_default_value())
      *pointers = get_default_value();
    else
      return not_found(get_name(), *i);
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_data(const SequenceManager* seqman,
                             Error* /* error */,
                             const Range& entities,
                             const void** pointers,
                             int* data_lengths) const
{
  ErrorCode rval;
  size_t avail = 0;
  const unsigned char* array = NULL;

  if (data_lengths) {
    int len = get_size();
    SysUtil::setmem(data_lengths, &len, sizeof(int), entities.size());
  }

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail);MB_CHK_ERR(rval);

      const size_t count = std::min<size_t>(p->second - start + 1, avail);
      if (array) {
        for (EntityHandle end = start + count; start != end; ++start) {
          *pointers = array;
          array += get_size();
          ++pointers;
        }
      }
      else if (const void* val = get_default_value()) {
        SysUtil::setmem(pointers, &val, sizeof(void*), count);
        pointers += count;
        start += count;
      }
      else {
        return not_found(get_name(), start);
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::set_data(SequenceManager* seqman,
                             Error* /* error */,
                             const EntityHandle* entities,
                             size_t num_entities,
                             const void* data)
{
  ErrorCode rval;
  const unsigned char* ptr = reinterpret_cast<const unsigned char*>(data);
  const EntityHandle* const end = entities + num_entities;
  unsigned char* array = NULL;
  size_t junk = 0;

  for (const EntityHandle* i = entities; i != end; ++i, ptr += get_size()) {
    rval = get_array(seqman, NULL, *i, array, junk, true);MB_CHK_ERR(rval);

    memcpy(array, ptr, get_size());
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::set_data(SequenceManager* seqman,
                             Error* /* error */,
                             const Range& entities,
                             const void* values)
{
  ErrorCode rval;
  const char* data = reinterpret_cast<const char*>(values);
  unsigned char* array = NULL;
  size_t avail = 0;

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail, true);MB_CHK_ERR(rval);

      const size_t count = std::min<size_t>(p->second - start + 1, avail);
      memcpy(array, data, get_size() * count);
      data += get_size() * count;
      start += count;
    }
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::set_data(SequenceManager* seqman,
                             Error* /* error */,
                             const EntityHandle* entities,
                             size_t num_entities,
                             void const* const* pointers,
                             const int* data_lengths)
{
  ErrorCode rval = validate_lengths(NULL, data_lengths, num_entities);MB_CHK_ERR(rval);

  const EntityHandle* const end = entities + num_entities;
  unsigned char* array = NULL;
  size_t junk = 0;

  for (const EntityHandle* i = entities; i != end; ++i, ++pointers) {
    rval = get_array(seqman, NULL, *i, array, junk, true);MB_CHK_ERR(rval);

    memcpy(array, *pointers, get_size());
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::set_data(SequenceManager* seqman,
                             Error* /* error */,
                             const Range& entities,
                             void const* const* pointers,
                             const int* /* data_lengths */)
{
  ErrorCode rval;
  unsigned char* array = NULL;
  size_t avail = 0;

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail, true);MB_CHK_ERR(rval);

      const EntityHandle end = std::min<EntityHandle>(p->second + 1, start + avail);
      while (start != end) {
        memcpy(array, *pointers, get_size());
        ++start;
        ++pointers;
        array += get_size();
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::clear_data(bool allocate,
                               SequenceManager* seqman,
                               Error* /* error */,
                               const EntityHandle* entities,
                               size_t num_entities,
                               const void* value_ptr)
{
  ErrorCode rval;
  const EntityHandle* const end = entities + num_entities;
  unsigned char* array = NULL;
  size_t junk = 0;

  for (const EntityHandle* i = entities; i != end; ++i) {
    rval = get_array(seqman, NULL, *i, array, junk, allocate);MB_CHK_ERR(rval);

    if (array) // Array should never be null if allocate == true
      memcpy(array, value_ptr, get_size());
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::clear_data(bool allocate,
                               SequenceManager* seqman,
                               Error* /* error */,
                               const Range& entities,
                               const void* value_ptr)
{
  ErrorCode rval;
  unsigned char* array = NULL;
  size_t avail = 0;

  for (Range::const_pair_iterator p = entities.const_pair_begin(); 
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail, allocate);MB_CHK_ERR(rval);

      const size_t count = std::min<size_t>(p->second - start + 1, avail);
      if (array) // Array should never be null if allocate == true
        SysUtil::setmem(array, value_ptr, get_size(), count);
      start += count;
    }
  }

  return MB_SUCCESS;
}

ErrorCode DenseTag::clear_data(SequenceManager* seqman,
                               Error* /* error */,
                               const EntityHandle* entities,
                               size_t num_entities,
                               const void* value_ptr,
                               int value_len)
{
  if (value_len && value_len != get_size())
    return MB_INVALID_SIZE;

  return clear_data(true, seqman, NULL, entities, num_entities, value_ptr);
}

ErrorCode DenseTag::clear_data(SequenceManager* seqman,
                               Error* /* error */,
                               const Range& entities,
                               const void* value_ptr,
                               int value_len)
{
  if (value_len && value_len != get_size())
    return MB_INVALID_SIZE;

  return clear_data(true, seqman, NULL, entities, value_ptr);
}

ErrorCode DenseTag::remove_data(SequenceManager* seqman,
                                Error* /* error */,
                                const EntityHandle* entities,
                                size_t num_entities)
{
  std::vector<unsigned char> zeros;
  const void* value = get_default_value();
  if (!value) {
    zeros.resize(get_size(), 0);
    value = &zeros[0];
  }

  return clear_data(false, seqman, NULL, entities, num_entities, value);
}

ErrorCode DenseTag::remove_data(SequenceManager* seqman,
                                Error* /* error */,
                                const Range& entities)
{
  std::vector<unsigned char> zeros;
  const void* value = get_default_value();
  if (!value) {
    zeros.resize(get_size(), 0);
    value = &zeros[0];
  }

  return clear_data(false, seqman, NULL, entities, value);
}

ErrorCode DenseTag::tag_iterate(SequenceManager* seqman,
                                Error* /* error */,
                                Range::iterator& iter,
                                const Range::iterator& end,
                                void*& data_ptr,
                                bool allocate)
{
  // If asked for nothing, successfully return nothing.
  if (iter == end)
    return MB_SUCCESS;

  unsigned char* array = NULL;
  size_t avail = 0;
  ErrorCode rval = get_array(seqman, NULL, *iter, array, avail, allocate);MB_CHK_ERR(rval);
  data_ptr = array;

  size_t count = std::min<size_t>(avail, *(iter.end_of_block()) - *iter + 1);
  if (0 != *end && *end <= *(iter.end_of_block()))
    iter = end;
  else
    iter += count;

  return MB_SUCCESS;
}

ErrorCode DenseTag::get_tagged_entities(const SequenceManager* seqman,
                                        Range& entities_in,
                                        EntityType type,
                                        const Range* intersect_list) const
{
  Range tmp;
  Range* entities = intersect_list ? &tmp : &entities_in;
  Range::iterator hint = entities->begin();
  std::pair<EntityType,EntityType> range = type_range(type);
  TypeSequenceManager::const_iterator i;
  for (EntityType t = range.first; t != range.second; ++t) {
    const TypeSequenceManager& map = seqman->entity_map(t);
    for (i = map.begin(); i != map.end(); ++i) 
      if ((*i)->data()->get_tag_data(mySequenceArray))
        hint = entities->insert(hint, (*i)->start_handle(), (*i)->end_handle());
  }

  if (intersect_list) 
    entities_in = intersect(*entities, *intersect_list);

  return MB_SUCCESS;
}

ErrorCode DenseTag::num_tagged_entities(const SequenceManager* seqman,
                                        size_t& output_count,
                                        EntityType type,
                                        const Range* intersect) const
{
  Range tmp;
  ErrorCode rval = get_tagged_entities(seqman, tmp, type, intersect);
  output_count += tmp.size();

  return rval;
}

ErrorCode DenseTag::find_entities_with_value(const SequenceManager* seqman,
                                             Error* /* error */,
                                             Range& output_entities,
                                             const void* value,
                                             int value_bytes,
                                             EntityType type,
                                             const Range* intersect_entities) const
{
  if (value_bytes && value_bytes != get_size()) {
    MB_SET_ERR(MB_INVALID_SIZE, "Cannot compare data of size " << value_bytes << " with tag of size " << get_size());
  }

  if (!intersect_entities) {
    std::pair<EntityType,EntityType> range = type_range(type);
    TypeSequenceManager::const_iterator i;
    for (EntityType t = range.first; t != range.second; ++t) {
      const TypeSequenceManager& map = seqman->entity_map(t);
      for (i = map.begin(); i != map.end(); ++i) {
        const void* data = (*i)->data()->get_tag_data(mySequenceArray);
        if (data) {
          ByteArrayIterator start((*i)->data()->start_handle(), data, *this);
          ByteArrayIterator end((*i)->end_handle() + 1, 0, 0);
          start += (*i)->start_handle() - (*i)->data()->start_handle();
          find_tag_values_equal(*this, value, get_size(), start, end, output_entities);
        }
      }
    }
  }
  else {
    const unsigned char* array = NULL; // Initialize to get rid of warning
    size_t count;
    ErrorCode rval;

    Range::const_pair_iterator p = intersect_entities->begin();
    if (type != MBMAXTYPE) {
      p = intersect_entities->lower_bound(type);
      assert(TYPE_FROM_HANDLE(p->first) == type);
    }
    for (;
         p != intersect_entities->const_pair_end() &&
         (MBMAXTYPE == type || TYPE_FROM_HANDLE(p->first) == type);
         ++p) {
      EntityHandle start = p->first;
      while (start <= p->second) {
        rval = get_array(seqman, NULL, start, array, count);MB_CHK_ERR(rval);

        if (p->second - start < count - 1)
          count = p->second - start + 1;

        if (array) {
          ByteArrayIterator istart(start, array, *this);
          ByteArrayIterator iend(start + count, 0, 0);
          find_tag_values_equal(*this, value, get_size(), istart, iend, output_entities);
        }
        start += count;
      }
    }
  }

  return MB_SUCCESS;
}

bool DenseTag::is_tagged(const SequenceManager* seqman, EntityHandle h) const
{
  const unsigned char* ptr = NULL; // Initialize to get rid of warning
  size_t count;
  return (MB_SUCCESS == get_array(seqman, 0, h, ptr, count)) && (NULL != ptr);
} 

ErrorCode DenseTag::get_memory_use(const SequenceManager* seqman,
                                   unsigned long& total,
                                   unsigned long& per_entity) const
{
  per_entity = get_size();
  total = TagInfo::get_memory_use() + sizeof(*this);
  for (EntityType t = MBVERTEX; t <= MBENTITYSET; ++t) {
    const TypeSequenceManager& map = seqman->entity_map(t);
    const SequenceData* prev_data = 0;
    for (TypeSequenceManager::const_iterator i = map.begin(); i != map.end(); ++i) {
      if ((*i)->data() != prev_data && (*i)->data()->get_tag_data(mySequenceArray)) {
        prev_data = (*i)->data();
        total += get_size() * (*i)->data()->size();
      }
    }
  }

  return MB_SUCCESS;
}

} // namespace moab
