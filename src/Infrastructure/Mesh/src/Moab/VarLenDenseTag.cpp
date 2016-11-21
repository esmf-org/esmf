/** \file   VarLenDenseTag.cpp
 *  \author Jason Kraftcheck 
 *  \date   2010-12-14
 */

#include "VarLenDenseTag.hpp"
#include "moab/Range.hpp"
#include "TagCompare.hpp"
#include "SysUtil.hpp"
#include "SequenceManager.hpp"
#include "SequenceData.hpp"
#include "RangeSeqIntersectIter.hpp"
#include "moab/Error.hpp"
#include "moab/ErrorHandler.hpp"
#include "moab/CN.hpp"
#include <utility>

namespace moab {

inline
static ErrorCode not_found(std::string /*name*/, EntityHandle /*h*/)
{
#if 0
  // MB_TAG_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
  // Print warning messages for debugging only
  if (h)
    fprintf(stderr, "[Warning]: No variable-length dense tag %s value for %s %lu\n",
        name.c_str(),
        CN::EntityTypeName(TYPE_FROM_HANDLE(h)),
        (unsigned long)ID_FROM_HANDLE(h));
  else
    fprintf(stderr, "[Warning]: No variable-length dense tag %s value for root set\n", name.c_str());
#endif

  return MB_TAG_NOT_FOUND;
}

VarLenDenseTag::VarLenDenseTag(int index,
                               const char* name,
                               DataType type,
                               const void* default_value,
                               int default_value_size)
  : TagInfo(name, MB_VARIABLE_LENGTH, type, default_value, default_value_size),
    mySequenceArray(index)
  {}

VarLenDenseTag* VarLenDenseTag::create_tag(SequenceManager* seqman,
                                           Error* error,
                                           const char* name,
                                           DataType type,
                                           const void* default_value,
                                           int default_value_size)
{
  int index; 
  if (MB_SUCCESS != seqman->reserve_tag_array(error, MB_VARIABLE_LENGTH, index))
    return NULL;

  return new VarLenDenseTag(index, name, type, default_value, default_value_size);
}

VarLenDenseTag::~VarLenDenseTag()
{
  assert(mySequenceArray < 0);
}

TagType VarLenDenseTag::get_storage_type() const
{
  return MB_TAG_DENSE;
}

ErrorCode VarLenDenseTag::release_all_data(SequenceManager* seqman,
                                           Error* error,
                                           bool delete_pending)
{
  Range all_ents;
  seqman->get_entities(all_ents);
  ErrorCode rval = remove_data(seqman, error, all_ents);
  if (MB_SUCCESS == rval) {
    rval = seqman->release_tag_array(error, mySequenceArray, delete_pending);
    if (MB_SUCCESS == rval && delete_pending)
      mySequenceArray = -1;
  }

  return rval;
}

ErrorCode VarLenDenseTag::get_array(const SequenceManager* seqman,
                                    Error* /* error */,
                                    EntityHandle h,
                                    const VarLenTag*& ptr,
                                    size_t& count) const
{
  const EntitySequence* seq = NULL;
  ErrorCode rval = seqman->find(h, seq);
  if (MB_SUCCESS != rval) {
    if (!h) { // Root set
      ptr = &meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else {
      ptr = NULL;
      count = 0;
      return not_found(get_name(), h);
    }
  }

  const void* mem = seq->data()->get_tag_data(mySequenceArray);
  ptr = reinterpret_cast<const VarLenTag*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += h - seq->data()->start_handle();

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::get_array(SequenceManager* seqman,
                                    Error* /* error */,
                                    EntityHandle h,
                                    VarLenTag*& ptr,
                                    size_t& count,
                                    bool allocate)
{
  EntitySequence* seq = NULL;
  ErrorCode rval = seqman->find(h, seq);
  if (MB_SUCCESS != rval) {
    if (!h) { // Root set
      ptr = &meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else {
      ptr = NULL;
      count = 0;
      return not_found(get_name(), h);
    }
  }

  void* mem = seq->data()->get_tag_data(mySequenceArray);
  if (!mem && allocate) {
    mem = seq->data()->allocate_tag_array(mySequenceArray, sizeof(VarLenTag));
    if (!mem) {
      MB_SET_ERR(MB_MEMORY_ALLOCATION_FAILED, "Memory allocation for variable-length dense tag data failed");
    }

    memset(mem, 0, sizeof(VarLenTag) * seq->data()->size());
  }

  ptr = reinterpret_cast<VarLenTag*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += h - seq->data()->start_handle();

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::get_data(const SequenceManager*,
                                   Error* /* error */,
                                   const EntityHandle*,
                                   size_t,
                                   void*) const
{
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
}

ErrorCode VarLenDenseTag::get_data(const SequenceManager*,
                                   Error* /* error */,
                                   const Range&,
                                   void*) const
{
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
}

ErrorCode VarLenDenseTag::get_data(const SequenceManager* seqman,
                                   Error* /* error */,
                                   const EntityHandle* entities,
                                   size_t num_entities,
                                   const void** pointers,
                                   int* lengths) const
{
  if (!lengths) {
    MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
  }

  ErrorCode result = MB_SUCCESS, rval;
  const EntityHandle *const end = entities + num_entities;
  size_t junk = 0;
  const VarLenTag* ptr = NULL;

  for (const EntityHandle* i = entities; i != end; ++i, ++pointers, ++lengths) {
    rval = get_array(seqman, NULL, *i, ptr, junk);MB_CHK_ERR(rval);

    if (ptr && ptr->size()) {
      *pointers = ptr->data();
      *lengths = ptr->size();
    }
    else if (get_default_value()) {
      *pointers = get_default_value();
      *lengths = get_default_value_size();
    }
    else {
      *pointers = 0;
      *lengths = 0;
      result = not_found(get_name(), *i);
    }
  }

  return result;
}

ErrorCode VarLenDenseTag::get_data(const SequenceManager* seqman,
                                   Error* /* error */,
                                   const Range& entities,
                                   const void** pointers,
                                   int* lengths) const
{
  if (!lengths) {
    MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
  }

  ErrorCode rval;
  size_t avail = 0;
  const VarLenTag* array = NULL;

  for (Range::const_pair_iterator p = entities.const_pair_begin(); 
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail);MB_CHK_ERR(rval);

      const size_t count = std::min<size_t>(p->second - start + 1, avail);

      if (!array) {
        const void* defval = get_default_value();
        const int len = get_default_value_size();
        SysUtil::setmem(pointers, &defval, sizeof(void*), count);
        SysUtil::setmem(lengths, &len, sizeof(int), count);
        pointers += count;
        lengths += count;
        if (!defval)
          return not_found(get_name(), start);
      }

      const VarLenTag* end_data = array + count;
      while (array != end_data) {
        if (array->size()) {
          *pointers = array->data();
          *lengths = array->size();
        }
        else if (get_default_value()) {
          *pointers = get_default_value();
          *lengths = get_default_value_size();
        }
        else {
          *pointers = NULL;
          *lengths = 0;
          return not_found(get_name(), start);
        }
        ++pointers;
        ++lengths;
        ++array;
        ++start;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::set_data(SequenceManager*,
                                   Error* /* error */,
                                   const EntityHandle*,
                                   size_t,
                                   const void*)
{
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
}

ErrorCode VarLenDenseTag::set_data(SequenceManager*,
                                   Error* /* error */,
                                   const Range&,
                                   const void*)
{
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag " << get_name() << " data");
}

ErrorCode VarLenDenseTag::set_data(SequenceManager* seqman,
                                   Error* /* error */,
                                   const EntityHandle* entities,
                                   size_t num_entities,
                                   bool one_value,
                                   void const* const* pointers,
                                   const int* lengths)
{
  ErrorCode rval = validate_lengths(NULL, lengths, one_value ? 1 : num_entities);MB_CHK_ERR(rval);

  const EntityHandle* const end = entities + num_entities;
  VarLenTag* array = NULL;
  size_t junk = 0;
  const size_t step = one_value ? 0 : 1;

  for (const EntityHandle* i = entities; i != end; ++i) {
    rval = get_array(seqman, NULL, *i, array, junk, true);MB_CHK_ERR(rval);

    array->set(*pointers, *lengths);
    pointers += step;
    lengths += step;
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::set_data(SequenceManager* seqman,
                                   Error* /* error */,
                                   const Range& entities,
                                   bool one_value,
                                   void const* const* pointers,
                                   const int* lengths)
{
  ErrorCode rval = validate_lengths(NULL, lengths, one_value ? 1 : entities.size());MB_CHK_ERR(rval);

  VarLenTag* array = NULL;
  size_t avail = 0;
  const size_t step = one_value ? 0 : 1;

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail, true);MB_CHK_ERR(rval);

      const EntityHandle end = std::min<EntityHandle>(p->second + 1, start + avail);
      while (start != end) {
        array->set(*pointers, *lengths);
        ++start;
        ++array;
        pointers += step;
        lengths += step;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::set_data(SequenceManager* seqman,
                                   Error* /* error */,
                                   const EntityHandle* entities,
                                   size_t num_entities,
                                   void const* const* pointers,
                                   const int* lengths)
{
  return set_data(seqman, NULL, entities, num_entities, false, pointers, lengths);
}

ErrorCode VarLenDenseTag::set_data(SequenceManager* seqman,
                                   Error* /* error */,
                                   const Range& entities,
                                   void const* const* pointers,
                                   const int* lengths)
{
  return set_data(seqman, NULL, entities, false, pointers, lengths);
}

ErrorCode VarLenDenseTag::clear_data(SequenceManager* seqman,
                                     Error* /* error */,
                                     const EntityHandle* entities,
                                     size_t num_entities,
                                     const void* value_ptr,
                                     int value_len)
{
  if (!value_ptr || !value_len)
    return remove_data(seqman, NULL, entities, num_entities);
  else
    return set_data(seqman, NULL, entities, num_entities, true, &value_ptr, &value_len);
}

ErrorCode VarLenDenseTag::clear_data(SequenceManager* seqman,
                                     Error* /* error */,
                                     const Range& entities,
                                     const void* value_ptr,
                                     int value_len)
{
  if (!value_ptr || !value_len)
    return remove_data(seqman, NULL, entities);
  else
    return set_data(seqman, NULL, entities, true, &value_ptr, &value_len);
}

ErrorCode VarLenDenseTag::remove_data(SequenceManager* seqman,
                                      Error* /* error */,
                                      const EntityHandle* entities,
                                      size_t num_entities)
{
  const EntityHandle* const end = entities + num_entities;
  VarLenTag* array = NULL;
  size_t junk = 0;
  ErrorCode rval;

  for (const EntityHandle* i = entities; i != end; ++i) {
    rval = get_array(seqman, NULL, *i, array, junk, false);MB_CHK_ERR(rval);

    if (array) 
      array->clear();
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::remove_data(SequenceManager* seqman,
                                      Error* /* error */,
                                      const Range& entities)
{
  VarLenTag* array = NULL;
  size_t avail = 0;
  ErrorCode rval;

  for (Range::const_pair_iterator p = entities.const_pair_begin();
       p != entities.const_pair_end(); ++p) {
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array(seqman, NULL, start, array, avail, false);MB_CHK_ERR(rval);

      const EntityHandle end = std::min<EntityHandle>(p->second + 1, start + avail);
      if (array) {
        while (start != end) {
          array->clear();
          ++start;
          ++array;
        }
      }
      else {
        start = end;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::tag_iterate(SequenceManager*,
                                      Error* /* error */,
                                      Range::iterator&,
                                      const Range::iterator&,
                                      void*&,
                                      bool)
{
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "Cannot iterate over variable-length tag data");
}

template <class Container> static inline
ErrorCode get_tagged(const SequenceManager* seqman,
                     int mySequenceArray,
                     EntityType type,
                     Container& entities)
{
  typename Container::iterator hint = entities.begin();
  std::pair<EntityType, EntityType> range = type_range(type);
  TypeSequenceManager::const_iterator i;
  const VarLenTag *data, *iter, *end;
  for (EntityType t = range.first; t != range.second; ++t) {
    const TypeSequenceManager& map = seqman->entity_map(t);
    for (i = map.begin(); i != map.end(); ++i) {
      data = reinterpret_cast<const VarLenTag*>((*i)->data()->get_tag_data(mySequenceArray));
      if (!data)
        continue;
      end = data + (*i)->end_handle() - (*i)->data()->start_handle() + 1;
      iter = data + (*i)->start_handle() - (*i)->data()->start_handle();
      EntityHandle handle = (*i)->start_handle();
      for (; iter != end; ++iter, ++handle)
        if (iter->size())
          hint = entities.insert(hint, handle);
    }
  }

  return MB_SUCCESS;
}

template <class Container> static inline
ErrorCode get_tagged(const SequenceManager* seqman,
                     int mySequenceArray,
                     Range::const_iterator begin,
                     Range::const_iterator end,
                     Container& entities)
{
  typename Container::iterator hint = entities.begin();
  RangeSeqIntersectIter iter(const_cast<SequenceManager*>(seqman));
  ErrorCode rval = iter.init(begin, end);
  const VarLenTag* data;
  for (; MB_SUCCESS == rval; rval = iter.step()) {
    data = reinterpret_cast<const VarLenTag*>(iter.get_sequence()->data()->get_tag_data(mySequenceArray));
    if (!data)
      continue;

    data += iter.get_start_handle() - iter.get_sequence()->data()->start_handle();
    size_t count = iter.get_end_handle() - iter.get_start_handle() + 1;
    for (size_t i = 0; i < count; ++i) 
      if (data[i].size())
        hint = entities.insert(hint, iter.get_start_handle() + i);
    rval = iter.step();
  }

  if (MB_FAILURE != rval) // We get MB_FAILURE at iterator end
    return rval;

  return MB_SUCCESS;
}

template <class Container> static inline
ErrorCode get_tagged(const SequenceManager* seqman,
                     int mySequenceArray,
                     Container& entities,
                     EntityType type,
                     const Range* intersect)
{
  if (!intersect)
    return get_tagged<Container>(seqman, mySequenceArray, type, entities);
  else if (MBMAXTYPE == type)
    return get_tagged<Container>(seqman, mySequenceArray, intersect->begin(), intersect->end(), entities);
  else {
    std::pair<Range::iterator, Range::iterator> r = intersect->equal_range(type);
    return get_tagged<Container>(seqman, mySequenceArray, r.first, r.second, entities);
  }
}

ErrorCode VarLenDenseTag::get_tagged_entities(const SequenceManager* seqman,
                                              Range& entities,
                                              EntityType type,
                                              const Range* intersect) const
{
  return get_tagged(seqman, mySequenceArray, entities, type, intersect);
}

ErrorCode VarLenDenseTag::num_tagged_entities(const SequenceManager* seqman,
                                              size_t& output_count,
                                              EntityType type,
                                              const Range* intersect) const
{
  InsertCount counter(output_count);
  ErrorCode rval = get_tagged(seqman, mySequenceArray, counter, type, intersect);
  output_count = counter.end();
  return rval;
}

ErrorCode VarLenDenseTag::find_entities_with_value(const SequenceManager* seqman,
                                                   Error* error,
                                                   Range& output_entities,
                                                   const void* value,
                                                   int value_bytes,
                                                   EntityType type,
                                                   const Range* intersect_entities) const
{
  if (!intersect_entities) {
    std::pair<EntityType, EntityType> range = type_range(type);
    TypeSequenceManager::const_iterator i;
    for (EntityType t = range.first; t != range.second; ++t) {
      const TypeSequenceManager& map = seqman->entity_map(t);
      for (i = map.begin(); i != map.end(); ++i) {
        const void* data = (*i)->data()->get_tag_data(mySequenceArray);
        if (data) {
          ByteArrayIterator start((*i)->data()->start_handle(), data, *this);
          ByteArrayIterator end((*i)->end_handle() + 1, 0, 0);
          start += (*i)->start_handle() - (*i)->data()->start_handle();
          find_tag_varlen_values_equal(*this, value, value_bytes, start, end, output_entities);
        }
      }
    }
  }
  else {
    const VarLenTag* array;
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
        rval = get_array(seqman, error, start, array, count);MB_CHK_ERR(rval);

        if (p->second - start < count - 1)
          count = p->second - start + 1;

        if (array) {
          ByteArrayIterator istart(start, array, *this);
          ByteArrayIterator iend(start + count, 0, 0);
          find_tag_varlen_values_equal(*this, value, value_bytes, istart, iend, output_entities);
        }
        start += count;
      }
    }
  }

  return MB_SUCCESS;
}

bool VarLenDenseTag::is_tagged(const SequenceManager* seqman, EntityHandle h) const
{
  const VarLenTag* ptr = NULL; // Initialize to get rid of warning
  size_t count;
  return (MB_SUCCESS == get_array(seqman, 0, h, ptr, count))
          && (NULL != ptr) && (NULL != ptr->data());
}

ErrorCode VarLenDenseTag::get_memory_use(const SequenceManager* seqman,
                                         unsigned long& total,
                                         unsigned long& per_entity) const

{
  total = 0;
  per_entity = 0;
  size_t count = 0;
  for (EntityType t = MBVERTEX; t <= MBENTITYSET; ++t) {
    const TypeSequenceManager& map = seqman->entity_map(t);
    const SequenceData* prev_data = 0;
    for (TypeSequenceManager::const_iterator i = map.begin(); i != map.end(); ++i) {
      const void* mem = (*i)->data()->get_tag_data(mySequenceArray);
      if (!mem) 
        continue;

      if ((*i)->data() != prev_data) {
        total += (*i)->data()->size();
        prev_data = (*i)->data();
      }

      count += (*i)->size();
      const VarLenTag* array = reinterpret_cast<const VarLenTag*>(mem);
      for (int j = 0; j < (*i)->size(); ++j)
        per_entity += array[j].mem();
    }
  }
  total *= sizeof(VarLenTag);
  total += per_entity + sizeof(*this) + TagInfo::get_memory_use();
  total += meshValue.mem() + sizeof(meshValue);
  if (count)
    per_entity /= count;
  per_entity += sizeof(VarLenTag);

  return MB_SUCCESS;
}

} // namespace moab
