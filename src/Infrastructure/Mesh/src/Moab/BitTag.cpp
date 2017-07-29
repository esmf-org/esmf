#include "BitTag.hpp"
#include "BitPage.hpp"
#include "moab/Range.hpp"
#include "TagCompare.hpp"
#include "SequenceManager.hpp"
#include "moab/Error.hpp"
#include "moab/ErrorHandler.hpp"
#include <stdlib.h>
#include <string.h>

namespace moab {

BitTag::~BitTag()
{
  release_all_data(0, 0, true);
}

TagType BitTag::get_storage_type() const
{
  return MB_TAG_BIT;
}

BitTag* BitTag::create_tag(const char* name,
                           int size,
                           const void* default_value)
{
  BitTag* result = new BitTag(name, size, default_value);
  if (MB_SUCCESS != result->reserve(size)) {
    delete result;
    result = NULL;
  }

  return result;
}

ErrorCode BitTag::reserve(unsigned bits)
{
  if (bits > 8)
    return MB_FAILURE;

  requestedBitsPerEntity = bits;
  // Store smallest power of two greater than or
  // equal to the number of bits
  storedBitsPerEntity = 1;
  unsigned ln2storedbits = 0;
  while (storedBitsPerEntity < bits) {
    storedBitsPerEntity *= 2;
    ++ln2storedbits;
  }

  // pageShift = log2(ents_per_page())
  //           = log2(8 * pageSize / storedBitsPerEntity )
  //           = log2(8) + log2(pageSize) - log2(storedBitsPerEntity)
  //           = 3 + Ln2PageSize - ln2storedbits;
  pageShift = 3 + Ln2PageSize - ln2storedbits;

  return MB_SUCCESS;
}

ErrorCode BitTag::release_all_data(SequenceManager*, Error*, bool)
{
  for (EntityType t = (EntityType)0; t != MBMAXTYPE; ++t) {
    for (size_t i = 0; i < pageList[t].size(); ++i)
      delete pageList[t][i];
    pageList[t].clear();
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::get_data(const SequenceManager*,
                           Error*,
                           const EntityHandle* handles,
                           size_t num_handles,
                           void* gen_data) const
{
  EntityType type;
  size_t page;
  int offset;
  unsigned char def = default_val();
  unsigned char* data = reinterpret_cast<unsigned char*>(gen_data);
  for (size_t i = 0; i < num_handles; ++i) {
    unpack(handles[i], type, page, offset);
    if (pageList[type].size() <= page || !pageList[type][page]) 
      data[i] = def;
    else
      data[i] = pageList[type][page]->get_bits(offset, storedBitsPerEntity);
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::set_data(SequenceManager* seqman,
                           Error* /* error */,
                           const EntityHandle* handles,
                           size_t num_handles,
                           const void* gen_data)
{
  ErrorCode rval = seqman->check_valid_entities(NULL, handles, num_handles, true);MB_CHK_ERR(rval);

  EntityType type;
  size_t page;
  int offset;
  const unsigned char* data = reinterpret_cast<const unsigned char*>(gen_data);
  for (size_t i = 0; i < num_handles; ++i) {
    unpack(handles[i], type, page, offset);
    if (pageList[type].size() <= page)
      pageList[type].resize(page + 1, 0);
    if (!pageList[type][page])
      pageList[type][page] = new BitPage(storedBitsPerEntity, default_val());
    pageList[type][page]->set_bits(offset, storedBitsPerEntity, data[i]);
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::clear_data(SequenceManager* seqman,
                             Error* /* error */,
                             const EntityHandle* handles,
                             size_t num_handles,
                             const void* value_ptr,
                             int value_len)
{
  if (value_len)
    return MB_INVALID_SIZE;

  ErrorCode rval = seqman->check_valid_entities(NULL, handles, num_handles, true);MB_CHK_ERR(rval);

  EntityType type;
  size_t page;
  int offset;
  const unsigned char value = *reinterpret_cast<const unsigned char*>(value_ptr);
  for (size_t i = 0; i < num_handles; ++i) {
    unpack(handles[i], type, page, offset);
    if (pageList[type].size() <= page)
      pageList[type].resize(page + 1, 0);
    if (!pageList[type][page])
      pageList[type][page] = new BitPage(storedBitsPerEntity, default_val());
    pageList[type][page]->set_bits(offset, storedBitsPerEntity, value);
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::remove_data(SequenceManager*,
                              Error*,
                              const EntityHandle* handles,
                              size_t num_handles)
{
  EntityType type;
  size_t page;
  int offset;
  const unsigned char val = default_val();
  for (size_t i = 0; i < num_handles; ++i) {
    unpack(handles[i], type, page, offset);
    if (pageList[type].size() > page && pageList[type][page])
      pageList[type][page]->set_bits(offset, storedBitsPerEntity, val);
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::get_data(const SequenceManager*,
                           Error*,
                           const Range& handles,
                           void* gen_data) const
{
  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  unsigned char def = default_val();
  unsigned char* data = reinterpret_cast<unsigned char*>(gen_data);
  Range::const_pair_iterator i;
  for (i = handles.const_pair_begin(); i != handles.const_pair_end(); ++i) {
    unpack(i->first, type, page, offset);
    assert(TYPE_FROM_HANDLE(i->second) == type); // Should be true because id of zero is never used
    count = i->second - i->first + 1;
    if (page >= pageList[type].size()) {
      memset(data, def, count);
      data += count;
      continue;
    }

    while (count) {
      size_t pcount = std::min((EntityID)(per_page - offset), count);
      if (pageList[type][page])
        pageList[type][page]->get_bits(offset, pcount, storedBitsPerEntity, data);
      else
        memset(data, def, pcount);
      data += pcount;
      count -= pcount;
      offset = 0;
      ++page;
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::set_data(SequenceManager* seqman,
                           Error* /* error */,
                           const Range& handles,
                           const void* gen_data)
{
  ErrorCode rval = seqman->check_valid_entities(NULL, handles);MB_CHK_ERR(rval);

  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  unsigned char def = default_val();
  const unsigned char* data = reinterpret_cast<const unsigned char*>(gen_data);
  Range::const_pair_iterator i;
  for (i = handles.const_pair_begin(); i != handles.const_pair_end(); ++i) {
    unpack(i->first, type, page, offset);
    assert(TYPE_FROM_HANDLE(i->second) == type); // Should be true because id of zero is never used
    count = i->second - i->first + 1;

    while (count) {
      if (page >= pageList[type].size())
        pageList[type].resize(page + 1, 0);
      if (!pageList[type][page])
        pageList[type][page] = new BitPage(storedBitsPerEntity, def);

      size_t pcount = std::min((EntityID)(per_page - offset), count);
      pageList[type][page]->set_bits(offset, pcount, storedBitsPerEntity, data);
      data += pcount;
      count -= pcount;
      offset = 0;
      ++page;
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::clear_data(SequenceManager* seqman,
                             Error* /* error */,
                             const Range& handles,
                             const void* value_ptr,
                             int value_len)
{
  if (value_len)
    return MB_INVALID_SIZE;

  ErrorCode rval = seqman->check_valid_entities(NULL, handles);MB_CHK_ERR(rval);

  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  const unsigned char value = *reinterpret_cast<const unsigned char*>(value_ptr);
  Range::const_pair_iterator i;
  for (i = handles.const_pair_begin(); i != handles.const_pair_end(); ++i) {
    unpack(i->first, type, page, offset);
    assert(TYPE_FROM_HANDLE(i->second) == type); // Should be true because id of zero is never used
    count = i->second - i->first + 1;

    while (count) {
      if (page >= pageList[type].size())
        pageList[type].resize(page + 1, 0);
      if (!pageList[type][page])
        pageList[type][page] = new BitPage(storedBitsPerEntity, default_val());

      size_t pcount = std::min((EntityID)(per_page - offset), count);
      pageList[type][page]->set_bits(offset, pcount, storedBitsPerEntity, value);
      count -= pcount; 
      offset = 0;
      ++page;
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::remove_data(SequenceManager*, Error*, const Range& handles)
{
  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  unsigned char val = default_val();
  Range::const_pair_iterator i;
  for (i = handles.const_pair_begin(); i != handles.const_pair_end(); ++i) {
    unpack(i->first, type, page, offset);
    assert(TYPE_FROM_HANDLE(i->second) == type); // Should be true because id of zero is never used
    count = i->second - i->first + 1;

    while (count) {
      size_t pcount = std::min((EntityID)(per_page - offset), count);
      if (page < pageList[type].size() && pageList[type][page])
        pageList[type][page]->set_bits(offset, pcount, storedBitsPerEntity, val);
      count -= pcount; 
      offset = 0;
      ++page;
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::get_data(const SequenceManager*,
                           Error* /* error */,
                           const EntityHandle*,
                           size_t,
                           const void**,
                           int*) const
{
  MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Operation get_data not supported for bit tags");
}

ErrorCode BitTag::get_data(const SequenceManager*,
                           Error* /* error */,
                           const Range&,
                           const void**,
                           int*) const
{
  MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Operation get_data not supported for bit tags");
}

ErrorCode BitTag::set_data(SequenceManager*,
                           Error* /* error */,
                           const EntityHandle*,
                           size_t,
                           void const* const*,
                           const int*)
{
  MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Operation set_data not supported for bit tags");
}

ErrorCode BitTag::set_data(SequenceManager*,
                           Error* /* error */,
                           const Range&,
                           void const* const*,
                           const int*)
{
  MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Operation set_data not supported for bit tags");
}

ErrorCode BitTag::tag_iterate(SequenceManager*,
                              Error* /* error */,
                              Range::iterator&,
                              const Range::iterator&,
                              void*&,
                              bool)
{
  MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, "Operation tag_iterate not supported for bit tags");
}

template <class Container> inline
void BitTag::get_tagged(EntityType type,
                        Container& entities) const
{
  std::pair<EntityType, EntityType> r = type_range(type);
  typename Container::iterator hint = entities.begin();
  const int per_page = ents_per_page();
  for (EntityType t = r.first; t != r.second; ++t) {
    for (size_t i = 0; i < pageList[t].size(); ++i) {
      if (pageList[t][i]) {
        EntityID id = i * per_page;
        EntityHandle h = CREATE_HANDLE(t, id);
        EntityHandle last = h + per_page - 1;
        // Never zero ID
        if (0 == id)
          ++h;
        hint = entities.insert(hint, h, last);
      }
    }
  }
}

template <class Container> inline
void BitTag::get_tagged(Range::const_iterator begin,
                        Range::const_iterator end,
                        Container& entities) const
{
  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  typename Container::iterator hint = entities.begin();
  EntityHandle h;
  Range::const_iterator i = begin;
  while (i != end) {
    h = *i;
    unpack(h, type, page, offset);

    i = i.end_of_block();
    count = *i - h + 1;
    ++i;
    while (count > 0) {
      EntityID pcount = std::min(count, (EntityID)(per_page - offset));
      if (page < pageList[type].size() && pageList[type][page]) 
        hint = entities.insert(hint, h, h + pcount - 1);

      count -= pcount;
      h += pcount;
      assert(TYPE_FROM_HANDLE(h) == type);
      offset = 0;
      ++page;
    }
  }
}

template <class Container> inline
void BitTag::get_tagged(Container& entities,
                        EntityType type,
                        const Range* intersect) const

{
  if (!intersect)
    get_tagged<Container>(type, entities);
  else if (MBMAXTYPE == type)
    get_tagged<Container>(intersect->begin(), intersect->end(), entities);
  else {
    std::pair<Range::iterator, Range::iterator> r = intersect->equal_range(type);
    get_tagged<Container>(r.first, r.second, entities);
  }
}

ErrorCode BitTag::get_tagged_entities(const SequenceManager*,
                                      Range& entities,
                                      EntityType type,
                                      const Range* intersect) const
{
  get_tagged<Range>(entities, type, intersect);
  return MB_SUCCESS;
}

ErrorCode BitTag::num_tagged_entities(const SequenceManager*,
                                      size_t& count,
                                      EntityType type,
                                      const Range* intersect) const
{
  InsertCount counter(count);
  get_tagged<InsertCount>(counter, type, intersect);
  count = counter.end();
  return MB_SUCCESS;
}

ErrorCode BitTag::find_entities_with_value(const SequenceManager*,
                                           Error* /* error */,
                                           Range& output_entities,
                                           const void* value,
                                           int value_bytes,
                                           EntityType type,
                                           const Range* intersect_entities) const
{
  if (value_bytes && value_bytes != 1) {
    MB_SET_ERR(MB_INVALID_SIZE, "Invalid tag size for bit tag: " << value_bytes << " bytes");
  }

  const signed char bits = *reinterpret_cast<const unsigned char*>(value);
  if (intersect_entities)
    return get_entities_with_bits(*intersect_entities,
                                  type,
                                  output_entities,
                                  bits);
  else
    return get_entities_with_bits(type,
                                  output_entities,
                                  bits);
}

ErrorCode BitTag::get_entities_with_bits(EntityType type,
                                         Range& entities,
                                         unsigned char bits) const
{
  std::pair<EntityType, EntityType> r = type_range(type);
  const int per_page = ents_per_page();
  for (EntityType t = r.first; t != r.second; ++t) {
    for (size_t i = 0; i < pageList[t].size(); ++i) {
      if (pageList[t][i]) {
        EntityID id = i * per_page;
        EntityHandle h = CREATE_HANDLE(t, id);
        int off = !i; // Never zero ID
        pageList[t][i]->search(bits, off, per_page-off, storedBitsPerEntity,
                               entities, h + off);
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::get_entities_with_bits(const Range &range,
                                         EntityType in_type,
                                         Range& entities,
                                         unsigned char bits) const
{
  if (MBMAXTYPE == in_type) {
    ErrorCode rval;
    for (--in_type; in_type >= MBVERTEX; --in_type) {
      rval = get_entities_with_bits(range, in_type, entities, bits);MB_CHK_ERR(rval);
    }
    return MB_SUCCESS;
  }

  EntityType type;
  EntityID count;
  size_t page;
  int offset, per_page = ents_per_page();
  Range::const_iterator i, end;
  std::pair<Range::iterator, Range::iterator> r = range.equal_range(in_type);
  i = r.first;
  end = r.second;
  EntityHandle h;
  while (i != end) {
    h = *i;
    unpack(h, type, page, offset);
    assert(MBMAXTYPE == in_type || type == in_type);

    i = i.end_of_block();
    count = *i - h + 1;
    ++i;
    while (count > 0) {
      EntityID pcount = std::min(count, (EntityID)(per_page - offset));
      if (page < pageList[type].size() && pageList[type][page])
        pageList[type][page]->search(bits, offset, pcount,
                                     storedBitsPerEntity,
                                     entities, h);

      count -= pcount;
      h += pcount;
      assert(TYPE_FROM_HANDLE(h) == type);
      offset = 0;
      ++page;
    }
  }

  return MB_SUCCESS;
}

ErrorCode BitTag::get_memory_use(const SequenceManager*,
                                 unsigned long& total,
                                 unsigned long& per_entity) const
{
  per_entity = (storedBitsPerEntity > 4); // Cannot return fraction of bytes, so round
  total = 0;
  for (EntityType t = (EntityType)0; t < MBMAXTYPE; ++t) {
    total += pageList[t].capacity() * sizeof(BitPage*);
    for (size_t i = 0; i < pageList[t].size(); ++i)
      if (pageList[t][i])
        total += sizeof(BitPage);
  }

  return MB_SUCCESS;
}

bool BitTag::is_tagged(const SequenceManager*, EntityHandle h) const
{
  EntityType type;
  size_t page;
  int offset;
  unpack(h, type, page, offset);
  return page < pageList[type].size() && pageList[type][page];
}

} // namespace moab
