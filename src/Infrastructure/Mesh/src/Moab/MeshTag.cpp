/** \file   MeshTag.cpp
 *  \author Jason Kraftcheck 
 *  \date   2010-12-14
 */

#include "moab/Interface.hpp"
#include "MeshTag.hpp"
#include "SysUtil.hpp"
#include "moab/Error.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"

namespace moab {

inline
static ErrorCode not_root_set(const std::string& /*name*/, EntityHandle /*h*/)
{
  // MB_TAG_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
  // Print warning messages for debugging only
#if 0
  MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "Cannot get/set mesh/global tag " << name << " on non-root-set " << CN::EntityTypeName(TYPE_FROM_HANDLE(h)) << " " << (unsigned long)ID_FROM_HANDLE(h));
#endif

  return MB_TAG_NOT_FOUND;
}

inline
static bool all_root_set(std::string name, const EntityHandle* array, size_t len)
{
  for (size_t i = 0; i < len; ++i) {
    if (array[i]) {
      not_root_set(name, array[i]);
      return false;
    }
  }

  return true;
}

inline
static ErrorCode not_found(const std::string& /*name*/)
{
  // MB_TAG_NOT_FOUND could be a non-error condition, do not call MB_SET_ERR on it
#if 0
  fprintf(stderr, "[Warning]: No mesh tag %s value for global/mesh tag\n", name.c_str());
#endif

  return MB_TAG_NOT_FOUND;
}

MeshTag::MeshTag(const char * name,
                 int size,
                 DataType type,
                 const void * default_value,
                 int default_value_size)
 : TagInfo(name, size, type, default_value, default_value_size)
 {}

MeshTag::~MeshTag() {}

TagType MeshTag::get_storage_type() const
{
  return MB_TAG_MESH;
}

ErrorCode MeshTag::release_all_data(SequenceManager*, Error*, bool)
{
  return MB_SUCCESS;
}

ErrorCode MeshTag::get_data(const SequenceManager*,
                            Error* /* error */,
                            const EntityHandle* entities,
                            size_t num_entities,
                            void* data) const
{
  if (!all_root_set(get_name(), entities, num_entities))
    return MB_TAG_NOT_FOUND;

  const void* ptr;
  int len;

  if (!mValue.empty()) {
    ptr = &mValue[0];
    len = mValue.size();
  }
  else if (get_default_value()) {
    ptr = get_default_value();
    len = get_default_value_size();
  }
  else {
    return not_found(get_name());
  }

  SysUtil::setmem(data, ptr, len, num_entities);
  return MB_SUCCESS;
}

ErrorCode MeshTag::get_data(const SequenceManager*,
                            Error* /* error */,
                            const Range& r,
                            void*) const
{
  if (variable_length()) {
    MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No length specified for variable-length tag " << get_name() << " value");
  }
  else if (r.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), r.front());
}

ErrorCode MeshTag::get_data(const SequenceManager*,
                            Error* /* error */,
                            const EntityHandle* entities,
                            size_t num_entities,
                            const void** data_ptrs,
                            int* data_lengths) const
{
  const void* ptr;
  int len;

  if (!mValue.empty()) {
    ptr = &mValue[0];
    len = mValue.size();
  }
  else if (get_default_value()) {
    ptr = get_default_value();
    len = get_default_value_size();
  }
  else {
    return not_found(get_name());
  }

  for (size_t i = 0; i < num_entities; ++i) {
    if (entities[i])
      return not_root_set(get_name(), entities[i]); // Not root set
    data_ptrs[i] = ptr;
    if (data_lengths)
      data_lengths[i] = len;
  }

  return MB_SUCCESS;
}

ErrorCode MeshTag::get_data(const SequenceManager*,
                            Error* /* error */,
                            const Range& range,
                            const void**,
                            int*) const
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), range.front());
}

ErrorCode MeshTag::set_data(SequenceManager*,
                            Error* /* error */,
                            const EntityHandle* entities,
                            size_t num_entities,
                            const void* data)
{
  if (variable_length()) {
    MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No length specified for variable-length tag " << get_name() << " value");
  }
  if (!all_root_set(get_name(), entities, num_entities))
    return MB_TAG_NOT_FOUND;

  if (num_entities > 0) {
    mValue.resize(get_size());
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(data);
    memcpy(&mValue[0], bytes + get_size() * (num_entities - 1), get_size());
  }

  return MB_SUCCESS;
}
 
ErrorCode MeshTag::set_data(SequenceManager*,
                            Error* /* error */,
                            const Range& range,
                            const void*)
{
  if (variable_length()) {
    MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No length specified for variable-length tag " << get_name() << " value");
  }
  else if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), range.front());
}

ErrorCode MeshTag::set_data(SequenceManager*,
                            Error* /* error */,
                            const EntityHandle* entities,
                            size_t num_entities,
                            void const* const* data_ptrs,
                            const int* data_lengths)
{
  if (!all_root_set(get_name(), entities, num_entities))
    return MB_TAG_NOT_FOUND;

  ErrorCode valid = validate_lengths(NULL, data_lengths, num_entities);MB_CHK_ERR(valid);

  if (num_entities > 0) {
    mValue.resize(data_lengths[num_entities - 1]);
    memcpy(&mValue[0], data_ptrs[num_entities - 1], mValue.size());
  }

  return MB_SUCCESS;
}

ErrorCode MeshTag::set_data(SequenceManager*,
                            Error* /* error */,
                            const Range& range,
                            void const* const*,
                            const int*)
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), range.front());
}

ErrorCode MeshTag::clear_data(SequenceManager*,
                              Error* /* error */,
                              const EntityHandle* entities,
                              size_t num_entities,
                              const void* value_ptr,
                              int value_len)
{
  if (!all_root_set(get_name(), entities, num_entities))
    return MB_TAG_NOT_FOUND;

  ErrorCode valid = validate_lengths(NULL, value_len ? &value_len : 0, 1);MB_CHK_ERR(valid);

  if (num_entities > 0) {
    mValue.resize(value_len);
    memcpy(&mValue[0], value_ptr, value_len);
  }

  return MB_SUCCESS;
}

ErrorCode MeshTag::clear_data(SequenceManager*,
                              Error* /* error */,
                              const Range& range,
                              const void*,
                              int)
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), range.front());
}

ErrorCode MeshTag::remove_data(SequenceManager*,
                               Error* /* error */,
                               const EntityHandle* entities,
                               size_t num_entities)
{
  if (!all_root_set(get_name(), entities, num_entities))
    return MB_TAG_NOT_FOUND;

  if (num_entities)
    mValue.clear();

  return MB_SUCCESS;
}

ErrorCode MeshTag::remove_data(SequenceManager*,
                               Error* /* error */,
                               const Range& range)
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), range.front());
}

ErrorCode MeshTag::tag_iterate(SequenceManager*,
                               Error* /* error */,
                               Range::iterator& beg,
                               const Range::iterator& end,
                               void*&,
                               bool)
{
  if (beg == end)
    return MB_SUCCESS;
  else
    return not_root_set(get_name(), *beg);
}

ErrorCode MeshTag::get_tagged_entities(const SequenceManager*,
                                       Range&,
                                       EntityType,
                                       const Range*) const
{
  return MB_SUCCESS;
}

ErrorCode MeshTag::num_tagged_entities(const SequenceManager*,
                                       size_t&,
                                       EntityType,
                                       const Range*) const
{
  return MB_SUCCESS;
}

ErrorCode MeshTag::find_entities_with_value(const SequenceManager*,
                                            Error*,
                                            Range&,
                                            const void*,
                                            int,
                                            EntityType,
                                            const Range*) const
{
  return MB_SUCCESS;
}

bool MeshTag::is_tagged(const SequenceManager*, EntityHandle h) const
{
  return (0 == h) && (!mValue.empty());
}

ErrorCode MeshTag::get_memory_use(const SequenceManager*,
                                  unsigned long& total,
                                  unsigned long& per_entity) const
{
  total = TagInfo::get_memory_use() + sizeof(*this) + mValue.size();
  per_entity = 0;
  return MB_SUCCESS;
}

} // namespace moab
