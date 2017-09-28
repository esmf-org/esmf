#include "TagInfo.hpp"
#include "moab/Error.hpp"
#include "moab/ErrorHandler.hpp"
#include <string.h>  /* memcpy */
#include <stdlib.h>  /* realloc & free */
#include <assert.h>

namespace moab {

TagInfo::TagInfo(const char* name,
                 int size,
                 DataType type,
                 const void* default_value,
                 int default_value_size)
 : mDefaultValue(NULL),
   mMeshValue(NULL),
   mDefaultValueSize(default_value_size),
   mMeshValueSize(0),
   mDataSize(size),
   dataType(type)
{
  if (default_value) {
    mDefaultValue = malloc(mDefaultValueSize);
    memcpy(mDefaultValue, default_value, mDefaultValueSize);
  }
  if (name)
    mTagName = name;
}

TagInfo::~TagInfo()
{
  free(mDefaultValue);
  mDefaultValue = 0;
  mDefaultValueSize = 0;
}

int TagInfo::size_from_data_type(DataType t)
{
  static const int sizes[] = {1,
                              sizeof(int),
                              sizeof(double),
                              1,
                              sizeof(EntityHandle),
                              0 };
   return sizes[t];
}

bool TagInfo::equals_default_value(const void* data, int size) const
{
  if (!get_default_value())
    return false;

  if (variable_length() && size != get_default_value_size())
    return false;

  if (!variable_length() && size >=0 && size != get_size())
    return false;

  if (get_data_type() == MB_TYPE_BIT) {
    assert(get_size() <= 8 && get_default_value_size() == 1);
    unsigned char byte1 = *reinterpret_cast<const unsigned char*>(data);
    unsigned char byte2 = *reinterpret_cast<const unsigned char*>(get_default_value());
    unsigned char mask = (unsigned char)((1u << get_size()) - 1);
    return (byte1 & mask) == (byte2 & mask);
  }
  else {
    return !memcmp(data, get_default_value(), get_default_value_size());
  }
}

// Check that all lengths are valid multiples of the type size.
// Returns true if all lengths are valid, false otherwise.
bool TagInfo::check_valid_sizes(const int* sizes, int num_sizes) const
{
  const unsigned size = size_from_data_type(get_data_type());
  if (1 == size)
    return true;

  unsigned sum = 0;
  for (int i = 0; i < num_sizes; ++i)
    sum |= ((unsigned)sizes[i]) % size;

  return (0 == sum);
}

ErrorCode TagInfo::validate_lengths(Error* /* error_handler */,
                                    const int* lengths,
                                    size_t num_lengths) const
{
  int bits = 0;
  if (variable_length()) {
    if (!lengths) {
      MB_SET_ERR(MB_VARIABLE_DATA_LENGTH, "No size specified for variable-length tag");
    }
    const unsigned type_size = size_from_data_type(get_data_type());
    if (type_size == 1)
      return MB_SUCCESS;
    for (size_t i = 0; i < num_lengths; ++i)
      bits |= lengths[i] % type_size;
  }
  else if (lengths) {
    for (size_t i = 0; i < num_lengths; ++i)
      bits |= lengths[i] - get_size();
  }
  if (0 == bits)
    return MB_SUCCESS;

  MB_SET_ERR(MB_INVALID_SIZE, "Tag data with invalid size");
}

} // namespace moab
