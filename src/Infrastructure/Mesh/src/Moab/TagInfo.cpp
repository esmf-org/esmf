#include "TagInfo.hpp"
#include "moab/Error.hpp"
#include <string.h>  /* memcpy */
#include <stdlib.h>  /* realloc & free */
#include <assert.h>

namespace moab {

TagInfo::TagInfo( const char* name, 
                  int size, 
                  DataType type,
                  const void* default_value,
                  int default_value_size)
 : mDefaultValue(0),
   mDefaultValueSize(default_value_size),
   mDataSize(size),
   dataType(type)
{
  if (default_value) {
    mDefaultValue = malloc( mDefaultValueSize );
    memcpy( mDefaultValue, default_value, mDefaultValueSize );
  }
  if (name)
    mTagName = name;
}

TagInfo::~TagInfo() 
{
  free( mDefaultValue );
  mDefaultValue = 0;
  mDefaultValueSize = 0;
}

int TagInfo::size_from_data_type( DataType t )
{
  static const int sizes[] = { 1, 
                               sizeof(int), 
                               sizeof(double), 
                               1, 
                               sizeof(EntityHandle),
                               0 };  
   return sizes[t];
}

bool TagInfo::equals_default_value( const void* data, int size ) const
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
    return (byte1&mask) == (byte2&mask);
  }
  else {
    return !memcmp( data, get_default_value(), get_default_value_size() );
  }
}
  
    // Check that all lengths are valid multiples of the type size.
    // Returns true if all lengths are valid, false othersize.
bool TagInfo::check_valid_sizes( const int* sizes, int num_sizes ) const
{
  unsigned sum = 0;
  const unsigned size = size_from_data_type( get_data_type() );
  if (size == 1)
    return true;
  for (int i = 0; i < num_sizes; ++i)
    sum |= ((unsigned)sizes[i]) % size;
  return (sum == 0);
}

ErrorCode TagInfo::validate_lengths( Error* error_handler,
                                     const int* lengths, 
                                     size_t num_lengths ) const
{
  int bits = 0;
  if (variable_length()) {
    if (!lengths) {
      error_handler->set_last_error("No size specified for variable-length tag");
      return MB_VARIABLE_DATA_LENGTH;
    }
    const unsigned type_size = size_from_data_type( get_data_type() );
    if (type_size == 1)
      return MB_SUCCESS;
    for (size_t i = 0; i < num_lengths; ++i)
      bits |= lengths[i] % type_size;
  }
  else if (lengths) {
    for (size_t i = 0; i < num_lengths; ++i)
      bits |= lengths[i] - get_size();
  }
  if (!bits)
    return MB_SUCCESS;
  error_handler->set_last_error("Tag data with invalid size");
  return MB_INVALID_SIZE;
}

} // namespace moab

