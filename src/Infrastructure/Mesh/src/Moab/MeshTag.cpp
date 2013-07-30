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


    static ErrorCode not_root_set( Error* error, std::string name, EntityHandle h )
{
  error->set_last_error( "Cannot get/set mesh/global tag %s on non-root-set %s %lu",
                         name.c_str(), 
                         CN::EntityTypeName(TYPE_FROM_HANDLE(h)), 
                         (unsigned long)ID_FROM_HANDLE(h) );
  return MB_VARIABLE_DATA_LENGTH;
}


static inline bool all_root_set( Error* error, std::string name, const EntityHandle* array, size_t len )
{
  for (size_t i = 0; i < len; ++i)
    if (array[i]) {
      not_root_set( error, name, array[i] );
      return false;
    }
  return true;
}

static ErrorCode not_found( Error* error, std::string name )
{
  error->set_last_error( "No mesh tag %s value for global/mesh tag", name.c_str());
  return MB_TAG_NOT_FOUND;
}

static ErrorCode var_len( Error* error, std::string name )
{
  error->set_last_error( "No length specified for variable-length tag %s value", name.c_str());
  return MB_VARIABLE_DATA_LENGTH;
}


MeshTag::MeshTag( const char * name, 
                  int size, 
                  DataType type, 
                  const void * default_value,
                  int default_value_size)
 : TagInfo( name, size, type, default_value, default_value_size )
 {}
  
MeshTag::~MeshTag() {}

TagType MeshTag::get_storage_type() const 
  { return MB_TAG_MESH; }

ErrorCode MeshTag::release_all_data( SequenceManager*, Error*, bool )
  { return MB_SUCCESS; }

ErrorCode MeshTag::get_data( const SequenceManager*,
                             Error* error,
                             const EntityHandle* entities,
                             size_t num_entities,
                             void* data ) const
{
  if (!all_root_set( error, get_name(), entities, num_entities ))
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
    return not_found(error, get_name());
  }

  SysUtil::setmem( data, ptr, len, num_entities );
  return MB_SUCCESS;
}
  

ErrorCode MeshTag::get_data( const SequenceManager*,
                             Error* error,
                             const Range& r,
                             void* ) const
{
  if (variable_length())
    return var_len(error, get_name());
  else if (r.empty())
    return MB_SUCCESS;
  else 
    return not_root_set( error, get_name(), r.front() );
}
                      
ErrorCode MeshTag::get_data( const SequenceManager*,
                             Error* error,
                             const EntityHandle* entities,
                             size_t num_entities,
                             const void** data_ptrs,
                             int* data_lengths ) const 
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
    return not_found( error, get_name() );
  }
    
  for (size_t i = 0; i < num_entities; ++i) {
    if (entities[i]) return not_root_set(error,get_name(), entities[i]); // not root set
    data_ptrs[i] = ptr;
    if (data_lengths)
      data_lengths[i] = len;
  }
  return MB_SUCCESS;
}
                      
                      
ErrorCode MeshTag::get_data( const SequenceManager*,
                             Error* error,
                             const Range& range,
                             const void**,
                             int* ) const
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), range.front() );
}
  
ErrorCode MeshTag::set_data( SequenceManager*,
                             Error* error,
                             const EntityHandle* entities,
                             size_t num_entities,
                             const void* data )
{
  if (variable_length())
    return var_len(error, get_name());;
  if (!all_root_set( error, get_name(), entities, num_entities ))
    return MB_TAG_NOT_FOUND;
  
  if (num_entities > 0) {
    mValue.resize( get_size() );
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(data);
    memcpy( &mValue[0], bytes + get_size() * (num_entities - 1), get_size() );
  }
  return MB_SUCCESS;
}
 
ErrorCode MeshTag::set_data( SequenceManager*,
                             Error* error,
                             const Range& range,
                             const void* )
{
  if (variable_length())
    return var_len(error, get_name());
  else if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), range.front() );
}

ErrorCode MeshTag::set_data( SequenceManager*,
                             Error* error,
                             const EntityHandle* entities,
                             size_t num_entities,
                             void const* const* data_ptrs,
                             const int* data_lengths )
{
  if (!all_root_set( error, get_name(), entities, num_entities ))
    return MB_TAG_NOT_FOUND;
  
  ErrorCode valid = validate_lengths( error, data_lengths, num_entities );
  if (MB_SUCCESS != valid)
    return valid;
  
  if (num_entities > 0) {
    mValue.resize( data_lengths[num_entities-1] );
    memcpy( &mValue[0], data_ptrs[num_entities-1], mValue.size() );
  }
  return MB_SUCCESS;
}
                      
                      
ErrorCode MeshTag::set_data( SequenceManager*,
                             Error* error,
                             const Range& range,
                             void const* const*,
                             const int* )
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), range.front() );
}

ErrorCode MeshTag::clear_data( SequenceManager*,
                               Error* error,
                               const EntityHandle* entities,
                               size_t num_entities,
                               const void* value_ptr,
                               int value_len )
{
  if (!all_root_set( error, get_name(), entities, num_entities ))
    return MB_TAG_NOT_FOUND;
  
  ErrorCode valid = validate_lengths( error, value_len ? &value_len : 0, 1 );
  if (MB_SUCCESS != valid)
    return valid;
  
  if (num_entities > 0) {
    mValue.resize( value_len );
    memcpy( &mValue[0], value_ptr, value_len );
  }

  return MB_SUCCESS;
}

ErrorCode MeshTag::clear_data( SequenceManager*,
                               Error* error,
                               const Range& range,
                               const void*,
                               int )
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), range.front() );
}

ErrorCode MeshTag::remove_data( SequenceManager*,
                                Error* error,
                                const EntityHandle* entities,
                                size_t num_entities )
{
  if (!all_root_set( error, get_name(), entities, num_entities ))
    return MB_TAG_NOT_FOUND;
  
  if (num_entities)
    mValue.clear();;
  return MB_SUCCESS;
}

ErrorCode MeshTag::remove_data( SequenceManager*,
                                Error* error,
                                const Range& range )
{
  if (range.empty())
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), range.front() );
}

ErrorCode MeshTag::tag_iterate( SequenceManager*,
                                Error* error,
                                Range::iterator& beg,
                                const Range::iterator& end,
                                void*&,
                                bool)
{
  if (beg == end)
    return MB_SUCCESS;
  else
    return not_root_set( error, get_name(), *beg );
}

ErrorCode MeshTag::get_tagged_entities( const SequenceManager*,
                                        Range&,
                                        EntityType,
                                        const Range* ) const
{
  return MB_SUCCESS;
}

ErrorCode MeshTag::num_tagged_entities( const SequenceManager*,
                                        size_t&,
                                        EntityType,
                                        const Range* ) const
{
  return MB_SUCCESS;
}

ErrorCode MeshTag::find_entities_with_value( const SequenceManager*,
                                             Error*,
                                             Range&,
                                             const void*,
                                             int,
                                             EntityType,
                                             const Range* ) const
{
  return MB_SUCCESS;
}

bool MeshTag::is_tagged( const SequenceManager*, EntityHandle h ) const
  { return !h && !mValue.empty(); }

ErrorCode MeshTag::get_memory_use( const SequenceManager*,
                                   unsigned long& total,
                                   unsigned long& per_entity ) const
{
  total = TagInfo::get_memory_use() + sizeof(*this) + mValue.size();
  per_entity = 0;
  return MB_SUCCESS;
}


} // namespace moab
