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
#include "moab/CN.hpp"
#include <utility>

namespace moab {

static ErrorCode not_found( Error* error, std::string name, EntityHandle h )
{
  if (error) {
    if (h)
      error->set_last_error( "No var length dense tag %s value for %s %lu", 
                             name.c_str(),
                             CN::EntityTypeName(TYPE_FROM_HANDLE(h)), 
                             (unsigned long)ID_FROM_HANDLE(h));
    else
      error->set_last_error( "No var length dense tag %s value for root set", name.c_str() );
  }
  
  return MB_TAG_NOT_FOUND;
}

static ErrorCode not_var_len( Error* error, std::string name )
{
  error->set_last_error( "No size specified for variable-length tag %s data", name.c_str());
  return MB_VARIABLE_DATA_LENGTH;
}

VarLenDenseTag::VarLenDenseTag( int index,
                                const char* name, 
                                DataType type, 
                                const void* default_value,
                                int default_value_size )
  : TagInfo( name, MB_VARIABLE_LENGTH, type, default_value, default_value_size ), 
    mySequenceArray(index)
  {}

VarLenDenseTag* VarLenDenseTag::create_tag( SequenceManager* seqman,
                                            Error* error,
                                            const char* name,
                                            DataType type,
                                            const void* default_value,
                                            int default_value_size )
{
  int index; 
  if (MB_SUCCESS != seqman->reserve_tag_array( error, MB_VARIABLE_LENGTH, index ))
    return 0;
  
  return new VarLenDenseTag( index, name, type, default_value, default_value_size );
}

  
VarLenDenseTag::~VarLenDenseTag()
{
  assert( mySequenceArray < 0 );
}

TagType VarLenDenseTag::get_storage_type() const 
  { return MB_TAG_DENSE; }

ErrorCode VarLenDenseTag::release_all_data( SequenceManager* seqman, 
                                            Error* error, 
                                            bool delete_pending )
{
  Range all_ents;
  seqman->get_entities( all_ents );
  ErrorCode rval = remove_data( seqman, error, all_ents );
  if (MB_SUCCESS == rval) {
    rval = seqman->release_tag_array( error, mySequenceArray, delete_pending );
    if (MB_SUCCESS == rval && delete_pending)
      mySequenceArray = -1;
  }
  return rval;
}

ErrorCode VarLenDenseTag::get_array( const SequenceManager* seqman, 
                                     Error* error, 
                                     EntityHandle h, 
                                     const VarLenTag*& ptr,
                                     size_t& count ) const
{
  const EntitySequence* seq = 0;
  ErrorCode rval = seqman->find( h, seq );
  if (MB_SUCCESS != rval) {
    if (!h) { // root set
      ptr = &meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else {
      ptr = 0;
      count = 0;
      return not_found( error, get_name(), h );
    }
  }
  
  const void* mem = seq->data()->get_tag_data( mySequenceArray );
  ptr = reinterpret_cast<const VarLenTag*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += h - seq->data()->start_handle();

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::get_array( SequenceManager* seqman, 
                                     Error* error, 
                                     EntityHandle h, 
                                     VarLenTag*& ptr,
                                     size_t& count,
                                     bool allocate ) 
{
  EntitySequence* seq = 0;
  ErrorCode rval = seqman->find( h, seq );
  if (MB_SUCCESS != rval) {
    if (!h) { // root set
      ptr = &meshValue;
      count = 1;
      return MB_SUCCESS;
    }
    else {
      ptr = 0;
      count = 0;
      return not_found( error, get_name(), h );
    }
  }
  
  void* mem = seq->data()->get_tag_data( mySequenceArray );
  if (!mem && allocate) {
    mem = seq->data()->allocate_tag_array( mySequenceArray, sizeof(VarLenTag) );
    if (!mem) {
      error->set_last_error( "Memory allocation for var-len tag data failed" );
      return MB_MEMORY_ALLOCATION_FAILED;
    }
    
    memset( mem, 0, sizeof(VarLenTag) * seq->data()->size() );
  }
  
  ptr = reinterpret_cast<VarLenTag*>(mem);
  count = seq->data()->end_handle() - h + 1;
  if (ptr)
    ptr += h - seq->data()->start_handle();
  return MB_SUCCESS;

}

ErrorCode VarLenDenseTag::get_data( const SequenceManager*,
                                    Error* error, 
                                    const EntityHandle*,
                                    size_t,
                                    void* ) const
{
  return not_var_len(error, get_name());
}


ErrorCode VarLenDenseTag::get_data( const SequenceManager*,
                                    Error* error, 
                                    const Range&,
                                    void* ) const
{
  return not_var_len(error, get_name());
}
                     
ErrorCode VarLenDenseTag::get_data( const SequenceManager* seqman,
                                    Error* error, 
                                    const EntityHandle* entities,
                                    size_t num_entities,
                                    const void** pointers,
                                    int* lengths ) const
{
  if (!lengths)
    return not_var_len(error, get_name());

  ErrorCode result = MB_SUCCESS, rval;
  const EntityHandle *const end = entities + num_entities;
  size_t junk;
  const VarLenTag* ptr;

  for (const EntityHandle* i = entities; i != end; ++i, ++pointers, ++lengths) {
    rval = get_array( seqman, error, *i, ptr, junk );
    if (MB_SUCCESS != rval) 
      return rval;
  
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
      result = not_found( error, get_name(), *i );;
    }
  }
  
  return result;
}

                      
ErrorCode VarLenDenseTag::get_data( const SequenceManager* seqman,
                                    Error* error, 
                                    const Range& entities,
                                    const void** pointers,
                                    int* lengths ) const
{
  if (!lengths)
    return not_var_len(error, get_name());

  ErrorCode rval;
  size_t avail;
  const VarLenTag* array = 0;
  
  for (Range::const_pair_iterator p = entities.const_pair_begin(); 
       p != entities.const_pair_end(); ++p) {
       
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array( seqman, error, start, array, avail );
      if (MB_SUCCESS != rval) 
        return rval;
      
      const size_t count = std::min<size_t>(p->second - start + 1, avail);

      if (!array) {
        const void* defval = get_default_value();
        const int len = get_default_value_size();
        SysUtil::setmem( pointers, &defval, sizeof(void*), count );
        SysUtil::setmem( lengths, &len, sizeof(int), count );
        pointers += count;
        lengths += count;
        if (!defval)
          return not_found( error, get_name(), start );
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
          *pointers = 0;
          *lengths = 0;
          return not_found( error, get_name(), start );
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
  
ErrorCode VarLenDenseTag::set_data( SequenceManager*,
                                    Error* error, 
                                    const EntityHandle*,
                                    size_t,
                                    const void* )
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenDenseTag::set_data( SequenceManager*,
                                    Error* error, 
                                    const Range&,
                                    const void* )
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenDenseTag::set_data( SequenceManager* seqman,
                                    Error* error, 
                                    const EntityHandle* entities,
                                    size_t num_entities,
                                    bool one_value,
                                    void const* const* pointers,
                                    const int* lengths )
{
  ErrorCode rval = validate_lengths( error, lengths, one_value ?  1 : num_entities );
  if (MB_SUCCESS != rval)
    return rval;
  
  const EntityHandle* const end = entities + num_entities;
  VarLenTag* array;
  size_t junk;
  const size_t step = one_value ? 0 : 1;
  
  for (const EntityHandle* i = entities; i != end; ++i ) {
    rval = get_array( seqman, error, *i, array, junk, true );
    if (MB_SUCCESS != rval)
      return rval;
    
    array->set( *pointers, *lengths );
    pointers += step;
    lengths += step;
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::set_data( SequenceManager* seqman,
                                    Error* error, 
                                    const Range& entities,
                                    bool one_value,
                                    void const* const* pointers,
                                    const int* lengths )
{
  ErrorCode rval = validate_lengths( error, lengths, one_value ?  1 : entities.size() );
  if (MB_SUCCESS != rval)
    return rval;
  
  VarLenTag* array;
  size_t avail;
  const size_t step = one_value ? 0 : 1;

  for (Range::const_pair_iterator p = entities.const_pair_begin(); 
       p != entities.const_pair_end(); ++p) {
       
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array( seqman, error, start, array, avail, true );
      if (MB_SUCCESS != rval)
        return rval;
      
      const EntityHandle end = std::min<EntityHandle>(p->second + 1, start + avail );
      while (start != end) {
        array->set( *pointers, *lengths );
        ++start;
        ++array;
        pointers += step;
        lengths += step;
      }
    }
  }
  
  return MB_SUCCESS;
}
                      
ErrorCode VarLenDenseTag::set_data( SequenceManager* seqman,
                                    Error* error, 
                                    const EntityHandle* entities,
                                    size_t num_entities,
                                    void const* const* pointers,
                                    const int* lengths )
{
  return set_data( seqman, error, entities, num_entities, false, pointers, lengths );
}
  
                      
ErrorCode VarLenDenseTag::set_data( SequenceManager* seqman,
                                    Error* error, 
                                    const Range& entities,
                                    void const* const* pointers,
                                    const int* lengths )
{
  return set_data( seqman, error, entities, false, pointers, lengths );
}

ErrorCode VarLenDenseTag::clear_data( SequenceManager* seqman,
                                      Error* error, 
                                      const EntityHandle* entities,
                                      size_t num_entities,
                                      const void* value_ptr,
                                      int value_len )
{ 
  if (!value_ptr || !value_len)
    return remove_data( seqman, error, entities, num_entities );
  else
    return set_data( seqman, error, entities, num_entities, true, &value_ptr, &value_len );
}

ErrorCode VarLenDenseTag::clear_data( SequenceManager* seqman,
                                      Error* error, 
                                      const Range& entities,
                                      const void* value_ptr,
                                      int value_len )
{
  if (!value_ptr || !value_len)
    return remove_data( seqman, error, entities );
  else
    return set_data( seqman, error, entities, true, &value_ptr, &value_len );
}

ErrorCode VarLenDenseTag::remove_data( SequenceManager* seqman,
                                       Error* error, 
                                       const EntityHandle* entities,
                                       size_t num_entities )
{
  const EntityHandle* const end = entities + num_entities;
  VarLenTag* array;
  size_t junk;
  ErrorCode rval;
  
  for (const EntityHandle* i = entities; i != end; ++i ) {
    rval = get_array( seqman, error, *i, array, junk, false );
    if (MB_SUCCESS != rval)
      return rval;
    
    if (array) 
      array->clear();
  }

  return MB_SUCCESS;
}

ErrorCode VarLenDenseTag::remove_data( SequenceManager* seqman,
                                       Error* error, 
                                       const Range& entities )
{
  VarLenTag* array;
  size_t avail;
  ErrorCode rval;

  for (Range::const_pair_iterator p = entities.const_pair_begin(); 
       p != entities.const_pair_end(); ++p) {
       
    EntityHandle start = p->first;
    while (start <= p->second) {
      rval = get_array( seqman, error, start, array, avail, false );
      if (MB_SUCCESS != rval)
        return rval;
      
      const EntityHandle end = std::min<EntityHandle>(p->second + 1, start + avail );
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


ErrorCode VarLenDenseTag::tag_iterate( SequenceManager*,
                                       Error* error, 
                                       Range::iterator&,
                                       const Range::iterator&,
                                       void*&,
                                       bool)
{
  error->set_last_error( "Cannot iterate over variable-length tag data" );
  return MB_VARIABLE_DATA_LENGTH;
}

template <class Container> static inline 
ErrorCode get_tagged( const SequenceManager* seqman,
                      int mySequenceArray,
                      EntityType type,
                      Container& entities )
{
  typename Container::iterator hint = entities.begin();
  std::pair<EntityType,EntityType> range = type_range(type);
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
          hint = entities.insert( hint, handle );
    }
  }
  return MB_SUCCESS;
}

template <class Container> static inline 
ErrorCode get_tagged( const SequenceManager* seqman,
                      int mySequenceArray,
                      Range::const_iterator begin,
                      Range::const_iterator end,
                      Container& entities )
{
  typename Container::iterator hint = entities.begin();
  RangeSeqIntersectIter iter(const_cast<SequenceManager*>(seqman));
  ErrorCode rval = iter.init( begin, end );
  const VarLenTag* data;
  for (; MB_SUCCESS == rval; rval = iter.step()) {
    data = reinterpret_cast<const VarLenTag*>(iter.get_sequence()->data()->get_tag_data(mySequenceArray));
    if (!data)
      continue;      
    
    data += iter.get_start_handle() - iter.get_sequence()->data()->start_handle();  
    size_t count = iter.get_end_handle() - iter.get_start_handle() + 1;
    for (size_t i = 0; i < count; ++i) 
      if (data[i].size())
        hint = entities.insert( hint, iter.get_start_handle() + i );
    rval = iter.step();
  }
  if (MB_FAILURE != rval) // we get MB_FAILURE at iterator end
    return rval;
  return MB_SUCCESS;
}

template <class Container> static inline 
ErrorCode get_tagged( const SequenceManager* seqman,
                      int mySequenceArray,
                      Container& entities,
                      EntityType type,
                      const Range* intersect )
{
  if (!intersect)
    return get_tagged<Container>( seqman, mySequenceArray, type, entities );
  else if (MBMAXTYPE == type)
    return get_tagged<Container>( seqman, mySequenceArray, intersect->begin(), intersect->end(), entities );
  else {
    std::pair<Range::iterator,Range::iterator> r = intersect->equal_range(type);
    return get_tagged<Container>( seqman, mySequenceArray, r.first, r.second, entities );
  }
}

ErrorCode VarLenDenseTag::get_tagged_entities( const SequenceManager* seqman,
                                               Range& entities,
                                               EntityType type,
                                               const Range* intersect ) const
{
  return get_tagged( seqman, mySequenceArray, entities, type, intersect );
}

ErrorCode VarLenDenseTag::num_tagged_entities( const SequenceManager* seqman,
                                               size_t& output_count,
                                               EntityType type,
                                               const Range* intersect ) const
{
  InsertCount counter( output_count );
  ErrorCode rval = get_tagged( seqman, mySequenceArray, counter, type, intersect );
  output_count = counter.end();
  return rval;
}
  
ErrorCode VarLenDenseTag::find_entities_with_value( const SequenceManager* seqman,
                                                    Error* error, 
                                                    Range& output_entities,
                                                    const void* value,
                                                    int value_bytes,
                                                    EntityType type,
                                                    const Range* intersect_entities ) const
{
  if (!intersect_entities) {
    std::pair<EntityType,EntityType> range = type_range(type);
    TypeSequenceManager::const_iterator i;
    for (EntityType t = range.first; t != range.second; ++i) {
      const TypeSequenceManager& map = seqman->entity_map(t);
      for (i = map.begin(); i != map.end(); ++i) {
        const void* data = (*i)->data()->get_tag_data( mySequenceArray );
        if (data) {
          ByteArrayIterator start( (*i)->data()->start_handle(), data, *this );
          ByteArrayIterator end( (*i)->end_handle() + 1, 0, 0 );
          start += (*i)->start_handle() - (*i)->data()->start_handle();
          find_tag_varlen_values_equal( *this, value, value_bytes, start, end, output_entities );
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
        rval = get_array( seqman, error, start, array, count );
        if (MB_SUCCESS != rval)
          return rval; 
        
        if (p->second - start < count-1)
          count = p->second - start + 1;
        
        if (array) {
          ByteArrayIterator istart( start, array, *this );
          ByteArrayIterator iend( start+count, 0, 0 );
          find_tag_varlen_values_equal( *this, value, value_bytes, istart, iend, output_entities );
        }
        start += count;
      }
    }
  }    
  
  return MB_SUCCESS;
}

bool VarLenDenseTag::is_tagged( const SequenceManager* seqman, EntityHandle h) const
{
  const VarLenTag* ptr;
  size_t count;
  return MB_SUCCESS == get_array( seqman, 0, h, ptr, count ) 
          && 0 != ptr && 0 != ptr->data();
}
  
ErrorCode VarLenDenseTag::get_memory_use( const SequenceManager* seqman,
                                          unsigned long& total,
                                          unsigned long& per_entity ) const

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
