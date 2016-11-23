#ifndef TAG_COMPARE_HPP
#define TAG_COMPARE_HPP

#include "TagInfo.hpp"
#include "VarLenTag.hpp"
#include <vector>

namespace moab {

/* OPAQUE FUNCTORS */

/** Test fixed-length opaque tags for equality */
class TagBytesEqual {
  private:
    const void* value;
    int size;
  public:
    TagBytesEqual( const void* v, int s ) : value(v), size(s) {}
    bool operator()( const void* data ) const
      { return !memcmp(value, data, size); }
};
/** Test if fixed-length opaque tag values are less than a value */
class TagBytesLess {
  private:
    const void* value;
    int size;
  public:
    TagBytesLess( const void* v, int s ) : value(v), size(s) {}
    bool operator()( const void* data ) const
      { return 0 < memcmp(value, data, size); }
};
/** Test variable-length opaque tags for equality */
class TagVarBytesEqual {
  private:
    const void* value;
    int size;
  public:
    TagVarBytesEqual( const void* v, int s ) : value(v), size(s) {}
    bool operator()( const void* data ) const {
      const VarLenTag* vdata = reinterpret_cast<const VarLenTag*>(data);
      return (int)vdata->size() == size && !memcmp(value, vdata->data(), size); 
    }
    bool operator()( const VarLenTag& vdata ) const {
      return (int)vdata.size() == size && !memcmp(value, vdata.data(), size); 
    }
};
/** Test if variable-length opaque tag values are less than a value */
class TagVarBytesLess {
  private:
    const void* value;
    int size;
  public:
    TagVarBytesLess( const void* v, int s ) : value(v), size(s) {}
    bool operator()( const void* data ) const {
      const VarLenTag* vdata = reinterpret_cast<const VarLenTag*>(data);
      if ((int)vdata->size() < size) 
        return 0 <= memcmp( vdata->data(), value, vdata->size() );
      else
        return 0 < memcmp( vdata->data(), value, size );
    }
    bool operator()( const VarLenTag& vdata ) const {
      if ((int)vdata.size() < size) 
        return 0 <= memcmp( vdata.data(), value, vdata.size() );
      else
        return 0 < memcmp( vdata.data(), value, size );
    }
};


/* TEMPLATE FUNCTORS */


/** Compare fixed-length tags containing a known data type */
template <typename T>
class TagTypeEqual {
  private:
    const T* value;
    int size;
  public:
    TagTypeEqual( const void* v, int s ) 
      : value(reinterpret_cast<const T*>(v)), 
        size(s/sizeof(T)) 
        {}
        
    bool operator()( const void* data ) const { 
      const T* ddata = reinterpret_cast<const T*>(data);
      for (int i = 0; i < size; ++i)
        if (value[i] != ddata[i])
          return false;
      return true;
    }
};

/** Compare fixed-length tags containing a known data type */
template <typename T>
class TagTypeLess {
  private:
    const T* value;
    int size;
  public:
    TagTypeLess( const void* v, int s ) 
      : value(reinterpret_cast<const T*>(v)), 
        size(s/sizeof(T)) 
        {}
    
    bool operator()( const void* data ) const {
      const T* ddata = reinterpret_cast<const T*>(data);
      for (int i = 0; i < size; ++i)
        if (value[i] <= ddata[i])
          return false;
      return true;
    }
};

/** Compare single-value tags containing a known data type
 * Optimization of TagTypeEqual for 1-value case. 
 */
template <typename T>
class TagOneTypeEqual {
  private:
    T value;
    int size;
  public:
    TagOneTypeEqual( const void* v ) 
      : value(*reinterpret_cast<const T*>(v)), size(0)
        {}
        
    bool operator()( const void* data ) const { 
      const T* ddata = reinterpret_cast<const T*>(data);
      return *ddata == value;
    }
};

/** Compare single-value tags containing a known data type
 * Optimization of TagTypeLess for 1-value case. 
 */
template <typename T>
class TagOneTypeLess {
  private:
    T value;
    int size;
  public:
    TagOneTypeLess( const void* v ) 
      : value(*reinterpret_cast<const T*>(v)), size(0)
        {}
    
    bool operator()( const void* data ) const {
      const T* ddata = reinterpret_cast<const T*>(data);
      return *ddata < value;
    }
};

/** Compare variable-length tags containing a known data type */
template <typename T>
class TagVarTypeEqual
{
  private:
    const T* value;
    int size;
  public:
    TagVarTypeEqual( const void* v, int s ) 
      : value(reinterpret_cast<const T*>(v)), 
        size(s/sizeof(T)) 
        {}
        
    bool operator()( const void* data ) const {
      const VarLenTag* vdata = reinterpret_cast<const VarLenTag*>(data);
      if (vdata->size() != size * sizeof(T))
        return false;
      const T* ddata = reinterpret_cast<const T*>(vdata->data());
      for (int i = 0; i < size; ++i)
        if (value[i] != ddata[i])
          return false;
      return true;
    }
        
    bool operator()( const VarLenTag& vdata ) const {
      if (vdata.size() != size * sizeof(T))
        return false;
      const T* ddata = reinterpret_cast<const T*>(vdata.data());
      for (int i = 0; i < size; ++i)
        if (value[i] != ddata[i])
          return false;
      return true;
    }
};

/** Compare variable-length tags containing a known data type */
template <typename T>
class TagVarTypeLess
{
  private:
    const T* value;
    int size;
  public:
    TagVarTypeLess( const void* v, int s ) 
      : value(reinterpret_cast<const T*>(v)), 
        size(s/sizeof(T)) 
        {}
    bool operator()( const void* data ) const {
      const VarLenTag* vdata = reinterpret_cast<const VarLenTag*>(data);
      const T* ddata = reinterpret_cast<const T*>(vdata->data());
      if ((int)vdata->size() < sizeof(T)*size) {
        for (int i = 0; i < vdata->size()/sizeof(T); ++i)
          if (value[i] < ddata[i])
            return false;
      }
      else {
        for (int i = 0; i < vdata->size()/sizeof(T); ++i)
          if (value[i] <= ddata[i])
            return false;
      }
      return true;
    }
    bool operator()( const VarLenTag& vdata ) const {
      const T* ddata = reinterpret_cast<const T*>(vdata.data());
      if ((int)vdata.size() < sizeof(T)*size) {
        for (int i = 0; i < vdata.size()/sizeof(T); ++i)
          if (value[i] < ddata[i])
            return false;
      }
      else {
        for (int i = 0; i < vdata.size()/sizeof(T); ++i)
          if (value[i] <= ddata[i])
            return false;
      }
      return true;
    }
};

/* TYPE FUNCTORS */

typedef TagBytesEqual        TagIntsEqual;
typedef TagVarBytesEqual     TagVarIntsEqual;
typedef TagTypeLess    <int> TagIntsLess;
typedef TagVarTypeLess <int> TagVarIntsLess;
typedef TagOneTypeEqual<int> TagOneIntEqual;
typedef TagOneTypeLess <int> TagOneIntLess;

typedef TagBytesEqual                   TagHandlesEqual;
typedef TagVarBytesEqual                TagVarHandlesEqual;
typedef TagTypeLess    <EntityHandle> TagHandlesLess;
typedef TagVarTypeLess <EntityHandle> TagVarHandlesLess;
typedef TagOneTypeEqual<EntityHandle> TagOneHandleEqual;
typedef TagOneTypeLess <EntityHandle> TagOneHandleLess;

typedef TagTypeEqual   <double> TagDoublesEqual;
typedef TagVarTypeEqual<double> TagVarDoublesEqual;
typedef TagTypeLess    <double> TagDoublesLess;
typedef TagVarTypeLess <double> TagVarDoublesLess;
typedef TagOneTypeEqual<double> TagOneDoubleEqual;
typedef TagOneTypeLess <double> TagOneDoubleLess;

/* SEARCHING */

template <class Functor,
          class IteratorType>
static inline
void find_tag_values( Functor compare,
                      IteratorType begin,
                      IteratorType end,
                      Range& results )
{
  Range::iterator insert = results.begin();
  for (IteratorType i = begin; i != end; ++i) 
    if (compare( i->second ))
      insert = results.insert( insert, i->first );
}

template <class Functor,
          class IteratorType>
static inline
void find_tag_values( Functor compare,
                      IteratorType begin,
                      IteratorType end,
                      std::vector<EntityHandle>& results )
{
  for (IteratorType i = begin; i != end; ++i) 
    if (compare( i->second ))
      results.push_back( i->first );
}

template <class Functor,
          class TagMap>
static inline
void find_map_values( Functor compare,
                      Range::const_iterator lower,
                      Range::const_iterator upper,
                      const TagMap& tag_map,
                      Range& results )
{
  Range::iterator insert = results.begin();
  for (; lower != upper; ++lower) {
    typename TagMap::const_iterator i = tag_map.find( *lower );
    if (i != tag_map.end() && compare( i->second ))
      insert = results.insert( insert, *lower );
  }
}

/** Find all entities for which a tag has a specific value
 *\param IteratorType : an iterator that has map behavior:
 *                      the value of 'first' is the entity handle.
 *                      the value of 'second' is a pointer to the tag data.
 *\param ContainerType : std::vector<EntityHandle> or Range
 */
template <class IteratorType, class ContainerType>
static inline
void find_tag_values_equal( const TagInfo& tag_info,
                            const void* value,
                            int size,
                            IteratorType begin,
                            IteratorType end,
                            ContainerType& results )
{
  switch (tag_info.get_data_type()) {
    case MB_TYPE_INTEGER:
      if (size == sizeof(int))
        find_tag_values<TagOneIntEqual,IteratorType>( TagOneIntEqual( value ), begin, end, results );
      else
        find_tag_values<TagIntsEqual,IteratorType>( TagIntsEqual( value, size ), begin, end, results );
      break;
        
    case MB_TYPE_DOUBLE:
      if (size == sizeof(double))
        find_tag_values<TagOneDoubleEqual,IteratorType>( TagOneDoubleEqual( value ), begin, end, results );
      else
        find_tag_values<TagDoublesEqual,IteratorType>( TagDoublesEqual( value, size ), begin, end, results );
      break;
        
    case MB_TYPE_HANDLE:
      if (size == sizeof(EntityHandle))
        find_tag_values<TagOneHandleEqual,IteratorType>( TagOneHandleEqual( value ), begin, end, results );
      else
        find_tag_values<TagHandlesEqual,IteratorType>( TagHandlesEqual( value, size ), begin, end, results );
      break;
        
    default:
      find_tag_values<TagBytesEqual,IteratorType>( TagBytesEqual( value, size ), begin, end, results );
      break;
  }
}
template <class IteratorType, class ContainerType>
static inline
void find_tag_varlen_values_equal( const TagInfo& tag_info,
                                   const void* value,
                                   int size,
                                   IteratorType begin,
                                   IteratorType end,
                                   ContainerType& results )
{
  switch (tag_info.get_data_type()) {
    case MB_TYPE_INTEGER:
      find_tag_values<TagVarIntsEqual,IteratorType>( TagVarIntsEqual( value, size ), begin, end, results );
      break;
    case MB_TYPE_DOUBLE:
      find_tag_values<TagVarDoublesEqual,IteratorType>( TagVarDoublesEqual( value, size ), begin, end, results );
      break;
    case MB_TYPE_HANDLE:
      find_tag_values<TagVarHandlesEqual,IteratorType>( TagVarHandlesEqual( value, size ), begin, end, results );
      break;
    default:
      find_tag_values<TagVarBytesEqual,IteratorType>( TagVarBytesEqual( value, size ), begin, end, results );
      break;
  }
}

/** Find all entities for which a tag has a specific value
 *\param IteratorType : an iterator that has map behavior:
 *                      the value of 'first' is the entity handle.
 *                      the value of 'second' is a pointer to the tag data.
 *\param ContainerType : std::vector<EntityHandle> or Range
 */
template <class TagMap>
static inline
void find_map_values_equal( const TagInfo& tag_info,
                            const void* value,
                            int size,
                            Range::const_iterator begin,
                            Range::const_iterator end,
                            const TagMap& tag_map,
                            Range& results )
{
  switch (tag_info.get_data_type()) {
    case MB_TYPE_INTEGER:
      if (size == sizeof(int))
        find_map_values<TagOneIntEqual,TagMap>( TagOneIntEqual( value ), begin, end, tag_map, results );
       else
        find_map_values<TagIntsEqual,TagMap>( TagIntsEqual( value, size ), begin, end, tag_map, results );
      break;
        
    case MB_TYPE_DOUBLE:
      if (size == sizeof(double))
        find_map_values<TagOneDoubleEqual,TagMap>( TagOneDoubleEqual( value ), begin, end, tag_map, results );
      else
        find_map_values<TagDoublesEqual,TagMap>( TagDoublesEqual( value, size ), begin, end, tag_map, results );
      break;
        
    case MB_TYPE_HANDLE:
      if (size == sizeof(EntityHandle))
        find_map_values<TagOneHandleEqual,TagMap>( TagOneHandleEqual( value ), begin, end, tag_map, results );
      else
        find_map_values<TagHandlesEqual,TagMap>( TagHandlesEqual( value, size ), begin, end, tag_map, results );
      break;
        
    default:
      find_map_values<TagBytesEqual,TagMap>( TagBytesEqual( value, size ), begin, end, tag_map, results );
      break;
  }
}
template <class TagMap>
static inline
void find_map_varlen_values_equal( const TagInfo& tag_info,
                                   const void* value,
                                   int size,
                                   Range::const_iterator begin,
                                   Range::const_iterator end,
                                   const TagMap& tag_map,
                                   Range& results )
{
  switch (tag_info.get_data_type()) {
    case MB_TYPE_INTEGER:
      find_map_values<TagVarIntsEqual,TagMap>( TagVarIntsEqual( value, size ), begin, end, tag_map, results );
      break;
    case MB_TYPE_DOUBLE:
      find_map_values<TagVarDoublesEqual,TagMap>( TagVarDoublesEqual( value, size ), begin, end, tag_map, results );
      break;
    case MB_TYPE_HANDLE:
      find_map_values<TagVarHandlesEqual,TagMap>( TagVarHandlesEqual( value, size ), begin, end, tag_map, results );
      break;
    default:
      find_map_values<TagVarBytesEqual,TagMap>( TagVarBytesEqual( value, size ), begin, end, tag_map, results );
      break;
  }
}

/** Iterator to use in find_tag_values_equal for arrays of data */
class ByteArrayIterator 
{
  public:
    typedef std::pair<EntityHandle, const char*> data_type;
  private:
    size_t step;
    data_type data;
  public:
    ByteArrayIterator( EntityHandle start_handle,
                       const void* data_array,
                       size_t tag_size )
      : step(tag_size),
        data(start_handle, reinterpret_cast<const char*>(data_array))
        
      {}
    ByteArrayIterator( EntityHandle start_handle,
                       const void* data_array,
                       const TagInfo& tag_info )
      : step(tag_info.get_size() == MB_VARIABLE_LENGTH ? sizeof(VarLenTag) : tag_info.get_size()),
        data(start_handle, reinterpret_cast<const char*>(data_array))
        {}
    bool operator==( const ByteArrayIterator& other ) const
      { return data.first == other.data.first; }
    bool operator!=( const ByteArrayIterator& other ) const
      { return data.first != other.data.first; }
    ByteArrayIterator& operator++()
      { ++data.first; data.second += step; return *this; }
    ByteArrayIterator operator++(int)
      { ByteArrayIterator result(*this); operator++(); return result; }
    ByteArrayIterator& operator--()
      { --data.first; data.second -= step; return *this; }
    ByteArrayIterator operator--(int)
      { ByteArrayIterator result(*this); operator--(); return result; }
    ByteArrayIterator& operator+=(size_t amt)
      { data.first += amt; data.second += amt*step; return *this; }
    ByteArrayIterator& operator-=(size_t amt)
      { data.first -= amt; data.second -= amt*step; return *this; }
    EntityHandle operator-( const ByteArrayIterator& other ) const
      { return data.first - other.data.first; }
    const data_type& operator*() const 
      { return data; }
    const data_type* operator->() const 
      { return &data; }
};


static inline
std::pair<EntityType,EntityType> type_range( EntityType type )
{
  if (type == MBMAXTYPE)
    return std::pair<EntityType,EntityType>(MBVERTEX,MBMAXTYPE);
  else {
    EntityType next = type; ++next;
    return std::pair<EntityType,EntityType>(type,next);
  }
}

/** Dummy container that counts insertions rather than maintaining a list of entities */
class InsertCount {
private:
  size_t mCount;
  
public:
  InsertCount( size_t initial_count = 0 ) : mCount(initial_count) {}

  typedef int iterator;
  iterator begin() const { return 0; }
  iterator end() const { return mCount; }
  iterator insert( iterator /* hint */, EntityHandle first, EntityHandle last )
    { mCount += last - first + 1; return end(); }
  iterator insert( iterator /* hint */, EntityHandle /* value */)
    { ++mCount; return end(); }
};


} // namespace moab

#endif

