#ifndef VAR_LEN_TAG_HPP
#define VAR_LEN_TAG_HPP

#include <stdlib.h>
#include <string.h>

namespace moab {

/* Remove this preprocessor macro to compile 
 * simple implementation w/ no inlined storage
 */
#define VAR_LEN_TAG_ELIDE_DATA


/* Define class data layout depending on macros:
 * VAR_LEN_TAG_ELIDE_DATA and TEMPLATE_SPECIALIZATION
 */
#ifndef VAR_LEN_TAG_ELIDE_DATA

/* The trivial implementation */
public VarLenTagData {
public:

  struct { struct { unsigned size; unsigned char* array; } mPointer; } mData;
};

#elif !defined(MOAB_TEMPLATE_SPECIALIZATION)

/* A little more advanced data structure for VarLenTag.
 * If the amount of tag data is less than or equal to the
 * size of the array pointer, store it inline in the pointer
 * value field so no memory needs to be allocated.
 */
class VarLenTagData {
public:

  enum {
    INLINE_COUNT = sizeof(unsigned char*)
  };

  union {
    struct { 
      unsigned char* array;
      unsigned size;
    } mPointer;
    struct {
      unsigned char array[INLINE_COUNT];
      unsigned size;
    } mInline;
  } mData;
};

#else

/* Most complex implementation.  Same as the previous one, except
 * when storing data inline, also utilize any padding in the struct.
 * This implementation requires support for template specialization.
 * 
 * - The data must be first in the struct to avoid alignment issues
 *   for double and 64-bit handle values on some platforms.
 * - The size must therefore be at the end of the struct (including
 *   after any padding) because a) it cannot be at the beginning and
 *   b) it must be at the same location in both structs in the union.
 * - For the mPointer variation, the padding must be declared
 *   explicitly in order for the size to be forced to the end.
 * - Template specialization is used to avoid declaring a
 *   zero-length array for pad on 32-bit platforms.  
 *   NOTE: GCC allows zero-length arrays, but Sun's compiler 
 *   (and most others) do not.
 */
template <unsigned>
class VarLenTagDataTemplate {
public:
  inline VarLenTagDataTemplate() {}

  struct MallocData {
    unsigned char* array;
    unsigned size;
  };

  enum {
    INLINE_COUNT = sizeof(MallocData) - sizeof(unsigned)
  };
  
  union {
    struct {
      unsigned char* array;
      unsigned char pad[INLINE_COUNT - sizeof(unsigned char*)];
      unsigned size;
    } mPointer;
    struct {
      unsigned char array[INLINE_COUNT];
      unsigned size;
    } mInline;
  } mData;
};

template <> class VarLenTagDataTemplate<0u>
{
public:
  inline VarLenTagDataTemplate<0u>() {}

  enum {
    INLINE_COUNT = sizeof(unsigned char*)
  };
  
  union {
    struct {
      unsigned char* array;
      unsigned size;
    } mPointer;
    struct {
      unsigned char array[INLINE_COUNT];
      unsigned size;
    } mInline;
  } mData;
};

typedef VarLenTagDataTemplate<sizeof(unsigned char*) - sizeof(unsigned)> VarLenTagData;

#endif

/**\brief Class for storing variable-length tag data
 *
 * Class for managing variable-length tag data.  
 *\NOTE This class must behave as if it were initialized to empty
 *      if it is memset to zero w/out invoking any constructor.
 */ 
class VarLenTag {
protected:
  VarLenTagData mData;
public:
  inline VarLenTag() { mData.mData.mPointer.size = 0; }
  inline VarLenTag( unsigned size );
  inline ~VarLenTag() { clear(); }
  inline VarLenTag( const VarLenTag& copy );
  inline VarLenTag( unsigned size, const void* data );
  
  inline unsigned size() const { return mData.mData.mPointer.size; }

  inline unsigned char* data() 
#ifdef VAR_LEN_TAG_ELIDE_DATA    
    { return size() <= VarLenTagData::INLINE_COUNT ? 
                        mData.mData.mInline.array : 
                        mData.mData.mPointer.array; }
#else
    { return mData.mData.mPointer.array; }
#endif

  inline unsigned long mem() const
#ifdef VAR_LEN_TAG_ELIDE_DATA    
    { return size() <= VarLenTagData::INLINE_COUNT ? 0 : size(); }
#else
    { return size(); }
#endif

  inline const unsigned char* data() const 
    { return const_cast<VarLenTag*>(this)->data(); }
  
  inline unsigned char* resize( unsigned size );
  
  inline void clear();
  
  inline void set( const void* dat, unsigned sz )
    { memcpy( resize(sz), dat, sz ); }
    
  inline VarLenTag& operator=( const VarLenTag& other )
    { set( other.data(), other.size() ); return *this; }
  
};
  
inline unsigned char* VarLenTag::resize( unsigned s ) 
{
#ifdef VAR_LEN_TAG_ELIDE_DATA
  if (s <= VarLenTagData::INLINE_COUNT) {
    if (size() > VarLenTagData::INLINE_COUNT) {
      unsigned char* tmp_ptr = mData.mData.mPointer.array;
      memcpy( mData.mData.mInline.array, tmp_ptr, s );
      free( tmp_ptr );
    }
    mData.mData.mInline.size = s;
    return mData.mData.mInline.array;
  }
  else if (size() <= VarLenTagData::INLINE_COUNT) {
    void* tmp_ptr = malloc(s);
    memcpy( tmp_ptr, mData.mData.mInline.array, size() );
    mData.mData.mPointer.array = reinterpret_cast<unsigned char*>(tmp_ptr);
  }
  else 
#endif
  if (size() < s) {
    void* tmp_ptr = size() ? realloc( mData.mData.mPointer.array, s ) : malloc( s );
    mData.mData.mPointer.array = reinterpret_cast<unsigned char*>(tmp_ptr);
  }
  mData.mData.mPointer.size = s;
  return mData.mData.mPointer.array;
}

inline VarLenTag::VarLenTag( unsigned sz )
{
#ifdef VAR_LEN_TAG_ELIDE_DATA
  if (sz > VarLenTagData::INLINE_COUNT) 
#endif
    mData.mData.mPointer.array = reinterpret_cast<unsigned char*>(malloc(sz));
  mData.mData.mPointer.size = sz;
}

inline void VarLenTag::clear()
{
#ifdef VAR_LEN_TAG_ELIDE_DATA
  if (size() > VarLenTagData::INLINE_COUNT)
#else
  if (size())
#endif
    free( mData.mData.mPointer.array );
  mData.mData.mPointer.size = 0;
}

inline VarLenTag::VarLenTag( const VarLenTag& copy )
  : mData( copy.mData )
{
#ifdef VAR_LEN_TAG_ELIDE_DATA
  if (size() > VarLenTagData::INLINE_COUNT)
#endif
  {
    mData.mData.mPointer.array = reinterpret_cast<unsigned char*>(malloc(size()));
    memcpy( mData.mData.mPointer.array, copy.mData.mData.mPointer.array, size() );
  }
}

inline VarLenTag::VarLenTag( unsigned sz, const void* dat )
{
  mData.mData.mPointer.size = 0;
  if (sz) 
    memcpy( resize(sz), dat, sz );
}

} // namespace moab

#endif


    
