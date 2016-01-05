#ifndef MB_SYS_UTIL_HPP
#define MB_SYS_UTIL_HPP

#include <string.h> // for size_t
#include <stdio.h>
#include <iosfwd>

namespace moab {

namespace SysUtil
{

/**\brief Similar to memset, but accepts values larger than 1 char
 *
 * Set block of memory to repeating copies of a sequene of bytes.
 *\param mem   Pointer to start of memory block to initialize
 *\param value Byte sequence to initialize mem with
 *\param value_size Size of 'value'
 *\param num_elem Size of 'mem' as a multiple of value_size (the number of
 *             copies of 'value' to write into 'mem'.)
 */
void setmem( void* mem, const void* value, unsigned value_size, size_t num_elem );

/**\brief Get size of file (if it is a regular file)
 *
 * Get size of regular file.  
 *\return - file size if known
 *        - -1 if file size cannot be determined (e.g. a pipe) 
 *        - -2 if an unexpected failure occured (may indicate change
 *           in file position.)
 */
long filesize( FILE* filp );

/**\brief Get size of file (if it is a regular file)
 *
 * Get size of regular file.  
 *\return - file size if known
 *        - -1 if file size cannot be determined (e.g. a pipe) 
 *        - -2 if an unexpected failure occured (may indicate change
 *           in file position.)
 */
long filesize( std::ifstream& str );

/**\brief Check if platform is little-endian
 * 
 * Check if platform is little-endian (least significant
 * byte at highest memory address.)
 */
inline bool little_endian()
{
  const unsigned one = 1;
  return !*((char*)&one);
}

/**\brief Check if platform is big-endian
 * 
 * Check if platform is big-endian (least significant
 * byte at lowest memory address.)
 */
inline bool big_endian()
{
  const unsigned one = 1;
  return !(((char*)&one)[sizeof(unsigned)-1]);
}

/**\brief Swap byte order (e.g. change from big-endian to little-endian)
 *
 * Reverse byte order or array of values.
 *\param data        Pointer to beginning of memory block to modify
 *\param values_size Size of one value
 *\param num_elem    Number of values of size 'value_size' in 'data'
 */
void byteswap( void* data, unsigned value_size, size_t num_elem );

/**\brief Alternate byteswap optimized for 2-byte values */
void byteswap2( void* data, size_t num_elem );
/**\brief Alternate byteswap optimized for 4-byte values */
void byteswap4( void* data, size_t num_elem );
/**\brief Alternate byteswap optimized for 8-byte values */
void byteswap8( void* data, size_t num_elem );

/**\brief Type-specific byte swap */
template <typename T> 
inline void byteswap( T* data, size_t num_elem )
{
  switch (sizeof(T)) {
    case 1:
      break;
    case 2:  
      byteswap2( data, num_elem ); 
      break;
    case 4:  
      byteswap4( data, num_elem ); 
      break;
    case 8: 
      byteswap8( data, num_elem ); 
      break;
    default: 
      byteswap( data, sizeof(T), num_elem ); 
      break;
  }
}


} // namespace SysUtil

} // namespace moab


#endif
