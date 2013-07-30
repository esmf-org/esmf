#ifndef BIT_PAGE_HPP
#define BIT_PAGE_HPP

#include <assert.h>
#include "BitTag.hpp"

namespace moab {

class Range;

/**\brief bit tag data
 *
 * This class represents a fixed-size block of memory in which bit tag
 * values are stored.  
 */
class BitPage
{
public:
  /**\brief Initialize memory
   *
   *\param bits_per_ent  Number of bits in each tag value.
   *                     MUST BE A POWER OF TWO.
   *\param init_val      The lower bits_per_ent bits of this byte are
   *                     used to initialize each tag value.
   */
  BitPage( int bits_per_ent, unsigned char init_val );
  
  /**\brief Get tag values
   *
   * Get 'count' tag values, beginning with the one at 'offset'.
   *\param offset Offset into list of values, where a value of zero indicates
   *              the first tag value, a value of one indicates the second
   *              tag value, etc.  NOTE:  This is the value offset, not the
   *              bit offset.
   *\param count  Number of consecutive tag values to get.
   *\param bits_per_ent  Number of bits composing each tag value.  
   *                     NOTE: Must be a power of two.
   *\param data   Memory into which to copy tag values.  Each value is copied
   *              into a separate byte, such that the lower bits of the bit
   *              contain the tag value and any unused higher bits are zero.
   */
  void get_bits( int offset, int count, int bits_per_ent, unsigned char* data ) const;
  
  /**\brief Set tag values
   *
   * Set 'count' tag values, beginning with the one at 'offset'.
   *\param offset Offset into list of values, where a value of zero indicates
   *              the first tag value, a value of one indicates the second
   *              tag value, etc.  NOTE:  This is the value offset, not the
   *              bit offset.
   *\param count  Number of consecutive tag values to set.
   *\param bits_per_ent  Number of bits composing each tag value.  
   *                     NOTE: Must be a power of two.
   *\param data   Memory from which to copy tag values.  Each value is copied
   *              from a separate byte.  The lower 'bits_per_ent' of each
   *              byte are used as the tag value.  Any additional higher bits
   *              are ignored.
   */
  void set_bits( int offset, int count, int bits_per_ent, const unsigned char* data );
  
  /**\brief Set several tag values to the same value.
   *
   * Set 'count' tag values to specified value.
   *\param offset Offset into list of values, where a value of zero indicates
   *              the first tag value, a value of one indicates the second
   *              tag value, etc.  NOTE:  This is the value offset, not the
   *              bit offset.
   *\param count  Number of consecutive tag values to set.
   *\param bits_per_ent  Number of bits composing each tag value.  
   *                     NOTE: Must be a power of two.
   *\param value  The lower 'bits_per_ent' of this
   *              byte are used as the tag value.  Any additional higher bits
   *              are ignored.
   */
  void set_bits( int offset, int count, int bits_per_ent, unsigned char value );

  /**\brief Get tag value
   *
   * Get one tag value.
   *\param offset Offset into list of values, where a value of zero indicates
   *              the first tag value, a value of one indicates the second
   *              tag value, etc.  NOTE:  This is the value offset, not the
   *              bit offset.
   *\param bits_per_ent  Number of bits composing each tag value.  
   *                     NOTE: Must be a power of two.
   *\return       A byte containing the tag value in the lower bits with
   *              any unused higher bits zeroed.
   */
  unsigned char get_bits( int offset, int bits_per_ent ) const;
  
  /**\brief Set tag value
   *
   * Set tag value.
   *\param offset Offset into list of values, where a value of zero indicates
   *              the first tag value, a value of one indicates the second
   *              tag value, etc.  NOTE:  This is the value offset, not the
   *              bit offset.
   *\param bits_per_ent  Number of bits composing each tag value.  
   *                     NOTE: Must be a power of two.
   *\param value  The lower 'bits_per_ent' of this
   *              byte are used as the tag value.  Any additional higher bits
   *              are ignored.
   */
  void set_bits( int offset, int bits_per_ent, unsigned char data );
  
  /**\brief Search stored values for specified value.
   *
   * Find the offsets n in the data at which the specified value occurs,
   * and for each one insert 'start + n' into the passed Range.
   *\param value   The value to look for
   *\param offset  The offset at which to begin searching
   *\param count   The number of values to search
   *\param bits_per_ent Number of bits composing each tag value.
   *\param results Result list.
   *\param start   The handle of the entity corresponding to the 
   *               tag value stored at 'offset'
   */
  void search( unsigned char value, int offset, int count, 
               int bits_per_ent, Range& results, EntityHandle start ) const;

private:

  /**\brief The actual array of bytes */
  char byteArray[BitTag::PageSize];
};

inline unsigned char BitPage::get_bits( int offset, int per_ent ) const
{
    // Assume per_ent is a power of two, which should be guaranteed
    // by higher-level code.
  unsigned char mask = (unsigned char)(1<<per_ent)-1; // 2^per_ent - 1
  int byte = (offset * per_ent) >> 3; // shifting 3 is dividing by eight
  int bit =  (offset * per_ent) & 7;  // masking with 7 is modulo eight
  assert(byte < BitTag::PageSize);
  return (unsigned char)(byteArray[byte] >> bit) & mask;
} 

inline void BitPage::set_bits( int offset, int per_ent, unsigned char bits )
{
  int byte = (offset * per_ent) >> 3; // shifting 3 is dividing by eight
  int bit =  (offset * per_ent) & 7;  // masking with 7 is modulo eight
  assert(byte < BitTag::PageSize);
    // Assume per_ent is a power of two, which should be guaranteed
    // by higher-level code.
  unsigned char mask = (unsigned char)((1<<per_ent)-1) << bit;
  byteArray[byte] = (char)((byteArray[byte] & ~mask) | ((bits << bit) & mask));
} 

inline void BitPage::get_bits( int offset, int count, int per_ent, unsigned char* data ) const
{
  unsigned char* end = data+count;
  while (data != end)
    *(data++) = get_bits( offset++, per_ent );
}

inline void BitPage::set_bits( int offset, int count, int per_ent, const unsigned char* data )
{
  const unsigned char* end = data+count;
  while (data != end)
    set_bits( offset++, per_ent, *(data++) );
}

inline void BitPage::set_bits( int offset, int count, int per_ent, unsigned char value )
{
  int end = offset + count;
  while (offset < end)
    set_bits( offset++, per_ent, value );
}
  
} // namespace moab

#endif
