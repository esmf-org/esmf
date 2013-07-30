#include "BitPage.hpp"
#include "moab/Range.hpp"
#include <stdlib.h>
#include <string.h>

namespace moab {

void BitPage::search( unsigned char value, int offset, int count, 
                      int per_ent, Range& results, EntityHandle start ) const
{
  const int end = offset + count;
  Range::iterator hint = results.begin();
  while (offset != end) {
    if (get_bits( offset, per_ent ) == value)
      hint = results.insert( hint, start );
    ++offset;
    ++start;
  }
}

BitPage::BitPage( int per_ent, unsigned char init_val )
{
  unsigned char mask = (unsigned char)(1<<per_ent)-1; // 2^per_ent - 1
  init_val &= (unsigned char)mask;
  switch (per_ent) {
    default: assert(false); abort(); break; // must be power of two
      // Note: no breaks. fall through such that all bits in init_val are set
    case 1: init_val |= (unsigned char)(init_val << 1);
    case 2: init_val |= (unsigned char)(init_val << 2);
    case 4: init_val |= (unsigned char)(init_val << 4);
    case 8: ;
  }
  memset( byteArray, init_val, BitTag::PageSize );
}

  
} // namespace moab
