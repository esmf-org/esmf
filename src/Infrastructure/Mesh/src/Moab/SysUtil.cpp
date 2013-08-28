#include "SysUtil.hpp"

#include <fstream>
#include <algorithm>
#include <assert.h>

#include "moab/EntityHandle.hpp"

namespace moab {

namespace SysUtil
{


void setmem( void* mem, const void* value, unsigned value_size, size_t num_elem )
{
  if (!num_elem)
    return;
  
  char* array = reinterpret_cast<char*>(mem);
  memcpy( array, value, value_size );
  size_t count;
  for (count = 1; count*2 < num_elem; count *= 2)
    memcpy( array + count * value_size, array, count * value_size );
  memcpy( array + count * value_size, array, (num_elem - count) * value_size );
}
    

long filesize( FILE* filp )
{
  long curr_pos = ftell( filp );
  if (fseek( filp, 0, SEEK_END ))
    return -1;
  
  long length = ftell( filp );
  if (fseek( filp, curr_pos, SEEK_SET))
  {
    assert(0); 
    return -2;
  }
  
  return length;
}


long filesize( std::ifstream& str )
{
  std::istream::pos_type curr_pos = str.tellg();
  if (!str.seekg( 0, std::ios_base::end ))
    return -1;
  
  long length = static_cast<long>(str.tellg());
  if (!str.seekg( curr_pos, std::ios_base::beg ))
  {
    assert(0);
    return -2;
  }
  
  return length;
}


void byteswap( void* data, unsigned value_size, size_t num_elem )
{
  char* mem = reinterpret_cast<char*>(data);
  char* const end = mem + value_size * num_elem;
  for ( ; mem < end; mem += value_size) {
    unsigned i = 0, j = value_size - 1;
    while (i < j)
      std::swap( mem[i++], mem[j--] );
  }
}

inline static uint16_t swap_bytes( uint16_t value )
{
  return (value >> 8) | (value << 8);
}

inline static uint32_t swap_bytes( uint32_t value )
{
  return ((value /*& (uint32_t)0xFF000000*/) >> 24) |
         ((value   & (uint32_t)0x00FF0000  ) >>  8) |
         ((value   & (uint32_t)0x0000FF00  ) <<  8) |
         ((value /*& (uint32_t)0X000000FF*/) << 24);
}

const uint64_t m64b1 = 0xFF;
const uint64_t m64b2 = m64b1 <<  8;
const uint64_t m64b3 = m64b1 << 16;
const uint64_t m64b4 = m64b1 << 24;
const uint64_t m64b5 = m64b1 << 32;
const uint64_t m64b6 = m64b1 << 40;
const uint64_t m64b7 = m64b1 << 48;
//const uint64_t m64b8 = m64b1 << 56;

inline static uint64_t swap_bytes( uint64_t value )
{
  return ((value /*& m64b8*/) >> 56) |
         ((value   & m64b7  ) >> 40) |
         ((value   & m64b6  ) >> 24) |
         ((value   & m64b5  ) >>  8) |
         ((value   & m64b4  ) <<  8) |
         ((value   & m64b3  ) << 24) |
         ((value   & m64b2  ) << 40) |
         ((value /*& m64b1*/) << 56);
}
/*
inline static uint32_t swap_byte_pairs( uint32_t value )
{
  return ((value & (uint32_t)0xFF000000) >> 8) |
         ((value & (uint32_t)0x00FF0000) << 8) |
         ((value & (uint32_t)0x0000FF00) >> 8) |
         ((value & (uint32_t)0X000000FF) << 8);
}

inline static uint64_t swap_byte_quads( uint64_t value )
{
  return ((value & m64b8) >> 24) |
         ((value & m64b7) >>  8) |
         ((value & m64b6) <<  8) |
         ((value & m64b5) << 24) |
         ((value & m64b4) >> 24) |
         ((value & m64b3) >>  8) |
         ((value & m64b2) <<  8) |
         ((value & m64b1) << 24);
}

inline static uint64_t swap_byte_pairs( uint64_t value )
{
  return ((value & m64b8) >> 8) |
         ((value & m64b7) << 8) |
         ((value & m64b6) >> 8) |
         ((value & m64b5) << 8) |
         ((value & m64b4) >> 8) |
         ((value & m64b3) << 8) |
         ((value & m64b2) >> 8) |
         ((value & m64b1) << 8);
}
*/
void byteswap2( void* data, size_t num_elem )
{
  uint16_t* mem = reinterpret_cast<uint16_t*>(data);
  uint16_t* end = mem + num_elem;
  for (; mem < end; ++mem)
    *mem = swap_bytes( *mem );
}

void byteswap4( void* data, size_t num_elem )
{
  uint32_t* mem = reinterpret_cast<uint32_t*>(data);
  uint32_t* end = mem + num_elem;
  for (; mem < end; ++mem)
    *mem = swap_bytes( *mem );
}

void byteswap8( void* data, size_t num_elem )
{
  if (sizeof(void*) >= 8) {
    uint64_t* mem = reinterpret_cast<uint64_t*>(data);
    uint64_t* end = mem + num_elem;
    for (; mem < end; ++mem)
      *mem = swap_bytes( *mem );
  }
  else {
    uint32_t* mem = reinterpret_cast<uint32_t*>(data);
    uint32_t* end = mem + 2*num_elem;
    for (; mem < end; mem += 2) {
      uint32_t tmp = swap_bytes( mem[0] );
      mem[0] = swap_bytes( mem[1] );
      mem[1] = tmp;
    }
  }
}

} // namespace SysUtil


} // namespace moab
