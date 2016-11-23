/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**
 * \class WriteSTL
 * \brief ASCII and Binary Stereo Lithography File writers.
 * \author Jason Kraftcheck
 */


#include "WriteSTL.hpp"
#include "moab/CN.hpp"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "FileOptions.hpp"
#include "SysUtil.hpp"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include <limits.h>

namespace moab {

#if defined(_MSC_VER) || defined(__MINGW32__) /* windows */
#  include <io.h>
#  ifndef __MINGW32__
typedef unsigned __int32 uint32_t;
#  endif
#else  /* posix */
#  include <unistd.h>
#  define _S_IREAD  (S_IRUSR|S_IRGRP|S_IROTH)
#  define _S_IWRITE (S_IWUSR|S_IWGRP|S_IWOTH)
#endif

const int DEFAULT_PRECISION = 6;

WriterIface *WriteSTL::factory( Interface* iface )
  { return new WriteSTL( iface ); }

WriteSTL::WriteSTL(Interface *impl) 
    : mbImpl(impl)
{
  impl->query_interface( mWriteIface );
}

WriteSTL::~WriteSTL() 
{
  mbImpl->release_interface(mWriteIface);
}


ErrorCode WriteSTL::write_file(const char *file_name, 
                                 const bool overwrite,
                                 const FileOptions& opts,
                                 const EntityHandle *ent_handles,
                                 const int num_sets,
                                 const std::vector<std::string>& qa_list, 
                                 const Tag* tag_list,
                                 int num_tags,
                                 int  )
{
  char header[81];
  Range triangles;
  ErrorCode rval;
  
  if (tag_list && num_tags) {
    mWriteIface->report_error( "STL file does not support tag data\n" );
    return MB_TYPE_OUT_OF_RANGE;
  }
  
  rval = make_header( header, qa_list );
  if (MB_SUCCESS != rval)
    return rval;
  
  rval = get_triangles( ent_handles, num_sets, triangles );
  if (MB_SUCCESS != rval)
    return rval;

  if (triangles.empty()) {
    mWriteIface->report_error( "No triangles to write." );
    return MB_ENTITY_NOT_FOUND;
  }
 
  bool is_ascii = false, is_binary = false;
  if (MB_SUCCESS == opts.get_null_option( "ASCII" ))
    is_ascii = true;
  if (MB_SUCCESS == opts.get_null_option( "BINARY" ))
    is_binary = true;
  if (is_ascii && is_binary) {
    mWriteIface->report_error( "Conflicting options: BINARY ASCII\n" );
    return MB_FAILURE;
  }
  
  bool big_endian = false, little_endian = false;
  if (MB_SUCCESS == opts.get_null_option( "BIG_ENDIAN" ))
    big_endian = true;
  if (MB_SUCCESS == opts.get_null_option( "LITTLE_ENDIAN" ))
    little_endian = true;
  if (big_endian && little_endian) {
    mWriteIface->report_error( "Conflicting options: BIG_ENDIAN LITTLE_ENDIAN\n" );
    return MB_FAILURE;
  }
  ByteOrder byte_order = big_endian ? STL_BIG_ENDIAN : little_endian ? STL_LITTLE_ENDIAN : STL_UNKNOWN_BYTE_ORDER;
    
  FILE* file = open_file( file_name, overwrite, is_binary );
  if (!file)
    return MB_FILE_DOES_NOT_EXIST; 
  
  if (is_binary)
    rval = binary_write_triangles( file, header, byte_order, triangles );
  else {
      // Get precision for node coordinates
    int precision;
    if (MB_SUCCESS != opts.get_int_option( "PRECISION", precision ))
      precision = DEFAULT_PRECISION;

    rval = ascii_write_triangles( file, header, triangles, precision );
  }
  fclose( file );
  return rval;
}


FILE* WriteSTL::open_file( const char* name, bool overwrite, bool binary )
{
    // Open file with write access, and create it if it doesn't exist.
  int flags = O_WRONLY|O_CREAT;
    // Select behavior if the named file already exists.  If 
    // overwrite is true, truncate the file.  If it is false,
    // make the call to open() fail.
  if (overwrite)
    flags |= O_TRUNC;
  else
    flags |= O_EXCL;
    // If platform defines a "binary" bit in the file access
    // flags (i.e. we're building on windows), then set it
    // if we're writing a binary file.
#ifdef O_BINARY
  if (binary)
    flags |= O_BINARY;
#endif

    // Give everyone read and write, but not execute, permision.
    // These are not the final permisions for the file.  Permissions
    // are removed according to the user's umask.  All we want to
    // say here is that the executable bits should not be set because
    // this isn't an executable file.  Everything else is a user
    // preference and should be left up to the umask.
  int creat_mode = _S_IREAD|_S_IWRITE;

    // Open the file.
  int fd = open( name, flags, creat_mode );
  if (fd < 0)
  {
    mWriteIface->report_error( "%s: %s\n", name, strerror(errno) );
    return 0;
  }
  FILE* result = fdopen( fd, binary ? "wb": "w" );
  if (!result)
    close( fd );
  
  return result;
}

ErrorCode WriteSTL::make_header( char header[81], 
                                   const std::vector<std::string>& qa_list )
{
  memset( header, 0, 81 );
  
  std::string result;
  for (std::vector<std::string>::const_iterator i = qa_list.begin(); i != qa_list.end(); ++i)
  {
    result += " ";
    result += *i;
  }
  
  size_t len = result.size();
  if (len > 80)
    len = 80;
  memcpy( header, result.c_str(), len );
  
  return MB_SUCCESS;
}

ErrorCode WriteSTL::get_triangles( const EntityHandle* set_array,
                                     int set_array_length,
                                     Range& triangles )
{
  if (!set_array || set_array_length == 0)
  {
    return mbImpl->get_entities_by_type( 0, MBTRI, triangles );
  }
  
  const EntityHandle* iter = set_array;
  const EntityHandle* end = iter + set_array_length;
  for (; iter != end; ++iter)
  {
    Range r;
    ErrorCode rval = mbImpl->get_entities_by_type( *iter, MBTRI, r, true );
    if (MB_SUCCESS != rval)
      return rval;
    triangles.merge( r );
  }
  
  return MB_SUCCESS;
}

ErrorCode WriteSTL::get_triangle_data( const double coords[9],
                                         float v1[3],
                                         float v2[3],
                                         float v3[3],
                                         float n[3] )
{
  float e1[3], e2[3];
  v1[0] = (float)coords[0];
  v1[1] = (float)coords[1];
  v1[2] = (float)coords[2];
  v2[0] = (float)coords[3];
  v2[1] = (float)coords[4];
  v2[2] = (float)coords[5];
  v3[0] = (float)coords[6];
  v3[1] = (float)coords[7];
  v3[2] = (float)coords[8];
  e1[0] = v2[0] - v1[0];
  e1[1] = v2[1] - v1[1];
  e1[2] = v2[2] - v1[2];
  e2[0] = v3[0] - v1[0];
  e2[1] = v3[1] - v1[1];
  e2[2] = v3[2] - v1[2];
  n[0] = e1[1]*e2[2] - e1[2]*e2[1];
  n[1] = e1[2]*e2[0] - e1[0]*e2[2];
  n[2] = e1[0]*e2[1] - e1[1]*e2[0];
  float inv_len = 1.0f / (float)sqrt( n[0]*n[0] + n[1]*n[1] + n[2]*n[2] );
  n[0] *= inv_len;
  n[1] *= inv_len;
  n[2] *= inv_len;
  return MB_SUCCESS;
}


ErrorCode WriteSTL::ascii_write_triangles( FILE* file,
                                             const char header[81],
                                             const Range& triangles,
                                             int prec )
{
  const char solid_name[] = "MOAB";
  
  char myheader[81] = "solid ";
  strcat( myheader, solid_name );
  strncat( myheader, header, 80 );
  
  if (EOF == fputs( myheader, file ) || EOF == fputs( "\n", file ))
    return MB_FILE_WRITE_ERROR;
  
  ErrorCode rval;
  double coords[9];
  float v1[3], v2[3], v3[3];
  float n[3];
  for (Range::const_iterator iter = triangles.begin();
       iter != triangles.end(); ++iter)
  {
    const EntityHandle* conn;
    int num_vtx;
    
    rval = mbImpl->get_connectivity( *iter, conn, num_vtx );
    if (MB_SUCCESS != rval)
      return rval;
    if (num_vtx != 3)
      return MB_FAILURE;
    
    rval = mbImpl->get_coords( conn, 3, coords );
    if (MB_SUCCESS != rval)
      return rval;
    
    rval = get_triangle_data( coords, v1, v2, v3, n );
    if (MB_SUCCESS != rval)
      return rval;
   
    fprintf( file,"facet normal %e %e %e\n", n[0], n[1], n[2] );
    fprintf( file,"outer loop\n" );
    fprintf( file,"vertex %.*e %.*e %.*e\n", prec, v1[0], prec, v1[1], prec, v1[2] );
    fprintf( file,"vertex %.*e %.*e %.*e\n", prec, v2[0], prec, v2[1], prec, v2[2] );
    fprintf( file,"vertex %.*e %.*e %.*e\n", prec, v3[0], prec, v3[1], prec, v3[2] );
    fprintf( file,"endloop\n" );
    fprintf( file,"endfacet\n" );
  }
  
  fprintf( file,"endsolid %s\n", solid_name );
  return MB_SUCCESS;
}

struct BinTri
{
  float normal[3];
  float vertex1[3];
  float vertex2[3];
  float vertex3[3];
  char pad[2];
};

ErrorCode WriteSTL::binary_write_triangles( FILE* file,
                                             const char header[81],
                                             ByteOrder byte_order,
                                             const Range& triangles )
{
  ErrorCode rval;
  if (fwrite( header, 80, 1, file ) != 1)
    return MB_FILE_WRITE_ERROR;
  
    // default to little endian if byte_order == UNKNOWN_BYTE_ORDER
  const bool want_big_endian = (byte_order == STL_BIG_ENDIAN);
  const bool am_big_endian = !SysUtil::little_endian();
  const bool swap_bytes = (want_big_endian == am_big_endian);
    
  if (triangles.size() > INT_MAX) // can't write that many triangles
    return MB_FAILURE;  
  
  uint32_t count = (uint32_t)triangles.size();
  if (swap_bytes)
    SysUtil::byteswap(&count, 1);
  if (fwrite( &count, 4, 1, file ) != 1)
    return MB_FILE_WRITE_ERROR;

  double coords[9];
  BinTri tri;
  tri.pad[0] = tri.pad[1] = '\0';
  for (Range::const_iterator iter = triangles.begin();
       iter != triangles.end(); ++iter)
  {
    const EntityHandle* conn;
    int num_vtx;
    
    rval = mbImpl->get_connectivity( *iter, conn, num_vtx );
    if (MB_SUCCESS != rval)
      return rval;
    if (num_vtx != 3)
      return MB_FAILURE;
    
    rval = mbImpl->get_coords( conn, 3, coords );
    if (MB_SUCCESS != rval)
      return rval;
    
    rval = get_triangle_data( coords, tri.vertex1, tri.vertex2, tri.vertex3, tri.normal );
    if (MB_SUCCESS != rval)
      return rval;
    
    if (swap_bytes)
    {
      SysUtil::byteswap( tri.normal, 3 );
      SysUtil::byteswap( tri.vertex1, 3 );
      SysUtil::byteswap( tri.vertex2, 3 );
      SysUtil::byteswap( tri.vertex3, 3 );
    }
   
    if (1 != fwrite( &tri, 50, 1, file ))
      return MB_FILE_WRITE_ERROR;
  }
  
  return MB_SUCCESS;
}

} // namespace moab
