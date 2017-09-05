/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <hdf5.h>
#include "util.h"
#include "status.h"
#include "names-and-paths.h"

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#else
#  define VALGRIND_CHECK_MEM_IS_DEFINED(A,B)
#  define VALGRIND_MAKE_MEM_UNDEFINED(A,B)
#endif

void* mhdf_malloc( size_t size, mhdf_Status* status )
{
  void* result;
  result = malloc(size);
  if (!result)
    mhdf_setFail( status, "Allocation of %d bytes failed.\n", (int)size );
  return result;
}

void* mhdf_realloc( void* ptr, size_t size, mhdf_Status* status )
{
  void* result;
  result = realloc(ptr, size);
  if (!result)
    mhdf_setFail( status, "Allocation of %d bytes failed.\n", (int)size );
  return result;
}

size_t mhdf_name_to_path( const char* name, char* path, size_t path_len )
{
  size_t length = 1;
  unsigned char *iter;
  
  if (0==strcmp(name, "."))
  {
    if (path_len >= 4)
      sprintf(path, "\\%02X", (int)*name);
    return 4;
  }
  
  for (iter = (unsigned char*)name; *iter; ++iter)
  {
    if ( iscntrl(*iter) || *iter == '/' || *iter == '\\' || *iter > 127 )
      length += 3;
    else
      length += 1;
  }
  if (path_len < length)
    return length;
  
  
  for (iter = (unsigned char*)name; *iter; ++iter)
  {
    if (iscntrl(*iter) || *iter == '/' || *iter == '\\' || *iter > 127)
    {
      sprintf(path, "\\%02X", (int)(*iter));
      path += 3;
    }
    else
    {
      *(path++) = *iter;
    }
  }
  
  *path = '\0';
  return length;
}

static int mhdf_hex_char( int c )
{
  if (isdigit(c))
    return c - '0';
  else
    return toupper(c) - ('A' - 10);
}

int mhdf_path_to_name( const char* path, char* name )
{
  const char* iter;
  char c1, c2;
  
  for (iter = path; *iter; ++iter, ++name)
  {
    if (*iter == '\\')
    {
      c1 = *++iter;
      c2 = *++iter;
      if (!isxdigit(c1) || !isxdigit(c2))
        return 0;
      
      *name = (char)(16 * mhdf_hex_char(c1) + mhdf_hex_char(c2));
    }
    else
    {
      *name = *iter;
    }
  }
  
  *name = '\0';
  return 1;
}

char* mhdf_name_to_path_copy( const char* name, mhdf_Status* status )
{
  size_t size;
  char* buffer;
  
  size = mhdf_name_to_path( name, NULL, 0 );
  buffer = (char*)mhdf_malloc( size, status );
  if (!buffer) return NULL;
  
  mhdf_name_to_path( name, buffer, size );
  return buffer;
}


char* mhdf_name_to_path_cat( const char* prefix, const char* name, mhdf_Status* status )
{
  size_t size, plen;
  char* buffer;
  
  plen = strlen( prefix );
  size = mhdf_name_to_path( name, NULL, 0 ) + 1;
  buffer = (char*)mhdf_malloc( size + plen, status );
  if (!buffer) return NULL;
  
  memcpy( buffer, prefix, plen );
  mhdf_name_to_path( name, buffer + plen, size );
  return buffer;
}


hid_t mhdf_elem_group_from_handle( FileHandle* file_ptr,
                                   const char* elem_handle,
                                   mhdf_Status* status )
{
  char* path;
  hid_t result;
  
  path = mhdf_name_to_path_cat( ELEMENT_GROUP, elem_handle, status );
  if (NULL == path)
    return -1;
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  result = H5Gopen2( file_ptr->hdf_handle, path, H5P_DEFAULT );
#else
  result = H5Gopen( file_ptr->hdf_handle, path );
#endif
  free( path );
  if (result < 0)
    mhdf_setFail( status, "Failed to open element group: \"%s\"", elem_handle );
  return result;
}    

int mhdf_create_scalar_attrib( hid_t object,
                              const char* name,
                              hid_t type,
                              const void* value,
                              mhdf_Status* status )
{
  hid_t dspace_id, attr_id;
  herr_t rval;
  
  dspace_id = H5Screate( H5S_SCALAR );
  if (dspace_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return 0;
  }
 
#if defined(H5Acreate_vers) && H5Acreate_vers > 1
  attr_id = H5Acreate2( object, name, type, dspace_id, H5P_DEFAULT, H5P_DEFAULT );
#else
  attr_id = H5Acreate( object, name, type, dspace_id, H5P_DEFAULT );
#endif
  H5Sclose( dspace_id );
  if (attr_id < 0)
  {
    mhdf_setFail( status, "Failed to create \"%s\" attrib.", name );
    return 0;
  }

  VALGRIND_CHECK_MEM_IS_DEFINED( value, H5Tget_size(type) );
  rval = H5Awrite( attr_id, type, value );
  H5Aclose( attr_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Failed to write \"%s\" attrib.", name );
    return 0;
  }
  
  return 1;
}


int mhdf_read_scalar_attrib( hid_t object,
                             const char* name,
                             hid_t type,
                             void* value,
                             mhdf_Status* status )
{
  hid_t attr_id, type_id;
  herr_t rval;
  
  attr_id = H5Aopen_name( object, name );
  if (attr_id < 0)
  {
    mhdf_setFail( status, "Failed to create \"%s\" attrib.", name );
    return 0;
  }
  
  if (type > 0)
  {
    type_id = type;
  }
  else
  {
    type_id = H5Aget_type( attr_id );
    if (type_id < 0)
    {
      H5Aclose( attr_id );
      return 0;
    }
  }
  
  rval = H5Aread( attr_id, type_id, value );
  H5Aclose( attr_id );
  if (type < 1)
    H5Tclose( type_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Failed to read \"%s\" attrib.", name );
    return 0;
  }
  
  return 1;
}

#if defined(H5Aiterate_vers) && H5Aiterate_vers > 1
static herr_t find_attr_by_name( hid_t handle, const char* name, const H5A_info_t* info, void* mydata ) {
    /* empty statement to remove compiler warning */
  if (info) {}
#else
static herr_t find_attr_by_name( hid_t handle, const char* name, void* mydata ) 
{
#endif
    /* empty statement to remove compiler warning */
  if (handle) {}
  return !strcmp( name, (const char*)mydata ); } 

int mhdf_find_attribute ( hid_t object, 
                          const char* attrib_name,
                          unsigned int* index_out,
                          mhdf_Status* status )
{
  herr_t rval;
#if defined(H5Aiterate_vers) && H5Aiterate_vers > 1
  hsize_t idx = 0;
  rval = H5Aiterate2( object, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, &idx, &find_attr_by_name, (void*)attrib_name );
  *index_out = (unsigned int)idx;
#else
  *index_out = 0;
  rval = H5Aiterate( object, index_out, &find_attr_by_name, (void*)attrib_name );
#endif
  if (rval < 0)
    mhdf_setFail( status, "Internal error calling H5Aiterate." );
  return (int)rval;
}
  

static herr_t find_link_by_name( hid_t handle, const char* name, void* mydata )
{ 
    /* empty statement to remove compiler warning */
  if (handle) {}
  return !strcmp( name, (const char*)mydata ); } 

int mhdf_is_in_group( hid_t group, const char* name, mhdf_Status* status )
{
  int rval;
  rval = H5Giterate( group, ".", NULL, &find_link_by_name, (void*)name );
  if (rval < 0)
    mhdf_setFail( status, "Internal error in H5Giterate." );
  return rval;
}

static int
mhdf_readwrite( hid_t data_id, int read,
                long offset, long count,
                hid_t type, void* array,
                hid_t io_prop,
                mhdf_Status* status )
{
  hid_t slab_id, mem_id;
  hsize_t offsets[2], counts[2] = {1,1};
  herr_t rval;
  int dims;
/*#if (1000 * H5_VERS_MAJOR + H5_VERS_MINOR) < 1008*/
  const hsize_t one = 1;
/*#endif*/
  
  if (offset < 0 || count < 0)
  {
    mhdf_setFail( status, "Invalid input for %s: "
                          "offset = %ld, count = %ld\n",
                          read ? "read" : "write",
                          offset, count );
    return 0;
  }
  
  slab_id = H5Dget_space( data_id );
  if (slab_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Dget_space.");
    return 0;
  }
  
  dims = H5Sget_simple_extent_ndims( slab_id );
  if (dims < 1 || dims > 2)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error: unexpected dataset rank: %d.", dims);
    return 0;
  }
  
  dims = H5Sget_simple_extent_dims( slab_id, counts, NULL );
  if (dims < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sget_simple_extend_dims.");
    return 0;
  }
 
  if ((unsigned long)(offset + count) > counts[0])
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, 
      "Requested %s of rows %ld to %ld of a %ld row table.\n",
      read ? "read" : "write", offset, offset+count-1, (long)counts[dims-1]);
    return 0;
  }
 
  
  counts[0] = (hsize_t)count;
  offsets[0] = (hsize_t)offset;
  offsets[1] = 0;
  if (count) 
    rval = H5Sselect_hyperslab( slab_id, H5S_SELECT_SET, offsets, NULL, counts, NULL );
  else 
    rval = H5Sselect_none( slab_id );
  if (rval < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
    return 0;
  }
  
  if (count)
    mem_id = H5Screate_simple( dims, counts, NULL );
  else {
/*#if H5_VERS_MAJOR > 1 || H5_VERS_MINOR >= 8
    mem_id = H5Screate(H5S_NULL); 
#else*/
    mem_id = H5Screate_simple( 1, &one, NULL );
    if (mem_id && 0 > H5Sselect_none( mem_id )) {
      H5Sclose( mem_id );
      mem_id = -1;
    }
/*#endif*/
  }
  
  if (mem_id < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return 0;
  }
  
  
  if (read)
    rval = H5Dread( data_id, type, mem_id, slab_id, io_prop, array );
  else {
    VALGRIND_CHECK_MEM_IS_DEFINED( array, counts[0]*counts[1]*H5Tget_size(type) );
    rval = H5Dwrite( data_id, type, mem_id, slab_id, io_prop, array );
  }
  H5Sclose( slab_id );
  H5Sclose( mem_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Internal error calling H5D%s.", read ? "read" : "write" );
    return 0;
  }
  
  mhdf_setOkay( status );
  return 1;
}


static int
mhdf_readwrite_column( hid_t data_id, int read, 
                       int column,
                       long offset, long count,
                       hid_t type, void* array,
                       hid_t io_prop,
                       mhdf_Status* status )
{
  hid_t slab_id, mem_id;
  hsize_t offsets[2], counts[2];
  herr_t rval;
  int dims;
/*#if (1000 * H5_VERS_MAJOR + H5_VERS_MINOR) < 1008*/
  const hsize_t one = 1;
/*#endif*/
  
  if (column < 0 || offset < 0 || count < 0)
  {
    mhdf_setFail( status, "Invalid input for %s: "
                          "column = %d, offset = %ld, count = %ld\n",
                          read ? "read" : "write",
                          column, offset, count );
    return 0;
  }
  
  slab_id = H5Dget_space( data_id );
  if (slab_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Dget_space.");
    return 0;
  }
  
  dims = H5Sget_simple_extent_ndims( slab_id );
  if (dims < 1 || dims > 2)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error: unexpected dataset rank: %d.", dims);
    return 0;
  }
  
  dims = H5Sget_simple_extent_dims( slab_id, counts, NULL );
  if (dims < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sget_simple_extend_dims.");
    return 0;
  }
 
  if ((unsigned long)(offset + count) > counts[0] || 
      (unsigned long)column > counts[1])
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, 
      "Requested %s of (%ld,%d)->(%ld,%ld) of (%ld, %ld) table.\n",
      read ? "read" : "write", offset, column, offset+count-1, 
      column, (long)counts[0], (long)counts[1]);
    return 0;
  }
  
  counts[0] = (hsize_t)count;
  offsets[0] = (hsize_t)offset;
  counts[1] = 1;
  offsets[1] = column;
  if (count)
    rval = H5Sselect_hyperslab( slab_id, H5S_SELECT_SET, offsets, NULL, counts, NULL );
  else 
    rval = H5Sselect_none( slab_id );
  if (rval < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
    return 0;
  }
  
  
  if (count)
    mem_id = H5Screate_simple( dims, counts, NULL );
  else {
/*#if H5_VERS_MAJOR > 1 || H5_VERS_MINOR >= 8
    mem_id = H5Screate(H5S_NULL); 
#else*/
    mem_id = H5Screate_simple( 1, &one, NULL );
    if (mem_id && 0 > H5Sselect_none( mem_id )) {
      H5Sclose( mem_id );
      mem_id = -1;
    }
/*#endif*/
  }

  if (mem_id < 0)
  {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return 0;
  }

  
  if (read)
    rval = H5Dread( data_id, type, mem_id, slab_id, io_prop, array );
  else {
    VALGRIND_CHECK_MEM_IS_DEFINED( array, count*H5Tget_size(type) );
    rval = H5Dwrite( data_id, type, mem_id, slab_id, io_prop, array );
    VALGRIND_MAKE_MEM_UNDEFINED( array, count*H5Tget_size(type) );
  }
  H5Sclose( slab_id );
  H5Sclose( mem_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Internal error calling H5D%s.", read ? "read" : "write" );
    return 0;
  }
  
  mhdf_setOkay( status );
  return 1;
}


int
mhdf_write_data( hid_t data_id,
                 long offset,
                 long count,
                 hid_t type_id,
                 const void* array,
                 hid_t prop,
                 mhdf_Status* status )
{
  return mhdf_readwrite( data_id, 0, offset, count, type_id, (void*)array, prop, status );
}

int
mhdf_read_data( hid_t data_id,
                long offset,
                long count,
                hid_t type_id,
                void* array,
                hid_t prop,
                mhdf_Status* status )
{
  return mhdf_readwrite( data_id, 1, offset, count, type_id, array, prop, status );
}

int
mhdf_read_column( hid_t data_id, 
                  int column,
                  long offset, 
                  long count,
                  hid_t type, 
                  void* array,
                  hid_t prop,
                  mhdf_Status* status )
{
  return mhdf_readwrite_column( data_id, 1, column, offset, count, 
                                type, array, prop, status );
}

int
mhdf_write_column( hid_t data_id, 
                   int column,
                   long offset, 
                   long count,
                   hid_t type, 
                   const void* array,
                   hid_t prop,
                   mhdf_Status* status )
{
  return mhdf_readwrite_column( data_id, 0, column, offset, count, 
                                type, (void*)array, prop, status );
}


hid_t
mhdf_create_table( hid_t group_id,
                   const char* path,
                   hid_t type,
                   int rank,
                   hsize_t* dims,
                   mhdf_Status* status )
{
  return mhdf_create_table_with_prop( group_id, path, type, rank, dims, H5P_DEFAULT, status );
}

hid_t
mhdf_create_table_with_prop( hid_t group_id,
                             const char* path,
                             hid_t type,
                             int rank,
                             hsize_t* dims,
                             hid_t create_prop,
                             mhdf_Status* status )
{
  hid_t space_id, table_id;

  space_id = H5Screate_simple( rank, dims, NULL );
  if (space_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return -1;
  }
  
#if defined(H5Dcreate_vers) && H5Dcreate_vers > 1
  table_id = H5Dcreate2( group_id, path, type, space_id, H5P_DEFAULT, create_prop, H5P_DEFAULT );
#else
  table_id = H5Dcreate( group_id, path, type, space_id, create_prop );
#endif
  H5Sclose(space_id);
  if (table_id < 0)
  {
    mhdf_setFail( status, "HDF5 DataSet creation failed.");
    return -1;
  }
  
  mhdf_setOkay( status );
  return table_id;
  
}

hid_t
mhdf_open_table( hid_t group_id,
                 const char* path,
                 int columns,
                 hsize_t* rows_out,
                 mhdf_Status* status )
{
  hid_t table_id, space_id;
  hsize_t dims[2];
  int rank;
  
#if defined(H5Dopen_vers) && H5Dopen_vers > 1  
  table_id = H5Dopen2( group_id, path, H5P_DEFAULT );
#else
  table_id = H5Dopen( group_id, path );
#endif
  if (table_id < 0)
  {
    mhdf_setFail( status, "HDF5 DataSet creation failed.");
    return -1;
  }
  
  space_id = H5Dget_space( table_id );
  if (space_id < 0)
  {
    mhdf_setFail( status, "Internal error in H5Dget_space.");
    H5Dclose( table_id );
    return -1;
  }
  
  rank = H5Sget_simple_extent_ndims( space_id );
  if (rank != (columns ? 1 : 2))
  {
    mhdf_setFail( status, "Incorrect DataSpace for DataSet." );
    H5Sclose( space_id );
    H5Dclose( table_id );
    return -1;
  }
  
  rank = H5Sget_simple_extent_dims( space_id, dims, NULL );
  H5Sclose( space_id );
  if (rank < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Sget_simple_extent_dims.");
    H5Dclose( table_id );
    return -1;
  }

  *rows_out = dims[0];
  mhdf_setOkay( status );
  return table_id;
}

hid_t
mhdf_open_table2( hid_t group_id,
                  const char* path,
                  int rank,
                  hsize_t* dims_out,
                  long* start_id_out,
                  mhdf_Status* status )
{
  hid_t table_id, space_id;
  
#if defined(H5Dopen_vers) && H5Dopen_vers > 1  
  table_id = H5Dopen2( group_id, path, H5P_DEFAULT );
#else
  table_id = H5Dopen( group_id, path );
#endif
  if (table_id < 0)
  {
    mhdf_setFail( status, "HDF5 DataSet creation failed.");
    return -1;
  }
  
  space_id = H5Dget_space( table_id );
  if (space_id < 0)
  {
    mhdf_setFail( status, "Internal error in H5Dget_space.");
    H5Dclose( table_id );
    return -1;
  }
  
  if (H5Sget_simple_extent_ndims( space_id ) != rank)
  {
    mhdf_setFail( status, "Incorrect DataSpace for DataSet." );
    H5Sclose( space_id );
    H5Dclose( table_id );
    return -1;
  }
  
  rank = H5Sget_simple_extent_dims( space_id, dims_out, NULL );
  H5Sclose( space_id );
  if (rank < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Sget_simple_extent_dims.");
    H5Dclose( table_id );
    return -1;
  }

  if (!mhdf_read_scalar_attrib( table_id, START_ID_ATTRIB, H5T_NATIVE_LONG, start_id_out, status ))
  {
    mhdf_setFail( status, "File format error.  Failed to retreive ID offset.");
    H5Dclose( table_id );
    return -1;
  }

  mhdf_setOkay( status );
  return table_id;
}

hid_t
mhdf_open_table_simple( hid_t group_id, const char* path, mhdf_Status* status )
{
  hid_t table_id;
  
#if defined(H5Dopen_vers) && H5Dopen_vers > 1  
  table_id = H5Dopen2( group_id, path, H5P_DEFAULT );
#else
  table_id = H5Dopen( group_id, path );
#endif
  if (table_id < 0)
  {
    mhdf_setFail( status, "HDF5 DataSet creation failed.");
  }
  else 
  {
    mhdf_setOkay( status );
  }

  return table_id;
}

static int qs_comp_int ( const void* ptr1, const void* ptr2 )
{
  const int* left = (const int*)ptr1;
  const int* right = (const int*)ptr2;
  return *left < *right ? -1 : *left > *right ? 1 : 0;
}
  
int mhdf_compact_to_ranges( int* length, int* ids, int ordered )
{
  int new_length = 0;
  int* iter, *end;
  int prev, count;
  int need_copy = 0;
  int *copy_ptr = 0, *w_iter;
  size_t blen;
  
  if (!ordered)
    qsort( ids, *length, sizeof(int), &qs_comp_int );
  
  iter = ids;
  end = ids + *length;
  while( iter != end )
  {
    prev = *(iter++);
    while( iter < end && *(iter++) == ++prev );
    new_length += 2;
    if (new_length > (iter - ids))
      need_copy = 1;
  }
  
  if (new_length > *length)
    return 0;
  
  if (need_copy)
  {
    blen = sizeof(int) * *length;
    copy_ptr = (int*)malloc( blen );
    memcpy( copy_ptr, ids, blen );
    iter = copy_ptr;
  }
  else
  {
    iter = ids;
  }
  
  end = iter + *length;
  w_iter = ids;
  while( iter != end )
  {
    prev = *(iter++);
    count = 1;
    while( iter < end && *(iter++) == ++prev );
    *(w_iter++) = prev - count;
    *(w_iter++) = count;
  }
  
  *length = new_length;
  if (need_copy)
    free( copy_ptr );
  return 1;
}
 
hid_t 
get_elem_type_enum( FileHandle* file_ptr, mhdf_Status* status )
{
  hid_t result;
#if defined(H5Topen_vers) && H5Topen_vers > 1  
  result = H5Topen2( file_ptr->hdf_handle, TYPE_ENUM_PATH, H5P_DEFAULT );
#else
  result = H5Topen( file_ptr->hdf_handle, TYPE_ENUM_PATH );
#endif
  if (result < 0)
    mhdf_setFail( status, "Element type enum does not exist in file.  Invalid file." );
  return result;
}

int
mhdf_write_max_id( FileHandle* file_ptr, mhdf_Status* status )
{
  hid_t group_id, attr_id, space_id;
  herr_t rval;
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, ROOT_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, ROOT_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "Internal error -- file invalid." );
    return 0;
  }
  
  attr_id = H5Aopen_name( group_id, MAX_ID_ATTRIB );
  if (attr_id < 0)
  {
    space_id = H5Screate( H5S_SCALAR );
#if defined(H5Acreate_vers) && H5Acreate_vers > 1
    attr_id = H5Acreate2( group_id, 
                          MAX_ID_ATTRIB, 
                          H5T_NATIVE_ULONG,
                          space_id, 
                          H5P_DEFAULT,
                          H5P_DEFAULT );
#else
    attr_id = H5Acreate( group_id, 
                         MAX_ID_ATTRIB, 
                         H5T_NATIVE_ULONG,
                         space_id, 
                         H5P_DEFAULT );
#endif
    H5Sclose( space_id );
  }
  H5Gclose( group_id );
  if (attr_id < 0)
  {
    mhdf_setFail( status, "Failed to create attribute \"%s\" on \"%s\"", MAX_ID_ATTRIB, ROOT_GROUP );
    return 0;
  }
                         
  
  rval = H5Awrite( attr_id, H5T_NATIVE_ULONG, &file_ptr->max_id );
  H5Aclose( attr_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Failed to write \"%s\" attrib.", MAX_ID_ATTRIB );
    return 0;
  }
  
  return 1;
}



static int mhdf_api_handle_count = 0;

static int num_open( )
{
  hid_t list[64];
  int nf, rval, i, count = 0;
  
  nf = H5Fget_obj_ids( H5F_OBJ_ALL, H5F_OBJ_FILE, sizeof(list)/sizeof(hid_t), list );
  if (nf <= 0 || nf > 64)
    return 0;
  
  for (i = 0; i < nf; i++)
  {
    rval = H5Fget_obj_count( list[i], H5F_OBJ_ALL );
    if (rval > 0)
      count += rval;
  }
  
  return count;
}
  

void mhdf_api_begin_internal( )
{
  /* HDF5 docs are incorrect.  Passing H5F_OBJ_ALL as the first
     arg to H5Fget_obj_count returns the total number of open 
     handles, not just those in files (i.e. temporary types and such.)
  mhdf_api_handle_count = H5Fget_obj_count( H5F_OBJ_ALL, H5F_OBJ_ALL );
     Need to loop to get actual file handles:
  */
  mhdf_api_handle_count = num_open();
}

void mhdf_api_end_internal( int expected_diff,
                            const char* filename,
                            int linenumber )
{
  if (mhdf_api_handle_count + expected_diff != num_open( ))
  {
    fprintf( stderr, "Unclosed handles at end of mhdf API call.\n" );
    fprintf( stderr, "Entered with %d, expected %d change, got %d.\n",
      mhdf_api_handle_count, expected_diff, num_open( ) );
    fprintf( stderr, "%s:%d\n", filename, linenumber );
    abort();
  }
  
  mhdf_api_handle_count = 0;
}

  
