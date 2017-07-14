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

#ifndef mhdf_FILE_UTIL_H
#define mhdf_FILE_UTIL_H

#include <sys/types.h>
#include <H5Ipublic.h>
#include "status.h"
#include "file-handle.h"

#ifdef __cplusplus
extern "C" {
#endif


void* mhdf_malloc( size_t size, mhdf_Status* status );
void* mhdf_realloc( void* ptr, size_t size, mhdf_Status* status );

size_t mhdf_name_to_path( const char* name, char* path, size_t path_len );

int mhdf_path_to_name( const char* path, char* name );

char* mhdf_name_to_path_copy( const char* name, mhdf_Status* status );

char* mhdf_name_to_path_cat( const char* prefix, const char* name, mhdf_Status* status );

hid_t mhdf_elem_group_from_handle( FileHandle* file_ptr,
                                   const char* elem_handle,
                                   mhdf_Status* status );

int mhdf_create_scalar_attrib( hid_t object,
                               const char* name,
                               hid_t type,
                               const void* value,
                               mhdf_Status* status );

/* If type is zero, assumes opaque type.
   On error, sets status and returns zero.
   On success, returns non-zero and does not modify status */
int mhdf_read_scalar_attrib( hid_t object,
                             const char* name,
                             hid_t type,
                             void* value,
                             mhdf_Status* status );

/* Search the specified object to see if it contains an 
   an attribute with passed name.  Returns -1 on error, 1 
   if attribute was found, and zero if attribute was not 
   found.
*/
int mhdf_find_attribute( hid_t object, 
                         const char* attrib_name,
                         unsigned int* index_out,
                         mhdf_Status* status );

int mhdf_is_in_group( hid_t group, const char* name, mhdf_Status* status );

int
mhdf_read_data( hid_t data_table,
                long offset,
                long count,
                hid_t type,
                void* array,
                hid_t read_prop,
                mhdf_Status* status );


int
mhdf_write_data( hid_t data_table,
                 long offset,
                 long count,
                 hid_t type,
                 const void* array,
                 hid_t write_prop,
                 mhdf_Status* status );

int
mhdf_read_column( hid_t data_table,
                  int column,
                  long offset,
                  long count,
                  hid_t type,
                  void* array,
                  hid_t read_prop,
                  mhdf_Status* status );


int
mhdf_write_column( hid_t data_table,
                   int column,
                   long offset,
                   long count,
                   hid_t type,
                   const void* array,
                   hid_t write_prop,
                   mhdf_Status* status );

hid_t 
mhdf_create_table( hid_t group,
                   const char* path,
                   hid_t type,
                   int rank,
                   hsize_t* dims,
                   mhdf_Status* status );

hid_t 
mhdf_create_table_with_prop( hid_t group,
                   const char* path,
                   hid_t type,
                   int rank,
                   hsize_t* dims,
                   hid_t dataset_creation_prop,
                   mhdf_Status* status );

hid_t
mhdf_open_table( hid_t group,
                 const char* path,
                 int columns,
                 hsize_t* rows_out,
                 mhdf_Status* status );

hid_t
mhdf_open_table2( hid_t group,
                  const char* path,
                  int rank,
                  hsize_t* dims_out,
                  long* start_id_out,
                  mhdf_Status* status );

hid_t
mhdf_open_table_simple( hid_t group, const char* path, mhdf_Status* status );

int
mhdf_compact_to_ranges( int* length_in_out, int* ids_in, int ordered );

hid_t 
get_elem_type_enum( FileHandle* file_ptr, mhdf_Status* status );

void mhdf_api_begin_internal( void );
void mhdf_api_end_internal( int expected_diff,
                            const char* filename,
                            int linenumber );

int mhdf_write_max_id( FileHandle* file_ptr, mhdf_Status* status );
                            
                            
#ifndef DEBUG_OPEN_HANDLES
#  define API_BEGIN
#  define API_END_H(n)
#else
#  define API_BEGIN mhdf_api_begin_internal()
#  define API_END_H(n) mhdf_api_end_internal(n, __FILE__, __LINE__)
#endif
#define API_END API_END_H(0)


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
 
