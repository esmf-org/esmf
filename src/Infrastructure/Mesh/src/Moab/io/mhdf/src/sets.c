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

#include <H5Tpublic.h>
#include <H5Dpublic.h>
#include <H5Gpublic.h>
#include <H5Spublic.h>
#include <H5Ppublic.h>
#include "mhdf.h"
#include "util.h"
#include "file-handle.h"
#include "status.h"
#include "names-and-paths.h"

int
mhdf_haveSets( mhdf_FileHandle file,
               int* have_data,
               int* have_child,
               int* have_parent,
               mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file;
  hid_t root_id, set_id;
  int result;
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  root_id = H5Gopen2( file_ptr->hdf_handle, ROOT_GROUP, H5P_DEFAULT );
#else
  root_id = H5Gopen( file_ptr->hdf_handle, ROOT_GROUP );
#endif
  if (root_id < 0)
  {
    mhdf_setFail( status, "H5Gopen( \"%s\" ) failed.", ROOT_GROUP );
    return -1;
  }
  
  result = mhdf_is_in_group( root_id, SET_GROUP_NAME, status );
  if (result < 1)
  {
    H5Gclose(root_id);
    return result;
  }
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  set_id = H5Gopen2( root_id, SET_GROUP_NAME, H5P_DEFAULT );
#else
  set_id = H5Gopen( root_id, SET_GROUP_NAME );
#endif
  H5Gclose( root_id );
  if (set_id < 0)
  {
    mhdf_setFail( status, "H5Gopen( \"%s\" ) failed.", SET_GROUP );
    return -1;
  }
  
  result = mhdf_is_in_group( set_id, SET_META_NAME, status );
  if (result < 0)
  {
    H5Gclose(set_id);
    return result;
  }
  
  if (have_data) 
  {
    *have_data = mhdf_is_in_group( set_id, SET_DATA_NAME, status );
    if (*have_data < 0)
    {
      H5Gclose(set_id);
      return *have_data;
    }
  }
  
  if (have_child)
  {
    *have_child = mhdf_is_in_group( set_id, SET_CHILD_NAME, status );
    if (*have_child < 0)
    {
      H5Gclose(set_id);
      return *have_child;
    }
  }
  
  if (have_parent)
  {
    *have_parent = mhdf_is_in_group( set_id, SET_PARENT_NAME, status );
    if (*have_parent < 0)
    {
      H5Gclose(set_id);
      return *have_parent;
    }
  }
  
  mhdf_setOkay( status );
  H5Gclose( set_id );
  API_END;
  return result;
}
  

hid_t
mhdf_createSetMeta( mhdf_FileHandle file,
                    long num_sets,
                    long* first_id_out,
                    mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file;
  hid_t table_id;
  hsize_t dims[2];
  long first_id;
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  dims[0] = (hsize_t)num_sets;
  dims[1] = (hsize_t)4;
  table_id = mhdf_create_table( file_ptr->hdf_handle,
                                SET_META_PATH,
                                MHDF_INDEX_TYPE,
                                2, dims,
                                status );
  if (table_id < 0)
    return -1;
  
  first_id = file_ptr->max_id + 1;
  if (!mhdf_create_scalar_attrib( table_id, 
                                 START_ID_ATTRIB, 
                                 H5T_NATIVE_LONG,
                                 &first_id,
                                 status ))
  {
    H5Dclose( table_id );
    return -1;
  }
  
  *first_id_out = first_id;
  file_ptr->max_id += num_sets;
  if (!mhdf_write_max_id( file_ptr, status))
  {
    H5Dclose( table_id );
    return -1;
  }
  file_ptr->open_handle_count++;
  mhdf_setOkay( status );
 
  API_END_H(1);
  return table_id;
}


hid_t
mhdf_openSetMeta( mhdf_FileHandle file,
                  long* num_sets,
                  long* first_id_out,
                  mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file;
  hid_t table_id;
  hsize_t dims[2];
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  table_id = mhdf_open_table2( file_ptr->hdf_handle,
                               SET_META_PATH, 2,
                               dims, first_id_out, status );
  if (table_id < 0)
    return -1;
  
    /* If dims[1] == 3, then old format of table.
     * Deal with it in mhdf_readSetMeta and mhdf_writeSetMeta
     */
  if (dims[1] != 4 && dims[1] != 3)
  { 
    mhdf_setFail( status, "Invalid format for meshset table.\n" );
    H5Dclose( table_id );
    return -1;
  }

 
  *num_sets = dims[0];
  file_ptr->open_handle_count++;
  mhdf_setOkay( status );
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_openSetMetaSimple( mhdf_FileHandle file, mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file;
  hid_t table_id;
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  table_id = mhdf_open_table_simple( file_ptr->hdf_handle,
                                     SET_META_PATH, status );
  if (table_id < 0)
    return -1;
 
  file_ptr->open_handle_count++;
  mhdf_setOkay( status );
  API_END_H(1);
  return table_id;
}

static int
mhdf_readwriteSetMeta( hid_t table_id, int read,
                       long offset, long count,
                       hid_t type, void* data,
                       hid_t prop,
                       mhdf_Status* status )
{
  hid_t slab_id, sslab_id, smem_id, mem_id;
  hsize_t offsets[2], counts[2], mcounts[2], moffsets[2] = {0,0};
  herr_t rval = 0;
  int dims, i;
  const int fill_val = -1;
  const hsize_t one = 1;

  mcounts[0] = count;
  mcounts[1] = 4;
  if (offset < 0 || count < 0)
  {
    mhdf_setFail( status, "Invalid input for %s: "
                          "offset = %ld, count = %ld\n",
                          read ? "read" : "write",
                          offset, count );
    return 0;
  }
  
  slab_id = H5Dget_space( table_id );
  if (slab_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Dget_space.");
    return 0;
  }
  
  dims = H5Sget_simple_extent_ndims( slab_id );
  if (dims != 2)
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

  if (count) 
    mem_id = H5Screate_simple( dims, mcounts, NULL );
  else { /* special case for 'NULL' read during collective parallel IO */
    mem_id = H5Screate_simple( 1, &one, NULL );
    if (mem_id && 0 > H5Sselect_none( mem_id )) {
      H5Sclose( mem_id );
      mem_id = -1;
    }
  }
  if (mem_id < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return 0;
  }
  
    /* Normal table: 4 columns */
  if (counts[1] == 4)
  {
    offsets[1] = 0;
    if (count) 
      rval = H5Sselect_hyperslab( slab_id, H5S_SELECT_SET, offsets, NULL, counts, NULL );
    else /* special case for 'NULL' read during collective parallel IO */
      rval = H5Sselect_none( slab_id );
    if (rval < 0)
    {
      H5Sclose( mem_id );
      H5Sclose( slab_id );
      mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
      return 0;
    }
    
    if (read)
      rval = H5Dread( table_id, type, mem_id, slab_id, prop, data );
    else
      rval = H5Dwrite( table_id, type, mem_id, slab_id, prop, data );
  }
    /* Old table: 3 columns, no parent link counts */
  else if (counts[1] == 3)
  {
    rval = 0;
    for (i = 0; i < 3 && rval >= 0; ++i)
    {
      smem_id = H5Scopy( mem_id );
      sslab_id = H5Scopy( slab_id );
      if (smem_id < 0 || sslab_id < 0) {
        if (smem_id >= 0)
          H5Sclose( smem_id );
        mhdf_setFail( status, "Internal error calling H5Scopy." );
        return 0;
      }
      
      counts[1] = 1;
      offsets[1] = i;
      if (count)
        rval = H5Sselect_hyperslab( sslab_id, H5S_SELECT_SET, offsets, NULL, counts, NULL );
      else
        rval = H5Sselect_none( sslab_id );
      if (rval < 0)
      {
        H5Sclose( slab_id );
        H5Sclose( mem_id );
        mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
        return 0;
      }
      
      mcounts[1] = 1;
      moffsets[1] = (i == 2) ? 3 : i;
      rval = H5Sselect_hyperslab( smem_id, H5S_SELECT_SET, moffsets, NULL, mcounts, NULL );
      if (rval < 0)
      {
        H5Sclose( sslab_id );
        H5Sclose( slab_id );
        H5Sclose( mem_id );
        mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
        return 0;
      }

      if (read)
        rval = H5Dread( table_id, type, smem_id, sslab_id, prop, data );
      else
        rval = H5Dwrite( table_id, type, smem_id, sslab_id, prop, data );
        
      H5Sclose( sslab_id );
      H5Sclose( smem_id );
    }
    
    if (read && rval >= 0)
    {
      mcounts[1] = 1;
      moffsets[1] = 2;
      H5Sselect_hyperslab( mem_id, H5S_SELECT_SET, moffsets, NULL, mcounts, NULL );
      rval = H5Dfill( &fill_val, H5T_NATIVE_INT, data, type, mem_id );
    }
  }
  else
  {
    H5Sclose( mem_id );
    H5Sclose( slab_id );
    mhdf_setFail( status, "Invalid dimension for meshset metadata table." );
    return 0;
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

void
mhdf_readSetMeta( hid_t table_id,
                  long offset,
                  long count,
                  hid_t type,
                  void* data,  
                  mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readwriteSetMeta( table_id, 1, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetMetaWithOpt( hid_t table_id,
                  long offset,
                  long count,
                  hid_t type,
                  void* data,
                  hid_t prop,
                  mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readwriteSetMeta( table_id, 1, offset, count, type, data, prop, status );
  API_END;
}

void
mhdf_writeSetMeta( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   const void* data,  
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readwriteSetMeta( table_id, 0, offset, count, type, (void*)data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeSetMetaWithOpt( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   const void* data,
                   hid_t prop,
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readwriteSetMeta( table_id, 0, offset, count, type, (void*)data, prop, status );
  API_END;
}


enum SetMetaCol {
  CONTENT = 0,
  CHILDREN = 1,
  PARENTS = 2,
  FLAGS = 3 
};

static int
mhdf_readSetMetaColumn( hid_t table_id,
                        enum SetMetaCol column, 
                        long offset, long count,
                        hid_t type, void* data,
                        hid_t prop,
                        mhdf_Status* status )
{
  hid_t slab_id, mem_id;
  hsize_t offsets[2], counts[2], mcount = count;
  herr_t rval = 0;
  int dims;
  const int fill_val = -1;
  
  if (offset < 0 || count < 0) {
    mhdf_setFail( status, "Invalid input for reading set description column: "
                          "offset = %ld, count = %ld\n", offset, count );
    return 0;
  }
  
  /* Get dimensions of table, and check against requested count and offset */
  
  slab_id = H5Dget_space( table_id );
  if (slab_id < 0) {
    mhdf_setFail( status, "Internal error calling H5Dget_space.");
    return 0;
  }
  
  dims = H5Sget_simple_extent_ndims( slab_id );
  if (dims != 2) {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error: unexpected dataset rank: %d.", dims);
    return 0;
  }
  
  dims = H5Sget_simple_extent_dims( slab_id, counts, NULL );
  if (dims < 0) {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sget_simple_extend_dims.");
    return 0;
  }
 
  if ((unsigned long)(offset + count) > counts[0]) {
    H5Sclose( slab_id );
    mhdf_setFail( status, 
      "Requested read of rows %ld to %ld of a %ld row table.\n",
      offset, offset+count-1, (long)counts[0]);
    return 0;
  }


    /* Create a slab definition for the block of memory we're reading into */

  mem_id = H5Screate_simple( 1, &mcount, NULL );
  if (mem_id < 0) {
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Screate_simple." );
    return 0;
  }
  
    /* Old, 3-column table.
     * New table is {contents, children, parents, flags}
     * Old table is {contents, children, flags}
     * If asking for parents, just return zeros.
     * If asking for flags, fix column value.
     */
  offsets[1] = column; 
  if (counts[1] == 3) {
    if (column == PARENTS) {
      rval = H5Dfill( &fill_val, H5T_NATIVE_INT, data, type, mem_id );
      H5Sclose( mem_id );
      H5Sclose( slab_id );
      if (rval < 0) {
        mhdf_setFail( status, "Internal error calling H5Dfill" );
        return 0;
      }
      else {
        mhdf_setOkay( status );
        return 1;
      }
    }
    else if (column == FLAGS)
      --offsets[1];
  }
  else if (counts[1] != 4) {
    H5Sclose( mem_id );
    H5Sclose( slab_id );
    mhdf_setFail( status, "Invalid dimension for meshset metadata table." );
    return 0;
  }
  
    /* Create a slab defintion for the portion of the table we want to read. */
  
  /* offsets[1] was initialized in the above block of code. */
  offsets[0] = (hsize_t)offset;
  counts[0] = (hsize_t)count;
  counts[1] = 1; /* one column */
  if (count) 
    rval = H5Sselect_hyperslab( slab_id, H5S_SELECT_SET, offsets, NULL, counts, NULL );
  else
    rval = H5Sselect_none( slab_id );
  if (rval < 0)
  {
    H5Sclose( mem_id );
    H5Sclose( slab_id );
    mhdf_setFail( status, "Internal error calling H5Sselect_hyperslab." );
    return 0;
  }
  
    /* Read the data */
  
  rval = H5Dread( table_id, type, mem_id, slab_id, prop, data );
  H5Sclose( mem_id );
  H5Sclose( slab_id );
  if (rval < 0)
  {
    mhdf_setFail( status, "Internal error calling H5Dread." );
    return 0;
  }

  mhdf_setOkay( status );
  return 1;
}


void
mhdf_readSetFlags( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   void* data,
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, FLAGS, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetFlagsWithOpt( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   void* data,
                   hid_t prop,
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, FLAGS, offset, count, type, data, prop, status );
  API_END;
}


void
mhdf_readSetContentEndIndices( hid_t table_id,
                               long offset,
                               long count,
                               hid_t type,
                               void* data,
                               mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, CONTENT, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetContentEndIndicesWithOpt( hid_t table_id,
                               long offset,
                               long count,
                               hid_t type,
                               void* data,
                               hid_t prop,
                               mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, CONTENT, offset, count, type, data, prop, status );
  API_END;
}

void
mhdf_readSetChildEndIndices( hid_t table_id,
                             long offset,
                             long count,
                             hid_t type,
                             void* data,
                             mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, CHILDREN, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetChildEndIndicesWithOpt( hid_t table_id,
                             long offset,
                             long count,
                             hid_t type,
                             void* data,
                             hid_t prop,
                             mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, CHILDREN, offset, count, type, data, prop, status );
  API_END;
}

void
mhdf_readSetParentEndIndices( hid_t table_id,
                              long offset,
                              long count,
                              hid_t type,
                              void* data,
                              mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, PARENTS, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetParentEndIndicesWithOpt( hid_t table_id,
                              long offset,
                              long count,
                              hid_t type,
                              void* data,
                              hid_t prop,
                              mhdf_Status* status )
{
  API_BEGIN;
  mhdf_readSetMetaColumn( table_id, PARENTS, offset, count, type, data, prop, status );
  API_END;
}

hid_t
mhdf_createSetData( mhdf_FileHandle file_handle,
                    long data_list_size,
                    mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim = (hsize_t)data_list_size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (data_list_size < 1)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_create_table( file_ptr->hdf_handle,
                                SET_DATA_PATH,
                                file_ptr->id_type,
                                1, &dim,
                                status );
  
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_openSetData( mhdf_FileHandle file_handle,
                  long* data_list_size_out,
                  mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!data_list_size_out)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_open_table( file_ptr->hdf_handle,
                              SET_DATA_PATH,
                              1, &dim,
                              status );
 
  *data_list_size_out = (long)dim;
  API_END_H(1);
  return table_id;
}


void
mhdf_writeSetData( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   const void* data,
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeSetDataWithOpt( hid_t table_id,
                   long offset,
                   long count,
                   hid_t type,
                   const void* data,
                   hid_t prop,
                   mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, type, data, prop, status );
  API_END;
}


void
mhdf_readSetData( hid_t table_id,
                  long offset,
                  long count,
                  hid_t type,
                  void* data,
                  mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetDataWithOpt( hid_t table_id,
                  long offset,
                  long count,
                  hid_t type,
                  void* data,
                  hid_t prop,
                  mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, type, data, prop, status );
  API_END;
}

hid_t
mhdf_createSetChildren( mhdf_FileHandle file_handle,
                        long child_list_size,
                        mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim = (hsize_t)child_list_size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (child_list_size < 1)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_create_table( file_ptr->hdf_handle,
                                SET_CHILD_PATH,
                                file_ptr->id_type,
                                1, &dim,
                                status );
  
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_openSetChildren( mhdf_FileHandle file_handle,
                      long* child_list_size,
                      mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!child_list_size)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_open_table( file_ptr->hdf_handle,
                              SET_CHILD_PATH,
                              1, &dim,
                              status );
 
  *child_list_size = (long)dim;
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_createSetParents( mhdf_FileHandle file_handle,
                       long parent_list_size,
                       mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim = (hsize_t)parent_list_size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (parent_list_size < 1)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_create_table( file_ptr->hdf_handle,
                                SET_PARENT_PATH,
                                file_ptr->id_type,
                                1, &dim,
                                status );
  
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_openSetParents( mhdf_FileHandle file_handle,
                      long* parent_list_size,
                      mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t table_id;
  hsize_t dim;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file_handle);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!parent_list_size)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  table_id = mhdf_open_table( file_ptr->hdf_handle,
                              SET_PARENT_PATH,
                              1, &dim,
                              status );
 
  *parent_list_size = (long)dim;
  API_END_H(1);
  return table_id;
}

void
mhdf_writeSetParentsChildren( hid_t table_id,
                       long offset,
                       long count,
                       hid_t type,
                       const void* data,
                       mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeSetParentsChildrenWithOpt( hid_t table_id,
                       long offset,
                       long count,
                       hid_t type,
                       const void* data,
                       hid_t prop,
                       mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, type, data, prop, status );
  API_END;
}

void
mhdf_readSetParentsChildren( hid_t table_id,
                      long offset,
                      long count,
                      hid_t type,
                      void* data,
                      mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, type, data, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSetParentsChildrenWithOpt( hid_t table_id,
                      long offset,
                      long count,
                      hid_t type,
                      void* data,
                      hid_t prop,
                      mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, type, data, prop, status );
  API_END;
}
