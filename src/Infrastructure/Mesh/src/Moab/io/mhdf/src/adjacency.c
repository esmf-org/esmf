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
#include <H5Gpublic.h>
#include <H5Ppublic.h>
#include "mhdf.h"
#include "util.h"
#include "file-handle.h"
#include "status.h"
#include "names-and-paths.h"

int
mhdf_haveAdjacency( mhdf_FileHandle file,
                    const char* elem_group,
                    mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t elem_id;
  int result;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  if (elem_group == mhdf_node_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen( file_ptr->hdf_handle, NODE_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, NODE_GROUP );
#endif
    if (elem_id < 0)
    {
      mhdf_setFail( status, "H5Gopen( \"%s\" ) failed.\n", NODE_GROUP );
      return -1;
    }
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, elem_group, status );
    if (elem_id < 0)
      return -1;
  }
  
  result = mhdf_is_in_group( elem_id, ADJACENCY_NAME, status );
  H5Gclose( elem_id );
  mhdf_setOkay( status );
  API_END;
  return result;
}


hid_t
mhdf_createAdjacency( mhdf_FileHandle file,
                      const char* elem_handle,
                      long adj_list_size,
                      mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t elem_id, table_id;
  hsize_t dim = (hsize_t)adj_list_size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (adj_list_size < 1)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  if (elem_handle == mhdf_node_type_handle())
  {
    table_id = mhdf_create_table( file_ptr->hdf_handle,
                                  NODE_ADJCY_PATH,
                                  file_ptr->id_type,
                                  1, &dim,
                                  status );
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, elem_handle, status );
    if (elem_id < 0) return -1;
    
    table_id = mhdf_create_table( elem_id,
                                  ADJACENCY_NAME,
                                  file_ptr->id_type,
                                  1, &dim,
                                  status );
    H5Gclose( elem_id );
  }
  
  API_END_H(1);
  return table_id;
}


  
hid_t
mhdf_openAdjacency( mhdf_FileHandle file,
                    const char* elem_handle,
                    long* adj_list_size_out,
                    mhdf_Status* status )

{
  FileHandle* file_ptr;
  hid_t elem_id, table_id;
  hsize_t dim;
  API_BEGIN;
  
  file_ptr = (FileHandle*)(file);
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!adj_list_size_out)
  {
    mhdf_setFail( status, "Invalid argument.\n" );
    return -1;
  }
  
  if (elem_handle == mhdf_node_type_handle())
  {
    table_id = mhdf_open_table( file_ptr->hdf_handle,
                                NODE_ADJCY_PATH,
                                1, &dim,
                                status );
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, elem_handle, status );
    if (elem_id < 0) return -1;
    table_id = mhdf_open_table( elem_id,
                                ADJACENCY_NAME,
                                1, &dim,
                                status );
    H5Gclose( elem_id );
  }
  
  *adj_list_size_out = (long)dim;
  API_END_H(1);
  return table_id;
}

void
mhdf_writeAdjacency( hid_t table_id,
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
mhdf_writeAdjacencyWithOpt( hid_t table_id,
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
mhdf_readAdjacency( hid_t table_id,
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
mhdf_readAdjacencyWithOpt( hid_t table_id,
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



