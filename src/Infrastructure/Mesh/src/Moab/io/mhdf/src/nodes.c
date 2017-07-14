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
#include <H5Ppublic.h>
#include <H5Gpublic.h>
#include "mhdf.h"
#include "status.h"
#include "names-and-paths.h"
#include "util.h"
#include "file-handle.h"


int
mhdf_haveNodes( mhdf_FileHandle file, mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file;
  hid_t root_id, node_id;
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
  
  result = mhdf_is_in_group( root_id, NODE_GROUP_NAME, status );
  if (result < 1)
  {
    H5Gclose(root_id);
    return result;
  }
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  node_id = H5Gopen2( root_id, NODE_GROUP_NAME, H5P_DEFAULT );
#else
  node_id = H5Gopen( root_id, NODE_GROUP_NAME );
#endif
  H5Gclose( root_id );
  if (node_id < 0)
  {
    mhdf_setFail( status, "H5Gopen( \"%s\" ) failed.", NODE_GROUP );
    return -1;
  }
  
  result = mhdf_is_in_group( node_id, NODE_COORD_NAME, status );
  if (result >= 0)
   mhdf_setOkay( status );
  H5Gclose( node_id );
  API_END;
  return result;
}

hid_t
mhdf_createNodeCoords( mhdf_FileHandle file_handle,
                       int dimension,
                       long num_nodes,
                       long* first_id_out,
                       mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file_handle;
  hid_t table_id;
  hsize_t dims[2];
  long first_id;
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  if (dimension < 1)
  {
    mhdf_setFail( status, "Invalid argument: dimension = %d.", dimension );
    return -1;
  }
  
  dims[0] = (hsize_t)num_nodes;
  dims[1] = (hsize_t)dimension;
  table_id = mhdf_create_table( file_ptr->hdf_handle,
                                NODE_COORD_PATH,
                                H5T_NATIVE_DOUBLE,
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
  file_ptr->max_id += num_nodes;
  if (!mhdf_write_max_id( file_ptr, status ))
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
mhdf_openNodeCoords( mhdf_FileHandle file_handle,
                     long* num_nodes_out,
                     int* dimension_out,
                     long* first_id_out,
                     mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file_handle;
  hid_t table_id;
  hsize_t dims[2];
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  table_id = mhdf_open_table2( file_ptr->hdf_handle,
                               NODE_COORD_PATH, 2,
                               dims, first_id_out, status );
  if (table_id < 0)
    return -1;
 
  *num_nodes_out = dims[0];
  *dimension_out = dims[1];
  file_ptr->open_handle_count++;
  mhdf_setOkay( status );
  API_END_H(1);
  return table_id;
}

hid_t
mhdf_openNodeCoordsSimple( mhdf_FileHandle file_handle, mhdf_Status* status )
{
  FileHandle* file_ptr = (FileHandle*)file_handle;
  hid_t table_id;
  API_BEGIN;
  
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
  table_id = mhdf_open_table_simple( file_ptr->hdf_handle,
                                     NODE_COORD_PATH, status );
  if (table_id < 0)
    return -1;
 
  file_ptr->open_handle_count++;
  mhdf_setOkay( status );
  API_END_H(1);
  return table_id;
}

void
mhdf_writeNodeCoords( hid_t table_id,
                      long offset,
                      long count,
                      const double* coords,
                      mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, H5T_NATIVE_DOUBLE, coords, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeNodeCoordsWithOpt( hid_t table_id,
                      long offset,
                      long count,
                      const double* coords,
                      hid_t prop,
                      mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, H5T_NATIVE_DOUBLE, coords, prop, status );
  API_END;
}

void
mhdf_readNodeCoords( hid_t table_id,
                     long offset,
                     long count,
                     double* coords,
                     mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, H5T_NATIVE_DOUBLE, coords, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readNodeCoordsWithOpt( hid_t table_id,
                     long offset,
                     long count,
                     double* coords,
                     hid_t prop,
                     mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, H5T_NATIVE_DOUBLE, coords, prop, status );
  API_END;
}

void 
mhdf_writeNodeCoord( hid_t table_id,
                     long offset,
                     long count,
                     int dimension,
                     const double* coords,
                     mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_column( table_id, dimension, offset, count, 
                      H5T_NATIVE_DOUBLE, coords, H5P_DEFAULT, status );
  API_END;
}
void 
mhdf_writeNodeCoordWithOpt( hid_t table_id,
                     long offset,
                     long count,
                     int dimension,
                     const double* coords,
                     hid_t prop,
                     mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_column( table_id, dimension, offset, count, 
                      H5T_NATIVE_DOUBLE, coords, prop, status );
  API_END;
}
  

void 
mhdf_readNodeCoord( hid_t table_id,
                    long offset,
                    long count,
                    int dimension,
                    double* coords,
                    mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_column( table_id, dimension, offset, count, 
                    H5T_NATIVE_DOUBLE, coords, H5P_DEFAULT, status );
  API_END;
}
void 
mhdf_readNodeCoordWithOpt( hid_t table_id,
                    long offset,
                    long count,
                    int dimension,
                    double* coords,
                    hid_t prop,
                    mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_column( table_id, dimension, offset, count, 
                    H5T_NATIVE_DOUBLE, coords, prop, status );
  API_END;
}

