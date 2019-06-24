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
#include <H5Tpublic.h>
#include <H5Gpublic.h>
#include <H5Dpublic.h>
#include <H5Spublic.h> /* for H5S_MAX_RANK */
#include <H5Apublic.h>
#include <H5Ppublic.h>
#include "status.h"
#include "file-handle.h"
#include "mhdf.h"
#include "util.h"
#include "names-and-paths.h"

hid_t
mhdf_getNativeType( hid_t input_type,
                    int size,
                    mhdf_Status* status )
{
  H5T_sign_t sgn;
  H5T_class_t cls;
  hid_t tmp_id, type_id;
  
  mhdf_setOkay( status );
  
  cls = H5Tget_class( input_type );
  switch( cls )
  {
    case H5T_FLOAT:
      switch( size ) 
      {
        case  4: return H5T_NATIVE_FLOAT;
        case  8: return H5T_NATIVE_DOUBLE;
        case 16: return H5T_NATIVE_LDOUBLE;
        default:
          mhdf_setFail( status, "Invalid size for floating point type: %d", size );
          return -1;
      }

    case H5T_INTEGER:
      sgn = H5Tget_sign( input_type );
      if (H5T_SGN_ERROR == sgn)
      {
        mhdf_setFail( status, "Internall errror calling H5Tget_sign." );
        return -1;
      }
      if (     sizeof(      char ) == size)
        return sgn == H5T_SGN_NONE ? H5T_NATIVE_UCHAR  : H5T_NATIVE_SCHAR;
      else if (sizeof(     short ) == size)
        return sgn == H5T_SGN_NONE ? H5T_NATIVE_USHORT : H5T_NATIVE_SHORT;
      else if (sizeof(       int ) == size)
        return sgn == H5T_SGN_NONE ? H5T_NATIVE_UINT   : H5T_NATIVE_INT;
      else if (sizeof(      long ) == size)
        return sgn == H5T_SGN_NONE ? H5T_NATIVE_ULONG  : H5T_NATIVE_LONG;
      else if ((int)H5Tget_size(H5T_NATIVE_LLONG) == size)
        return sgn == H5T_SGN_NONE ? H5T_NATIVE_ULLONG : H5T_NATIVE_LLONG;
      
      mhdf_setFail( status, "Invalid size for integer type: %d", size );
      return -1;
      
    case H5T_ENUM:
      tmp_id = H5Tget_super( input_type );
      if (tmp_id < 0)
      {
        mhdf_setFail( status, "Internal error calling H5Tget_super." );
        return -1;
      }
      type_id = mhdf_getNativeType( tmp_id, size, status );
      H5Tclose( tmp_id );
      return type_id;

    case H5T_TIME:
    case H5T_OPAQUE:
    case H5T_REFERENCE:
      mhdf_setFail( status, "Unsupported type class." );
      return -1;

    case H5T_COMPOUND:
    case H5T_VLEN:
    case H5T_ARRAY:
    case H5T_STRING:
      mhdf_setFail( status, "Only atomic types are supported." );
      return -1;

    default:
      mhdf_setFail( status, "Internal error calling H5Tget_class.  Bad handle?" );
      return -1;
  }
}

static hid_t get_tag( mhdf_FileHandle file_handle,
                      const char* tag_name,
                      hid_t* id_type,
                      mhdf_Status* status )
{
  hid_t group_id, tag_id;
  char* path;
  FileHandle* file_ptr;

  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (NULL != id_type)
    *id_type = file_ptr->id_type;

  path = mhdf_name_to_path_copy( tag_name, status );
  if (NULL == path)
    return -1;
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, TAG_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, TAG_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "Failed to open tag group." );
    free( path );
    return -1;
  }
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  tag_id = H5Gopen2( group_id, path, H5P_DEFAULT );
#else
  tag_id = H5Gopen( group_id, path );
#endif
  H5Gclose( group_id );
  free( path );
  if (tag_id < 0)
  {
    mhdf_setFail( status, "Failed to open tag data for tag \"%s\".", tag_name );
    return -1;
  }
  
  mhdf_setOkay( status );
  return tag_id;
}
  

static hid_t get_tag_type( FileHandle* file_ptr,
                           const char* tag_path,
                           mhdf_Status* status )
{
  hid_t group_id, tag_id, type_id;
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, TAG_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, TAG_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "Failed to open tag group." );
    return -1;
  }
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  tag_id = H5Gopen2( group_id, tag_path, H5P_DEFAULT );
#else
  tag_id = H5Gopen( group_id, tag_path );
#endif
  H5Gclose( group_id );
  if (tag_id < 0)
  {
    mhdf_setFail( status, "Failed to open group for tag \"%s\".", tag_path );
    return -1;
  }
  
#if defined(H5Topen_vers) && H5Topen_vers > 1  
  type_id = H5Topen2( tag_id, TAG_TYPE_NAME, H5P_DEFAULT );
#else
  type_id = H5Topen( tag_id, TAG_TYPE_NAME );
#endif
  H5Gclose( tag_id );
  if (type_id < 0)
  {
    mhdf_setFail( status, "Failed to open type data for tag \"%s\".", tag_path );
    return -1;
  }
  
  return type_id;
}

/** Helper function to write default and mesh values for tag 
 *\param tag_id       The file object upon which to attach the attribute
 *\param attrib_name  The name of the attribute object
 *\param type_id      The data type of the attribute data
 *\param value        Pointer to attribute data
 *\param value_size   Size of attribute data, as multiple of type indicated 
 *                    by type_id.  Should be 1 except for variable-length tag data.
 */
static
int store_tag_val_in_attrib( hid_t tag_id, 
                             const char* attrib_name,
                             hid_t type_id,
                             const void* value,
                             hsize_t value_size,
                             mhdf_Status* status )
{
  hid_t write_type;
  int rval;
  if (value_size == 1) 
    write_type = type_id;
  else if (H5Tget_class(type_id) == H5T_OPAQUE) {
    write_type = H5Tcreate( H5T_OPAQUE, value_size );
  }
  else {
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1  
    write_type = H5Tarray_create2( type_id, 1, &value_size );
#else
    write_type = H5Tarray_create( type_id, 1, &value_size, 0 );
#endif
  }
  
  if (write_type < 0) {
    mhdf_setFail( status, "Error constructing type object for tag mesh/default value" );
    return -1;
  }
  
  rval = mhdf_create_scalar_attrib( tag_id, attrib_name, write_type, value, status );
  if (write_type != type_id)
    H5Tclose( write_type );
  
  return rval;
}

static
hid_t create_tag_common( mhdf_FileHandle file_handle,
                         const char* tag_name,
                         enum mhdf_TagDataType tag_type,
                         int size,
                         int storage,
                         const void* default_value,
                         int default_value_size_in,
                         const void* global_value,
                         int global_value_size_in,
                         hid_t hdf_type,
                         hid_t hdf_base_type,
                         mhdf_Status* status )
{
  hid_t temp_id, group_id, tag_id;
  char* path;
  FileHandle* file_ptr;
  herr_t rval;
  hsize_t arr_len;
  int one = 1, var_len=0;
  hsize_t default_value_size = default_value_size_in;
  hsize_t global_value_size = global_value_size_in;
  int close_base_type = 0;

    /* Force standard data types over user-specified types */

  if (tag_type != mhdf_OPAQUE)
    hdf_type = 0;
  if (tag_type != mhdf_ENTITY_ID)
    hdf_base_type = 0;


    /* Validate input */
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!tag_name || !*tag_name)
  {
    mhdf_setFail( status, "Invalid tag name" );
    return -1;
  }
  
  
    /* Open the tag group */

#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, TAG_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, TAG_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "H5Gopen(\"%s\") failed.", TAG_GROUP );
    return -1;
  }

    /* Create path string for tag object */

  path = mhdf_name_to_path_copy( tag_name, status );
  if (!path) 
  { 
    H5Gclose( group_id );
    return -1; 
  }
  
    /* Create group for this tag */

#if defined(H5Gcreate_vers) && H5Gcreate_vers > 1
  tag_id = H5Gcreate2( group_id, path, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
#else
  tag_id = H5Gcreate( group_id, path, 3 );
#endif
  if (tag_id < 0)
  {
     mhdf_setFail( status, "H5Gcreate( \"%s\" ) failed.", path );
     free( path );
     H5Gclose( group_id );
     return -1;
  }
  
    /* Store the tag name as the comment on the group entry */
  
  rval = H5Gset_comment( group_id, path, tag_name );
  H5Gclose( group_id );
  free( path );
  if (rval < 0)
  {
    mhdf_setFail( status, "H5Gset_comment failed for tag \"%s\"", tag_name );
    H5Gclose( tag_id );
    return -1;
  }

    /* Store TSTT tag type as attribute */

  rval = mhdf_create_scalar_attrib( tag_id, 
                                   TAG_TYPE_ATTRIB,
                                   H5T_NATIVE_INT,
                                   &storage,
                                   status );
  if (!rval)
  {
    H5Gclose( tag_id );
    return -1;
  }
  
  if (hdf_type)
  {
    hdf_type = H5Tcopy( hdf_type );
    arr_len = 1;
  }
  else
  {
    switch (tag_type) 
    {
      default:
      case mhdf_OPAQUE:
        arr_len = 1;
        hdf_type = H5Tcreate( H5T_OPAQUE, abs(size) );
        H5Tset_tag( hdf_type, "tag_data" );
        break;
      
      case mhdf_BITFIELD:
        arr_len = 1;
        if (size <= 0) 
        {
          mhdf_setFail( status, "Invalid size (%d) for bit tag.", (int)size );
          return -1;
        }
        else if (size <= 8)
          hdf_type = H5Tcopy( H5T_NATIVE_B8 );
        else if (size <= 16)
          hdf_type = H5Tcopy( H5T_NATIVE_B16 );
        else if (size <= 32)
          hdf_type = H5Tcopy( H5T_NATIVE_B32 );
        else if (size <= 64)
          hdf_type = H5Tcopy( H5T_NATIVE_B64 );
        else
        {
          mhdf_setFail( status, "Cannot create a bit tag larger than 64-bits.  %d bits requested.\n", (int)size);
          return -1;
        }
        
        if (0 > H5Tset_precision( hdf_type, size ))
        {
          mhdf_setFail( status, "H5Tset_precision failed.");
          return -1;
        }
        break;
      
      case mhdf_ENTITY_ID:
        arr_len = abs(size);
        hdf_type = H5Tcopy( H5T_NATIVE_ULONG );
        break;
      
      case mhdf_BOOLEAN:
        arr_len = abs(size);
        hdf_type = H5Tcopy( H5T_NATIVE_UCHAR );
        break;
      
      case mhdf_INTEGER:
        arr_len = abs(size);
        hdf_type = H5Tcopy( H5T_NATIVE_INT );
        break;
      
      case mhdf_FLOAT:
        arr_len = abs(size);
        hdf_type = H5Tcopy( H5T_NATIVE_DOUBLE );
        break;
    }
  }
  
  if (hdf_type <= 0)
  {
    mhdf_setFail( status, "Failed to create tag type object." );
    H5Gclose( tag_id );
    return -1;
  }
  
  if (hdf_base_type && H5Tget_class(hdf_type) != H5Tget_class(hdf_base_type)) {
    mhdf_setFail( status, "Invalid base type for tag default/global data" );
    H5Gclose( tag_id );
    return -1;
  }

  if (size < -1 || !arr_len) 
  {
    mhdf_setFail( status, "Invalid 'size' parameter passed to mhdf_createTag (%d)", (int)size);
    H5Gclose( tag_id );
    return -1;
  }
  else if (size == -1)
  {
      /* Note: we don't do anything with this here.  We rely on
       *       the app to ask us to create the index table later.
       */
    arr_len = 1;
      /* need to know this later, when storing default/global values */
    var_len = 1;
  }
  else if (arr_len > 1)
  {
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1  
    temp_id = H5Tarray_create2( hdf_type, 1, &arr_len);
#else
    temp_id = H5Tarray_create( hdf_type, 1, &arr_len, NULL );
#endif
    H5Tclose( hdf_type );
    if (temp_id < 0)
    {
      mhdf_setFail( status, "Failed to create tag type object." );
      H5Gclose( tag_id );
      return -1;
    }
    hdf_type = temp_id;
    
    if (hdf_base_type) {
      if (H5Tequal( hdf_base_type, hdf_type ) > 0) {
        hdf_base_type = hdf_type;
      }
      else {
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1  
        temp_id = H5Tarray_create2( hdf_base_type, 1, &arr_len);
#else
        temp_id = H5Tarray_create( hdf_base_type, 1, &arr_len, NULL );
#endif
        if (temp_id < 0)
        {
          mhdf_setFail( status, "Failed to create tag type object." );
          H5Gclose( tag_id );
          H5Tclose( hdf_type );
          return -1;
        }
        hdf_base_type = temp_id;
        close_base_type = 1;
      }
    }
  }
    
  
  if (!hdf_base_type) 
    hdf_base_type = hdf_type;
  
    /* Create tag type object, or write attribute if opaque */
 
#if defined(H5Tcommit_vers) && H5Tcommit_vers > 1
  rval = H5Tcommit2( tag_id, TAG_TYPE_NAME, hdf_type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
#else
  rval = H5Tcommit( tag_id, TAG_TYPE_NAME, hdf_type );
#endif
  if (rval < 0)
  {
    mhdf_setFail( status, "H5Tcommit failed for tag \"%s\"", tag_name );
    if (close_base_type)
      H5Tclose( hdf_base_type );
    H5Tclose( hdf_type );
    H5Gclose( tag_id );
    return -1;
  }
  
    /* If tag is entity handle, make note of it */
  if (tag_type == mhdf_ENTITY_ID)
  {
    rval = mhdf_create_scalar_attrib( tag_id, 
                                      TAG_HANDLE_TYPE_ATTRIB,
                                      H5T_NATIVE_INT,
                                      &one,
                                      status );
    if (!rval) 
    { 
      if (close_base_type)
        H5Tclose( hdf_base_type );
      H5Gclose( tag_id );
      H5Tclose( hdf_type );
      return -1; 
    }
  }
                                     
  

    /* Store the default value as a attribute of the tag group */

  if (default_value)
  {
    rval = store_tag_val_in_attrib( tag_id, TAG_DEFAULT_ATTRIB, hdf_base_type, 
                                    default_value,
                                    var_len ? default_value_size : 1, status );
    if (!rval) {
      if (close_base_type)
        H5Tclose( hdf_base_type );
      H5Gclose( tag_id );
      H5Tclose( hdf_type );
      return -1;
    }
  }
    
    /* Store global tag value as attribute */
  
  if (global_value)
  {
    rval = store_tag_val_in_attrib( tag_id, TAG_GLOBAL_ATTRIB, hdf_base_type, 
                                    global_value,
                                    var_len ? global_value_size : 1, status );
    if (!rval) {
      if (close_base_type)
        H5Tclose( hdf_base_type );
      H5Gclose( tag_id );
      H5Tclose( hdf_type );
      return -1;
    }
  }

  if (close_base_type)
    H5Tclose( hdf_base_type );
  H5Tclose( hdf_type );
  mhdf_setOkay( status );
  return tag_id;
}


hid_t
mhdf_getTagDataType( mhdf_FileHandle file_handle, 
                     const char* tag_name, 
                     mhdf_Status* status )
{
  FileHandle* file_ptr;
  hid_t result;
  char* path;
  API_BEGIN;

    /* Validate input */
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;

  if (!tag_name || !*tag_name)
  {
    mhdf_setFail( status, "Invalid tag name" );
    return -1;
  }

    /* Create path string for tag object */

  path = mhdf_name_to_path_copy( tag_name, status );
  if (!path) 
  { 
    return -1; 
  }
  
  result = get_tag_type( file_ptr, path, status );
  
  free(path);
  API_END;
  return result;
}
  

void
mhdf_createTag( mhdf_FileHandle file_handle,
                const char* tag_name,
                enum mhdf_TagDataType tag_type,
                int size, 
                int storage,
                const void* default_value,
                const void* global_value,
                hid_t hdf_type,
                hid_t hdf_base_type,
                mhdf_Status* status )
{
  hid_t tag_id;
  API_BEGIN;
  tag_id = create_tag_common( file_handle, tag_name, tag_type, size, storage, 
                     default_value, 1, global_value, 1, hdf_type, 
                     hdf_base_type, status );
  if (tag_id >= 0)
    H5Gclose( tag_id );
  API_END;
}

void
mhdf_createVarLenTag( mhdf_FileHandle file_handle,
                      const char* tag_name,
                      enum mhdf_TagDataType tag_type,
                      int storage,
                      const void* default_value,
                      int default_value_length,
                      const void* global_value,
                      int global_value_length,
                      hid_t hdf_type,
                      hid_t hdf_base_type,
                      mhdf_Status* status )
{
  hid_t tag_id;
  int one = 1;
  
  API_BEGIN;
  tag_id = create_tag_common( file_handle, tag_name, tag_type, -1, storage, 
                              default_value, default_value_length, 
                              global_value, global_value_length, 
                              hdf_type, hdf_base_type, status );
  if (tag_id >= 0) {
    mhdf_create_scalar_attrib( tag_id, 
                               TAG_VARLEN_ATTRIB,
                               H5T_NATIVE_INT,
                               &one,
                               status );
    H5Gclose( tag_id );
  }
  API_END;
}

int
mhdf_getNumberTags( mhdf_FileHandle file_handle, mhdf_Status* status )
{
  hid_t group_id;
  hsize_t result;
  FileHandle* file_ptr;
  API_BEGIN;

    /* Validate input */
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status ))
    return -1;
  
    /* Open the tags group */
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, TAG_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, TAG_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "H5Gopen(\"%s\") failed", TAG_GROUP);
    return -1;
  }
  
    /* Get number of objects in tags group */
  
  if (H5Gget_num_objs( group_id, &result ) < 0)
  {
    mhdf_setFail( status, "Internal failure calling H5Gget_num_objs.");
    H5Gclose(group_id);
    return -1;
  }
  
  H5Gclose( group_id );
  mhdf_setOkay( status );
  API_END;
  return (int)result;
}

char**
mhdf_getTagNames( mhdf_FileHandle file_handle,
                  int* num_names_out,
                  mhdf_Status* status )
{
  hid_t group_id;
  FileHandle* file_ptr;
  hsize_t count, idx;
  char* name;
  char** result;
  ssize_t size;
  API_BEGIN;
  

    /* Validate input */
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status ))
    return NULL;
  
    /* Open the tags group */
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( file_ptr->hdf_handle, TAG_GROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( file_ptr->hdf_handle, TAG_GROUP );
#endif
  if (group_id < 0)
  {
    mhdf_setFail( status, "H5Gopen(\"%s\") failed", TAG_GROUP);
    return NULL;
  }
  
    /* Get number of objects in tags group */
  
  if (H5Gget_num_objs( group_id, &count ) < 0)
  {
    mhdf_setFail( status, "Internal failure calling H5Gget_num_objs.");
    H5Gclose(group_id);
    return NULL;
  }
  
    /* No tags? */

  *num_names_out = (int)count;
  if (count == 0)
  {
    H5Gclose( group_id );
    mhdf_setOkay( status );
    return NULL;
  }
  
    /* Allocate string array */
  
  result = (char**)mhdf_malloc( sizeof(char*) * count, status );
  if (NULL == result)
  {
    H5Gclose( group_id );
    return NULL;
  }
  
    /* Get names */
  
  for (idx = 0; idx < count; ++idx)
  {
    size = H5Gget_objname_by_idx( group_id, idx, NULL, 0 );
    if (size < 1 || NULL == (name = (char*)mhdf_malloc( size+1, status )))
    {
      while ((--idx) > 0)
        free( result[idx] );
      free ( result );
      H5Gclose( group_id );
      mhdf_setFail( status, "Internal failure calling H5Gget_objname_by_idx.");
      return NULL;
    }
    
    H5Gget_objname_by_idx( group_id, idx, name, size + 1 );
    if (!mhdf_path_to_name( name, name ))
    {
      mhdf_setFail( status, "Invalid character string in internal file path: \"%s\"\n",
        name );
      return NULL;
    }
    result[idx] = name;
  }
  
  H5Gclose( group_id );
  mhdf_setOkay( status );
  API_END;
  return result;
}

static int get_attrib_array_length_handle( hid_t attrib_id )
{
  hid_t type_id;
  int rank;
  hsize_t dims[H5S_MAX_RANK];
  int perm[H5S_MAX_RANK];
  
  type_id = H5Aget_type( attrib_id );
  switch (H5Tget_class(type_id)) {
    case H5T_NO_CLASS:
      dims[0] = -1;
      break;
    case H5T_OPAQUE:
      dims[0] = H5Tget_size( type_id );
      break;
    case H5T_ARRAY:
#if defined(H5Tget_array_dims_vers) && H5Tget_array_dims_vers > 1
      (void)perm; /* suppress warning */
      rank = H5Tget_array_dims2( type_id, dims );
#else
      rank = H5Tget_array_dims( type_id, dims, perm );
#endif
      if (rank == 1)
        break;
      else
        return -1;
    default:
      dims[0] = 1;
      break;
  }
  
  H5Tclose( type_id );
  return dims[0];
}
      
     
/*
static int get_attrib_array_length_index( hid_t object_id, unsigned int index )
{
  hid_t attrib_id;
  int result;
  
  attrib_id = H5Aopen_idx( object_id, index );
  if (attrib_id < 0)
    return -1;
  
  result = get_attrib_array_length_handle( attrib_id );
  H5Aclose( attrib_id );
  return result;
}
*/

static int get_attrib_array_length_name( hid_t file, const char* path )
{
  hid_t attrib_id;
  int result;
  
  attrib_id = H5Aopen_name( file, path );
  if (attrib_id < 0)
    return -1;
  
  result = get_attrib_array_length_handle( attrib_id );
  H5Aclose( attrib_id );
  return result;
}
      

void
mhdf_getTagInfo( mhdf_FileHandle file_handle,
                 const char* tag_name,
                 enum mhdf_TagDataType* class_out,
                 int* size_out,
                 int* tstt_storage_out,
                 int* have_default_out,
                 int* have_global_out,
                 int* have_sparse_out,
                 mhdf_Status* status )
{
  hid_t tag_id, type_id, super_id;
  int i, rval, is_handle;
  hsize_t size, sup_size;
  unsigned int idx;
  int rank, var_data;
  hsize_t dims[H5S_MAX_RANK];
  int perm[H5S_MAX_RANK];
  H5T_class_t class_tmp;

  API_BEGIN;


    /* Validate input */
  if (NULL == tag_name         ||
      NULL == class_out        ||
      NULL == size_out         ||
      NULL == tstt_storage_out ||
      NULL == have_default_out ||
      NULL == have_global_out  ||
      NULL == have_sparse_out  )
  {
    mhdf_setFail( status, "Invalid input." );
    return;
  }
  
    /* Get group for tag */
  tag_id = get_tag( file_handle, tag_name, NULL, status );
  if (tag_id < 0)
    return;
  
    /* Check for sparse data */
  rval = mhdf_is_in_group( tag_id, SPARSE_ENTITY_NAME, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  *have_sparse_out = rval ? 1 : 0;
  
    /* Check for variable-length tag data */
  rval = mhdf_find_attribute( tag_id, TAG_VARLEN_ATTRIB, &idx, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  var_data = rval ? 1 : 0;

    /* Check if have default value for tag */
  rval = mhdf_find_attribute( tag_id, TAG_DEFAULT_ATTRIB, &idx, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  if (!rval)
    *have_default_out = 0;
  else if (!var_data)
    *have_default_out = 1;
  else {
    /* *have_default_out = get_attrib_array_length_index( tag_id, index ); */
    *have_default_out = get_attrib_array_length_name( tag_id, TAG_DEFAULT_ATTRIB );
    if (*have_default_out < 0) {
      mhdf_setFail( status, "Error checking length of default value for tag: %s\n", tag_name );
      H5Gclose( tag_id );
      return;
    }
  }

    /* Check if have global value for tag */
  rval = mhdf_find_attribute( tag_id, TAG_GLOBAL_ATTRIB, &idx, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  if (!rval)
    *have_global_out = 0;
  else if (!var_data)
    *have_global_out = 1;
  else {
    /* *have_global_out = get_attrib_array_length_index( tag_id, index ); */
    *have_global_out = get_attrib_array_length_name( tag_id, TAG_GLOBAL_ATTRIB );
    if (*have_global_out < 0) {
      mhdf_setFail( status, "Error checking length of global value for tag: %s\n", tag_name );
      H5Gclose( tag_id );
      return;
    }
  }
  
    /* Get TSTT tag class */
  rval = mhdf_read_scalar_attrib( tag_id, TAG_TYPE_ATTRIB, 
                                  H5T_NATIVE_INT, tstt_storage_out,
                                  status );
  if (rval < 1)
  {
    H5Gclose( tag_id );
    return;
  }
  
    /* Check if tag is storing entity handles */
  rval = mhdf_find_attribute( tag_id, TAG_HANDLE_TYPE_ATTRIB, &idx, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  is_handle = rval;
    
    /* Get tag type */
#if defined(H5Topen_vers) && H5Topen_vers > 1  
  type_id = H5Topen2( tag_id, TAG_TYPE_NAME, H5P_DEFAULT );
#else
  type_id = H5Topen( tag_id, TAG_TYPE_NAME );
#endif
  if (type_id < 0)
  {
    H5Gclose( tag_id );
    mhdf_setFail( status, "Failed to get type object for tag \"%s\".", tag_name );
    return ;
  }
  
  class_tmp = H5Tget_class( type_id );
  if (class_tmp < 0)
  {
    mhdf_setFail( status, "H5Tget_class failed." );
    H5Gclose( tag_id );
    H5Tclose( type_id );
    return;
  }
  
  size = H5Tget_size( type_id );
  if (size <= 0)
  {
    mhdf_setFail( status, "H5Tget_size failed." );
    H5Gclose( tag_id );
    H5Tclose( type_id );
    return;
  }
    
  switch (class_tmp)
  {
    case H5T_INTEGER:
      *class_out = (size == 1) ? mhdf_BOOLEAN : mhdf_INTEGER;
      *size_out = 1;
      break;
    
    case H5T_FLOAT:
      *class_out = mhdf_FLOAT;
      *size_out = 1;
      break;
    
    case H5T_BITFIELD:
      *class_out = mhdf_BITFIELD;
      *size_out = H5Tget_precision( type_id );
      if (*size_out <= 0)
      {
        mhdf_setFail( status, "H5Tget_precision failed." );
        H5Gclose( tag_id );
        H5Tclose( type_id );
        return;
      }
      break;
    
    default:
    case H5T_OPAQUE:
      *class_out = mhdf_OPAQUE;
      *size_out = size;
      break;
    
    case H5T_ARRAY:
      
#if defined(H5Tget_array_dims_vers) && H5Tget_array_dims_vers > 1
      (void)perm; /* suppress warning */
      rank = H5Tget_array_dims2( type_id, dims );
#else
      rank = H5Tget_array_dims( type_id, dims, perm );
#endif
      if (rank <= 0)
      {
        mhdf_setFail( status, "H5Tget_size failed." );
        H5Gclose( tag_id );
        H5Tclose( type_id );
        return;
      }
      for (i = 1; i < rank; ++i)
        dims[0] *= dims[i];
      

      super_id = H5Tget_super( type_id );
      if (super_id < 0)
      {
        mhdf_setFail( status, "H5Tget_super failed" );
        H5Gclose( tag_id );
        H5Tclose( type_id );
        return;
      }
       
      class_tmp = H5Tget_class( super_id );
      if (class_tmp < 0)
      {
        mhdf_setFail( status, "H5Tget_class failed." );
        H5Gclose( tag_id );
        H5Tclose( type_id );
        H5Tclose( super_id );
        return;
      }

      sup_size = H5Tget_size( super_id );
      H5Tclose( super_id );
      if (sup_size <= 0)
      {
        mhdf_setFail( status, "H5Tget_size failed." );
        H5Gclose( tag_id );
        H5Tclose( type_id );
        return;
      }
      
      
      switch (class_tmp)
      {
        case H5T_INTEGER:
          *class_out = (sup_size == 1) ? mhdf_BOOLEAN : mhdf_INTEGER;
          *size_out = dims[0];
          break;

        case H5T_FLOAT:
          *class_out = mhdf_FLOAT;
          *size_out = dims[0];
          break;
    
        default:
          *class_out = mhdf_OPAQUE;
          *size_out = size;
          break;
      }
      
      break;
  }
  H5Tclose( type_id );
  H5Gclose( tag_id );
  
  
  if (is_handle)
  {
    if (*class_out != mhdf_INTEGER)
    {
      mhdf_setFail( status, "Non-integer tag marked as handle type." );
      return;
    }
    *class_out = mhdf_ENTITY_ID;
  }
  
  if (var_data) 
  {
    if (*size_out != 1 || *class_out == mhdf_BITFIELD)
    {
      mhdf_setFail( status, "Invalid or unexpected variable-length tag data" );
      return;
    }
    *size_out = -1;
  }

  mhdf_setOkay( status );
  API_END;
}    
 
 
static int
read_tag_attrib_data( hid_t tag_id, 
                      const char* attrib_name, 
                      hid_t type_id,
                      void* data, 
                      int is_var_len, 
                      mhdf_Status* status )
{
  int rval, ilen;
  unsigned idx;
  hid_t read_type = type_id;
  hsize_t len;
 
    /* Check if tag has attribute */
  rval = mhdf_find_attribute( tag_id, attrib_name, &idx, status );
  if (rval < 0)
    return 0;
  else if (0 == rval)
    return 1;
  
  if (NULL == data)
  {
    mhdf_setFail( status, "Invalid input." );
    return 0;
  }
  
  if (is_var_len) 
  {
    /* len = get_attrib_array_length_index(tag_id, index); */
    ilen = get_attrib_array_length_name(tag_id, attrib_name);
    if (ilen < 0) 
    {
      mhdf_setFail( status, "Failed to read length of default/mesh value for tag" );
      return 0;
    }
    len = ilen;
    
      /* caller passes type_id == 0 for OPAQUE */
    if (0 == type_id)
      read_type = H5Tcreate( H5T_OPAQUE, len );
    else {
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1  
      read_type = H5Tarray_create2( type_id, 1, &len );
#else
      read_type = H5Tarray_create( type_id, 1, &len, 0 );
#endif
    }
    if (read_type < 0)
    {
      mhdf_setFail( status, "Failed to read mesh/default value for tag" );
      return 0;
    }
  }
  
  rval = mhdf_read_scalar_attrib( tag_id, attrib_name, read_type, data, status );
  if (is_var_len)
    H5Tclose( read_type );
  
  return rval;
}

void
mhdf_getTagValues( mhdf_FileHandle file_handle,
                   const char* tag_name,
                   hid_t output_data_type,
                   void* default_value,
                   void* global_value,
                   mhdf_Status* status )
{
  hid_t tag_id;
  int rval, var_data;
  unsigned int idx;
  API_BEGIN;
  
    /* check args */
  if (NULL == tag_name || !*tag_name)
  {
    mhdf_setFail( status, "Invalid input." );
    return;
  }
  
    /* Get the tag group */
  tag_id = get_tag( file_handle, tag_name, NULL, status );
  if (tag_id < 0)
    return;
   
    /* Check for variable-length tag data */
  rval = mhdf_find_attribute( tag_id, TAG_VARLEN_ATTRIB, &idx, status );
  if (rval < 0)
  {
    H5Gclose( tag_id );
    return;
  }
  var_data = rval ? 1 : 0;
 
    /* Read default value if present */
  rval = read_tag_attrib_data( tag_id, TAG_DEFAULT_ATTRIB, output_data_type,
                               default_value, var_data, status );
  if (!rval) 
  {
    H5Gclose( tag_id );
    return;
  }
  
    /* Read mesh value if present */
  rval = read_tag_attrib_data( tag_id, TAG_GLOBAL_ATTRIB, output_data_type,
                               global_value, var_data, status );
  if (!rval) 
  {
    H5Gclose( tag_id );
    return;
  }
  
  H5Gclose( tag_id );
  mhdf_setOkay( status );
  API_END;
}

int
mhdf_haveDenseTag( mhdf_FileHandle file_handle,
                   const char* tag_name,
                   const char* type_handle,
                   mhdf_Status* status )
{
  char* path;
  hid_t elem_id, group_id;
  FileHandle* file_ptr;
  int rval = 0;
  API_BEGIN;
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status )) return -1;
  
  if (type_handle == mhdf_node_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, NODE_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, NODE_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open node group." );
  }
  else if (type_handle == mhdf_set_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, SET_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, SET_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open set group." );
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, type_handle, status );
  }
  if (elem_id < 0) return -1;
  
  rval = mhdf_is_in_group( elem_id, TAG_GROUP_NAME, status );
  if (rval < 0)
  {
    H5Gclose( elem_id );
    return -1;
  }
  else if (rval == 0)
  {
    H5Gclose( elem_id );
    mhdf_setOkay( status );
    return 0;
  }
  
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
  group_id = H5Gopen2( elem_id, DENSE_TAG_SUBGROUP, H5P_DEFAULT );
#else
  group_id = H5Gopen( elem_id, DENSE_TAG_SUBGROUP );
#endif
  H5Gclose( elem_id );
  if (group_id < 0)
  {
    mhdf_setFail( status, "Could not open tag subgroup." );
    return -1;
  }
  
  path = mhdf_name_to_path_copy( tag_name, status );
  if (NULL == path) { H5Gclose( group_id ); return -1; }
  
  rval = mhdf_is_in_group( group_id, path, status );
  H5Gclose( group_id );
  free( path );
  
  if (rval >= 0)
  {
    mhdf_setOkay( status );
  }
  
  API_END;
  return rval;
}

hid_t
mhdf_createDenseTagData( mhdf_FileHandle file_handle,
                         const char* tag_name,
                         const char* type_handle,
                         long num_values,
                         mhdf_Status* status )
{
  char* path;
  hid_t elem_id, data_id, type_id;
  FileHandle* file_ptr;
  size_t name_len, path_len, dir_len;
  hsize_t size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status )) return -1;
  
  if (type_handle == mhdf_node_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, NODE_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, NODE_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open node group." );
  }
  else if (type_handle == mhdf_set_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, SET_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, SET_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open set group." );
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, type_handle, status );
  }
  if (elem_id < 0) return -1;
  
  dir_len = strlen( DENSE_TAG_SUBGROUP );
  name_len = mhdf_name_to_path( tag_name, NULL, 0 );
  path_len = dir_len + name_len + 1;
  path = (char*)mhdf_malloc( path_len, status );
  if (NULL == path) 
    { H5Gclose( elem_id ); return -1; }
  strcpy( path, DENSE_TAG_SUBGROUP );
  mhdf_name_to_path( tag_name, path + dir_len, name_len + 1 );

  type_id = get_tag_type( file_ptr, path + dir_len, status );
  if (type_id < 0) 
    { H5Gclose( elem_id ); return -1; }
  
  size = (hsize_t)num_values;
  data_id = mhdf_create_table( elem_id, path, type_id, 1, &size, status );
  free( path );
  H5Gclose( elem_id );
  H5Tclose( type_id );
  
  if (data_id > 0)
    mhdf_setOkay( status );
  
  API_END_H( 1 );
  return data_id;
}

hid_t
mhdf_openDenseTagData(  mhdf_FileHandle file_handle,
                        const char* tag_name,
                        const char* type_handle,
                        long* num_values_out,
                        mhdf_Status* status )
{
  char* path;
  hid_t elem_id, data_id;
  FileHandle* file_ptr;
  size_t name_len, path_len, dir_len;
  hsize_t size;
  API_BEGIN;
  
  file_ptr = (FileHandle*)file_handle;
  if (!mhdf_check_valid_file( file_ptr, status )) return -1;
  
  if (type_handle == mhdf_node_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, NODE_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, NODE_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open node group." );
  }
  else if (type_handle == mhdf_set_type_handle())
  {
#if defined(H5Gopen_vers) && H5Gopen_vers > 1  
    elem_id = H5Gopen2( file_ptr->hdf_handle, SET_GROUP, H5P_DEFAULT );
#else
    elem_id = H5Gopen( file_ptr->hdf_handle, SET_GROUP );
#endif
    if (elem_id < 0)
      mhdf_setFail( status, "Could not open set group." );
  }
  else
  {
    elem_id = mhdf_elem_group_from_handle( file_ptr, type_handle, status );
  }
  if (elem_id < 0) return -1;
  
  dir_len = strlen( DENSE_TAG_SUBGROUP );
  name_len = mhdf_name_to_path( tag_name, NULL, 0 );
  path_len = dir_len + name_len + 1;
  path = (char*)mhdf_malloc( path_len, status );
  if (NULL == path) 
    { H5Gclose( elem_id ); return -1; }
  strcpy( path, DENSE_TAG_SUBGROUP );
  mhdf_name_to_path( tag_name, path + dir_len, name_len + 1 );
  
  data_id = mhdf_open_table( elem_id, path, 1, &size, status );
  free( path );
  H5Gclose( elem_id );
  *num_values_out = (long)size;
  
  if (data_id >= 0)
    mhdf_setOkay( status );
  
  API_END_H( 1 );
  return data_id;
}

void
mhdf_createSparseTagData( mhdf_FileHandle file_handle,
                          const char* tag_name,
                          long num_values,
                          hid_t handles_out[2],
                          mhdf_Status* status )
{
  hid_t tag_id, index_id, data_id, type_id, id_type;
  hsize_t count = (hsize_t)num_values;
  API_BEGIN;
  
  tag_id = get_tag( file_handle, tag_name, &id_type, status );
  if (tag_id < 0) return ;
  
#if defined(H5Topen_vers) && H5Topen_vers > 1  
  type_id = H5Topen2( tag_id, TAG_TYPE_NAME, H5P_DEFAULT );
#else
  type_id = H5Topen( tag_id, TAG_TYPE_NAME );
#endif
  if (type_id < 0)
  {
    H5Gclose( tag_id );
    mhdf_setFail( status, "Failed to get type object for tag \"%s\".", tag_name );
    return ;
  }
  
  index_id = mhdf_create_table( tag_id, SPARSE_ENTITY_NAME,
                                id_type, 1, &count,
                                status );
  if (index_id < 0) 
  { 
    H5Gclose( tag_id ); 
    H5Tclose( type_id );
    return ; 
  }
  
  data_id = mhdf_create_table( tag_id, SPARSE_VALUES_NAME,
                               type_id, 1, &count, status );
  H5Tclose( type_id );
  H5Gclose( tag_id ); 
  if (data_id < 0) 
  { 
    H5Dclose( index_id );
    return ; 
  }
  
  handles_out[0] = index_id;
  handles_out[1] = data_id;
  mhdf_setOkay( status );
  API_END_H(2);
}

void
mhdf_createVarLenTagData( mhdf_FileHandle file_handle,
                          const char* tag_name,
                          long num_entities,
                          long num_values,
                          hid_t handles_out[3],
                          mhdf_Status* status )
{
  hid_t tag_id, index_id, data_id, type_id, offset_id, id_type;
  hsize_t count = (hsize_t)num_entities;
  API_BEGIN;
  
  tag_id = get_tag( file_handle, tag_name, &id_type, status );
  if (tag_id < 0) return ;
  
#if defined(H5Topen_vers) && H5Topen_vers > 1  
  type_id = H5Topen2( tag_id, TAG_TYPE_NAME, H5P_DEFAULT );
#else
  type_id = H5Topen( tag_id, TAG_TYPE_NAME );
#endif
  if (type_id < 0)
  {
    H5Gclose( tag_id );
    mhdf_setFail( status, "Failed to get type object for tag \"%s\".", tag_name );
    return ;
  }
  
  index_id = mhdf_create_table( tag_id, SPARSE_ENTITY_NAME,
                                id_type, 1, &count,
                                status );
  if (index_id < 0) 
  { 
    H5Gclose( tag_id ); 
    H5Tclose( type_id );
    return ; 
  }
  
  offset_id = mhdf_create_table( tag_id, TAG_VAR_INDICES,
                                 MHDF_INDEX_TYPE, 1, &count,
                                 status );
  if (index_id < 0) 
  { 
    H5Dclose( offset_id );
    H5Gclose( tag_id ); 
    H5Tclose( type_id );
    return ; 
  }
  
  count = (hsize_t)num_values;
  data_id = mhdf_create_table( tag_id, SPARSE_VALUES_NAME,
                               type_id, 1, &count, status );
  H5Tclose( type_id );
  H5Gclose( tag_id ); 
  if (data_id < 0) 
  { 
    H5Dclose( offset_id );
    H5Dclose( index_id );
    return ; 
  }
  
  handles_out[0] = index_id;
  handles_out[1] = data_id;
  handles_out[2] = offset_id;
  mhdf_setOkay( status );
  API_END_H(3);
}


void
mhdf_openSparseTagData( mhdf_FileHandle file_handle,
                        const char* tag_name,
                        long* num_entity_out,
                        long* num_values_out,
                        hid_t handles_out[3],
                        mhdf_Status* status )
{
  hid_t tag_id, index_id, data_id, offset_id = -1;
  hsize_t num_ent, data_size, num_data;
  int rval;
  unsigned idx;
  API_BEGIN;
  
  tag_id = get_tag( file_handle, tag_name, NULL, status );
  if (tag_id < 0) return ;
 
  index_id = mhdf_open_table( tag_id, SPARSE_ENTITY_NAME, 1, &num_ent, status );
  if (index_id < 0) 
  { 
    H5Gclose( tag_id ); 
    return ; 
  }
  
  data_id = mhdf_open_table( tag_id, SPARSE_VALUES_NAME, 1, &data_size, status );
  if (data_id < 0) 
  { 
    H5Gclose( tag_id ); 
    H5Dclose( index_id );
    return ; 
  }
  
    /* check if tag is variable-lentgth */
  rval = mhdf_find_attribute( tag_id, TAG_VARLEN_ATTRIB, &idx, status );
  if (rval < 0) {
    H5Gclose( tag_id );
    H5Dclose( index_id );
    H5Dclose( data_id );
    return ;
  }
  
    /* If variable length... */
  if (rval) {
    offset_id = mhdf_open_table( tag_id, TAG_VAR_INDICES, 1, &num_data, status );
    if (offset_id < 0) {
      H5Gclose( tag_id );
      H5Dclose( index_id );
      H5Dclose( data_id );
      return ;
    }
  }
    /* Otherwise the number of values is the same as the size of the data table */
  else {
    num_data = data_size;
  }

  H5Gclose( tag_id ); 
  if (num_ent != num_data)
  {
    mhdf_setFail( status, "Data length mismatch for sparse tag data -- invalid file.");
    if (offset_id >= 0)
      H5Dclose( offset_id );
    H5Dclose( index_id );
    H5Dclose( data_id );
    return ;
  }
  *num_entity_out = (long)num_ent;
  if (num_values_out)
    *num_values_out = (long)data_size;
  
  handles_out[0] = index_id;
  handles_out[1] = data_id;
  if (offset_id >= 0)
    handles_out[2] = offset_id;
  mhdf_setOkay( status );
  API_END_H(2);
}

void
mhdf_writeSparseTagEntities( hid_t table_id,
                             long offset,
                             long count,
                             hid_t int_type,
                             const void* id_list,
                             mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, int_type, id_list, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeSparseTagEntitiesWithOpt( hid_t table_id,
                             long offset,
                             long count,
                             hid_t int_type,
                             const void* id_list,
                             hid_t io_prop,
                             mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, int_type, id_list, io_prop, status );
  API_END;
}
                        
void
mhdf_writeTagValues( hid_t table_id,
                     long offset,
                     long count,
                     hid_t tag_type,
                     const void* tag_data,
                     mhdf_Status* status )
{
  mhdf_writeTagValuesWithOpt( table_id, offset, count, tag_type, tag_data,
                              H5P_DEFAULT, status );
}

void
mhdf_writeTagValuesWithOpt( hid_t table_id,
                     long offset,
                     long count,
                     hid_t tag_type,
                     const void* tag_data,
                     hid_t io_prop,
                     mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, tag_type, tag_data, io_prop, status );
  API_END;
}

void
mhdf_writeSparseTagIndices( hid_t table_id,
                            long offset,
                            long count,
                            hid_t int_type,
                            const void* indices,
                            mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, int_type, indices, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_writeSparseTagIndicesWithOpt( hid_t table_id,
                            long offset,
                            long count,
                            hid_t int_type,
                            const void* indices,
                            hid_t io_prop,
                            mhdf_Status* status )
{
  API_BEGIN;
  mhdf_write_data( table_id, offset, count, int_type, indices, io_prop, status );
  API_END;
}

void
mhdf_readSparseTagEntities( hid_t table_id,
                            long offset,
                            long count,
                            hid_t int_type,
                            void* id_list,
                            mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, int_type, id_list, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSparseTagEntitiesWithOpt( hid_t table_id,
                            long offset,
                            long count,
                            hid_t int_type,
                            void* id_list,
                            hid_t io_prop,
                            mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, int_type, id_list, io_prop, status );
  API_END;
}
                        
void
mhdf_readTagValues( hid_t table_id,
                    long offset,
                    long count,
                    hid_t tag_type,
                    void* tag_data,
                    mhdf_Status* status )
{
  mhdf_readTagValuesWithOpt( table_id, offset, count, tag_type, tag_data,
                             H5P_DEFAULT, status );
}
void
mhdf_readTagValuesWithOpt( hid_t table_id,
                           long offset,
                           long count,
                           hid_t tag_type,
                           void* tag_data,
                           hid_t io_prop,
                           mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, tag_type, tag_data, io_prop, status );
  API_END;
}

void
mhdf_readSparseTagIndices( hid_t table_id,
                           long offset,
                           long count,
                           hid_t int_type,
                           void* indices,
                           mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, int_type, indices, H5P_DEFAULT, status );
  API_END;
}
void
mhdf_readSparseTagIndicesWithOpt( hid_t table_id,
                           long offset,
                           long count,
                           hid_t int_type,
                           void* indices,
                           hid_t io_prop,
                           mhdf_Status* status )
{
  API_BEGIN;
  mhdf_read_data( table_id, offset, count, int_type, indices, io_prop, status );
  API_END;
}
