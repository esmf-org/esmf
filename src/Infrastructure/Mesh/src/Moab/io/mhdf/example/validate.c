#include "mhdf.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <H5Tpublic.h>



/* Top-level validation functions */

/* check that group ID ranges are valid and non-overlapping */
static int check_valid_file_ids( struct mhdf_FileDesc* desc );

/* check that file doesn't contain holes (unwritten regions) */
static int check_file_contains_holes( const char* filename );

/* check IDs are valid for all element connectivity data */
static int check_valid_connectivity( mhdf_FileHandle file, struct mhdf_FileDesc* desc );

/* check that any adjacency lists contain valid IDs */
static int check_valid_adjacencies( mhdf_FileHandle file, struct mhdf_FileDesc* desc );

/* check that set data is consistent and that sets contain valid ids */
static int check_valid_sets( mhdf_FileHandle file, struct mhdf_FileDesc* desc );

/* check tag consistency and for handle tags verify that values are valid ids */
static int check_valid_tags( mhdf_FileHandle file, struct mhdf_FileDesc* desc );



/* Low-level helper routines */

/* Get string name from mhdf_EntDesc pointer */
static const char* desc_name( struct mhdf_FileDesc* desc, struct mhdf_EntDesc* grp );

/* Given a list of ID range pairs of [start_id,count] interleaved in 'ranges', 
   test if the passed file_id is contained in at least one passed range */
static int id_contained( long file_id, const long* ranges, int num_range );

/* Like id_contained, only returns logical and for all ids in a passed array */
static int ids_contained( const long* ids, int num_ids, const long* ranges, int num_ranges );

/* Like ids_contained, both input lists are in ranged format */
static int ranges_contained( const long* id_ranges, int num_id_ranges, const long* ranges, int num_ranges );

/* Search for a string in a null-terminated list of strings */
static int string_contained( const char* str, const char* const* const list );

/* Check if an array of longs contains duplicate values.  Will sort passed array */
static int contains_duplicates( long* array, long n );

/* Check if an array of [start_id,count] range pairs containes duplicate values/overlapping ranges.  
   Will sort passed array */
static int ranges_contain_duplicates( long* ranges, long nranges );

/* Get dynamically allocated array of [start_id,count] range pairs corresponding to all valid
   file IDs.  If include_null is non-zero, results will also include special ID of zero. */
static long* all_id_ranges( struct mhdf_FileDesc* desc, int include_null, int* num_ranges_out );

/* Get dynamically allocated array of [start_id,count] range pairs corresponding to all valid
   file IDs for entities with a specific dimension. */
static long* get_dim_ranges( struct mhdf_FileDesc* desc, int dim, int* num_ranges_out );

/* Merge adjacent ranges */
static int merge_ranges( long* ranges, int nranges );

/* Misc. high-level helper routines */

/* Check that end-index lists used in various file data for variable-length data
   (e.g. set contents, poly connectivity, variable-length tags) is valid.  If
   min_len is non-zero, it will be considered an error if the data for any entity
   contains less than that number of values.  It is considered an error if any
   index in the list is past the end of a dataset containing max_value entries. */
static int check_valid_end_indices( const long* indices, long num_idx, int min_len,
                                    long start_id, long max_value, 
                                    const char* typestr, const char* name );

/* Do check_valid_connectivity for an element group with constant connectivity length */
static int check_valid_elem_conn( int idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc, int conn_dim );

/* Do check_valid_connectivity for an element group with old-format variable-length connectivity */
static int check_valid_poly_conn( int idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc, int conn_dim );

/* Do check_valid_tags for a fixed-length tag */
static int check_valid_tag( int tag_idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc );

/* Do check_valid_tags for a variable-length tag */
static int check_valid_var_len_tag( int tag_idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc );

/* Do check_valid_adjacencies for the adjacency list of a single entity group */
static int check_valid_adj_list( long start_id, long count, const long* data, long data_len, 
                                 const long* valid_ranges, long num_valid_ranges, const char* name );

/* Do subset of check_valid_sets for either set parent or set child data */
static int check_valid_parents_children( long start_id, 
                                         long count,
                                         hid_t meta_handle,
                                         hid_t data_handle,
                                         long data_len,
                                         int parents );

/* Do subset of check_valid_sets for set contents */
static int check_valid_set_contents( struct mhdf_FileDesc* desc,
                                     long start_id, 
                                     long count,
                                     hid_t meta_handle,
                                     hid_t data_handle,
                                     long data_len );


/* Comparison routines for use with qsort */

/* Compare two long int values */
static int lcomp( const void* p1, const void* p2 )
{
  long l1 = *(const long*)p1;
  long l2 = *(const long*)p2;
  return l1 < l2 ? -1 : l1 > l2 ? 1 : 0;
}

/* Compare start_ids of mhdf_EntDesc pointed to by passed values */
static int dcomp( const void* p1, const void* p2 )
{
  const struct mhdf_EntDesc* d1 = *(const struct mhdf_EntDesc**)(p1);
  const struct mhdf_EntDesc* d2 = *(const struct mhdf_EntDesc**)(p2);
  return lcomp( &d1->start_id, &d2->start_id );
}



int VERBOSE = 0;

int main( int argc, char* argv[] )
{
  int result = 0;
  mhdf_FileHandle file;
  mhdf_Status status;
  unsigned long max_id;
  struct mhdf_FileDesc* desc;
  
  if (argc != 2) {
    fprintf( stderr,"Usage: %s <filename>\n", argv[0] );
    return 1;
  }
  
  file = mhdf_openFile( argv[1], 0, &max_id, -1, &status );
  if (mhdf_isError( &status )) {
    fprintf( stderr,"%s: %s\n", argv[1], mhdf_message( &status ) );
    return 1;
  }
  
  desc = mhdf_getFileSummary( file, H5T_NATIVE_LONG, &status, 0 ); /*no extra set info*/
  if (mhdf_isError( &status )) {
    fprintf( stderr,"%s: %s\n", argv[1], mhdf_message( &status ) );
    return 1;
  }
  
  if (desc->nodes.count < 1) 
    puts("WARNING: file contains no vertices.");
  if (desc->num_elem_desc < 1) 
    puts("WARNING: file contains no elements.");
  
  result += check_valid_file_ids( desc );
  result += check_file_contains_holes( argv[1] );
  result += check_valid_connectivity( file, desc );
  result += check_valid_adjacencies( file, desc );
  result += check_valid_sets( file, desc );
  result += check_valid_tags( file, desc );

  free( desc );
  mhdf_closeFile( file, &status );
  return result;
}


static const char* desc_name( struct mhdf_FileDesc* desc, struct mhdf_EntDesc* grp )
{
  struct mhdf_ElemDesc junk, *elem;
  const size_t diff = (char*)&junk.desc - (char*)&junk;
  static const char nodes[] = "Vertices";
  static const char sets[] = "Sets";
  if (grp == &desc->nodes)
    return nodes;
  if (grp == &desc->sets)
    return sets;
  
  elem = (struct mhdf_ElemDesc*)((char*)grp - diff);
  return elem->handle;
}

int check_valid_file_ids( struct mhdf_FileDesc* desc )
{
  const int ngrp = 2 + desc->num_elem_desc;
  int i, err = 0;
  struct mhdf_EntDesc** sorted = malloc(ngrp*sizeof(struct mhdf_EntDesc*));
  for (i = 0; i < desc->num_elem_desc; ++i) 
    sorted[i] = &desc->elems[i].desc;
  sorted[i++] = &desc->nodes;
  sorted[i] = &desc->sets;
  qsort( sorted, ngrp, sizeof(struct mhdf_EntDesc*), &dcomp );
  for (i = 0; i < ngrp; ++i) {
    if (sorted[i]->count < 0) {
      printf("Group \"%s\" has negative count!\n", desc_name( desc, sorted[i] ));
      ++err;
    }
    if (sorted[i]->count && sorted[i]->start_id == 0) {
      printf("Group \"%s\" contains NULL ID!\n", desc_name( desc, sorted[i] ));
      ++err;
    }
    
  
    if (i > 0 && sorted[i-1]->start_id + sorted[i-1]->count > sorted[i]->start_id) {
      printf("Conflicting group IDs for \"%s\" [%ld,%ld] and \"%s\" [%ld,%ld]\n",
        desc_name(desc,sorted[i-1]), sorted[i-1]->start_id, sorted[i-1]->start_id + sorted[i-1]->count - 1,
        desc_name(desc,sorted[i  ]), sorted[i  ]->start_id, sorted[i  ]->start_id + sorted[i  ]->count - 1 );
      ++err;
    }
  }
  free(sorted);
  return err;
}

int check_file_contains_holes( const char* filename )
{
#ifndef _MSC_VER
  const int blocksize = 512;
#endif
  int errorcode;
  struct stat buf;
  
  errorcode = stat( filename, &buf );
  if (errorcode) {
    perror(filename);
    return 1;
  }

#ifndef _MSC_VER  /*Does not have st_blocks*/
  if (buf.st_size/blocksize > buf.st_blocks+1) {
    printf("File \"%s\" contains holes.  This indicates that portions of the file were never written.\n",
      filename);
    return 1;
  }
#endif

  return 0;
}


int check_valid_connectivity( mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  int idx, dim, result = 0;
  
  for (idx = 0; idx < desc->num_elem_desc; ++idx) {
    if (!strcmp(desc->elems[idx].type,mhdf_POLYHEDRON_TYPE_NAME))
      dim = 2;
    else
      dim = 0;
    
    if (desc->elems[idx].desc.vals_per_ent == -1)
      result += check_valid_poly_conn( idx, file, desc, dim );
    else
      result += check_valid_elem_conn( idx, file, desc, dim );
  }
  
  return result;
}

static int id_contained( long file_id, const long* ranges, int num_range )
{
  const long* end = ranges + 2*num_range;
  for (; ranges != end; ranges+=2) {
    if (file_id >= ranges[0] && (file_id - ranges[0]) < ranges[1])
      return 1;
  }
  return 0;
}

static int ids_contained( const long* ids, int num_ids, const long* ranges, int num_ranges )
{
  int i;
  for (i = 0; i < num_ids; ++i)
    if (!id_contained( ids[i], ranges, num_ranges ))
      return 0;
  return 1;
}

static int ranges_contained( const long* id_ranges, int num_id_ranges, const long* ranges, int num_ranges )
{
  int i;
  long start, count, avail;
  const long* end = ranges + 2*num_ranges;
  const long* iter;
  for (i = 0; i < num_id_ranges; ++i) {
    start = id_ranges[2*i];
    count = id_ranges[2*i+1];
    while (count > 0) {
      for (iter = ranges; iter != end; iter+=2) {
        if (start >= iter[0] && (start - iter[0]) < iter[1])
          break;
      }
      if (iter == end)
        return 0;
      avail = iter[1] - (start - iter[0]);
      count -= avail;
      start += avail;
    }
  }
  return 1;
}

static int string_contained( const char* str, const char* const* list )
{
  for (; *list; ++list) 
    if (!strcmp(str,*list))
      return 1;
  return 0;
}

static long* get_dim_ranges( struct mhdf_FileDesc* desc, int dim, int* num_ranges_out )
{
  long* ranges = 0;
  int i, j;
  const char* const types1D[] = { mhdf_EDGE_TYPE_NAME, 0 };
  const char* const types2D[] = { mhdf_TRI_TYPE_NAME, mhdf_QUAD_TYPE_NAME, mhdf_POLYGON_TYPE_NAME, 0 };
  const char* const types3D[] = { mhdf_TET_TYPE_NAME, mhdf_PYRAMID_TYPE_NAME, mhdf_PRISM_TYPE_NAME,
                                  mdhf_KNIFE_TYPE_NAME, mdhf_HEX_TYPE_NAME, mhdf_POLYHEDRON_TYPE_NAME,
                                  mhdf_SEPTAHEDRON_TYPE_NAME, 0 };
  
  char const* const* typelist;
  switch (dim) {
    case 0:
      *num_ranges_out = 1;
      ranges = malloc(2*sizeof(long));
      ranges[0] = desc->nodes.start_id;
      ranges[1] = desc->nodes.count;
      return ranges;
    case 1:
      typelist = types1D;
      break;
    case 2:
      typelist = types2D;
      break;
    case 3:
      typelist = types3D;
      break;
    default:
      fprintf(stderr,"Internal error at %s:%d: request for entities of dimesion %d\n",
        __FILE__, __LINE__, dim );
      abort();
  }

  *num_ranges_out = 0;
  for (i = 0; i < desc->num_elem_desc; ++i) 
    if (string_contained( desc->elems[i].type, typelist ))
      ++*num_ranges_out;
  ranges = malloc(*num_ranges_out * 2 * sizeof(long));
  for (i = 0, j = 0; i < desc->num_elem_desc; ++i) 
    if (string_contained( desc->elems[i].type, typelist )) {
      ranges[j++]   = desc->elems[i].desc.start_id;
      ranges[j++] = desc->elems[i].desc.count;
    }
    
  *num_ranges_out = merge_ranges( ranges, *num_ranges_out );
  return ranges;  
}

int check_valid_elem_conn( int idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc, int conn_dim )
{
  long *ranges, *buffer, *iter;
  int num_ranges;
  long i, invalid = 0;
  hid_t handle;
  mhdf_Status status;
  const long count = desc->elems[idx].desc.count;
  const long len = desc->elems[idx].desc.vals_per_ent;

  ranges = get_dim_ranges( desc, conn_dim, &num_ranges );
  handle = mhdf_openConnectivitySimple( file, desc->elems[idx].handle, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error opening connectivity for %s: %s\n", 
            desc->elems[idx].handle, mhdf_message(&status));
    free(ranges);
    return 1;
  }
  
  buffer = malloc( sizeof(long) * count * len );
  mhdf_readConnectivity( handle, 0, count, H5T_NATIVE_LONG, buffer, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading connectivity for %s: %s\n", 
            desc->elems[idx].handle, mhdf_message(&status));
    free(ranges);
    free(buffer);
    mhdf_closeData( file, handle, &status );
    return 1;
  }
  mhdf_closeData( file, handle, &status );
  
  iter = buffer;
  for (i = 0; i < count; ++i, iter += len) {
    if (!ids_contained( iter, len, ranges, num_ranges)) {
      if (VERBOSE)
        printf("Invalid connectivity for element %ld (ID %ld) in %s\n",
          i, i + desc->elems[idx].desc.start_id, desc->elems[idx].handle );
      ++invalid;
    }
  }
  free( buffer );
  free( ranges );

  if (invalid) {
    printf("%ld elements with invalid connectivity in %s\n", invalid, desc->elems[idx].handle );
    return 1;
  }
  return 0;
}

static int check_valid_end_indices( const long* indices, long num_idx, int min_len,
                                    long start_id, long max_value, const char* typestr, const char* name )
{
  long i, invalid = 0, prev = -1;
  
  if (num_idx == 0) {
    printf("WARNING: Empty index list for %s %s\n", name, typestr );
    return 0;
  }
  
  for (i = 0; i < num_idx; ++i) {
    if (indices[i] < prev) {
      if (VERBOSE) {
        if (start_id > 0)
          printf("Invalid end index %ld for %s %ld (ID %ld).  Prev index is %ld\n", indices[i], name, i, i+start_id, prev );
        else
          printf("Invalid end index %ld for entry %ld in %s %s.  Prev index is %ld\n", indices[i], i, name, typestr, prev );
      }
      ++invalid;
    }
    else if (indices[i] - prev < min_len) {
      if (VERBOSE) {
        if (start_id > 0)
          printf("%s %ld (ID %ld) has only %ld values\n", name, i, start_id, indices[i] - prev );
        else
          printf("Entry %ld in %s %s has only %ld values\n", i, name, typestr, indices[i] - prev );
      }
      ++invalid;
    }
    else if (indices[i] >= max_value) {
      if (VERBOSE) {
        if (start_id > 0)
          printf("%s %ld (ID %ld) end index exceeds upper bound of %ld\n", name, i, start_id, max_value );
        else
          printf("Entry %ld in %s %s end index exceeds uppper bound of %ld\n", i, name, typestr, max_value );
      }
      ++invalid;
    }
    prev = indices[i];
  }
  
  if (invalid) {
    printf("%ld invalid end indices for %s %s\n", invalid, typestr, name );
  }
  
  return invalid;
}

int check_valid_poly_conn( int idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc, int conn_dim )
{
  long *ranges, *buffer, *indices, *iter;
  int num_ranges;
  long i, invalid, num_poly, num_conn, first_id, prev, len;
  hid_t handles[2];
  mhdf_Status status;
  int min_conn_len;
  if (!strcmp( mhdf_POLYHEDRON_TYPE_NAME, desc->elems[idx].type ))
    min_conn_len = 4;
  else
    min_conn_len = 3;

  mhdf_openPolyConnectivity( file, desc->elems[idx].handle, &num_poly, &num_conn, &first_id, handles, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error opening connectivity for %s: %s\n", 
            desc->elems[idx].handle, mhdf_message(&status));
    return 1;
  }
  
  indices = malloc( sizeof(long) * num_poly );
  mhdf_readPolyConnIndices( handles[0], 0, num_poly, H5T_NATIVE_LONG, indices, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading poly indices for %s: %s\n", 
            desc->elems[idx].handle, mhdf_message(&status));
    free(indices);
    mhdf_closeData( file, handles[0], &status );
    mhdf_closeData( file, handles[1], &status );
    return 1;
  }
  mhdf_closeData( file, handles[0], &status );
  
  invalid = check_valid_end_indices( indices, num_poly, min_conn_len, first_id, num_conn, "Connectivity", desc->elems[idx].handle );
  if (invalid) {
    free( indices );
    mhdf_closeData( file, handles[1], &status );
    return 1;
  }
  
  ranges = get_dim_ranges( desc, conn_dim, &num_ranges );
  
  buffer = malloc( sizeof(long) * num_conn );
  mhdf_readPolyConnIDs( handles[1], 0, num_conn, H5T_NATIVE_LONG, buffer, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading connectivity for %s: %s\n", 
            desc->elems[idx].handle, mhdf_message(&status));
    free(ranges);
    free(indices);
    free(buffer);
    mhdf_closeData( file, handles[1], &status );
    return 1;
  }
  mhdf_closeData( file, handles[1], &status );
  
  prev = -1;
  iter = buffer;
  for (i = 0; i < num_poly; ++i) {
    len = indices[i] - prev;
    prev = indices[i];
    if (!ids_contained( iter, len, ranges, num_ranges)) {
      if (VERBOSE)
        printf("Invalid connectivity for element %ld (ID %ld) in %s\n",
          i, i + first_id, desc->elems[idx].handle );
      ++invalid;
    }
    iter += len;
  }
  free( indices );
  free( buffer );
  free( ranges );

  if (invalid) {
    printf("%ld elements with invalid connectivity in %s\n", invalid, desc->elems[idx].handle );
    return 1;
  }
  return 0;
}

int check_valid_adjacencies( mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  const int num_ranges = desc->num_elem_desc;
  long *ranges, *buffer;
  int i;
  int invalid = 0;
  long count;
  hid_t handle;
  mhdf_Status status;

  /* Build list of ID ranges for all elements.  Consider any element ID to be a valid
   * thing to be adjacent to.  So we disallow adjacency to nodes, sets, or undefined IDs */
  ranges = malloc( sizeof(long) * 2 * num_ranges );
  for (i = 0; i < num_ranges; ++i) {
    ranges[2*i  ] = desc->elems[i].desc.start_id;
    ranges[2*i+1] = desc->elems[i].desc.count;
  }
  
  for (i = 0; i < num_ranges; ++i) {
    if (!desc->elems[i].have_adj)
      continue;
    
    handle = mhdf_openAdjacency( file, desc->elems[i].handle, &count, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error openening adjacency list for %s: %s\n", 
              desc->elems[i].handle, mhdf_message(&status));
      free(ranges);
      return 1;
    }
    
    buffer = malloc( sizeof(long) * count );
    mhdf_readAdjacency( handle, 0, count, H5T_NATIVE_LONG, buffer, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading adjacency list for %s: %s\n", 
              desc->elems[i].handle, mhdf_message(&status));
      free(ranges);
      mhdf_closeData( file, handle, &status );
      return 1;
    }
    mhdf_closeData( file, handle, &status );
    
    invalid += check_valid_adj_list( desc->elems[i].desc.start_id,
                                     desc->elems[i].desc.count,
                                     buffer, count,
                                     ranges, num_ranges,
                                     desc->elems[i].handle );
    free(buffer);
  } 
  
  free(ranges);
  return invalid;
}

static int check_valid_adj_list( long start_id, long count, const long* data, long data_len, 
                                 const long* valid_ranges, long num_valid_ranges, const char* name )
{
  long i, n, id, invalid_id = 0, invalid_vals = 0;
  const long* iter = data;
  const long* const end = data + data_len;
  int result = 0;
  
  for (i = 0; iter != end; ++i) {
    id = *iter; ++iter;
    if (iter == end) {
      printf("Entry %ld in %s adjacency data (ID %ld) is truncated by the end of the adjacency list.\n",
          i, name, id);
      result = 1;
      break;
    }
    
    if (id < start_id || (id - start_id) >= count) {
      if (VERBOSE)
        printf("Entry %ld in %s adjacency data has ID %ld outside of group range [%ld,%ld].\n",
          i, name, id, start_id, start_id+count-1);
      ++invalid_id;
    }
    
    n = *iter; ++iter;
    if (n < 1) {
      printf("Entry %ld in %s adjacency data (ID %ld) has %ld adjacency entries.\n",
          i, name, id, n);
      result = 1;
      if (n < 0)
        break; /* data is corrupt.  don't proceed further */
    }
    
    if (iter + n > end) {
      printf("Entry %ld in %s adjacency data (ID %ld) is truncated by the end of the adjacency list.\n",
          i, name, id);
      result = 1;
      break;
    }
      
    if (!ids_contained( iter, n, valid_ranges, num_valid_ranges)) {
      if (VERBOSE)
        printf("Entry %ld in %s adjacency data (ID %ld) has invalid IDs in its adjacency list.\n",
          i, name, id);
    
      ++invalid_vals;
    }
    iter += n;
  }

  if (invalid_id) {
    printf("%ld entries in %s adjacency data were for entities not in %s\n", invalid_id, name, name );
    result = 1;
  }
  if (invalid_vals) {
    printf("%ld entries in %s adjacency data contained invalid adjacent entity ids\n", invalid_id, name );
    result = 1;
  }
  return result;
}


int check_valid_sets( mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  long count, start_id, data_len;
  mhdf_Status status;
  hid_t meta, handle;
  int result = 0;
  
  if (desc->sets.count == 0)
    return 0;
  
  meta = mhdf_openSetMeta( file, &count, &start_id, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error opening set description table: %s\n", mhdf_message(&status));
    return 1;
  }
  
  if (desc->have_set_contents) {
    handle = mhdf_openSetData( file, &data_len, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error opening set contents table: %s\n", mhdf_message(&status));
    }
    else {
      result += check_valid_set_contents( desc, start_id, count, meta, handle, data_len );
      mhdf_closeData( file, handle, &status );
    }
  }
  
  if (desc->have_set_children) {
    handle = mhdf_openSetChildren( file, &data_len, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error opening set child table: %s\n", mhdf_message(&status));
      mhdf_closeData( file, meta, &status );
    }
    else {
      result += check_valid_parents_children( start_id, count, meta, handle, data_len, 0 );
      mhdf_closeData( file, handle, &status );
    }
  }
  
  if (desc->have_set_parents) {
    handle = mhdf_openSetParents( file, &data_len, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error opening set child table: %s\n", mhdf_message(&status));
      mhdf_closeData( file, meta, &status );
    }
    else {
      result += check_valid_parents_children( start_id, count, meta, handle, data_len, 1 );
      mhdf_closeData( file, handle, &status );
    }
  }
   
  mhdf_closeData( file, meta, &status );
  return result;
}
  
static int contains_duplicates( long* array, long n )
{
  long i;
  qsort( array, n, sizeof(long), &lcomp );
  for (i = 1; i < n;  ++i)
    if (array[i-1] == array[i])
      return 1;
  return 0;
}
  
static int ranges_contain_duplicates( long* ranges, long nranges )
{
  long i;
  qsort( ranges, nranges, 2*sizeof(long), &lcomp ); /* sort by first value in each pair */
  for (i = 1; i < nranges;  ++i)
    if (ranges[2*i-2] + ranges[2*i-1] > ranges[2*i])
      return 1;
  return 0;
}


static int check_valid_parents_children( long start_id, 
                                         long count,
                                         hid_t meta_handle,
                                         hid_t data_handle,
                                         long data_len,
                                         int parents )
{
  mhdf_Status status;
  long *indices, *contents;
  const char parstr[] = "Parent";
  const char cldstr[] = "Child";
  const char* name = parents ? parstr : cldstr;
  long i, prev, start, n;
  long invalid = 0, invalid_dup = 0;
  long range[2];
  int result = 0;
  range[0] = start_id;
  range[1] = count;
  
  indices = malloc( sizeof(long) * count );
  if (parents) 
    mhdf_readSetParentEndIndices( meta_handle, 0, count, H5T_NATIVE_LONG, indices, &status );
  else
    mhdf_readSetChildEndIndices( meta_handle, 0, count, H5T_NATIVE_LONG, indices, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading set %s end indices: %s\n", name, mhdf_message(&status));
    free(indices);
    return 1;
  }
    
  if (check_valid_end_indices( indices, count, 0, start_id, data_len, "Set", name )) {
    free(indices);
    return 1;
  }
  
  contents = malloc( sizeof(long) * data_len );
  mhdf_readSetParentsChildren( data_handle, 0, data_len, H5T_NATIVE_LONG, contents, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading set %s IDs: %s\n", name, mhdf_message(&status));
    free(indices);
    free(contents);
    return 1;
  }
  
  prev = -1;
  for (i = 0; i < count; ++i) {
    start = prev + 1;
    n = indices[i] - prev;
    prev = indices[i];
    
    if (!ids_contained( contents+start, n, range, 1)) {
      if (VERBOSE)
        printf("Set %ld (ID %ld) has invalid %s IDs.\n", i, start_id+i, name );
      ++invalid;
    }
    else if (contains_duplicates( contents+start, n )) {
      if (VERBOSE)
        printf("Set %ld (ID %ld) %s list contains duplicate IDs.\n", i, start_id+i, name );
      ++invalid_dup;
    }
  }
  
  free(indices);
  free(contents);
  
  if (invalid) {
    printf( "%ld sets had invalid %s lists.\n", invalid, name );
    result = 1;
  }
  
  if (invalid_dup) {
    printf( "%ld sets had duplicate handles in %s lists.\n", invalid_dup, name );
    result = 1;
  }
  
  return result;
}

static int merge_ranges( long* ranges, int nranges )
{
  long i, n;

  if (nranges < 1)
    return 0;

    /* merge adjacent */
  qsort( ranges, nranges, 2*sizeof(long), &lcomp );
  n = 1;
  for (i = 1; i < nranges; ++i) {
    if (ranges[2*n-2] + ranges[2*n-1] == ranges[2*i]) {
      ranges[2*n-1] += ranges[2*i+1]; /*compact the range*/
    }
    else {
      /* do not compact, just copy, and increase number of ranges*/
      ranges[2*n  ] = ranges[2*i];
      ranges[2*n+1] = ranges[2*i+1];
      ++n;
    }
  }
  
  return n;
}

static long* all_id_ranges( struct mhdf_FileDesc* desc, int include_null, int* num_ranges_out )
{
  int i, num_ranges = 0;
  struct mhdf_EntDesc* group;
  long* ranges = malloc(2*sizeof(long)*(desc->num_elem_desc + 2 + !!include_null));
  if (include_null) {
    num_ranges = 1;
    ranges[0] = 0;
    ranges[1] = 1;
  }
  for (i = -1; i <= desc->num_elem_desc; ++i) {
    if (i == -1) 
      group = &desc->nodes;
    else if (i == desc->num_elem_desc)
      group = &desc->sets;
    else
      group = &desc->elems[i].desc;

    if (num_ranges && ranges[2*num_ranges - 2] + ranges[2*num_ranges-1] == group->start_id) {
      ranges[2*num_ranges-1] += group->count;
    }
    else {
      ranges[2*num_ranges] = group->start_id;
      ranges[2*num_ranges+1] = group->count;
      ++num_ranges;
    }
  }

  
  *num_ranges_out = merge_ranges( ranges, num_ranges );
  return ranges;
}

static int check_valid_set_contents( struct mhdf_FileDesc* desc,
                                     long start_id, 
                                     long count,
                                     hid_t meta_handle,
                                     hid_t data_handle,
                                     long data_len )
{
  mhdf_Status status;
  long *indices, *contents;
  short* flags;
  long i, prev, start, n;
  long invalid_len = 0, invalid_handle = 0, invalid_dup = 0;
  long *ranges;
  int num_ranges;
  int tmpresult;
  
  indices = malloc( sizeof(long) * count );
  mhdf_readSetContentEndIndices( meta_handle, 0, count, H5T_NATIVE_LONG, indices, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading set content end indices: %s\n", mhdf_message(&status));
    free(indices);
    return 1;
  }
    
  if (check_valid_end_indices( indices, count, 0, start_id, data_len, "Set", "Content" )) {
    free(indices);
    return 1;
  }
  
  ranges = all_id_ranges( desc, 0, &num_ranges );
  
  flags = malloc( sizeof(short) * count );
  mhdf_readSetFlags( meta_handle, 0, count, H5T_NATIVE_SHORT, flags, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading set flags: %s\n", mhdf_message(&status));
    free(indices);
    free(flags);
    free(ranges);
    return 1;
  }
  
  contents = malloc( sizeof(long) * data_len );
  mhdf_readSetData( data_handle, 0, data_len, H5T_NATIVE_LONG, contents, &status );
  if (mhdf_isError(&status)) {
    fprintf(stderr,"Internal error reading set content IDs: %s\n", mhdf_message(&status));
    free(indices);
    free(contents);
    free(flags);
    free(ranges);
    return 1;
  }
  
  prev = -1;
  for (i = 0; i < count; ++i) {
    start = prev + 1;
    n = indices[i] - prev;
    prev = indices[i];
    
    if (flags[i] & mhdf_SET_RANGE_BIT) {
      if (n%2) {
        if (VERBOSE)
          printf("Set %ld (ID %ld) has is marked as range-compressed but has odd number of content values.\n", i, start_id+i );
        ++invalid_len;
        continue;
      }
      tmpresult = ranges_contained( contents+start, n/2, ranges, num_ranges );
    }
    else {
      tmpresult = ids_contained( contents+start, n, ranges, num_ranges);
    }
    if (!tmpresult) {
      if (VERBOSE)
        printf("Set %ld (ID %ld) has invalid content IDs.\n", i, start_id+i );
      ++invalid_handle;
      continue;
    }
    
    if (flags[i] & mhdf_SET_ORDER_BIT)
      continue;
    
    if (flags[i] & mhdf_SET_RANGE_BIT) 
      tmpresult = ranges_contain_duplicates( contents+start, n/2 );
    else
      tmpresult = contains_duplicates( contents+start, n );
    if(tmpresult) {
      if (VERBOSE)
        printf("Set %ld (ID %ld) is not ordered but contains duplicate handles.\n", i, start_id+i );
      ++invalid_dup;
    }
  }
  
  free(indices);
  free(contents);
  free(flags);
  free(ranges);
  
  tmpresult = 0;
  if (invalid_len) {
    printf( "%ld ranged sets had invalid (odd) content list lengths.\n", invalid_len );
    tmpresult = 1;
  }
  if (invalid_handle) {
    printf( "%ld sets had invalid IDs in their content lists.\n", invalid_handle );
    tmpresult = 1;
  }
  if (invalid_dup) {
    printf( "%ld unordered sets had duplicate IDs in their content lists.\n", invalid_dup );
    tmpresult = 1;
  }
  
  return tmpresult;
}

int check_valid_tags( mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  int i, result = 0;
  for (i = 0; i < desc->num_tag_desc; ++i) {
    if (desc->tags[i].size < 0)
      result += check_valid_var_len_tag( i, file, desc );
    else
      result += check_valid_tag( i, file, desc );
  }
  return result;
}

static int check_valid_tag( int tag_idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  long *ids = 0, count, junk;
  long *ranges;
  int nranges;
  hid_t handles[3];
  mhdf_Status status;
  const struct mhdf_TagDesc* tag = &(desc->tags[tag_idx]);
  int i, result = 0;
  long srange[2];
  const char* name;
  struct mhdf_EntDesc* group;
  hid_t h5type;
  hsize_t size;
  
  
  if (tag->have_sparse) {
    mhdf_openSparseTagData( file, tag->name, &count, &junk, handles, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error opening sparse data for tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      return 1;
    }
    
    ids = malloc( sizeof(long) * count );
    mhdf_readSparseTagEntities( handles[0], 0, count, H5T_NATIVE_LONG, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading sparse entities for tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      mhdf_closeData( file, handles[0], &status );
      mhdf_closeData( file, handles[1], &status );
      free(ids);
      return 1;
    }
    
    mhdf_closeData( file, handles[0], &status );
    ranges = all_id_ranges( desc, 0, &nranges );
    if (!ids_contained( ids, count, ranges, nranges )) {
      ++result;
      printf("Sparse data for tag \"%s\" has values for invalid IDs\n", tag->name );
    }
    else if (contains_duplicates( ids, count )) {
      ++result;
      printf("Sparse data for tag \"%s\" has duplicate values for one or more entities\n", tag->name );
    }
    free(ranges);
    
    for (i = 0; i < tag->num_dense_indices; ++i) {
      if (tag->dense_elem_indices[i] == -2) {
        name = mhdf_set_type_handle();
        group = &desc->sets;
      }
      else if (tag->dense_elem_indices[i] == -1) {
        name = mhdf_node_type_handle();
        group = &desc->nodes;
      }
      else {
        name = desc->elems[ tag->dense_elem_indices[i] ].handle;
        group = &desc->elems[ tag->dense_elem_indices[i] ].desc;
      }
      
      srange[0] = group->start_id;
      srange[1] = group->count;
      if (ids_contained( ids, count, srange, 2 )) {
        ++result;
        printf("Tag \"%s\" has both sparse values and dense values for one or more entities in \"%s\"\n", tag->name, name );
      }
    }
    
    free(ids);
  }
  
  
  if (tag->type != mhdf_ENTITY_ID) {
    if (tag->have_sparse) 
      mhdf_closeData( file, handles[1], &status );
    return result;
  }
  
  ranges = all_id_ranges( desc, 1, &nranges );
  if (tag->default_value && !ids_contained( tag->default_value, tag->size, ranges, nranges )) {
    ++result;
    printf("Handle tag \"%s\" has invalid ID(s) in its default value.\n", tag->name );
  }
  if (tag->global_value && !ids_contained( tag->global_value, tag->size, ranges, nranges )) {
    ++result;
    printf("Handle tag \"%s\" has invalid ID(s) in its global/mesh value.\n", tag->name );
  }
  
  h5type = H5T_NATIVE_LONG;
  if (tag->size > 1) {
    size = tag->size;
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1  
    h5type = H5Tarray_create( H5T_NATIVE_LONG, 1, &size );
#else
    h5type = H5Tarray_create( H5T_NATIVE_LONG, 1, &size, NULL );
#endif
  }
  
  if (tag->have_sparse) {
    ids = malloc( tag->size * count * sizeof(long) );
    mhdf_readTagValues( handles[1], 0, count, h5type, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading sparse values for handle tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      mhdf_closeData( file, handles[1], &status );
      free(ids);
      free(ranges);
      if (tag->size > 1)
        H5Tclose( h5type );
      return 1;
    }
    mhdf_closeData( file, handles[1], &status );
    
    if (!ids_contained( ids, tag->size * count, ranges, nranges )) {
      ++result;
      printf("Sparse data for one or more entities with handle tag \"%s\" has invalid ID(s).\n", tag->name );
    }
    free(ids);
  }
  
  for (i = 0; i < tag->num_dense_indices; ++i) {
    if (tag->dense_elem_indices[i] == -2) {
      name = mhdf_set_type_handle();
      /*group = &desc->sets;*/
    }
    else if (tag->dense_elem_indices[i] == -1) {
      name = mhdf_node_type_handle();
      /*group = &desc->nodes;*/
    }
    else {
      name = desc->elems[ tag->dense_elem_indices[i] ].handle;
      /*group = &desc->elems[ tag->dense_elem_indices[i] ].desc;*/
    }
      
    handles[0] = mhdf_openDenseTagData( file, tag->name, name, &count, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal dense values for handle tag \"%s\" on \"%s\": %s\n", tag->name, name, mhdf_message(&status));
      ++result;
      continue;
    }
    
    ids = malloc( tag->size * count * sizeof(long) );
    mhdf_readTagValues( handles[0], 0, count, h5type, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading dense values for handle tag \"%s\" on \"%s\": %s\n", tag->name, name, mhdf_message(&status));
      mhdf_closeData( file, handles[0], &status );
      free(ids);
      ++result;
      continue;
    }
    mhdf_closeData( file, handles[1], &status );
    
    if (!ids_contained( ids, count, ranges, nranges )) {
      ++result;
      printf("Dense data on \"%s\" for handle tag \"%s\" has invalid ID(s) for one or more entities.\n", name, tag->name );
    }
    free(ids);
  }

  if (tag->size > 1)
    H5Tclose( h5type );

  return result;
}

static int check_valid_var_len_tag( int tag_idx, mhdf_FileHandle file, struct mhdf_FileDesc* desc )
{
  long *ids = 0, count, num_val;
  long *ranges;
  int nranges;
  hid_t handles[3];
  mhdf_Status status;
  const struct mhdf_TagDesc* tag = &(desc->tags[tag_idx]);
  int result = 0;
 
  if (tag->num_dense_indices != 0) {
    printf("Dense data for tag \"%s\" not allowed for variable-length tags\n", tag->name);
    ++result;
  }
  
  if (tag->have_sparse) {
    mhdf_openSparseTagData( file, tag->name, &count, &num_val, handles, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error opening sparse data for tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      return 1;
    }
    
    ids = malloc( sizeof(long) * count );
    mhdf_readSparseTagEntities( handles[0], 0, count, H5T_NATIVE_LONG, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading sparse entities for tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      mhdf_closeData( file, handles[0], &status );
      mhdf_closeData( file, handles[1], &status );
      mhdf_closeData( file, handles[2], &status );
      free(ids);
      return 1;
    }
    
    mhdf_closeData( file, handles[0], &status );
    ranges = all_id_ranges( desc, 0, &nranges );
    if (!ids_contained( ids, count, ranges, nranges )) {
      ++result;
      printf("Sparse data for tag \"%s\" has values for invalid IDs\n", tag->name );
    }
    else if (contains_duplicates( ids, count )) {
      ++result;
      printf("Sparse data for tag \"%s\" has duplicate values for one or more entities\n", tag->name );
    }
    free(ranges);
    
    mhdf_readSparseTagIndices( handles[2], 0, count, H5T_NATIVE_LONG, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading indices for variable length tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      mhdf_closeData( file, handles[1], &status );
      mhdf_closeData( file, handles[2], &status );
      free(ids);
      return 1;
    }
    mhdf_closeData( file, handles[2], &status );
    
    if (check_valid_end_indices( ids, count, 1, 0, num_val, "Varible-length tag", tag->name ))
      ++result;
    free(ids);
  }
  
  
  if (tag->type != mhdf_ENTITY_ID) {
    if (tag->have_sparse) 
      mhdf_closeData( file, handles[1], &status );
    return result;
  }
  
  ranges = all_id_ranges( desc, 1, &nranges );
  if (tag->default_value && !ids_contained( tag->default_value, tag->default_value_size, ranges, nranges )) {
    ++result;
    printf("Handle tag \"%s\" has invalid ID(s) in its default value.\n", tag->name );
  }
  if (tag->global_value && !ids_contained( tag->global_value, tag->global_value_size, ranges, nranges )) {
    ++result;
    printf("Handle tag \"%s\" has invalid ID(s) in its global/mesh value.\n", tag->name );
  }
  
  if (tag->have_sparse) {
    ids = malloc( num_val * sizeof(long) );
    mhdf_readTagValues( handles[1], 0, num_val, H5T_NATIVE_LONG, ids, &status );
    if (mhdf_isError(&status)) {
      fprintf(stderr,"Internal error reading values for variable-length handle tag \"%s\": %s\n", tag->name, mhdf_message(&status));
      mhdf_closeData( file, handles[1], &status );
      free(ids);
      free(ranges);
      return 1;
    }
    mhdf_closeData( file, handles[1], &status );
    
    if (!ids_contained( ids, tag->size * count, ranges, nranges )) {
      ++result;
      printf("Data for one or more entities with variable-length handle tag \"%s\" has invalid ID(s).\n", tag->name );
    }
    free(ids);
  }

  return result;
}
