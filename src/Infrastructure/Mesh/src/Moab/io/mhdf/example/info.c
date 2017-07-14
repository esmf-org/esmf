#include "mhdf.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <H5Tpublic.h>

static int print_file_summary( struct mhdf_FileDesc* data );

int main( int argc, char* argv[] )
{
  int result;
  mhdf_FileHandle file;
  mhdf_Status status;
  unsigned long max_id;
  struct mhdf_FileDesc* data;
  
  if (argc != 2) {
    fprintf( stderr,"Usage: %s <filename>\n", argv[0] );
    return 1;
  }
  
  file = mhdf_openFile( argv[1], 0, &max_id, -1, &status );
  if (mhdf_isError( &status )) {
    fprintf( stderr,"%s: %s\n", argv[1], mhdf_message( &status ) );
    return 1;
  }
  
  data = mhdf_getFileSummary( file, H5T_NATIVE_ULONG, &status, 0 ); /* no extra set info here*/
  if (mhdf_isError( &status )) {
    fprintf( stderr,"%s: %s\n", argv[1], mhdf_message( &status ) );
    return 1;
  }
  
  mhdf_closeFile( file, &status );
  
  printf( "%s:\n", argv[1] );
  result = print_file_summary( data );
  free( data );
  return result;
}

static void print_ent_desc( const char* name,
                            const char* subname,
                            struct mhdf_EntDesc* data,
                            const char* vals_label,
                            const char* extra_label,
                            struct mhdf_FileDesc* all )
{
  int i, len = 10;
  
  if (vals_label && (int)strlen(vals_label) > len)
    len = strlen(vals_label);
  if (extra_label && (int)strlen(extra_label) > len)
    len = strlen(extra_label);
  
  if (subname) 
    printf( "    %s (%s):\n", name, subname );
  else
    printf( "    %s:\n", name );
  
  if (vals_label)
    printf( "      %-*s: %d\n", len, vals_label, data->vals_per_ent );
 
  printf( "      %-*s: %ld [%ld - %ld]\n", len, "entities", data->count, data->start_id, data->start_id + data->count - 1 );
   
  if (extra_label)
    printf( "      %-*s\n", len, extra_label );

  if (!data->num_dense_tags)
    return;
  
  printf( "      %-*s: \"%s\"", len, "dense tags", all->tags[data->dense_tag_indices[0]].name );
  for (i = 1; i < data->num_dense_tags; ++i)
    printf( ", \"%s\"", all->tags[data->dense_tag_indices[i]].name );
  printf ("\n");  
}

static void print_elem_desc( struct mhdf_ElemDesc* data, struct mhdf_FileDesc* all )
{
  const char* adj = data->have_adj ? "adjacencies" : "no adjencies";
  print_ent_desc( data->handle, data->type, &data->desc, "nodes per element", adj, all );
}

static const char* tag_type_name( enum mhdf_TagDataType type )
{
  static const char opaque[] = "opaque";
  static const char integer[] = "integer";
  static const char real[] = "real";
  static const char bits[] = "bit field";
  static const char boolean[] = "boolean";
  static const char id[] = "entity id";
  static const char unknown[] = "(UNKNOWN TYPE ID)";
  switch (type) {
    case mhdf_OPAQUE:    return opaque;
    case mhdf_INTEGER:   return integer;
    case mhdf_FLOAT:     return real;
    case mhdf_BITFIELD:  return bits;
    case mhdf_BOOLEAN:   return boolean;
    case mhdf_ENTITY_ID: return id;
  }
  return unknown;
}

static const char* string_tag_value( const void* value, 
                                     enum mhdf_TagDataType type,
                                     int size )
{
  static char buffer[1024];
  const char* data = value;
  char* offset = buffer;
  int print, i;
  const int* intptr = value;
  const double* dblptr = value;
  const unsigned long* idptr = value;
  

  if (size <= 0) {
    *buffer = '\0';
    return buffer;
  }
  
  switch (type) {
    case mhdf_OPAQUE:
      print = 1;
      for (i = 0; i < size; ++i)
        if (!isprint(data[i]))
          print = 0;
      if (print) {
        offset[0] = '"';
        memcpy( offset+1, data, size );
        offset[size+1] = '"';
        offset[size+2] = '\0';
        offset += size+2;
      }
      else {
        strcpy( offset, "0x" );
        offset += 2;
        for (i = 0; i < size; ++i)
          offset += sprintf( offset, "%02x", (unsigned int)data[i] );
      }
      break;
    case mhdf_INTEGER:
      if (size == 1) {
        offset += sprintf( offset, "%d", intptr[0] );
      }
      else {
        offset += sprintf( offset, "{%d", intptr[0] );
        for (i = 1; i < size; ++i)
          offset += sprintf( offset, ",%d", intptr[i] );
        offset += sprintf( offset, "}" );
      }
      break;
    case mhdf_FLOAT:
      if (size == 1) {
        offset += sprintf( offset, "%g", dblptr[0] );
      }
      else {
        offset += sprintf( offset, "{%g", dblptr[0] );
        for (i = 1; i < size; ++i)
          offset += sprintf( offset, ",%g", dblptr[i] );
        offset += sprintf( offset, "}" );
      }
      break;
    case mhdf_BITFIELD:
      if (size > 8)
        offset += sprintf( offset, "(more than 8 bits)" );
      else {
        for (i = size - 1; i >= 0; --i)
          *(offset++) = (char)(*data & (1<<i) ? '1' : '0');
        *offset = '\0';
      }
      break;
    case mhdf_BOOLEAN:
      if (size == 1) {
        offset += sprintf( offset, "%s", data[0] ? "true" : "false" );
      }
      else {
        offset += sprintf( offset, "{%s", data[0] ? "true" : "false" );
        for (i = 1; i < size; ++i)
          offset += sprintf( offset, ",%s", data[i] ? "true" : "false" );
        offset += sprintf( offset, "}" );
      }
      break;
    case mhdf_ENTITY_ID:
      if (size == 1) {
        offset += sprintf( offset, "%lu", idptr[0] );
      }
      else {
        offset += sprintf( offset, "{%lu", idptr[0] );
        for (i = 1; i < size; ++i)
          offset += sprintf( offset, ",%lu", idptr[i] );
        offset += sprintf( offset, "}" );
      }
      break;
    default:
      strcpy( buffer, "(unknown data type)" );
      break;
  }
  
  return buffer;
}

static const char* ent_desc_name( struct mhdf_FileDesc* all, int idx )
{
  static const char nodes[] = "Nodes";
  static const char sets[] = "Sets";
  static const char invalid[] = "<INVALID INDEX!>";
  if (idx == -2) return sets;
  if (idx == -1) return nodes;
  if (idx >= all->num_elem_desc || idx < -2) return invalid;
  return all->elems[idx].handle;
}

static void print_tag_desc( struct mhdf_TagDesc* data, struct mhdf_FileDesc* all )
{
  int i, width = 8;

  printf( "    \"%s\":\n", data->name );
  printf( "      %-*s: %s\n", width, "type", tag_type_name( data->type ) );
  if (data->size < 0)
    printf( "      %-*s: (variable)\n", width, "size" );
  else
    printf( "      %-*s: %d (%d bytes)\n", width, "size", data->size, data->bytes );
  printf( "      %-*s: %x\n", width, "flags", data->storage );
  if (data->default_value)
    printf( "      %-*s: %s\n", width, "default", 
      string_tag_value( data->default_value, data->type, data->default_value_size ) );
  if (data->global_value)
    printf( "      %-*s: %s\n", width, "mesh val", 
      string_tag_value( data->global_value, data->type, data->global_value_size ) );
  if (data->have_sparse) {
    printf( "      %-*s: (sparse)", width, "tables" );
    for (i = 0; i < data->num_dense_indices; ++i)
      printf( ", %s", ent_desc_name( all, data->dense_elem_indices[i] ) );
  }
  else if (data->num_dense_indices) {
    printf( "      %-*s: %s", width, "tables", ent_desc_name( all, data->dense_elem_indices[0] ) );
    for (i = 1; i < data->num_dense_indices; ++i)
      printf( ", %s", ent_desc_name( all, data->dense_elem_indices[i] ) );
  }
  else {
    printf( "      %-*s: (none)", width, "tables" );
  }
  printf( "\n" );
}

static int print_file_summary( struct mhdf_FileDesc* data )
{
  int i;
  
  printf( "  Entities:\n" );
  print_ent_desc( "Nodes", NULL, &data->nodes, "dimension", NULL, data );
  for (i = 0; i < data->num_elem_desc; ++i)
    print_elem_desc( data->elems + i, data );
  print_ent_desc( "Sets", NULL, &data->sets, NULL, NULL, data );
  
  printf( "  Tags:\n" );
  for (i = 0; i < data->num_tag_desc; ++i)
    print_tag_desc( data->tags + i, data );
  
  return 0;
}
