/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2006 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/* Example file for using mhdf interface for reading MOAB native file foramt
 *
 * Requires libmhdf from MOAB source and HDF5 library 
 *
 * Reads:
 *  - all node coordinates
 *  - a single group of hexahedral elements
 *  - GLOBAL_ID tag if present
 * and writes as Gmsh file version 1.0.
 *
 * Does not contain examples for reading meshsets or
 * reading polygon or polyhedron elements.
 *
 * A simple exorcise:
 *    Clean up handling of variable number of dimensions by reading
 *    node coordinate data one dimension at a time using 
 *    mhdf_readNodeCoord
 *
 * A more complex exorcise:
 *    Read meshsets with MATERIAL_SET tag, which MOAB uses to
 *    represent element blocks, and assign appropriate element
 *    block for each hex in Gmsh file. 
 *    Hint: look for and read MATERIAL_SET tag first to get list of entities
 *          then read set contents for sets with that tag
 *          then match hex file ids to lists of set contents
 *    Be careful of the mhdf_SET_RANGE_BIT for each set when reading set
 *    contents.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* mhdf API */
#include "mhdf.h"
/* need constants for native types (e.g. H5T_NATIVE_UINT)*/
#include <H5Tpublic.h>

/* Macro to check return status from mhdf calls.  Exit on error. */
#define CHK_ERR( A ) \
do { \
  if (mhdf_isError(A)) { \
    fprintf( stderr, "Error: %s\n", mhdf_message( A ) ); \
    exit( 2 ); \
  } \
} while(0)

int main( int argc, char* argv[] )
{
  /* input file */
  const char* filename;
  mhdf_FileHandle file;
  mhdf_Status status;
  mhdf_Status *const sptr = &status;
  hid_t handle; /* generic handle used to refer to any data block in file */

  /* output file */
  const char* gmsh_filename;
  FILE* gmsh;
  unsigned gmsh_type;            /* hexahedral element type number */
  double x, y, z;                /* temp storage of node coordinates */
  unsigned node_offset, node_id; /* temporary values */
  unsigned* connectivity;        /* temporary value */
  
  /* node data */
  long numnode;       /* total number of nodes */
  long nodestart;     /* file id of first node in list */
  int dimension;      /* coordinate values per node */
  double* nodecoords; /* interleaved node coordinates */
  unsigned* nodeids;  /* GLOBAL_ID value for nodes */
  int have_nodeids = 0;   
  
  /* hex data */
  char* hexgroup = NULL;     /* name of element group containing hexes */
  long numhex;               /* total number of hexahedral elements */
  long hexstart;             /* file id of first hex in group */
  int nodes_per_hex;         /* length of connectivity list for a hex */
  unsigned* hexconnectivity; /* hex connectivity data */
  unsigned* hexids;          /* GLOBAL_ID value for hexes */
  int have_hexids = 0;
  
  /* list of element groups in file */
  char** elem_groups;
  unsigned num_elem_groups;
  char namebuffer[64];
  
  /* tag data for accessing GLOBAL_ID */
  int tagsize;                    /* number of values for each entity */
  int ts, td, tg;                 /* unused tag properties */
  int havesparse, havedense;      /* Boolean values */
  enum mhdf_TagDataType tagtype;  /* base data type of tag */
  hid_t sparse_handle[2];         /* handle pair for sparse tag data */
  unsigned* sparse_entities;      /* temp storage of sparse tag file ids */
  unsigned* sparse_ids;           /* temp storage of GLOBAL_ID values in spasre tag */
  long junk, numtag;              /* number of entities for which tag data is available */
  long fileid, globalid;          /* temporary values */
  long ncount = 0, hcount = 0;    /* temporary count of number of tag values */
  
  /* iteration */
  long i;
  int j;
  unsigned k;
  
    /* process CL args (expect input .h5m file and output .gmsh file name) */
  if (argc != 3) 
  {
    fprintf(stderr, "Usage: %s <input_file> <output_file>\n", argv[0] );
    return 1;
  }
  filename = argv[1];
  gmsh_filename = argv[2];
  
    /* Open the file */
  file = mhdf_openFile( filename, 0, 0, sptr ); CHK_ERR(sptr);

    /* Read node coordinates. */
  handle = mhdf_openNodeCoords( file, &numnode, &dimension, &nodestart, sptr ); CHK_ERR(sptr);
  nodecoords = (double*)malloc( dimension * numnode * sizeof(double) );
  mhdf_readNodeCoords( handle, 0, numnode, nodecoords, sptr ); CHK_ERR(sptr);
  mhdf_closeData( file, handle, sptr ); CHK_ERR(sptr);
  
    /* Find first element group containing hexahedra */
  elem_groups = mhdf_getElemHandles( file, &num_elem_groups, sptr ); CHK_ERR(sptr);
  for (k = 0; k < num_elem_groups; ++k)
  {
    mhdf_getElemTypeName( file, elem_groups[k], namebuffer, sizeof(namebuffer), sptr ); CHK_ERR(sptr);
    if (!hexgroup && !strcmp(mdhf_HEX_TYPE_NAME, namebuffer))
      hexgroup = strdup(elem_groups[k]);
    else
      printf( "Skipping element group '%s' containing element of type '%s'\n", elem_groups[k], namebuffer );
  }
  free( elem_groups );
  
  if (!hexgroup) {
    fprintf( stderr, "No Hexahedra defined in file\n" );
    return 4;
  }
  
    /* Read Hexahedron connectivity */
  handle = mhdf_openConnectivity( file, hexgroup, &nodes_per_hex, 
                                  &numhex, &hexstart, sptr ); CHK_ERR(sptr);
  hexconnectivity = (unsigned*)malloc(numhex * nodes_per_hex * sizeof(unsigned));
  mhdf_readConnectivity( handle, 0, numhex, H5T_NATIVE_UINT, hexconnectivity, sptr ); CHK_ERR(sptr);
  mhdf_closeData( file, handle, sptr ); CHK_ERR(sptr);
  /* Note: hex connectivity list contains file-space node IDs, which are
           the nodes in the sequence they are read from the file, with
           the first node having an ID of 'nodestart' */
  
    /* Check for "GLOBAL_ID" tag */
  nodeids = (unsigned*)malloc( numnode * sizeof(unsigned) );
  hexids = (unsigned*)malloc( numhex * sizeof(unsigned) );
  mhdf_getTagInfo( file, "GLOBAL_ID", &tagtype, &tagsize, &ts, &td, &tg, &havesparse, sptr ); 

    /* If have GLOBAL_ID tag, try to read values for nodes and hexes */
  if (!mhdf_isError(sptr)) 
  {
      /* Check that the tag contains what we expect */
    if (tagtype != mhdf_INTEGER || tagsize != 1) 
    {
      fprintf(stderr, "ERROR: Invalid data type for 'GLOBAL_ID' tag.\n");
      exit(3);
    }
    
      /* Check for and read dense-format tag data for nodes */
    havedense = mhdf_haveDenseTag( file, "GLOBAL_ID", mhdf_node_type_handle(), sptr ); CHK_ERR(sptr);
    if (havedense) 
    {
      handle = mhdf_openDenseTagData( file, "GLOBAL_ID", mhdf_node_type_handle(), &numtag, sptr ); CHK_ERR(sptr);
      assert( numtag == numnode );
      mhdf_readDenseTag( handle, 0, numtag, H5T_NATIVE_UINT, nodeids, sptr ); CHK_ERR(sptr);
      mhdf_closeData( file, handle, sptr ); CHK_ERR(sptr);
      have_nodeids = 1;
    }
      /* Check for and read dense-format tag data for hexes */
    havedense = mhdf_haveDenseTag( file, "GLOBAL_ID", hexgroup, sptr ); CHK_ERR(sptr);
    if (havedense) 
    {
      handle = mhdf_openDenseTagData( file, "GLOBAL_ID", hexgroup, &numtag, sptr ); CHK_ERR(sptr);
      assert( numtag == numhex );
      mhdf_readDenseTag( handle, 0, numtag, H5T_NATIVE_UINT, hexids, sptr ); CHK_ERR(sptr);
      mhdf_closeData( file, handle, sptr ); CHK_ERR(sptr);
      have_hexids = 1;
    }
      /* Check for and read sparse-format tag data */
    if (havesparse) 
    {
      mhdf_openSparseTagData( file, "GLOBAL_ID", &numtag, &junk, sparse_handle, sptr ); CHK_ERR(sptr);
      sparse_entities = (unsigned*)malloc(numtag * sizeof(unsigned));
      mhdf_readSparseTagEntities( sparse_handle[0], 0, numtag, H5T_NATIVE_UINT, 
                                  sparse_entities, sptr ); CHK_ERR(sptr);
      sparse_ids = (unsigned*)malloc(numtag * sizeof(unsigned));
      mhdf_readSparseTagValues( sparse_handle[1], 0, numtag, H5T_NATIVE_UINT,
                                   sparse_ids, sptr ); CHK_ERR(sptr);
      mhdf_closeData( file, sparse_handle[0], sptr ); CHK_ERR(sptr);
      mhdf_closeData( file, sparse_handle[1], sptr ); CHK_ERR(sptr);
      
        /* Set hex and node ids from sparse tag data */
      for (i = 0; i < numtag; ++i) 
      {
        fileid = sparse_entities[i];
        globalid = sparse_ids[i];
        if (fileid >= nodestart && fileid - nodestart < numnode) 
        {
          nodeids[fileid - nodestart] = globalid;
          ++ncount;
        }
        else if (fileid >= hexstart && fileid - hexstart < numhex)
        {
          hexids[fileid - hexstart] = globalid;
          ++hcount;
        }
      }
      free( sparse_ids );
      free( sparse_entities );
      
      /* make sure there was an ID for each node and each hex */
      if (ncount == numnode)
        have_nodeids = 1;
      if (hcount == numhex)
        have_hexids = 1;
        
    } /* end have sparse tag for GLOBAL_ID */
  } /* end have GLOBAL_ID tag */
  
    /* done with input file */
  free( hexgroup );
  mhdf_closeFile( file, sptr ); CHK_ERR(sptr);

  
    /* if no GLOBAL_ID, just use incrementing values */
  if (!have_nodeids)
    for (i = 0; i < numnode; ++i)
      nodeids[i] = i+1;
  if (!have_hexids)
    for (i = 0; i < numhex; ++i)
      hexids[i] = i+1;

  
    /* write out as gmesh file version 1.0 */
  
    /* get gmsh type for hexahedrons */
  if (nodes_per_hex == 8) 
    gmsh_type = 5;
  else if (nodes_per_hex == 27)
    gmsh_type = 12;
  else {
    fprintf(stderr, "Cannot store %d node hex in gmsh file.\n", nodes_per_hex);
    exit(4);
  }
  
    /* open file */
  gmsh = fopen( gmsh_filename, "w" );
  
    /* Write node data.  If dimension is less than 3, 
       write zero for other coordinate values.  In the
       (highly unlikely) case that dimension is greater
       than three, disregard higher-dimension coordinate
       values. */
  fprintf(gmsh, "$NOD\n" );
  fprintf(gmsh, "%lu\n", numnode );
  for (i = 0; i < numnode; ++i) 
  {
    x = nodecoords[dimension*i];
    y = z = 0.0;
    if (dimension > 1) {
      y = nodecoords[dimension*i+1];
      if (dimension > 2) {
        z = nodecoords[dimension*i+2];
      }
    }
    fprintf(gmsh, "%u %f %f %f\n", nodeids[i], x, y, z );
  }
  
    /* Write element connectivity data */
  fprintf(gmsh, "$ENDNOD\n$ELM\n" );
  fprintf(gmsh, "%lu\n", numhex );
  for (i = 0; i < numhex; ++i) 
  {
    fprintf( gmsh, "%u %u 1 1 %d", hexids[i], gmsh_type, nodes_per_hex );
      /* connectivity list for this hex */
    connectivity = hexconnectivity + i*nodes_per_hex;
    for (j = 0; j < nodes_per_hex; ++j) 
    {
        /* get offset in node list from file id */
      node_offset = connectivity[j] - nodestart;
        /* get node id from ID list */
      node_id = nodeids[node_offset];
      fprintf( gmsh, " %u", node_id );
    }
    fprintf( gmsh, "\n" );
  }
  fprintf( gmsh, "$ENDELM\n" );
  fclose( gmsh );
  return 0;
}

