// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/Legacy/ESMCI_GlobalIds.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"
#include "Mesh/include/ESMCI_FileIO_Util.h"

// These internal functions can only be used if PIO is available
#ifdef ESMF_PIO

#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif

#include <pio.h>
#include "IO/include/ESMCI_PIO_Handler.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

// Get the id of mesh topology dummy variable
// TODO: allow the user to specify the name of the mesh topo variable
void get_mesh_topo_id_from_UGRID_file(int pioFileDesc, char *filename, int &mesh_topo_id) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_mesh_topo_id_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Get number of variables in the file
  int num_vars=0;
  piorc = PIOc_inq(pioFileDesc, NULL, &num_vars, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error reading number of variables from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;  

  // Loop over variables to find the first one that looks like mesh topo
  mesh_topo_id=-1;
  for (int v=0; v<num_vars; v++) {

    // Declarations
    nc_type type;
    PIO_Offset len_attr;
#define MAX_LEN_ATTR 1024    
    char attr[MAX_LEN_ATTR+1]; // Add 1 for NULL padding
    
    // See if cf_role is an attribute
    bool found_attr=false;
    piorc = PIOc_inq_att(pioFileDesc, v, "cf_role", &type, &len_attr);
    if (piorc == PIO_NOERR) {
      
      // Skip if too long, since we're only looking for a short value
      if (len_attr <= MAX_LEN_ATTR) {
        
        // Get attribute value
        piorc = PIOc_get_att_text(pioFileDesc, v, "cf_role", attr);
        if (!CHECKPIOERROR(piorc, std::string("Error reading cf_role attribute from file ") + filename,
                           ESMF_RC_FILE_OPEN, localrc)) throw localrc;
        
        // Add null terminator
        attr[len_attr]='\0'; 
        
        // Look at  attr value to see if this is the topo variable
        // Using strncmp to only compare chars that match
        // because sometimes attr can
        // have garbage on the end.
        if (strncmp(attr,"mesh_topology",13) == 0) {
          mesh_topo_id=v;
          break;
        }        
      }
    }
      
    // See if sstandard_name is an attribute
    piorc = PIOc_inq_att(pioFileDesc, v, "standard_name", &type, &len_attr);
    if (piorc == PIO_NOERR) {

      // Skip if too long, since we're only looking for a short value
      if (len_attr <= MAX_LEN_ATTR) {
        
        // Get attribute value
        piorc = PIOc_get_att_text(pioFileDesc, v, "standard_name", attr);
        if (!CHECKPIOERROR(piorc, std::string("Error reading standard_name attribute from file ") + filename,
                           ESMF_RC_FILE_OPEN, localrc)) throw localrc;
        
        // Add null terminator
        attr[len_attr]='\0'; 

        // Look at  attr value to see if this is the topo variable
        // Using strncmp to only compare chars that match
        // because sometimes attr can
        // have garbage on the end.
        if (strncmp(attr,"mesh_topology",13) == 0) {
          mesh_topo_id=v;
          break;
        }        
      }
    }
  }

  // Error if we didn't find anything
  if (mesh_topo_id == -1) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " couldn't find mesh_topology variable in file",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

#undef MAX_LEN_ATTR    
}


// Get the dimension of the mesh in the UGRID file
// (This dimension is both the pdim and orig_sdim of the mesh)
void get_dim_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, 
                              int &dim) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_dim _from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Try getting topology dim this is correct as of 2/28/2013
  piorc = PIOc_get_att_int(pioFileDesc, mesh_topo_id, "topology_dimension", &dim);

  // If topology dimension didn't work, then try the old style: dimension
  if (piorc != PIO_NOERR) {
    
    // Try getting just dimension
    piorc = PIOc_get_att_int(pioFileDesc, mesh_topo_id, "dimension", &dim);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find topology_dimension or dimension attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  }

}


// Get the id of the elementConn array
void get_elementConn_id_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, 
                                        int &elementConn_id) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementConn_id_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Get name of elementConn variable
  nc_type type;
  PIO_Offset len_elemConn_name;
  char *elemConn_name;
  if (dim == 2) {

    // Get attribute len
    piorc = PIOc_inq_att(pioFileDesc, mesh_topo_id, "face_node_connectivity", &type, &len_elemConn_name);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find face_node_connectivity attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Allocate space
    elemConn_name=new char[len_elemConn_name+1]; // +1 for null terminator
    
    // Get attribute value
    piorc = PIOc_get_att_text(pioFileDesc, mesh_topo_id, "face_node_connectivity", elemConn_name);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find face_node_connectivity attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
  } else if (dim == 3) {

    // Get attribute len
    piorc = PIOc_inq_att(pioFileDesc, mesh_topo_id, "volume_node_connectivity", &type, &len_elemConn_name);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find face_node_connectivity attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Allocate space
    elemConn_name=new char[len_elemConn_name+1]; // +1 for null terminator
    
    // Get attribute value
    piorc = PIOc_get_att_text(pioFileDesc, mesh_topo_id, "volume_node_connectivity", elemConn_name);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find volume_node_connectivity attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " Currently only 2 or 3 dimensional meshes are supported.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
    
  }

  // Add null terminator
  elemConn_name[len_elemConn_name]='\0'; 
  
  // Now that we have the name get id of elemConn var
  piorc = PIOc_inq_varid(pioFileDesc, elemConn_name, &elementConn_id);
  if (!CHECKPIOERROR(piorc, std::string("Error finding element connection variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Free name memory
  delete [] elemConn_name;
}


void get_elementCount_from_UGRID_file(int pioFileDesc, char *filename, int elementConn_id, 
                                      PIO_Offset &elementCount) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementCount_from_UGRID_file()"

  // Declare some useful vars
  int localrc;
  int piorc;

  // Get number of dims dimensions of element connectivity
  int num_dims;
  piorc = PIOc_inq_varndims(pioFileDesc, elementConn_id, &num_dims);
  if (!CHECKPIOERROR(piorc, std::string("Error finding number of dims of element connection variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Error if number of dims isn't 2
  if (num_dims != 2) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " element connection variable should have two dimensions.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get dim ids
  int dim_ids[2];
  piorc = PIOc_inq_vardimid(pioFileDesc, elementConn_id, dim_ids);
  if (!CHECKPIOERROR(piorc, std::string("Error finding the dim ids of element connection variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Get elementCount from 1st dim_id
  piorc = PIOc_inq_dimlen(pioFileDesc, dim_ids[0], &elementCount);
  if (!CHECKPIOERROR(piorc, std::string("Error reading number of elements from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
}


// Get elementConn info out from a UGRID file. 
// Note that the output is a 1D array 
// (The elem conn info is collapsed to get rid of unnecessary values)
void get_elementConn_info_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, int elementConn_id, 
                                          PIO_Offset elementCount, int num_elems, int *elem_ids, 
                                          int &totNumElementConn, int *&numElementConn, int *&elementConn) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementConn_info_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;
  int varid;
  int rearr = PIO_REARR_SUBSET;


  // See if there's a _FillValue attribute to mark unused connections, otherwise
  // default to -1
  int unused_conn_mark=-1; // Set to default
  int fillValue=-1;
  piorc = PIOc_get_att_int(pioFileDesc, elementConn_id, "_FillValue", &fillValue);
  if (piorc == PIO_NOERR) {
    unused_conn_mark=fillValue;
  }

  ////  Get max number of nodes per element

  // Get number of dims just to make sure array is correct size
  int num_dims;
  piorc = PIOc_inq_varndims(pioFileDesc, elementConn_id, &num_dims);
  if (!CHECKPIOERROR(piorc, std::string("Error finding number of dims of element connection variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Error if number of dims isn't 2
  if (num_dims != 2) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " element connection variable should have two dimensions.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get dim ids
  int dim_ids[2]; // Checked that it's 2 above
  piorc = PIOc_inq_vardimid(pioFileDesc, elementConn_id, dim_ids);
  if (!CHECKPIOERROR(piorc, std::string("Error finding the dim ids of element connection variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Get max_num_conns_per_elem from 2nd dim_id
  PIO_Offset max_num_conns_per_elem;
  piorc = PIOc_inq_dimlen(pioFileDesc, dim_ids[1], &max_num_conns_per_elem);
  if (!CHECKPIOERROR(piorc, std::string("Error reading max number of connections per element from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  // Define offsets for elementConn decomp
  int totNumRectElementConn=max_num_conns_per_elem*num_elems;
  PIO_Offset *ec_offsets=new PIO_Offset[totNumRectElementConn];
  for (int i=0,pos=0; i<num_elems; i++) {
    int elem_start_ind=(elem_ids[i]-1)*max_num_conns_per_elem+1; // +1 to make base-1
    for (int j=0; j<max_num_conns_per_elem; j++) {
      ec_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
      pos++;
    }
  }
  

  // Init elementConn decomp
  int ec_iodesc;
  int gdimlen2D[2]={(int)elementCount,(int)max_num_conns_per_elem};
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 
                          2, gdimlen2D, totNumRectElementConn,
                          ec_offsets, &ec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] ec_offsets;
  piorc = PIOc_setframe(pioFileDesc, elementConn_id, -1);
  if (!CHECKPIOERROR(piorc, std::string("Error setting frame for variable elementConn ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  
  // Get rectElementConn  
  int *rectElementConn= new int[totNumRectElementConn];
  piorc = PIOc_read_darray(pioFileDesc, elementConn_id, ec_iodesc, 
                           totNumRectElementConn, rectElementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading variable elementConn from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  
  // Get rid of elementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, ec_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing elementConn decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


  // Count number of valid connections in each element slot
  numElementConn=new int[num_elems];
  for (int i=0; i<num_elems; i++) {

    // Calc begining index of element
    int elem_beg_ind=i*max_num_conns_per_elem;

    // Count the number of valid conns in current elem
    int num_valid_conns=0;
    for (int j=0; j<max_num_conns_per_elem; j++) {

      // Leave once we've hit an invalid value
      if (rectElementConn[elem_beg_ind+j] == unused_conn_mark) break;
      
      // Count valid value
      num_valid_conns++;
    }

    // Set in numElementConn
    numElementConn[i]=num_valid_conns;
  }

  // Get total number of connections on this PET
  totNumElementConn=0;
  for (int i=0; i<num_elems; i++) {
    totNumElementConn += numElementConn[i];
  }

  // Conpress from rectangular element conns to 1D version with just valid conns. 
  elementConn=new int[totNumElementConn];
  for (int i=0,ec_pos=0; i<num_elems; i++) {

    // Calc begining index of element
    int elem_beg_ind=i*max_num_conns_per_elem;
    
    // Loop over valid elements and copy valid ones
    for (int j=0; j<numElementConn[i]; j++) {
      elementConn[ec_pos]=rectElementConn[elem_beg_ind+j]; 
      ec_pos++;
    }
  }

  // Get rid of rectElementConn
  delete [] rectElementConn;



  //// Handle start_index attribute

  // Get start_index attribute, otherwise default to 0.
  int start_index=0; // Set to default
  int att_start_index=0;
  piorc = PIOc_get_att_int(pioFileDesc, elementConn_id, "start_index", &att_start_index);
  if (piorc == PIO_NOERR) {
    start_index=att_start_index;
  }
  
  // Given start_index switch base of connections to be 1-based (what's expected by mesh create code) 
  for (int i=0; i<totNumElementConn; i++) {
    elementConn[i] = elementConn[i]-start_index+1;
  }



  //// Handle polygon_break_value attribute

  // See if there's a polygon_break_value attribute
  int polygon_break_value;
  piorc = PIOc_get_att_int(pioFileDesc, elementConn_id, "polygon_break_value", &polygon_break_value);

  // If there is a polygon break attribute, switch matching values to be the internal break value
  if (piorc == PIO_NOERR) {

    // Modify poly_break_value by start_index to correspond with that change to elementConns 
    polygon_break_value = polygon_break_value-start_index+1;

    // Loop changing any connection entries that match to value expected by mesh create code
    for (int i=0; i<totNumElementConn; i++) {
      if (elementConn[i] == polygon_break_value) elementConn[i] = MESH_POLYBREAK_IND;
    }
  }


  //     // DEBUG: output numElementConn
//       printf("numElementConn=");
//        for (int i=0; i<num_elems; i++) {
//          printf(" [%d]=%d ",elem_ids[i],numElementConn[i]);
//        }
//        printf("\n");
  

  //     // DEBUG: output elementConn
//       int pos=0;
//       for (int i=0; i<num_elems; i++) {
//         printf(" [%d] =",elem_ids[i]);
//         for (int j=0; j<(int)numElementConn[i]; j++) {
//           printf(" %d ",elementConn[pos]);
//           pos++;
//         }
//         printf("\n");
//       }
//       printf("\n");


}


// Get the ids of the node coordinate variables
// nodeCoord_ids - must at least be of size dim
void get_nodeCoord_ids_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, int *nodeCoord_ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCoord_ids_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  //// Get node_coordinates attribute from mesh topology variable
  nc_type type;
  PIO_Offset len_node_coord_attr;
  char *node_coord_attr;

  // Get attribute len
  piorc = PIOc_inq_att(pioFileDesc, mesh_topo_id, "node_coordinates", &type, &len_node_coord_attr);
  if (!CHECKPIOERROR(piorc, std::string("Couldn't find node_coordinates attribute on mesh topology var in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  // Allocate space
  node_coord_attr=new char[len_node_coord_attr+1]; // +1 for null terminator
  
  // Get attribute value
  piorc = PIOc_get_att_text(pioFileDesc, mesh_topo_id, "node_coordinates", node_coord_attr);
  if (!CHECKPIOERROR(piorc, std::string("Couldn't find node_coordinates attribute on mesh topology var in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;


  // Array of pointers to names
  // Make same length as attribute because that will always be greater than the possible number of contained names
  char **node_coord_names=new char*[len_node_coord_attr+1];

  // Loop finding names
  int num_node_coord_names=0;
  bool prev_is_space=true; // Initial previous is space
  for (int i=0; i<len_node_coord_attr; i++) {

    // Detect if characters are spaces
    bool curr_is_space=(node_coord_attr[i] == ' ');

    // Going from space to not space, so front border of name
    // save pointer to front
    if (prev_is_space && !curr_is_space) {
      node_coord_names[num_node_coord_names]=node_coord_attr+i;
      num_node_coord_names++;
    } 
    
    // Going from non-space to space, so end of name
    // mark with null terminator
    if (!prev_is_space && curr_is_space) {
      node_coord_attr[i] = '\0';
    } 
    
    // Change prev to curr
    prev_is_space=curr_is_space;
  }

  // Add null terminator to end of whole attribute to terminate last name
  node_coord_attr[len_node_coord_attr]='\0';

  // Make sure the number of names matches the dim of the mesh
  if (num_node_coord_names != dim) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Mesh has more names in node_coordinates attribute than the dimension of the mesh.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Debug Output
  //for (int n=0; n<num_node_coord_names; n++) {
  //  printf("%d %s \n",n,node_coord_names[n]);
  //}

  // Loop getting node coordinate ids from names
  for (int n=0; n<num_node_coord_names; n++) {
    
    piorc = PIOc_inq_varid(pioFileDesc, node_coord_names[n], nodeCoord_ids+n);
    if (!CHECKPIOERROR(piorc, std::string("Error finding node coordinate variable in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
  }

  // Free name memory
  delete [] node_coord_attr;
  delete [] node_coord_names;
}


void get_nodeCount_from_UGRID_file(int pioFileDesc, char *filename, int dim, int *nodeCoord_ids, PIO_Offset &nodeCount) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCount_from_UGRID_file()"

  // Declare some useful vars
  int localrc;
  int piorc;

  // Get dim id
  int dim_id;
  piorc = PIOc_inq_vardimid(pioFileDesc, nodeCoord_ids[0], &dim_id);
  if (!CHECKPIOERROR(piorc, std::string("Error finding the dim id of node coordinate variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Get nodeCount from dim_id
  piorc = PIOc_inq_dimlen(pioFileDesc, dim_id, &nodeCount);
  if (!CHECKPIOERROR(piorc, std::string("Error reading number of nodes from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Make sure the other nodeCoords match
  for (int d=1; d<dim; d++) {

    // Get dim id
    piorc = PIOc_inq_vardimid(pioFileDesc, nodeCoord_ids[d], &dim_id);
    if (!CHECKPIOERROR(piorc, std::string("Error finding the dim id of node coordinate variable in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Get nodeCount from dim_id
    PIO_Offset other_nodeCount;
    piorc = PIOc_inq_dimlen(pioFileDesc, dim_id, &other_nodeCount);
    if (!CHECKPIOERROR(piorc, std::string("Error reading number of nodes from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Make sure sizes match
    if (other_nodeCount != nodeCount) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                        "Size of node coordinate variables don't match.",
                                        ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
}


// Get coords from UGRID format file
void get_coords_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                ESMC_CoordSys_Flag coord_sys, int dim, int *coordVar_ids, 
                                PIO_Offset global_count, 
                                int num_ids, int *ids, 
                                double *&coords) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_coords_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Init output
  coords=NULL;

  // Define offsets for Coord decomp
  PIO_Offset *offsets= new PIO_Offset[num_ids];
  for (int i=0; i<num_ids; i++) {
    offsets[i] = (PIO_Offset) (ids[i]);
  }
  
  // Init coords decomp
  int iodesc;
  int gdimlen=global_count;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, 1, &gdimlen, num_ids, offsets, &iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of node offsets
  delete [] offsets;
  
  // Array for holding packed node coordinates
  coords= new double[dim*num_ids];
  
  // Temp array for getting one dimensions coordinates
  double *onedim_coords= new double[num_ids];
  
  // Loop through dimensions getting coords from each variable in file
  for (int d=0; d<dim; d++) {
    piorc = PIOc_setframe(pioFileDesc, coordVar_ids[d], -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for coordinate variable ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Get coords
    piorc = PIOc_read_darray(pioFileDesc, coordVar_ids[d], iodesc, num_ids, onedim_coords);
    if (!CHECKPIOERROR(piorc, std::string("Error reading coordinate variable from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Pack this dimension's coords into final array
    double *curr_coord=coords+d;
    for (int i=0; i<num_ids; i++) {
      *curr_coord = onedim_coords[i];
      curr_coord += dim;
    }      
  }

  // Get rid of temporary array
  delete [] onedim_coords;
  
  // Get rid of coords decomp
  piorc = PIOc_freedecomp(pioSystemDesc, iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing coordinate decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;



  // If this is 3D spherical, normalize the 3rd dimension to the radius of the earth to match
  // how it was in the original version of mesh create from file.
  // TODO: Add a base_height attribute, to allow the user to control this
  if ((dim == 3) && (coord_sys != ESMC_COORDSYS_CART)) {
    
    // Get Units of height
    nc_type type;
    PIO_Offset units_len;
    piorc = PIOc_inq_att(pioFileDesc, coordVar_ids[2], "units", &type, &units_len);
    if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from third coordinates varible in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Get units attribute
    char *units=new char[units_len+1]; // +1 for NULL terminator (sometimes the len from PIO doesn't seem to include that so adding just in case)
    piorc = PIOc_get_att_text(pioFileDesc, coordVar_ids[2], "units", units);
    if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from third coordinates variable in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Add null terminator
    units[units_len]='\0'; 
    
    // Look at units to determine base height (default is Earth radius)
    // (Using strncmp to only compare chars that match
    // the unit name because sometimes the units can
    // have garbage on the end.)
    double earth_radius;
    if (strncmp(units,"km",2) == 0) {
      earth_radius= 6371.0;
    } else if (strncmp(units,"kilometers",10) == 0) {
      earth_radius= 6371.0;
    } else if (strncmp(units,"m",1) == 0) {
      earth_radius=6371000.0;
    } else if (strncmp(units,"meters",6) == 0) {
      earth_radius=6371000.0;
    } else {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                        "Unrecognized units for third (height) coordinate variable",
                                        ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Done with units so free
    delete [] units;

    // Normalize 3rd (height) dim to Earth radius
    double *curr_coord=coords+2; // Set to third dim
    for (int i=0; i<num_ids; i++) {
      *curr_coord = 1.0 + *curr_coord/earth_radius;
      curr_coord += 3; // Advance to next third dim
    }      
  }
}



/* XMRKX */
void get_coordsys_from_UGRID_file(int pioFileDesc, char *filename, 
                                  int dim, int *nodeCoord_ids, 
                                  ESMC_CoordSys_Flag &coord_sys) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_coordsys_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  nc_type type;

  // Just double check that we have 2 dims because we are accessing them below
  ThrowRequire(dim >= 2); 

  // Get information about the units attribute for first variable
  PIO_Offset units0_len;
  piorc = PIOc_inq_att(pioFileDesc, nodeCoord_ids[0], "units", &type, &units0_len);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from first node coordinates varible in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Debug
  //printf("units len=%d\n",len);

  // Get units attribute
  char *units0=new char[units0_len+1]; // +1 for NULL terminator (sometimes the len from PIO doesn't seem to include that so adding just in case)
  piorc = PIOc_get_att_text(pioFileDesc, nodeCoord_ids[0], "units", units0);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from first node coordinates variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Add null terminator
  units0[units0_len]='\0'; 

  // Debug
  //printf("units value=%s\n",units);


  // Look at units to determine coordinate system
  // Using strncmp to only compare chars that match
  // the unit name because sometimes the units can
  // have garbage on the end.
  ESMC_CoordSys_Flag coord_sys0;
  if (strncmp(units0,"degrees",7) == 0) {
    coord_sys0=ESMC_COORDSYS_SPH_DEG;
  } else if (strncmp(units0,"radians",7) == 0) {
    coord_sys0=ESMC_COORDSYS_SPH_RAD;
  } else if (strncmp(units0,"km",2) == 0) {
    coord_sys0=ESMC_COORDSYS_CART;
  } else if (strncmp(units0,"kilometers",10) == 0) {
    coord_sys0=ESMC_COORDSYS_CART;
  } else if (strncmp(units0,"m",1) == 0) {
    coord_sys0=ESMC_COORDSYS_CART;
  } else if (strncmp(units0,"meters",6) == 0) {
    coord_sys0=ESMC_COORDSYS_CART;
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Unrecognized units for first node coords variable",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get rid of units0 string
  delete [] units0;

  // Get information about the units attribute for first variable
  PIO_Offset units1_len;
  piorc = PIOc_inq_att(pioFileDesc, nodeCoord_ids[1], "units", &type, &units1_len);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from second node coordinates varible in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Debug
  //printf("units len=%d\n",len);

  // Get units attribute
  char *units1=new char[units1_len+1]; // +1 for NULL terminator (sometimes the len from PIO doesn't seem to include that so adding just in case)
  piorc = PIOc_get_att_text(pioFileDesc, nodeCoord_ids[1], "units", units1);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from second coordinates variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Add null terminator
  units1[units1_len]='\0'; 

  // Look at units to determine coordinate system
  // Using strncmp to only compare chars that match
  // the unit name because sometimes the units can
  // have garbage on the end.
  ESMC_CoordSys_Flag coord_sys1;
  if (strncmp(units1,"degrees",7) == 0) {
    coord_sys1=ESMC_COORDSYS_SPH_DEG;
  } else if (strncmp(units1,"radians",7) == 0) {
    coord_sys1=ESMC_COORDSYS_SPH_RAD;
  } else if (strncmp(units1,"km",2) == 0) {
    coord_sys1=ESMC_COORDSYS_CART;
  } else if (strncmp(units1,"kilometers",10) == 0) {
    coord_sys1=ESMC_COORDSYS_CART;
  } else if (strncmp(units1,"m",1) == 0) {
    coord_sys1=ESMC_COORDSYS_CART;
  } else if (strncmp(units1,"meters",6) == 0) {
    coord_sys1=ESMC_COORDSYS_CART;
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " unrecognized units for second node coords variable",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get rid of units0 string
  delete [] units1;


  // If first and second coord sys match, then use, otherwise complain. 
  if (coord_sys0 == coord_sys1) {
    coord_sys=coord_sys0;
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Coordinate system of first and second node coord. variables must be the same.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Do I need to check what the units of the 3rd coordinate are? It doesn't really 
  // matter as long as it's a height, so why constrain the user unnecessarily? Unless we
  // are going to just constrain them to one specific unit, but we are already allowing both
  // m and km, so I don't think that we can go down to 1...

}


void get_elemCoord_ids_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, int *elemCoord_ids, int &centerCoordsPresent) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elemCoord_ids_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;
  nc_type type;

  // Init centerCoordsPresent to 0 (for the case it isn't present)
  centerCoordsPresent=0;

  // Get elem coordinate attribute depending on dim
  PIO_Offset len_elem_coord_attr;
  char *elem_coord_attr;
  if (dim == 2) {

    // Get attribute len
    piorc = PIOc_inq_att(pioFileDesc, mesh_topo_id, "face_coordinates", &type, &len_elem_coord_attr);
    
    // If they aren't present, then leave (init. as not present above)
    if (piorc != PIO_NOERR) return;
  
    // Allocate space
    elem_coord_attr=new char[len_elem_coord_attr+1]; // +1 for null terminator
    
    // Get attribute value
    piorc = PIOc_get_att_text(pioFileDesc, mesh_topo_id, "face_coordinates", elem_coord_attr);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find face_coordinates attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
  } else if (dim == 3) {
    // Get attribute len
    piorc = PIOc_inq_att(pioFileDesc, mesh_topo_id, "volume_coordinates", &type, &len_elem_coord_attr);

    // If they aren't present, then leave (init. as not present above)
    if (piorc != PIO_NOERR) return;  

    // Allocate space
    elem_coord_attr=new char[len_elem_coord_attr+1]; // +1 for null terminator
    
    // Get attribute value
    piorc = PIOc_get_att_text(pioFileDesc, mesh_topo_id, "volume_coordinates", elem_coord_attr);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find volume_coordinates attribute on mesh topology var in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " Currently only 2 or 3 dimensional meshes are supported.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
    
  }


  // Array of pointers to names
  // Make same length as attribute because that will always be greater than the possible number of contained names
  char **elem_coord_names=new char*[len_elem_coord_attr+1];

  // Loop finding names
  int num_elem_coord_names=0;
  bool prev_is_space=true; // Initial previous is space
  for (int i=0; i<len_elem_coord_attr; i++) {

    // Detect if characters are spaces
    bool curr_is_space=(elem_coord_attr[i] == ' ');

    // Going from space to not space, so front border of name
    // save pointer to front
    if (prev_is_space && !curr_is_space) {
      elem_coord_names[num_elem_coord_names]=elem_coord_attr+i;
      num_elem_coord_names++;
    } 
    
    // Going from non-space to space, so end of name
    // mark with null terminator
    if (!prev_is_space && curr_is_space) {
      elem_coord_attr[i] = '\0';
    } 
    
    // Change prev to curr
    prev_is_space=curr_is_space;
  }

  // Add null terminator to end of whole attribute to terminate last name
  elem_coord_attr[len_elem_coord_attr]='\0';

  // Make sure the number of names matches the dim of the mesh
  if (num_elem_coord_names != dim) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Mesh has more names in elem_coordinates attribute than the dimension of the mesh.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Debug Output
  //for (int n=0; n<num_elem_coord_names; n++) {
  //  printf("%d %s \n",n,elem_coord_names[n]);
  //}

  // Loop getting elem coordinate ids from names
  for (int n=0; n<num_elem_coord_names; n++) {
    
    piorc = PIOc_inq_varid(pioFileDesc, elem_coord_names[n], elemCoord_ids+n);
    if (!CHECKPIOERROR(piorc, std::string("Error finding elem coordinate variable in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
  }

  // Mark as present
  centerCoordsPresent=1;


  // Free name memory
  delete [] elem_coord_attr;
  delete [] elem_coord_names;
}


// Get mask from a variable in a UGRID format file
// This was set up to work for either element or node masks. The variable just has
// to have the correct size, and the global count and ids have to be for 
// the correct entity (either elems or nodes).
void get_mask_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                              char *maskVarName, 
                              PIO_Offset global_count, 
                              int num_ids, int *ids, 
                              int *&mask) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_mask_from_UGRID_file()"

  // Declare some useful vars
  int dimid;
  int mask_id;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

/* XMRKX */
  // Init mask to NULL (for the case it isn't present)
  mask=NULL; 

  // Get id for mask variable
  piorc = PIOc_inq_varid(pioFileDesc, maskVarName, &mask_id);
  if (!CHECKPIOERROR(piorc, std::string("Error finding mask variable: ") + maskVarName,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

  // Make sure that it has the correct size
  int num_dims;
  piorc = PIOc_inq_varndims(pioFileDesc, mask_id, &num_dims);
  if (!CHECKPIOERROR(piorc, std::string("Error finding number of dims of mask variable: ") + maskVarName,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Error if number of dims is bigger than supported
  if (num_dims > ESMF_MAXDIM) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Mask variable has more dimensions that are supported.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get dim ids
  int dim_ids[ESMF_MAXDIM];
  piorc = PIOc_inq_vardimid(pioFileDesc, mask_id, dim_ids);
  if (!CHECKPIOERROR(piorc, std::string("Error finding the dim ids of the mask variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Get the size of the dimensions
  int dim_sizes[ESMF_MAXDIM];
  for (int d=0; d<num_dims; d++) {
    // Get dim length
    PIO_Offset dim_size_po;
    piorc = PIOc_inq_dimlen(pioFileDesc, dim_ids[d], &dim_size_po);
    if (!CHECKPIOERROR(piorc, std::string("Error reading dimension from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;  

    // Convert to int
    dim_sizes[d]=(int)dim_size_po;
  }

  // If the last dime doesn't match, then complain
  if (dim_sizes[num_dims-1] != global_count) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Mask variable not the correct size. It might not be built on the correct location (e.g. nodes) for the location of masking specified by maskFlag, or maybe the dimesnions aren't in the correct order.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }



  // Define offsets for mask decomp for the first level 
  // (If there are multiple levels for the variable we just use the first one)
  PIO_Offset *em_offsets=new PIO_Offset[num_ids];
  for (int i=0; i<num_ids; i++) {
    em_offsets[i] = (PIO_Offset)ids[i];
  }
    
  // Init elem mask decomp
  int em_iodesc;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, num_dims, dim_sizes, num_ids, em_offsets, &em_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for element mask from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
  // Get rid of offsets
  delete [] em_offsets;
  
  // Allocate space for mask variable
  double *mask_vals=new double[num_ids];
  
  piorc = PIOc_setframe(pioFileDesc, mask_id, -1);
  if (!CHECKPIOERROR(piorc, std::string("Error setting frame for mask variable ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  // Get mask from file
  piorc = PIOc_read_darray(pioFileDesc, mask_id, em_iodesc, num_ids, mask_vals);
  if (!CHECKPIOERROR(piorc, std::string("Error reading mask variable from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of mask decomp
  piorc = PIOc_freedecomp(pioSystemDesc, em_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing mask decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


  // Get missing value
  double missing_val;

  // Try getting _FillValue this is correct as of 2/28/2013
  piorc = PIOc_get_att_double(pioFileDesc, mask_id, "_FillValue", &missing_val);

  // If _FillValue didn't work, then try the old style: missing_value
  if (piorc != PIO_NOERR) {
    
    // Try getting missing_value
    piorc = PIOc_get_att_double(pioFileDesc, mask_id, "missing_value", &missing_val);
    if (!CHECKPIOERROR(piorc, std::string("Couldn't find _FillValue or missing_value attribute on mask variable in file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  }

  // Allocate mask array
  mask=new int[num_ids];

  // Loop through and mask out locations that match missing value
  for (int i=0; i<num_ids; i++) {
    if (mask_vals[i] == missing_val) mask[i]=0;
    else mask[i]=1;
  }



  // Get rid of mask values
  delete [] mask_vals;
}

#endif // ifdef ESMF_PIO

