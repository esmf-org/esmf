// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
#include "IO/include/ESMCI_PIO_Handler.h"

#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif
#include <pio.h>
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

void CheckPIOError(int err, char *_str, int rc_code, int rc,bool warn) {
    if(err != PIO_NOERR)
        printf("err = %d str = %s\n",err, _str);
}

struct NODE_INFO {
  int node_id;
  int local_elem_conn_pos;

  bool operator< (const NODE_INFO &rhs) const {
    return node_id < rhs.node_id;
  }

};


// Note that local_elem_conn is base-1 (as expected by mesh create routines)
void convert_global_elem_conn_to_local_node_and_elem_info(int num_local_elem, int tot_num_elem_conn, int *num_elem_conn, int *global_elem_conn,
                                                                  int &num_node, int*& node_ids, int*& local_elem_conn) {
  // Init output
  num_node=0;
  node_ids=NULL;
  local_elem_conn=NULL;

  // If nothing to do leave
  if (tot_num_elem_conn < 1) return;

  // Allocate conversion list
  NODE_INFO *convert_list=new NODE_INFO[tot_num_elem_conn];

  // Copy global elem connection info into conversion list
  for (int i=0; i<tot_num_elem_conn; i++) {
    convert_list[i].node_id=global_elem_conn[i];
    convert_list[i].local_elem_conn_pos=i;
  }

  // Sort list by node_id, to make it easy to find unique node_ids
  std::sort(convert_list,convert_list+tot_num_elem_conn);

  // Count number of unique node ids in  convert_list
  int num_unique_node_ids=1;                 // There has to be at least 1, 
  int prev_node_id=convert_list[0].node_id;  // because we leave if < 1 above
  for (int i=1; i<tot_num_elem_conn; i++) {

    // If not the same as the last one count a new one
    if (convert_list[i].node_id != prev_node_id) {
      num_unique_node_ids++;
      prev_node_id=convert_list[i].node_id;
    }
  }

  // Allocate node_ids
  node_ids=new int[num_unique_node_ids];

  // Set output number of nodes
  num_node=num_unique_node_ids;

  // Allocate local elem conn
  local_elem_conn=new int[tot_num_elem_conn];

  // Translate convert_list to node_ids and local_elem_conn
  int node_ids_pos=0;                             // There has to be at least 1, 
  node_ids[node_ids_pos]=convert_list[0].node_id; // because we leave if < 1 above
  local_elem_conn[convert_list[0].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  for (int i=1; i<tot_num_elem_conn; i++) {

    // If not the same as the last one add a new one
    if (convert_list[i].node_id != node_ids[node_ids_pos]) {
      node_ids_pos++;
      node_ids[node_ids_pos]=convert_list[i].node_id; 
    }

    // Add an entry for this in local_elem_conn
    local_elem_conn[convert_list[i].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  }


  // Get rid of conversion list
  delete [] convert_list;    
}


// Divide num_ids as evenly as possible across pet_count pets, return min_id and max_id as the part 
// of the range on local_pet. Note that the ids start on 1, so the global range of ids is 1 to num_ids.
// If pet_count > num_ids, then the empty PETs will have min_id > max_id
void divide_ids_evenly_as_possible(int num_ids, int local_pet, int pet_count, int &min_id, int &max_id) {

  // Approx. number per PET
  int num_per_pet=num_ids/pet_count;
  
  // Remainder from even division
  int remainder=num_ids-num_per_pet*pet_count;

  // Figure out tentative range for this PET
  min_id=local_pet*num_per_pet+1;
  max_id=(local_pet+1)*num_per_pet;

  // Add in remainder (1 per PET) to bottom remainder PETs
  if (local_pet < remainder) min_id += local_pet;
  else min_id += remainder;

  if (local_pet < remainder) max_id += local_pet+1;
  else max_id += remainder;
}


void get_coordDim_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &coordDim) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_coordDim_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Get global coordDim
  piorc = PIOc_inq_dimid(pioFileDesc, "coordDim", &dimid);
  if (!CHECKPIOERROR(piorc, std::string("Error reading coordDim from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &coordDim);
  if (!CHECKPIOERROR(piorc, std::string("Error reading coordDim length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

}


void get_elementCount_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &elementCount) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_elementCount_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Get elementCount from file
  piorc = PIOc_inq_dimid(pioFileDesc, "elementCount", &dimid);
  if (!CHECKPIOERROR(piorc, std::string("Error reading  elementCount dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  
  piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &elementCount);
  if (!CHECKPIOERROR(piorc, std::string("Error reading  elementCount dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
}

void get_nodeCount_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &nodeCount) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCount_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Get nodeCount from file
  piorc = PIOc_inq_dimid(pioFileDesc, "nodeCount", &dimid);
  if (!CHECKPIOERROR(piorc, std::string("Error reading nodeCount dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  
  piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &nodeCount);
  if (!CHECKPIOERROR(piorc, std::string("Error reading nodeCount dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
}


void get_coordsys_from_ESMFMesh_file(int pioFileDesc, char *filename, ESMC_CoordSys_Flag &coord_sys) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_coordsys_from_ESMFMesh_file()"


  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;

  // Get variable id for nodeCoords
  piorc = PIOc_inq_varid(pioFileDesc, "nodeCoords", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error with finding nodeCoords variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;


  // Get information about the units attribute
  nc_type type;
  PIO_Offset len;
  piorc = PIOc_inq_att(pioFileDesc, varid, "units", &type, &len);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from nodeCoords in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Debug
  //printf("units len=%d\n",len);

  // Get units attribute
  char *units=new char[len+1]; // +1 for NULL terminator (sometimes the len from PIO doesn't seem to include that so adding just in case)
  piorc = PIOc_get_att_text(pioFileDesc, varid, "units", units);
  if (!CHECKPIOERROR(piorc, std::string("Error with getting units attribute from nodeCoords in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

  // Add null terminator
  units[len]='\0'; 

  // Debug
  //printf("units value=%s\n",units);


  // Look at units to determine coordinate system
  // Using strncmp to only compare chars that match
  // the unit name because sometimes the units can
  // have garbage on the end.
  if (strncmp(units,"degrees",7) == 0) {
    coord_sys=ESMC_COORDSYS_SPH_DEG;
  } else if (strncmp(units,"radians",7) == 0) {
    coord_sys=ESMC_COORDSYS_SPH_RAD;
  } else if (strncmp(units,"km",2) == 0) {
    coord_sys=ESMC_COORDSYS_CART;
  } else if (strncmp(units,"kilometers",10) == 0) {
    coord_sys=ESMC_COORDSYS_CART;
  } else if (strncmp(units,"m",1) == 0) {
    coord_sys=ESMC_COORDSYS_CART;
  } else if (strncmp(units,"meters",6) == 0) {
    coord_sys=ESMC_COORDSYS_CART;
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " unrecognized units for nodeCoords",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Debug
  //printf("coordsys=%d\n",coord_sys);

  // Get rid of units string
  delete [] units;

}

// Get elementConn info out of a 1D variable in an ESMFMesh file. 
void get_elemConn_info_1Dvar_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename,
                                                PIO_Offset elementCount, int num_elem, int *elem_ids, 
                                                int &totNumElementConn, int *&numElementConn, int *&elementConn) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_elemConn_info_1Dvar_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;
  int varid;
  int rearr = PIO_REARR_SUBSET;

  // Get VM 
  ESMCI::VM *vm=VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &localrc)) throw localrc;

  // Get VM info
  int local_pet = vm->getLocalPet();  
  MPI_Comm mpi_comm = vm->getMpi_c();  
  int pet_count = vm->getPetCount();


  // Getting elementConn from 1D list is more complicated than 2D.
  // The potential different size of each element connection means that the list won't be regular, 
  // so you can't index directly into the list. Instead we have to first calculate the offsets
  // into the lists by summing the sizes up to each element and then using those to index into
  // the elementConn array.

  // THINK ABOUT ADDING AN OPTIMIZATION To handle the default case (where elem_ids match the min_ed_id, etc.) then we
  // may be able to skip some of the work below.


  //// First read in numElementConn at evenly divided positions ////

  // Get even division of ids
  int min_ed_id, max_ed_id;
  divide_ids_evenly_as_possible(elementCount, local_pet, pet_count, min_ed_id, max_ed_id);
  
  // Get number of ids on this PET for even division
  int num_ed_ids=max_ed_id-min_ed_id+1;
  if (num_ed_ids < 0) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "Number of ids on this PET < 0. Could be from creating a Mesh from a file in a VM when that VM contains more PETs than element in the file.",
         ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // DEBUG OUTPUT
  // printf("%d# num_ed_ids=%d\n",local_pet,num_ed_ids);

  // Define offsets for numElementConn with even decomp
  PIO_Offset *nec_ed_offsets=new PIO_Offset[num_ed_ids];
  for (int id=min_ed_id,pos=0; id <= max_ed_id; id++) {
    nec_ed_offsets[pos] = (PIO_Offset)id;
    pos++;
  }

  // Init numElementConn even decomp
  int nec_ed_iodesc;
  int gdimlen = (int) elementCount;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_BYTE, 1, &gdimlen, num_ed_ids, nec_ed_offsets, &nec_ed_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] nec_ed_offsets;
  
  // Get numElementConn
  piorc = PIOc_inq_varid(pioFileDesc, "numElementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error NumElementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  char *char_ed_numElementConn=new char[num_ed_ids];
  piorc = PIOc_read_darray(pioFileDesc, varid, nec_ed_iodesc, num_ed_ids, char_ed_numElementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of numElementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, nec_ed_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing nodeCoord decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;



  //// Now that we have the local list of sizes for evenly divided parts calculate the total offsets for the elements
  
  // Total this PETs size
  int local_tot_ed_numElementConn=0;
  for (int i=0; i<num_ed_ids; i++) {
    local_tot_ed_numElementConn += (int)char_ed_numElementConn[i];
  }  
  
  // DEBUG OUTPUT
  // printf("%d# local_tot_ed_numElementConn=%d\n",local_pet,local_tot_ed_numElementConn);

  // Figure out the start of each PET
  int local_start_ed_offsets=0;
  MPI_Scan(&local_tot_ed_numElementConn,&local_start_ed_offsets,1,MPI_INT,MPI_SUM,mpi_comm);

  // Remove this processor's number from the sum to get the beginning
  local_start_ed_offsets=local_start_ed_offsets-local_tot_ed_numElementConn;

  // Allocate arrays of offsets to pass into DDir
  UInt *ed_gids=new UInt[num_ed_ids];
  UInt *ed_offsets=new UInt[num_ed_ids];

  // Fill rest of positions of arrays of offsets to pass into DDir
  int partial_sum=0;
  for (int i=0; i < num_ed_ids; i++) {
    ed_gids[i] = (UInt)(min_ed_id+i);
    ed_offsets[i]=(UInt)(local_start_ed_offsets + partial_sum);
    partial_sum += (int)(char_ed_numElementConn[i]);
  }

  
  // Get rid of char version
  delete [] char_ed_numElementConn;

  // DEBUG OUTPUT
  //  for (int i=0; i < num_ed_ids; i++) {
  //  printf("%d# elem id=%d offset=%d\n",local_pet,ed_gids[i],ed_offsets[i]);
  //}



  //// Get the offsets for the elem_ids on this PET

  // Set even division offsets in a DDir so each PET can get the ones they want in parallel
  DDir<> dir;
  dir.Create(num_ed_ids, ed_gids, ed_offsets);

  // Free unneeded memory
  delete [] ed_gids;
  delete [] ed_offsets;

  // Allocate arrays of requests to pass into DDir
  UInt *elem_uint_ids=new UInt[num_elem]; // Needed because DDir wants ids in UInt
  UInt *elem_offset_pets=new UInt[num_elem]; // Not used, but pets where offset was calc'd
  UInt *elem_offsets=new UInt[num_elem]; // Offset of start of elem in file
  
  // Fill array of requests
  for (int i=0; i<num_elem; i++) {

    // Fill requests
    elem_uint_ids[i]=(UInt)elem_ids[i];

    // Init to defaults
    elem_offset_pets[i]=0;
    elem_offsets[i]=0;
  }

  // Get offsets from DDir
  dir.RemoteGID(num_elem, elem_uint_ids, elem_offset_pets, elem_offsets);

  // Free unneeded memory
  delete [] elem_uint_ids;
  delete [] elem_offset_pets;



  //// Get numElementConn at elem_ids positions and totNumElementConn

  // Define offsets numElementConn decomp
  PIO_Offset *nec_offsets=new PIO_Offset[num_elem];
  for (int i=0; i<num_elem; i++) {
    nec_offsets[i] = (PIO_Offset)elem_ids[i];
  }
  
  // Init numElementConn decomp
  int nec_iodesc;
  int nec_gdimlen = (int) elementCount;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_BYTE, 1, &nec_gdimlen, num_elem, nec_offsets, &nec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] nec_offsets;
  
  // Get numElementConn
  piorc = PIOc_inq_varid(pioFileDesc, "numElementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error NumElementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  char *char_numElementConn=new char[num_elem];
  piorc = PIOc_read_darray(pioFileDesc, varid, nec_iodesc, num_elem, char_numElementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of numElementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, nec_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing numElementConn decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  // Copy to int array
  numElementConn=new int[num_elem];
  for (int i=0; i<num_elem; i++) {
    numElementConn[i]=(int)char_numElementConn[i];
  }
  
  // Get rid of char version
  delete [] char_numElementConn;

  // Get total number of connections on this PET
  totNumElementConn=0;
  for (int i=0; i<num_elem; i++) {
    totNumElementConn += numElementConn[i];
  }



  //// Get elementConn at elem_ids positions

  // Define offsets for elementConn decomp
  // (Only define offsets for valid elementConn entries)
  PIO_Offset *ec_offsets=new PIO_Offset[totNumElementConn];
  for (int i=0,pos=0; i<num_elem; i++) {
    int elem_start_ind=(int)elem_offsets[i]+1; // +1 to make base-1
    for (int j=0; j<numElementConn[i]; j++) {
      ec_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
      pos++;
    }
  }  

  delete [] elem_offsets;

  // DEBUG OUTPUT
//   for (int i=0,pos=0; i<num_elem; i++) {
//     printf("elem id=%d \n",elem_ids[i]);
//     for (int j=0; j<numElementConn[i]; j++) {
//       printf("   %d  %d \n",j,ec_offsets[pos]);
//       pos++;
//     }
//   }  


  // Get connectionCount
  piorc = PIOc_inq_dimid(pioFileDesc, "connectionCount", &dimid);
  if (!CHECKPIOERROR(piorc, std::string("Error reading connectionCount dimension from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  PIO_Offset connectionCount;
  piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &connectionCount);
  if (!CHECKPIOERROR(piorc, std::string("Error reading connectionCount dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;


  // Init elementConn decomp
  int ec_iodesc;
  int ec_gdimlen=connectionCount;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 1, &ec_gdimlen, totNumElementConn, ec_offsets, &ec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] ec_offsets;
  
  
  // Get elementConn
  piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  elementConn= new int[totNumElementConn];
  piorc = PIOc_read_darray(pioFileDesc, varid, ec_iodesc, totNumElementConn, elementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading variable elementConn from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  // Get rid of elementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, ec_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing elementConn decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

}

// Get elementConn info out of a 2D variable in an ESMFMesh file. 
// Note that the output is still a 1D array (the array it collapsed internally).
void get_elemConn_info_2Dvar_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, PIO_Offset elementCount, int num_elem, int *elem_ids, 
                                                 int &totNumElementConn, int *&numElementConn, int *&elementConn) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_elemConn_info_2Dvar_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;
  int varid;
  int rearr = PIO_REARR_SUBSET;


  // Define offsets numElementConn decomp
  PIO_Offset *nec_offsets=new PIO_Offset[num_elem];
  for (int i=0; i<num_elem; i++) {
    nec_offsets[i] = (PIO_Offset)elem_ids[i];
  }
  
  // Init numElementConn decomp
  int nec_iodesc;
  int gdimlen = (int) elementCount;
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_BYTE, 1, &gdimlen, num_elem, nec_offsets, &nec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] nec_offsets;
  
  // Get numElementConn
  piorc = PIOc_inq_varid(pioFileDesc, "numElementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error NumElementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  char *char_numElementConn=new char[num_elem];
  piorc = PIOc_read_darray(pioFileDesc, varid, nec_iodesc, num_elem, char_numElementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of numElementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, nec_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing numElementConn decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  // Copy to int array
  numElementConn=new int[num_elem];
  for (int i=0; i<num_elem; i++) {
    numElementConn[i]=(int)char_numElementConn[i];
  }
  
  // Get rid of char version
  delete [] char_numElementConn;

  // Get total number of connections on this PET
  totNumElementConn=0;
  for (int i=0; i<num_elem; i++) {
    totNumElementConn += numElementConn[i];
  }


  // Get maxNodePElement
  piorc = PIOc_inq_dimid(pioFileDesc, "maxNodePElement", &dimid);
  if (!CHECKPIOERROR(piorc, std::string("Error reading maxNodePElement dimension from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  PIO_Offset maxNodePElement;
  piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &maxNodePElement);
  if (!CHECKPIOERROR(piorc, std::string("Error reading maxNodePElement dimension length from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  

  // Define offsets for elementConn decomp
  // (Only define offsets for valid elementConn entries)
  PIO_Offset *ec_offsets=new PIO_Offset[totNumElementConn];
  for (int i=0,pos=0; i<num_elem; i++) {
    int elem_start_ind=(elem_ids[i]-1)*maxNodePElement+1; // +1 to make base-1
    for (int j=0; j<numElementConn[i]; j++) {
      ec_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
      pos++;
    }
  }
  

  // Init elementConn decomp
  int ec_iodesc;
  int gdimlen2D[2]={elementCount,maxNodePElement};
  piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 2, gdimlen2D, totNumElementConn, ec_offsets, &ec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] ec_offsets;
  
  
  // Get elementConn
  piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  elementConn= new int[totNumElementConn];
  piorc = PIOc_read_darray(pioFileDesc, varid, ec_iodesc, totNumElementConn, elementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error reading variable elementConn from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
  // Get rid of elementConn decomp
  piorc = PIOc_freedecomp(pioSystemDesc, ec_iodesc);
  if (!CHECKPIOERROR(piorc, std::string("Error freeing elementConn decomp "),
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
}

// Get elemConn info as 1D arrays transparent to what elemConn variables look like in file (dim, etc.)
void get_elemConn_info_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, PIO_Offset elementCount, int num_elem, int *elem_ids, 
                                           int &totNumElementConn, int *&numElementConn, int *&elementConn) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_elemConn_info_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;

  // Figure out dimension of elementConn array
  int dimElementConn=2;

  // Get elementConn varid
  piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

  // Get elementConn number of dims
  piorc = PIOc_inq_varndims(pioFileDesc, varid, &dimElementConn);
  if (!CHECKPIOERROR(piorc, std::string("Error getting number of dims for elementConn variable in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


  // Get elementConn depending on dimension
  if (dimElementConn == 1) {
    get_elemConn_info_1Dvar_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename,
                                               elementCount, num_elem, elem_ids, 
                                               totNumElementConn, numElementConn, elementConn);
  } else if (dimElementConn == 2) {
    get_elemConn_info_2Dvar_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, elementCount, num_elem, elem_ids, 
                                                totNumElementConn, numElementConn, elementConn);
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " Only 1D or 2D elementConn variables supported in ESMFMesh format.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  
  //     // DEBUG: output numElementConn
  //     printf("numElementConn=");
//       for (int i=0; i<num_elem; i++) {
//         printf(" [%d]=%d ",elem_ids[i],numElementConn[i]);
//       }
//       printf("\n");
  

  //     // DEBUG: output elementConn
  //     // NEED TO CHANGE THIS SO IT WORKS WITH THE NEW 1D elementConn
//       int pos=0;
//       for (int i=0; i<num_elem; i++) {
//         printf(" [%d] =",elem_ids[i]);
//         for (int j=0; j<(int)numElementConn[i]; j++) {
//           printf(" %d ",elementConn[pos]);
//           pos++;
//         }
//         printf("\n");
//       }
//       printf("\n");
  
}

// Get nodeCoords from ESMFMesh format file
void get_nodeCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                       PIO_Offset nodeCount, PIO_Offset coordDim, 
                                       int num_node, int *node_ids, 
                                       double *&nodeCoords) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCoords_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Define offsets for nodeCoord decomp
  PIO_Offset *node_offsets= new PIO_Offset[num_node*coordDim];
  for (int i=0,pos=0; i<num_node; i++) {
    int node_start_ind=(node_ids[i]-1)*coordDim+1;
    for (int j=0; j<coordDim; j++) {
      node_offsets[pos] = (PIO_Offset) (node_start_ind+j);
      pos++;
    }
  }

    // Init nodeCoords decomp
    int node_iodesc;
    int node_gdimlen2D[2]={nodeCount, coordDim};
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, 2, node_gdimlen2D, num_node*coordDim, node_offsets, &node_iodesc, 
                    &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get rid of node offsets
    delete [] node_offsets;

    // Get variable id for nodeCoords
    piorc = PIOc_inq_varid(pioFileDesc, "nodeCoords", &varid);
    if (!CHECKPIOERROR(piorc, std::string("Error nodeCoords variable not in file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get nodeCoords
    nodeCoords= new double[num_node*coordDim];
    piorc = PIOc_read_darray(pioFileDesc, varid, node_iodesc, num_node*coordDim, nodeCoords);
    if (!CHECKPIOERROR(piorc, std::string("Error reading variable nodeCoords from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Get rid of nodeCoords decomp
    piorc = PIOc_freedecomp(pioSystemDesc, node_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing nodeCoord decomp "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
}


// Get nodeMask from ESMFMesh format file
// if nodeMask isn't present in file, then it will be set to NULL
void get_nodeMask_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                       PIO_Offset nodeCount, 
                                       int num_node, int *node_ids, 
                                       int *&nodeMask) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeMask_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Init nodeMask to NULL (for the case it isn't present)
  nodeMask=NULL; 

  // Check if mask is present
  piorc = PIOc_inq_varid(pioFileDesc, "nodeMask", &varid);
  
  // If mask is present, then get it
  if (piorc == PIO_NOERR) {
    
      // Define offsets for nodeMask decomp
      PIO_Offset *nm_offsets=new PIO_Offset[num_node];
      for (int i=0; i<num_node; i++) {
        nm_offsets[i] = (PIO_Offset)node_ids[i];
      }

      // Init nodeMask decomp
      int nm_iodesc;
      int nm_gdimlen = (int) nodeCount;
      piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 1, &nm_gdimlen, num_node, nm_offsets, &nm_iodesc, 
                              &rearr, NULL, NULL);
      if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for nodeMask ") + filename,
                         ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

      // Get rid of offsets
      delete [] nm_offsets;

      // Allocate space
      nodeMask=new int[num_node];

      // Get mask from file
      piorc = PIOc_read_darray(pioFileDesc, varid, nm_iodesc, num_node, nodeMask);
      if (!CHECKPIOERROR(piorc, std::string("Error reading nodeMask variable from file ") + filename,
                         ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

      // Get rid of nodeMask decomp
      piorc = PIOc_freedecomp(pioSystemDesc, nm_iodesc);
      if (!CHECKPIOERROR(piorc, std::string("Error freeing nodeMask decomp "),
                         ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  }  
}



// Get elementMask from ESMFMesh format file
// if elementMask isn't present in file, then it will be set to NULL
void get_elementMask_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                     PIO_Offset elementCount, 
                                     int num_elem, int *elem_ids, 
                                     int *&elementMask) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementMask_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Init elementMask to NULL (for the case it isn't present)
  elementMask=NULL; 

  // Check if mask is present
  piorc = PIOc_inq_varid(pioFileDesc, "elementMask", &varid);


  // If mask is present, then get it
  if (piorc == PIO_NOERR) {
    
    // Define offsets for elementMask decomp
    PIO_Offset *em_offsets=new PIO_Offset[num_elem];
    for (int i=0; i<num_elem; i++) {
      em_offsets[i] = (PIO_Offset)elem_ids[i];
    }
    
    // Init elementMask decomp
    int em_iodesc;
    int em_gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 1, &em_gdimlen, num_elem, em_offsets, &em_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for elementMask ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of offsets
    delete [] em_offsets;
    
    // Allocate space
    elementMask=new int[num_elem];
    
    // Get mask from file
    piorc = PIOc_read_darray(pioFileDesc, varid, em_iodesc, num_elem, elementMask);
    if (!CHECKPIOERROR(piorc, std::string("Error reading elementMask variable from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of elementMask decomp
    piorc = PIOc_freedecomp(pioSystemDesc, em_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing elementMask decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  }
  
}


// Get elementArea from ESMFMesh format file
// if elementArea isn't present in file, then it will be set to NULL
// If elementArea is present in file, areaPresent will be 1 (0 if not present)
// (areaPresent is also used, because if num_elem=0 on this PET then elementArea could
// be NULL even though elementArea is present, so that variable is used to cover that case)
void get_elementArea_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                        PIO_Offset elementCount, 
                                        int num_elem, int *elem_ids, 
                                        int &areaPresent, double *&elementArea) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementArea_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Init elementArea to NULL (for the case it isn't present)
  elementArea=NULL; 

  // Init areaPresent to 0 (for the case it isn't present)
  areaPresent=0;

  // See if area present
  piorc = PIOc_inq_varid(pioFileDesc, "elementArea", &varid);
  
  // If area is present, then get it
  if (piorc == PIO_NOERR) {
    
    // Define offsets for elementArea decomp
    PIO_Offset *ea_offsets=new PIO_Offset[num_elem];
    for (int i=0; i<num_elem; i++) {
      ea_offsets[i] = (PIO_Offset)elem_ids[i];
    }
    
    // Init elementArea decomp
    int ea_iodesc;
    int ea_gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, 1, &ea_gdimlen, num_elem, ea_offsets, &ea_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for elementMask ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of offsets
    delete [] ea_offsets;
    
    // record that area present
    areaPresent=1;
    
    // Allocate space to hold area
    elementArea=new double[num_elem];
    
    // Get area from file
    piorc = PIOc_read_darray(pioFileDesc, varid, ea_iodesc, num_elem, elementArea);
    if (!CHECKPIOERROR(piorc, std::string("Error reading elementArea variable from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Get rid of elementArea decomp
    piorc = PIOc_freedecomp(pioSystemDesc, ea_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing elementArea decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  }  
}



// Get centerCoords from ESMFMesh format file
// if centerCoords isn't present in file, then it will be set to NULL
// If centerCoords is present in file, centerCoordsPresent will be 1 (0 if not present)
// (centerCoordsPresent is also used, because if num_elem=0 on this PET then centerCoords could
// be NULL even though centerCoords is present, so that variable is used to cover that case)
void get_centerCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                         PIO_Offset elementCount, PIO_Offset coordDim, 
                                         int num_elem, int *elem_ids, 
                                         int &centerCoordsPresent, double *&centerCoords) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_centerCoords_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Init centerCoords to NULL (for the case it isn't present)
  centerCoords=NULL; 

  // Init centerCoordsPresent to 0 (for the case it isn't present)
  centerCoordsPresent=0;

  // Check if centerCoords are present
  piorc = PIOc_inq_varid(pioFileDesc, "centerCoords", &varid);
  
  // If centerCoords are present, then get them
  if (piorc == PIO_NOERR) {
    
    // Define offsets for centerCoords decomp
    PIO_Offset cc_offsets[num_elem*coordDim];
    for (int i=0,pos=0; i<num_elem; i++) {
      int elem_start_ind=(elem_ids[i]-1)*coordDim+1;
      for (int j=0; j<coordDim; j++) {
        cc_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
        pos++;
      }
    }
    
    // Init elementConn decomp
    int cc_iodesc;
    int cc_gdimlen2D[2]={elementCount,coordDim};
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, 2, cc_gdimlen2D, num_elem*coordDim, cc_offsets, &cc_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for centerCoords ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Record the centerCoords present
    centerCoordsPresent=1;
    
    // Allocate space
    centerCoords=new double[num_elem*coordDim];
    
    // Get the centerCoords
    piorc = PIOc_read_darray(pioFileDesc, varid, cc_iodesc, num_elem*coordDim, centerCoords);
    if (!CHECKPIOERROR(piorc, std::string("Error reading variable centerCoords from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Get rid of elementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, cc_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing centerCoords decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // DEEBUG: output centerCoords
    //     for(int i=0; i<num_elem; i++) {
    // printf("%f %f\n",centerCoords[i*coordDim],centerCoords[i*coordDim+1]);
    //}
  }
}

// This gets a list of ids from a distgrid 
void get_ids_from_distgrid(ESMCI::DistGrid *distgrid, std::vector<int> &ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_ids_from_distgrid()"

  // Sanity check
  ThrowRequire(distgrid != NULL);

  // Declare some useful variables
  int localrc;

  // Currently only support distgrids with 1 localDE
  if (distgrid->getDELayout()->getLocalDeCount() != 1) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      " Currently only distgrids with 1 DE per PET are supported",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Get seqIndexList
  // TODO: right now assumes 1 localDE, fix this
  localrc=distgrid->fillSeqIndexList(ids, 0);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &localrc)) throw localrc;
}

// This generates a list of ids divided as evenly as possible across pets and returns a 
// vector containing the list of the ones on the local pet.
void get_ids_divided_evenly_across_pets(int num_ids, int local_pet, int pet_count, std::vector<int> &ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_ids_divided_evenly_across_pets()"

  // No distgrid provided so divide things up equally
  int min_id, max_id;
  divide_ids_evenly_as_possible(num_ids, local_pet, pet_count, min_id, max_id);
  
  //printf("%d# min,max ids=%d %d num=%d\n",local_pet,min_id,max_id,max_id-min_id+1);
  
  // Reserve space for ids
  ids.reserve(max_id-min_id+1);
  
  // Fill ids
  for (int id=min_id; id <= max_id; id++) {
    ids.push_back(id);
  }
}



// INPUTS: 
//  filename - file name in NULL delimited form
//  fileformat - the format of the file
//  convert_to_dual - specifies if mesh should be converted to dual before returning
//                    if NULL, then user didn't speficify so default to NOT
//  add_user_area - specifies if areas should be added to mesh. 
//                  If NULL, then user didn't specify, so don't
//  node_distgrid - If not NULL, redist so nodes are on this distgrid
//  elem_distgrid - If not NULL, redist so elems are on this distgrid
//
// OUTPUTS:
//   out_mesh - the new mesh created from the file
//   rc       - the return code
//
void ESMCI_mesh_create_from_file(char *filename, 
                                 ESMC_FileFormat_Flag fileformat, 
                                 bool convert_to_dual, bool add_user_area, 
                                 ESMCI::DistGrid *node_distgrid, 
                                 ESMCI::DistGrid *elem_distgrid, 
                                 Mesh **out_mesh, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_file()"

  //  printf("in new scalable mesh create from file filename=%s\n",filename);


  // Try-catch block around main part of method
  try {
    // local return code
    int localrc;

    //// Error check some unhandled options

    // Only support ESMFMesh right now
    if (fileformat != ESMC_FILEFORMAT_ESMFMESH) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " Only ESMFMesh format files supported right now.",
                           ESMC_CONTEXT, &localrc)) throw localrc;
    }    

    // Don't currently support redisting to node_distgrid
    if (node_distgrid != NULL) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " redistribution to node DistGrid currently not supported.",
                           ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Don't currently support converting to dual
    if (convert_to_dual) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " converting to dual currently not supported.",
                           ESMC_CONTEXT, &localrc)) throw localrc;
    }



    //// Set up PIO

    // Set pio_type based on what's available
#ifdef ESMF_PNETCDF
    int pio_type = PIO_IOTYPE_PNETCDF;
#else
    int pio_type = PIO_IOTYPE_NETCDF;
#endif

    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get VM info
    int local_pet = vm->getLocalPet();  
    MPI_Comm mpi_comm = vm->getMpi_c();  
    int pet_count = vm->getPetCount();
    int pets_per_Ssi = vm->getSsiLocalPetCount();


    /// Initialize IO system
    int num_iotasks = pet_count/pets_per_Ssi;
    int stride = pets_per_Ssi;
    int pioSystemDesc;
    int piorc;
    piorc = PIOc_Init_Intracomm(mpi_comm, num_iotasks, stride, 0, PIO_REARR_SUBSET, &pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Unable to init PIO Intracomm for file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Open file
    int pioFileDesc;
    int mode = 0;
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    if (!CHECKPIOERROR(piorc, std::string("Unable to open existing file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    piorc = PIOc_Set_File_Error_Handling(pioFileDesc, PIO_RETURN_ERROR);
    //    if (!CHECKPIOERROR(piorc, std::string("Unable to set PIO error handling for file: ") + filename,
    //                   ESMF_RC_FILE_OPEN, localrc)) throw localrc;



    //// Get information about element distribution 

    // Get global elementCount
    PIO_Offset elementCount;
    int dimid;
    get_elementCount_from_ESMFMesh_file(pioFileDesc, filename, elementCount);

    // Don't currently support more pets than elements
    if (pet_count > elementCount) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " Can't create a Mesh from a file in a VM when that VM contains more PETs than elements in the file.",
                           ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Get positions at which to read element information
    std::vector<int> elem_ids_vec;
    if (elem_distgrid == NULL) {
      get_ids_divided_evenly_across_pets(elementCount, local_pet, pet_count, elem_ids_vec);
    } else {
      // Have elem_distgrid, so get ids from that
      get_ids_from_distgrid(elem_distgrid, elem_ids_vec);
    }

    // Assign vector info to pointer, because PIO and mesh calls don't accept vectors
    int num_elem=0;
    int *elem_ids=NULL;
    if (!elem_ids_vec.empty()) {
      num_elem=elem_ids_vec.size();
      elem_ids=&elem_ids_vec[0];
    } 



    //// Create Mesh 

    // Get coordsys
    ESMC_CoordSys_Flag coord_sys;
    get_coordsys_from_ESMFMesh_file(pioFileDesc, filename, coord_sys);

    // Get coordDim
    PIO_Offset coordDim;
    get_coordDim_from_ESMFMesh_file(pioFileDesc, filename, coordDim);

    // Convert file coordDim into pdim and orig_sdim to use in mesh create
    int pdim, orig_sdim;
    if (coordDim == 2) {
      pdim=orig_sdim=2;
    } else if (coordDim == 3) {
      pdim=orig_sdim=3;
    } else {
      Throw() << "Meshes can only be created with dim=2 or 3.";
    }

    // Create Mesh    
    ESMCI_meshcreate(out_mesh,
                     &pdim, &orig_sdim, &coord_sys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;



    //// Add nodes to Mesh

    // Get element connection info
    int totNumElementConn=0;
    int *numElementConn=NULL, *elementConn=NULL;
    get_elemConn_info_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, elementCount, num_elem, elem_ids, 
                                          totNumElementConn, numElementConn, elementConn);

    // Convert global elem info into node info
    int num_node;
    int *node_ids=NULL;
    int *local_elem_conn=NULL;
    convert_global_elem_conn_to_local_node_and_elem_info(num_elem, totNumElementConn, numElementConn, elementConn,
                                                          num_node, node_ids, local_elem_conn);

    // Free element connection info, because we don't need it any more
    delete [] elementConn;

    // Get global nodeCount
    PIO_Offset nodeCount;
    get_nodeCount_from_ESMFMesh_file(pioFileDesc, filename, nodeCount);


    // Get nodeCoords at positions indicated by node_ids
    double *nodeCoords;
    get_nodeCoords_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                      nodeCount, coordDim, 
                                      num_node, node_ids, 
                                      nodeCoords);

    // Get nodeMask
    // (If not present in file, nodeMask variable will be NULL)
    int *nodeMask=NULL;
    get_nodeMask_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                    nodeCount, 
                                    num_node, node_ids, 
                                    nodeMask);


    // Set up InterArray variable for nodeMask
    // Note that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> nodeMaskIA(nodeMask, num_node);

    // Add nodes
    ESMCI_meshaddnodes(out_mesh, &num_node, node_ids,
                       nodeCoords, NULL, &nodeMaskIA,
                       &coord_sys, &orig_sdim,
                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Get rid of things used for adding nodes
    delete [] node_ids;
    delete [] nodeCoords;
    if (nodeMask != NULL) delete [] nodeMask;



    //// Add elements to Mesh and finish it up

    // Get elementMask
    // (If not present in file, elementMask variable will be NULL)
    int *elementMask=NULL;
    get_elementMask_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                       elementCount, 
                                       num_elem, elem_ids, 
                                       elementMask);

    // Set up InterArray variable for elementMask
    // Note, that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> elementMaskIA(elementMask, num_elem);


    // Get elementArea, if requested and if present in file
    double *elementArea=NULL;
    int areaPresent=0;
    if (add_user_area) {
      get_elementArea_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                         elementCount, 
                                         num_elem, elem_ids, 
                                         areaPresent, elementArea);
    }


    // Get centerCoords, if present in file
    double *centerCoords=NULL;
    int centerCoordsPresent=0;
    get_centerCoords_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                        elementCount, coordDim, 
                                        num_elem, elem_ids, 
                                        centerCoordsPresent, centerCoords);

    // Add elements
    ESMCI_meshaddelements(out_mesh,
                          &num_elem, elem_ids, numElementConn,
                          &elementMaskIA,
                          &areaPresent, elementArea,
                          &centerCoordsPresent, centerCoords, 
                          &totNumElementConn, local_elem_conn, 
                          &coord_sys, &orig_sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Free things used for element creation
    delete [] numElementConn;
    if (elementMask != NULL) delete [] elementMask;
    if (elementArea != NULL) delete [] elementArea;
    if (centerCoords != NULL) delete [] centerCoords;
    delete [] local_elem_conn;


    //// Close down PIO

    // Close file
    piorc = PIOc_closefile(pioFileDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error closing file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


    // maybe free the iosystem here?
    piorc = PIOc_free_iosystem(pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing pio file system description "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    

  } catch(std::exception &x) {

    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // We've gotten to bottom successfully, so return success
  if(rc != NULL) *rc = ESMF_SUCCESS;
}




