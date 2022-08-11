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

// Get numElementConn given offsets.
// Since both Byte and Ints are supported for numElementConn this converts the output to ints
// NOTE: that this method takes offsets rather than the elem_ids that the other reads take.
void get_numElementConn_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                           PIO_Offset elementCount, 
                                           int num_elems, PIO_Offset *nec_offsets,
                                           int *&numElementConn) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_numElementConn_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int varid;
  int localrc;
  int piorc;
  int rearr = PIO_REARR_SUBSET;

  // Get numElementConn var id
  piorc = PIOc_inq_varid(pioFileDesc, "numElementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error NumElementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  // See what type the variable is
  nc_type nEC_type;
  piorc = PIOc_inq_vartype(pioFileDesc, varid, &nEC_type);
  if (!CHECKPIOERROR(piorc, std::string("Error getting type of numElementConn variable from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  
  // Read based on type
  if (nEC_type == NC_BYTE) {
    
    // Init numElementConn decomp
    int nec_iodesc;
    int gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_BYTE, 1, &gdimlen, num_elems, nec_offsets, &nec_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for numElementConn variable ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    char *char_numElementConn=new char[num_elems];
    piorc = PIOc_read_darray(pioFileDesc, varid, nec_iodesc, num_elems, char_numElementConn);
    if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of numElementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, nec_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing numElementConn decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    
    // Copy to int array
    numElementConn=new int[num_elems];
    for (int i=0; i<num_elems; i++) {
      numElementConn[i]=(int)char_numElementConn[i];
    }
    
    // Get rid of char version
    delete [] char_numElementConn;
    
  } else  if (nEC_type == NC_INT) {

    // Init numElementConn decomp
    int nec_iodesc;
    int gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_INT, 1, &gdimlen, num_elems, nec_offsets, &nec_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for numElementConn variable ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    numElementConn=new int[num_elems];
    piorc = PIOc_read_darray(pioFileDesc, varid, nec_iodesc, num_elems, numElementConn);
    if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of numElementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, nec_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing numElementConn decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
        
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      "Unsupported type for numElementConn variable.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

}

// Get elementConn info out of a 1D variable in an ESMFMesh file. 
void get_elemConn_info_1Dvar_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename,
                                                PIO_Offset elementCount, int num_elems, int *elem_ids, 
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

  // Get evenly divided numElementconn
  int *ed_numElementConn;
  get_numElementConn_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                        elementCount, 
                                        num_ed_ids, nec_ed_offsets, 
                                        ed_numElementConn);
  // Get rid of offsets
  delete [] nec_ed_offsets;


  //// Now that we have the local list of sizes for evenly divided parts calculate the total offsets for the elements
  
  // Total this PETs size
  int local_tot_ed_numElementConn=0;
  for (int i=0; i<num_ed_ids; i++) {
    local_tot_ed_numElementConn += ed_numElementConn[i];
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
    partial_sum += ed_numElementConn[i];
  }

  
  // Get rid of evenly divided nums
  delete [] ed_numElementConn;

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
  UInt *elem_uint_ids=new UInt[num_elems]; // Needed because DDir wants ids in UInt
  UInt *elem_offset_pets=new UInt[num_elems]; // Not used, but pets where offset was calc'd
  UInt *elem_offsets=new UInt[num_elems]; // Offset of start of elem in file
  
  // Fill array of requests
  for (int i=0; i<num_elems; i++) {

    // Fill requests
    elem_uint_ids[i]=(UInt)elem_ids[i];

    // Init to defaults
    elem_offset_pets[i]=0;
    elem_offsets[i]=0;
  }

  // Get offsets from DDir
  dir.RemoteGID(num_elems, elem_uint_ids, elem_offset_pets, elem_offsets);

  // Free unneeded memory
  delete [] elem_uint_ids;
  delete [] elem_offset_pets;



  //// Get numElementConn at elem_ids positions and totNumElementConn

  // Define offsets numElementConn decomp
  PIO_Offset *nec_offsets=new PIO_Offset[num_elems];
  for (int i=0; i<num_elems; i++) {
    nec_offsets[i] = (PIO_Offset)elem_ids[i];
  }

  // read from file
  get_numElementConn_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                        elementCount, 
                                        num_elems, nec_offsets, 
                                        numElementConn);  
  // Get rid of offsets
  delete [] nec_offsets;
  
  // Get total number of connections on this PET
  totNumElementConn=0;
  for (int i=0; i<num_elems; i++) {
    totNumElementConn += numElementConn[i];
  }



  //// Get elementConn at elem_ids positions

  // Define offsets for elementConn decomp
  // (Only define offsets for valid elementConn entries)
  PIO_Offset *ec_offsets=new PIO_Offset[totNumElementConn];
  for (int i=0,pos=0; i<num_elems; i++) {
    int elem_start_ind=(int)elem_offsets[i]+1; // +1 to make base-1
    for (int j=0; j<numElementConn[i]; j++) {
      ec_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
      pos++;
    }
  }  

  delete [] elem_offsets;

  // DEBUG OUTPUT
//   for (int i=0,pos=0; i<num_elems; i++) {
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
  piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_INT, 1, &ec_gdimlen, totNumElementConn, ec_offsets, &ec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] ec_offsets;
  
  
  // Get elementConn
  piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  piorc = PIOc_setframe(pioFileDesc, varid, -1);
  if (!CHECKPIOERROR(piorc, std::string("Error setting frame for variable elementConn ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
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
void get_elemConn_info_2Dvar_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, PIO_Offset elementCount, int num_elems, int *elem_ids, 
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
  PIO_Offset *nec_offsets=new PIO_Offset[num_elems];
  for (int i=0; i<num_elems; i++) {
    nec_offsets[i] = (PIO_Offset)elem_ids[i];
  }
  
  // read from file
  get_numElementConn_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                        elementCount, 
                                        num_elems, nec_offsets, 
                                        numElementConn);    
  // Get rid of offsets
  delete [] nec_offsets;
  
  // Get total number of connections on this PET
  totNumElementConn=0;
  for (int i=0; i<num_elems; i++) {
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
  for (int i=0,pos=0; i<num_elems; i++) {
    int elem_start_ind=(elem_ids[i]-1)*maxNodePElement+1; // +1 to make base-1
    for (int j=0; j<numElementConn[i]; j++) {
      ec_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
      pos++;
    }
  }
  

  // Init elementConn decomp
  int ec_iodesc;
  int gdimlen2D[2]={(int)elementCount,(int)maxNodePElement};
  piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_INT, 2, gdimlen2D, totNumElementConn, ec_offsets, &ec_iodesc, 
                          &rearr, NULL, NULL);
  if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
  
  // Get rid of offsets
  delete [] ec_offsets;
  
  
  // Get elementConn
  piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
  if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

  piorc = PIOc_setframe(pioFileDesc, varid, -1);
  if (!CHECKPIOERROR(piorc, std::string("Error setting frame for variable elementConn ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;
  
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
void get_elemConn_info_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, PIO_Offset elementCount, int num_elems, int *elem_ids, 
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
                                               elementCount, num_elems, elem_ids, 
                                               totNumElementConn, numElementConn, elementConn);
  } else if (dimElementConn == 2) {
    get_elemConn_info_2Dvar_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, elementCount, num_elems, elem_ids, 
                                                totNumElementConn, numElementConn, elementConn);
  } else {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
                                      " Only 1D or 2D elementConn variables supported in ESMFMesh format.",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Check if there's a polybreak attribute on elementConn and if so get it
  int polygon_break_value;
  piorc = PIOc_get_att_int(pioFileDesc, varid, "polygon_break_value", &polygon_break_value);

  // Found value so convert to internal mesh polygon_break_value
  if (piorc == PIO_NOERR) {
    for (int i=0; i<totNumElementConn; i++) {
      if (elementConn[i] == polygon_break_value) elementConn[i] = MESH_POLYBREAK_IND;
    }
  }


  
  //     // DEBUG: output numElementConn
  //     printf("numElementConn=");
//       for (int i=0; i<num_elems; i++) {
//         printf(" [%d]=%d ",elem_ids[i],numElementConn[i]);
//       }
//       printf("\n");
  

  //     // DEBUG: output elementConn
  //     // NEED TO CHANGE THIS SO IT WORKS WITH THE NEW 1D elementConn
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

// Get nodeCoords from ESMFMesh format file
void get_nodeCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                       PIO_Offset nodeCount, PIO_Offset coordDim, 
                                       int num_nodes, int *node_ids, 
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
  PIO_Offset *node_offsets= new PIO_Offset[num_nodes*coordDim];
  for (int i=0,pos=0; i<num_nodes; i++) {
    int node_start_ind=(node_ids[i]-1)*coordDim+1;
    for (int j=0; j<coordDim; j++) {
      node_offsets[pos] = (PIO_Offset) (node_start_ind+j);
      pos++;
    }
  }

    // Init nodeCoords decomp
    int node_iodesc;
    int node_gdimlen2D[2]={(int)nodeCount, (int)coordDim};
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_DOUBLE, 2, node_gdimlen2D, num_nodes*coordDim, node_offsets, &node_iodesc, 
                    &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get rid of node offsets
    delete [] node_offsets;

    // Get variable id for nodeCoords
    piorc = PIOc_inq_varid(pioFileDesc, "nodeCoords", &varid);
    if (!CHECKPIOERROR(piorc, std::string("Error nodeCoords variable not in file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for nodeCoords variable") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Get nodeCoords
    nodeCoords= new double[num_nodes*coordDim];
    piorc = PIOc_read_darray(pioFileDesc, varid, node_iodesc, num_nodes*coordDim, nodeCoords);
    if (!CHECKPIOERROR(piorc, std::string("Error with finding nodeCoords variable in file ") + filename,
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
                                       int num_nodes, int *node_ids, 
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
      PIO_Offset *nm_offsets=new PIO_Offset[num_nodes];
      for (int i=0; i<num_nodes; i++) {
        nm_offsets[i] = (PIO_Offset)node_ids[i];
      }

      // Init nodeMask decomp
      int nm_iodesc;
      int nm_gdimlen = (int) nodeCount;
      piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_INT, 1, &nm_gdimlen, num_nodes, nm_offsets, &nm_iodesc, 
                              &rearr, NULL, NULL);
      if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for nodeMask ") + filename,
                         ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

      // Get rid of offsets
      delete [] nm_offsets;

      piorc = PIOc_setframe(pioFileDesc, varid, -1);
      if (!CHECKPIOERROR(piorc, std::string("Error setting frame for nodeMask variable ") + filename,
                         ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

      // Allocate space
      nodeMask=new int[num_nodes];

      // Get mask from file
      piorc = PIOc_read_darray(pioFileDesc, varid, nm_iodesc, num_nodes, nodeMask);
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
                                     int num_elems, int *elem_ids, 
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
    PIO_Offset *em_offsets=new PIO_Offset[num_elems];
    for (int i=0; i<num_elems; i++) {
      em_offsets[i] = (PIO_Offset)elem_ids[i];
    }
    
    // Init elementMask decomp
    int em_iodesc;
    int em_gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_INT, 1, &em_gdimlen, num_elems, em_offsets, &em_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for elementMask ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of offsets
    delete [] em_offsets;
    
    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for elementMask variable ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Allocate space
    elementMask=new int[num_elems];
    
    // Get mask from file
    piorc = PIOc_read_darray(pioFileDesc, varid, em_iodesc, num_elems, elementMask);
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
// (areaPresent is also used, because if num_elems=0 on this PET then elementArea could
// be NULL even though elementArea is present, so that variable is used to cover that case)
void get_elementArea_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                        PIO_Offset elementCount, 
                                        int num_elems, int *elem_ids, 
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
    PIO_Offset *ea_offsets=new PIO_Offset[num_elems];
    for (int i=0; i<num_elems; i++) {
      ea_offsets[i] = (PIO_Offset)elem_ids[i];
    }
    
    // Init elementArea decomp
    int ea_iodesc;
    int ea_gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_DOUBLE, 1, &ea_gdimlen, num_elems, ea_offsets, &ea_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for elementMask ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Get rid of offsets
    delete [] ea_offsets;
    
    // record that area present
    areaPresent=1;
    
    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for elementArea variable ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Allocate space to hold area
    elementArea=new double[num_elems];
    
    // Get area from file
    piorc = PIOc_read_darray(pioFileDesc, varid, ea_iodesc, num_elems, elementArea);
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
// (centerCoordsPresent is also used, because if num_elems=0 on this PET then centerCoords could
// be NULL even though centerCoords is present, so that variable is used to cover that case)
void get_centerCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                         PIO_Offset elementCount, PIO_Offset coordDim, 
                                         int num_elems, int *elem_ids, 
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
    PIO_Offset cc_offsets[num_elems*coordDim];
    for (int i=0,pos=0; i<num_elems; i++) {
      int elem_start_ind=(elem_ids[i]-1)*coordDim+1;
      for (int j=0; j<coordDim; j++) {
        cc_offsets[pos] = (PIO_Offset) (elem_start_ind+j);
        pos++;
      }
    }
    
    // Init elementConn decomp
    int cc_iodesc;
    int cc_gdimlen2D[2]={(int)elementCount,(int)coordDim};
    piorc = PIOc_InitDecomp_ReadOnly(pioSystemDesc, PIO_DOUBLE, 2, cc_gdimlen2D, num_elems*coordDim, cc_offsets, &cc_iodesc, 
                            &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for centerCoords ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // Record the centerCoords present
    centerCoordsPresent=1;
    piorc = PIOc_setframe(pioFileDesc, varid, -1);
    if (!CHECKPIOERROR(piorc, std::string("Error setting frame for variable centerCoords ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Allocate space
    centerCoords=new double[num_elems*coordDim];
    
    // Get the centerCoords
    piorc = PIOc_read_darray(pioFileDesc, varid, cc_iodesc, num_elems*coordDim, centerCoords);
    if (!CHECKPIOERROR(piorc, std::string("Error reading variable centerCoords from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    // Get rid of elementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, cc_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing centerCoords decomp "),
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    // DEEBUG: output centerCoords
    //     for(int i=0; i<num_elems; i++) {
    // printf("%f %f\n",centerCoords[i*coordDim],centerCoords[i*coordDim+1]);
    //}
  }
}

void get_origGridRank_from_ESMFMesh_file(int pioFileDesc, char *filename, bool &has_origGridRank, PIO_Offset &origGridRank) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_origGridRank_from_ESMFMesh_file()"

  // Declare some useful vars
  int dimid;
  int localrc;
  int piorc;

  // Init values
  has_origGridRank=false;
  origGridRank=0;

  // Get id of origGridRank 
  piorc = PIOc_inq_dimid(pioFileDesc, "origGridRank", &dimid);

  // IF present, then get value
  if (piorc == PIO_NOERR) {  

    // Get value    
    piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &origGridRank);
    if (!CHECKPIOERROR(piorc, std::string("Error reading origGridRank from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Mark as present
    has_origGridRank=true;
  }

}

// Get the original grid dimensions from the file (if present) 
// origGridDims must be at least allocated to the size of origGridRank
void get_origGridDims_from_ESMFMesh_file(int pioFileDesc, char *filename, bool &has_origGridDims, int *origGridDims) {
#undef ESMC_METHOD
#define ESMC_METHOD "_get_origGridDims_from_ESMFMesh_file()"

  // Declare some useful vars
  int varid;
  int localrc;
  int piorc;

  // Init values
  has_origGridDims=false;

  // Get variable id for nodeCoords
  piorc = PIOc_inq_varid(pioFileDesc, "origGridDims", &varid);
 
  // If present, then get value
  if (piorc == PIO_NOERR) {  

    // Get value    
    piorc = PIOc_get_var_int(pioFileDesc, varid, origGridDims);
    if (!CHECKPIOERROR(piorc, std::string("Error reading origGridDims from file ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Mark as present
    has_origGridDims=true;
  }

}

#endif // ifdef ESMF_PIO

