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

void CheckPIOError(int err, char *_str, int rc_code, int rc,bool warn)
{
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
static void _convert_global_elem_conn_to_local_node_and_elem_info(int num_local_elem, int max_num_elem_conn, int *num_elem_conn, int *global_elem_conn,
                                                                  int &num_nodes, int*& node_ids, int &num_local_elem_conn, int*& local_elem_conn) {
  // Init output
  num_nodes=0;
  node_ids=NULL;
  num_local_elem_conn=0;
  local_elem_conn=NULL;

  // Count local elems conns
  num_local_elem_conn=0;
  for (int i=0; i<num_local_elem; i++) {
    num_local_elem_conn += num_elem_conn[i];
  }

  // If nothing to do leave
  if (num_local_elem_conn < 1) return;

  // Allocate conversion list
  NODE_INFO *convert_list=new NODE_INFO[num_local_elem_conn];

  // Copy global elem connection info into conversion list
  for (int i=0,pos=0; i<num_local_elem; i++) {
    int global_elem_conn_pos=i*max_num_elem_conn; // base elem_conn position    

    for (int j=0; j<(int)num_elem_conn[i]; j++) {
      convert_list[pos].node_id=global_elem_conn[global_elem_conn_pos+j];
      convert_list[pos].local_elem_conn_pos=pos;
      pos++;
    }
  }

  // Sort list by node_id, to make it easy to find unique node_ids
  std::sort(convert_list,convert_list+num_local_elem_conn);

  // Count number of unique node ids in  convert_list
  int num_unique_node_ids=1;                 // There has to be at least 1, 
  int prev_node_id=convert_list[0].node_id;  // because we leave if < 1 above
  for (int i=1; i<num_local_elem_conn; i++) {

    // If not the same as the last one count a new one
    if (convert_list[i].node_id != prev_node_id) {
      num_unique_node_ids++;
      prev_node_id=convert_list[i].node_id;
    }
  }

  // Allocate node_ids
  node_ids=new int[num_unique_node_ids];

  // Set output number of nodes
  num_nodes=num_unique_node_ids;

  // Allocate local elem conn
  local_elem_conn=new int[num_local_elem_conn];

  // Translate convert_list to node_ids and local_elem_conn
  int node_ids_pos=0;                             // There has to be at least 1, 
  node_ids[node_ids_pos]=convert_list[0].node_id; // because we leave if < 1 above
  local_elem_conn[convert_list[0].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  for (int i=1; i<num_local_elem_conn; i++) {

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
    int pio_type = PIO_IOTYPE_NETCDF;


    // Error check some unhandled options

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

    // Don't currently support adding user area
    if (add_user_area) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " adding user area currently not supported.",
                           ESMC_CONTEXT, &localrc)) throw localrc;
    }



    // Get VM info
    int local_pet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    MPI_Comm mpi_comm = VM::getCurrent(&localrc)->getMpi_c();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    int pet_count = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    int pets_per_Ssi = VM::getCurrent(&localrc)->getSsiLocalPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Debug output
    //printf("%d# filename=%s\n",local_pet,filename);
    /// Maybe initialize IO system here?
    int num_iotasks = pet_count/pets_per_Ssi;
    int stride = pets_per_Ssi;
    int pioSystemDesc;
    int piorc;
    piorc = PIOc_Init_Intracomm(mpi_comm, num_iotasks, stride, 0, PIO_REARR_SUBSET, &pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Unable to init PIO Intracomm for file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Open file - Jim
    int pioFileDesc;
    int mode = 0;
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    if (!CHECKPIOERROR(piorc, std::string("Unable to open existing file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    piorc = PIOc_Set_File_Error_Handling(pioFileDesc, PIO_RETURN_ERROR);
    //    if (!CHECKPIOERROR(piorc, std::string("Unable to set PIO error handling for file: ") + filename,
    //                   ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Get global elementCount
    int dimid;
    piorc = PIOc_inq_dimid(pioFileDesc, "elementCount", &dimid);
    if (!CHECKPIOERROR(piorc, std::string("Error reading  elementCount dimension length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    PIO_Offset elementCount;
    piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &elementCount);
    if (!CHECKPIOERROR(piorc, std::string("Error reading  elementCount dimension length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


    // Get positions at which to read element information
    std::vector<int> elem_ids_vec;
    if (elem_distgrid == NULL) {

      // No distgrid provided so divide things up equally

      // Approx. number per PET
      int num_per_pet=elementCount/pet_count;

      // Figure out range for this PET
      int min_id=local_pet*num_per_pet+1;
      int max_id=(local_pet+1)*num_per_pet;

      // Add rest to end
      // TODO: Spread these more evenly
      if (local_pet == pet_count-1) max_id=elementCount;

      //  printf("%d# min,max ids=%d %d\n",local_pet,min_id,max_id);

      // Reserve space for ids
      elem_ids_vec.reserve(max_id-min_id+1);
      
      // Fill ids
      for (int id=min_id; id <= max_id; id++) {
        elem_ids_vec.push_back(id);
      }

    } else {

      // Have elem_distgrid, so get ids from that

      // Currently only support distgrids with 1 localDE
      if (elem_distgrid->getDELayout()->getLocalDeCount() != 1) {
        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         " Currently only distgrids with 1 DE per PET are supported",
                                          ESMC_CONTEXT, &localrc)) throw localrc;
      }

      // Get seqIndexList
      // TODO: right now assumes 1 localDE, fix this
      localrc=elem_distgrid->fillSeqIndexList(elem_ids_vec, 0);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;
    }

    // Assign vector info to pointer for below
    // TODO: get rid of this
    int num_elems=0;
    int *elem_ids=NULL;
    if (!elem_ids_vec.empty()) {
      num_elems=elem_ids_vec.size();
      elem_ids=&elem_ids_vec[0];
    }

    // Get maxNodePElement
    piorc = PIOc_inq_dimid(pioFileDesc, "maxNodePElement", &dimid);
    if (!CHECKPIOERROR(piorc, std::string("Error reading maxNodePElement dimension from file ") + filename,
                     ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    PIO_Offset maxNodePElement;
    piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &maxNodePElement);
    if (!CHECKPIOERROR(piorc, std::string("Error reading maxNodePElement dimension length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Get global coordDim
    piorc = PIOc_inq_dimid(pioFileDesc, "coordDim", &dimid);
    if (!CHECKPIOERROR(piorc, std::string("Error reading coordDim from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    PIO_Offset coordDim;
    piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &coordDim);
    if (!CHECKPIOERROR(piorc, std::string("Error reading coordDim length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;
    
    //printf("coordDim=%d\n",coordDim);


    // Get global nodeCount
    piorc = PIOc_inq_dimid(pioFileDesc, "nodeCount", &dimid);
    if (!CHECKPIOERROR(piorc, std::string("Error reading nodeCount dimension length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    PIO_Offset nodeCount;
    piorc = PIOc_inq_dim(pioFileDesc, dimid, NULL, &nodeCount);
    if (!CHECKPIOERROR(piorc, std::string("Error reading nodeCount dimension length from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


    // Define offsets numElementConn decomp
    PIO_Offset dof1d[num_elems];
    for (int i=0; i<num_elems; i++) {
      dof1d[i] = (PIO_Offset)elem_ids[i];
    }

    // Init numElementConn decomp
    int nec_iodesc;
    int rearr = PIO_REARR_SUBSET;
    int gdimlen = (int) elementCount;
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_BYTE, 1, &gdimlen, num_elems, dof1d, &nec_iodesc, 
                    &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get numElementConn
    int varid;
    piorc = PIOc_inq_varid(pioFileDesc, "numElementConn", &varid);
    if (!CHECKPIOERROR(piorc, std::string("Error NumElementConn variable not in file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


    char *char_numElementConn=new char[num_elems];
    piorc = PIOc_read_darray(pioFileDesc, varid, nec_iodesc, num_elems, char_numElementConn);
    if (!CHECKPIOERROR(piorc, std::string("Error reading numElementConn variable from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Copy to int array
    int *numElementConn=new int[num_elems];
    for (int i=0; i<num_elems; i++) {
      numElementConn[i]=(int)char_numElementConn[i];
    }

    // Get rid of char version
    delete [] char_numElementConn;


//     // DEBUG: output numElementConn
//     printf("%d# numElementConn=\n",local_pet);
//     for (int i=0; i<num_elems; i++) {
//       printf(" [%d]=%d ",elem_ids[i],numElementConn[i]);
//     }
//     printf("\n");


    // Define offsets for elementConn decomp
    PIO_Offset dof2d[num_elems*maxNodePElement];
    for (int i=0,pos=0; i<num_elems; i++) {
      int elem_start_ind=(elem_ids[i]-1)*maxNodePElement+1;
      for (int j=0; j<maxNodePElement; j++) {
        dof2d[pos] = (PIO_Offset) (elem_start_ind+j);
        pos++;
      }
    }

    // Init elementConn decomp
    int ec_iodesc;
    int gdimlen2D[2]={elementCount,maxNodePElement};
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_INT, 2, gdimlen2D, num_elems*maxNodePElement, dof2d, &ec_iodesc, 
                    &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get elementConn
    piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
    if (!CHECKPIOERROR(piorc, std::string("Error elementConn variable not in file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    int *elementConn= new int[num_elems*maxNodePElement];
    piorc = PIOc_read_darray(pioFileDesc, varid, ec_iodesc, num_elems*maxNodePElement, elementConn);
    if (!CHECKPIOERROR(piorc, std::string("Error reading variable elementConn from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;
    
    //     // DEBUG: output elementConn
    //     for (int i=0; i<num_elems; i++) {
    //       int pos=i*maxNodePElement; // base elementConn position
    //       printf(" [%d] =",elem_ids[i]);
    //       for (int j=0; j<(int)numElementConn[i]; j++) {
    //         printf(" %d ",elementConn[pos]);
    //         pos++;
    //       }
    //       printf("\n");
    //     }
    //     printf("\n");
    

    // Convert global elem info
    int num_nodes;
    int *node_ids=NULL;
    int num_local_elem_conn;
    int *local_elem_conn=NULL;
    _convert_global_elem_conn_to_local_node_and_elem_info(num_elems, maxNodePElement, numElementConn, elementConn,
                                                          num_nodes, node_ids, num_local_elem_conn, local_elem_conn);

    // DEBUG output num_nodes
    //printf("%d# num_nodes=%d\n",local_pet, num_nodes);

    // Define offsets for elementConn decomp
    PIO_Offset node_offsets[num_nodes*coordDim];
    for (int i=0,pos=0; i<num_nodes; i++) {
      int node_start_ind=(node_ids[i]-1)*coordDim+1;
      for (int j=0; j<coordDim; j++) {
        node_offsets[pos] = (PIO_Offset) (node_start_ind+j);
        pos++;
      }
    }

    // Init elementConn decomp
    int node_iodesc;
    int node_gdimlen2D[2]={nodeCount, coordDim};
    piorc = PIOc_InitDecomp(pioSystemDesc, PIO_DOUBLE, 2, node_gdimlen2D, num_nodes*coordDim, node_offsets, &node_iodesc, 
                    &rearr, NULL, NULL);
    if (!CHECKPIOERROR(piorc, std::string("Error initializing PIO decomp for file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get nodeCoords
    piorc = PIOc_inq_varid(pioFileDesc, "nodeCoords", &varid);
    if (!CHECKPIOERROR(piorc, std::string("Error nodeCoords variable not in file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    double *nodeCoords= new double[num_nodes*coordDim];
    piorc = PIOc_read_darray(pioFileDesc, varid, node_iodesc, num_nodes*coordDim, nodeCoords);
    if (!CHECKPIOERROR(piorc, std::string("Error reading variable nodeCoords from file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Create Mesh    
    int pdim, sdim;
    if (coordDim == 2) {
      pdim=2;
      sdim=2;
    } else if (coordDim == 3) {
      pdim=3;
      sdim=3;
    } else {
      Throw() << "Meshes can only be created with dim=2 or 3.";
    }
    ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_SPH_DEG;
    ESMCI_meshcreate(out_mesh,
                     &pdim, &sdim, &coord_sys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Add nodes
    ESMCI_meshaddnodes(out_mesh, &num_nodes, node_ids,
                       nodeCoords, NULL, NULL,
                       &coord_sys, &sdim,
                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Add elements
    int areaPresent=0;
    int elemCoordsPresent=0;
    int regridConserve=1;
    ESMCI_meshaddelements(out_mesh,
                          &num_elems, elem_ids, numElementConn,
                          NULL,
                          &areaPresent, NULL,
                          &elemCoordsPresent, NULL, 
                          &num_local_elem_conn, local_elem_conn, &regridConserve,
                          &coord_sys, &sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;


    // Close file
    piorc = PIOc_closefile(pioFileDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error closing file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get rid of numElementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, nec_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing numElementConn decomp "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get rid of elementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, ec_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing elementConn decomp "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Get rid of elementConn decomp
    piorc = PIOc_freedecomp(pioSystemDesc, node_iodesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing nodeCoord decomp "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;


    // maybe free the iosystem here?
    piorc = PIOc_free_iosystem(pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing pio file system description "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    // Free
    delete [] elementConn;
    delete [] numElementConn;
    delete [] node_ids;
    delete [] local_elem_conn;

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          " Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // We've gotten to bottom successfully, so return success
  if(rc != NULL) *rc = ESMF_SUCCESS;
}




