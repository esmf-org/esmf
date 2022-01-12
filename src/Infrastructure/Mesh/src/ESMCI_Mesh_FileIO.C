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
#include "Mesh/include/ESMCI_FileIO_Util.h"
#include "Mesh/include/ESMCI_ESMFMesh_Util.h"

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




