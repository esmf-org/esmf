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
                                 int *convert_to_dual, int *add_user_area, 
                                 ESMCI::DistGrid *node_distgrid, 
                                 ESMCI::DistGrid *elem_distgrid, 
                                 Mesh **out_mesh, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_file()"

  // Try-catch block around main part of method
  try {
    // local return code
    int localrc;

    // Only support ESMFMesh right now
    if (fileformat != ESMC_FILEFORMAT_ESMFMESH) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          " Only ESMFMesh format files supported right now.",
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
    piorc = PIOc_Init_Intracomm(mpi_comm, num_iotasks, stride, 0, PIO_REARR_SUBSET, &pioSystemDesc);

    
    // Open file - Jim
    int pioFileDesc;
    int mode = 0;
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, PIO_IOTYPE_PNETCDF, filename, mode);
    if (!CHECKPIOWARN(piorc, std::string("Unable to open existing file: ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }
    piorc = PIOc_Set_File_Error_Handling(pioFileDesc, PIO_RETURN_ERROR);


    // Get elem_distgrid ids
    int num_elem_ids=0;
    long int *elem_ids=NULL;
    std::vector<long int> elem_ids_vec;
    if (elem_distgrid != NULL) {

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

      // Assign to pointer
      if (!elem_ids_vec.empty()) {
        num_elem_ids=elem_ids_vec.size();
        elem_ids=&elem_ids_vec[0];

      }
    }

    // Debug output
    //printf(" %d# ",local_pet);
    //for (int i=0; i<num_elem_ids; i++) {
    //  printf(" %d ",elem_ids[i]);
    //}
    //printf("\n");


    // Get maxNodePElement - Jim
    piorc = PIOc_inq_dimid(pioFileDesc, "maxNodePElement", &dimid);
    if (!CHECKPIOWARN(piorc, std::string("Error reading maxNodePElement dimension from file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }

    // Get elementConn and numElementConn at elem_ids positions - Jim
    piorc = PIOc_InitDecomp(pioSystemDesc, pio_type, 1, global_vec.size(), num_elem_ids, elem_ids, &iodesc, 
                    PIO_REARR_SUBSET, NULL, NULL);
    if (!CHECKPIOWARN(piorc, std::string("Error initializing PIO decomp for file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }

    piorc = PIOc_inq_varid(pioFileDesc, "elementConn", &varid);
    if (!CHECKPIOWARN(piorc, std::string("Error elementConn variable not in file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }
    piorc = PIOc_read_darray(pioFileDesc, varid, iodesc, num_elem_ids, &elementConn);
    if (!CHECKPIOWARN(piorc, std::string("Error reading variable elementConn from file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }
    piorc = PIOc_inq_varid(pioFileDesc, "NumElementConn", &varid);
    if (!CHECKPIOWARN(piorc, std::string("Error NumElementConn variable not in file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }
    piorc = PIOc_read_darray(pioFileDesc, varid, iodesc, num_elem_ids, &NumElementConn);
    if (!CHECKPIOWARN(piorc, std::string("Error reading NumElementConn variable from file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }

    ///// Close file - Jim
    piorc = PIOc_closefile(pioFileDesc);
    if (!CHECKPIOWARN(piorc, std::string("Error closing file ") + filename,
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }

    piorc = PIOc_freedecomp(pioSystemDesc, iodesc);
    if (!CHECKPIOWARN(piorc, std::string("Error freeing decomp ")
	ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }

    // maybe free the iosystem here?

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




