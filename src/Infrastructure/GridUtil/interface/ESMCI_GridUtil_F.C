// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_GridUtil_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <iostream>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/ESMCI_MeshRead.h"
#include "Mesh/include/ESMCI_MeshRegrid.h" // only for the REGRID flags
#include "Mesh/include/ESMCI_Exception.h"
#include "Mesh/include/ESMCI_Interp.h"
#include "Mesh/include/ESMCI_ParEnv.h"


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMCI;


extern "C" {

void FTN_X(c_esmc_meshio)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int *num_arrays,
                    char*name, int *rc,
                             ESMCI::Array **arraypp1,
                             ESMCI::Array **arraypp2,
                             ESMCI::Array **arraypp3,
                             ESMCI::Array **arraypp4,
                             ESMCI::Array **arraypp5,
                             ESMCI::Array **arraypp6,
                             int *spherical,
                             ESMCI_FortranStrLenArg nlen
                             ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshio()" 
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &grid = **gridpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  std::vector<ESMCI::Array*> arrays;

  // How to do this?? Any way I can think of is ugly:
  ESMCI::Array **ar[] = {
    arraypp1,
    arraypp2,
    arraypp3,
    arraypp4,
    arraypp5,
    arraypp6
  };


  if (*spherical != 0) grid.setSphere();

  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  Mesh mesh;

  try {

    int regridConserve = ESMC_REGRID_CONSERVE_OFF;
    ESMCI::GridToMesh(grid, *staggerLoc, mesh, arrays, NULL, &regridConserve);

    char *meshname = ESMC_F90toCstring(name, nlen);

    WriteMesh(mesh, meshname);

    delete [] meshname;

  }
  catch(std::exception &x) {
    std::cout << "Error!!! Exception, P:" << localPet << ", <" << x.what() << ">" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }
  catch(...) {
    std::cout << "Error, unknown exception" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }

  *rc = ESMF_SUCCESS;

}



void FTN_X(c_esmc_gridio)(ESMCI::Grid **gridpp, int *staggerLoc, int *num_arrays,
                    char*name, int *rc,
                             ESMCI::Array **arraypp1,
                             ESMCI::Array **arraypp2,
                             ESMCI::Array **arraypp3,
                             ESMCI::Array **arraypp4,
                             ESMCI::Array **arraypp5,
                             ESMCI::Array **arraypp6,
                             int *spherical, int *islatlondeg,
                             ESMCI_FortranStrLenArg nlen
			  ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridio()" 
  ESMCI::Grid &grid = **gridpp;

  // Get VM
  int localrc;
  ESMCI::VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

  // Get pet info
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  std::vector<ESMCI::Array*> arrays;

  // How to do this?? Any way I can think of is ugly:
  ESMCI::Array **ar[] = {
    arraypp1,
    arraypp2,
    arraypp3,
    arraypp4,
    arraypp5,
    arraypp6
  };

  bool prevIsSphere=grid.isSphere();
  if (*spherical != 0) grid.setSphere();

  ESMC_CoordSys_Flag prevCoordSys=grid.getCoordSys();
  if (*islatlondeg != 0) grid.setCoordSys(ESMC_COORDSYS_SPH_DEG);


  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  Mesh mesh;

  try {

    int regridConserve = ESMC_REGRID_CONSERVE_OFF;
    ESMCI::GridToMesh(grid, *staggerLoc, mesh, arrays, NULL, &regridConserve);

    char *meshname = ESMC_F90toCstring(name, nlen);

    WriteMesh(mesh, meshname);

    delete [] meshname;

  }
  catch(std::exception &x) {
    std::cout << "Error!!! Exception, P:" << localPet << ", <" << x.what() << ">" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }
  catch(...) {
    std::cout << "Error, unknown exception" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }

  // Reset Grid back to previous state
  //  if (!prevIsSphere) grid.clearSphere();
  //grid.setCoordSys(prevCoordSys);


  *rc = ESMF_SUCCESS;

}





  void FTN_X(c_esmc_gridtomesh)(ESMCI::Grid **gridpp, int *staggerLoc, int *isSphere, int *islatlondeg, Mesh **meshpp, 					  ESMCI::InterfaceInt **maskValuesArg, int *regridConserve, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridtomesh()" 

  ESMCI::Grid &grid = **gridpp;

  // Make grid spherical if requested
  bool prevIsSphere=grid.isSphere();
  if (*isSphere != 0) grid.setSphere();

  // Map coords to surface of a sphere if reqeusted
  ESMC_CoordSys_Flag prevCoordSys=grid.getCoordSys();
  if (*islatlondeg != 0) grid.setCoordSys(ESMC_COORDSYS_SPH_DEG);


  Mesh *meshp = new Mesh();

  try {

    std::vector<ESMCI::Array*> arrays;
    ESMCI::GridToMesh(grid, *staggerLoc, *meshp, arrays, *maskValuesArg, regridConserve);
//WriteMesh(*meshp, "gridtomesh");

    *meshpp = meshp;


  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Reset Grid back to previous state
  //if (!prevIsSphere) grid.clearSphere();
  //grid.setCoordSys(prevCoordSys);


  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
}
