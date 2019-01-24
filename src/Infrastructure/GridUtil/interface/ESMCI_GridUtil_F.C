// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
#include "Mesh/include/ESMCI_MeshCap.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
//#include "Mesh/include/ESMCI_MeshRegrid.h" // only for the REGRID flags
#include "Mesh/include/Legacy/ESMCI_Exception.h"
//#include "Mesh/include/ESMCI_Interp.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMCI;


extern "C" {

void FTN_X(c_esmc_meshio)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp,
  int *staggerLoc, int *num_arrays, char*name, int *rc,
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

    // Don't do the below. It should come from the grid CoordSys
#if 0
  if (*spherical != 0) grid.setSphere();
#endif

  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  // Convert Grid to Mesh
  int regridConserve = 0; //ESMC_REGRID_CONSERVE_OFF;
  int localrc;
  MeshCap *meshp=MeshCap::GridToMesh(grid, *staggerLoc,
                                     arrays,
                                     NULL,
                                     &regridConserve, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  meshp->meshwrite(name, &localrc, nlen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  // Get rid of Mesh
  delete meshp;

  // Return success
  if (rc) *rc = ESMF_SUCCESS;
}



void FTN_X(c_esmc_gridio)(ESMCI::Grid **gridpp, int *staggerLoc,
  int *num_arrays, char*name, int *rc,
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
  int localrc;

  // Get VM
  ESMCI::VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT,NULL)) throw localrc;  // bail out with exception

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

    // Don't do the below. It should come from the grid CoordSys
#if 0
  bool prevIsSphere=grid.isSphere();
  if (*spherical != 0) grid.setSphere();

  ESMC_CoordSys_Flag prevCoordSys=grid.getCoordSys();
  if (*islatlondeg != 0) grid.setCoordSys(ESMC_COORDSYS_SPH_DEG);
#endif

  // Make list of arrays
  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  // Convert Grid to Mesh
  int regridConserve = 0; // ESMC_REGRID_CONSERVE_OFF;
  MeshCap *meshp=MeshCap::GridToMesh(grid, *staggerLoc,
                                     arrays,
                                     NULL,
                                     &regridConserve, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  meshp->meshwrite(name, &localrc, nlen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  // Get rid of Mesh
  delete meshp;

  // Return success
  if (rc) *rc = ESMF_SUCCESS;
}



  void FTN_X(c_esmc_gridcellio)(ESMCI::Grid **gridpp,
                                int *num_arrays, char*name, int *rc,
                                ESMCI::Array **arraypp1,
                                ESMCI::Array **arraypp2,
                                ESMCI::Array **arraypp3,
                                ESMCI::Array **arraypp4,
                                ESMCI::Array **arraypp5,
                                ESMCI::Array **arraypp6,
                                ESMCI_FortranStrLenArg nlen
                                ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcellio()"
  ESMCI::Grid &grid = **gridpp;
  int localrc;

  // How to do this?? Any way I can think of is ugly:
  ESMCI::Array **ar[] = {
    arraypp1,
    arraypp2,
    arraypp3,
    arraypp4,
    arraypp5,
    arraypp6
  };

  // Make list of arrays
  std::vector<ESMCI::Array*> arrays;
  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  // Convert Grid to Mesh
  MeshCap *meshp=MeshCap::GridToMeshCell(grid, arrays, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  meshp->meshwrite(name, &localrc, nlen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

  // Get rid of Mesh
  delete meshp;

  // Return success
  if (rc) *rc = ESMF_SUCCESS;
}


  void FTN_X(c_esmc_gridtomesh)(ESMCI::Grid **gridpp, int *staggerLoc,
                                int *isSphere, int *islatlondeg, MeshCap **meshpp,
                                ESMCI::InterArray<int> *maskValuesArg, int *regridConserve, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridtomesh()"

    ESMCI::Grid &grid = **gridpp;


    // Don't do the below. It should come from the grid CoordSys
#if 0
    // Make grid spherical if requested
    bool prevIsSphere=grid.isSphere();
    if (*isSphere != 0) grid.setSphere();

    // Map coords to surface of a sphere if reqeusted
    ESMC_CoordSys_Flag prevCoordSys=grid.getCoordSys();
    if (*islatlondeg != 0) grid.setCoordSys(ESMC_COORDSYS_SPH_DEG);
#endif

    // Temp vector
    std::vector<ESMCI::Array*> arrays;

    // Convert Grid to Mesh
    int localrc;
    *meshpp=MeshCap::GridToMesh(grid, *staggerLoc,
                                arrays,
                                maskValuesArg,
                                regridConserve, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;


    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


  void FTN_X(c_esmc_gridtomeshcell)(ESMCI::Grid **gridpp, MeshCap **meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridtomeshcell()"

    ESMCI::Grid &grid = **gridpp;

    // Temp vector
    std::vector<ESMCI::Array*> arrays;

    // Convert Grid to Mesh
    int localrc;
    *meshpp=MeshCap::GridToMeshCell(grid, arrays, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;


    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}
