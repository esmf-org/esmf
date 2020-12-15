// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB
#include "ESMC_MBMeshTestGenMBMesh.C"
#include "ESMCI_MBMesh.h"
#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#endif

#include <cstring>

int main(int argc, char *argv[]) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Throw an error from the MBMesh");
  strcpy(failMsg, "Did not return ESMC_RC_MOAB_ERROR");
#if defined ESMF_MOAB
  int localrc;
  MBMesh *mesh = create_mesh_halfway(localrc);
  if (localrc == ESMF_SUCCESS) {
    int elemIds[1];
    InterArray<int> *eii = new InterArray<int>(elemIds,1);
    MBMesh_GetElemCreateInfo(mesh, eii, NULL, NULL, NULL, NULL, NULL, &rc);
    delete eii;
  }
#else
  rc = ESMC_RC_MOAB_ERROR;
#endif
  ESMC_Test((rc==ESMC_RC_MOAB_ERROR), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


