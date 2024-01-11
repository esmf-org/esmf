// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------
#define ESMC_FILENAME "NUOPC.c"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
// include associated header file
#include "NUOPC.h"

int NUOPC_CompDerive(
  ESMC_GridComp comp,                           // in
  void (*userRoutine)(ESMC_GridComp, int *)     // in
){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  (*userRoutine)(comp, &rc);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

void FTN_X(f_nuopc_modelsetservices)(void* gcomp, int* rc);
void NUOPC_ModelSetServices(ESMC_GridComp comp, int *rc){
  FTN_X(f_nuopc_modelsetservices)((void*)comp.ptr, rc);
}
