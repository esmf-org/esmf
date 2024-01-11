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
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  (*userRoutine)(comp, &rc);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

void FTN_X(f_nuopc_compspecialize)(void* gcomp, char *specLabel,
  void (*specRoutine)(ESMC_GridComp, int *), int *rc, int);
//TODO: the last argument should actually be of type ESMCI_FortranStrLenArg
//TODO: for this to be included this file here needs to change to be .C, i.e C++
//TODO: That'd be better anyway, because it allows standard error checking with
//TODO: backtrace generation in the calls below!!!
int NUOPC_CompSpecialize(
  ESMC_GridComp comp,                           // in
  char *specLabel,                              // in
  void (*specRoutine)(ESMC_GridComp, int *)     // in
){
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  FTN_X(f_nuopc_compspecialize)((void*)comp.ptr, specLabel, specRoutine, &rc,
    strlen(specLabel));

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

void FTN_X(f_nuopc_modelsetservices)(void* gcomp, int* rc);
void NUOPC_ModelSetServices(ESMC_GridComp comp, int *rc){
  FTN_X(f_nuopc_modelsetservices)((void*)comp.ptr, rc);
}
