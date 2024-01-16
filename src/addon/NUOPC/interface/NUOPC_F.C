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
#define ESMC_FILENAME "NUOPC_F.C"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
// include associated header file
#include "NUOPC.h"

// include ESMF headers
#include "ESMCI_Base.h"
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Comp.h"
#include "ESMCI_State.h"

// include std headers
#include <cstring>

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_CompDerive()"
int NUOPC_CompDerive(
  ESMC_GridComp comp,                           // in
  void (*userRoutine)(ESMC_GridComp, int *)     // in
){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  (*userRoutine)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
void FTN_X(f_nuopc_compspecialize)(const ESMCI::Comp *, const char *,
  void (*specRoutine)(ESMC_GridComp, int *), int *rc, ESMCI_FortranStrLenArg);
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_CompSpecialize()"
int NUOPC_CompSpecialize(
  ESMC_GridComp comp,                           // in
  const char *specLabel,                              // in
  void (*specRoutine)(ESMC_GridComp, int *)     // in
){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  FTN_X(f_nuopc_compspecialize)((const ESMCI::Comp *)comp.ptr, specLabel,
    specRoutine, &localrc, (ESMCI_FortranStrLenArg)strlen(specLabel));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
void FTN_X(f_nuopc_modelsetservices)(const ESMCI::Comp *, int* rc);
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_ModelSetServices()"
void NUOPC_ModelSetServices(ESMC_GridComp comp, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  FTN_X(f_nuopc_modelsetservices)((const ESMCI::Comp *)comp.ptr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
void FTN_X(f_nuopc_modelsetvm)(const ESMCI::Comp *, int* rc);
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_ModelSetVM()"
void NUOPC_ModelSetVM(ESMC_GridComp comp, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  FTN_X(f_nuopc_modelsetvm)((const ESMCI::Comp *)comp.ptr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
void FTN_X(f_nuopc_modelgetexportstate)(const ESMCI::Comp *, ESMCI::State*,
  int* rc);
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_ModelGetExportState()"
ESMC_State NUOPC_ModelGetExportState(ESMC_GridComp comp, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  ESMC_State state;
  state.ptr = new ESMCI::State;  //TODO: this leaves a memory leak!
  FTN_X(f_nuopc_modelgetexportstate)((const ESMCI::Comp *)comp.ptr,
    (ESMCI::State *)state.ptr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return state;  // bail out
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return state;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
void FTN_X(f_nuopc_modelgetimportstate)(const ESMCI::Comp *, ESMCI::State*,
  int* rc);
#undef  ESMC_METHOD
#define ESMC_METHOD "NUOPC_ModelGetImportState()"
ESMC_State NUOPC_ModelGetImportState(ESMC_GridComp comp, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  ESMC_State state;
  state.ptr = new ESMCI::State;  //TODO: this leaves a memory leak!
  FTN_X(f_nuopc_modelgetimportstate)((const ESMCI::Comp *)comp.ptr,
    (ESMCI::State *)state.ptr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return state;  // bail out
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return state;
}
//-----------------------------------------------------------------------------

}; // extern "C"
