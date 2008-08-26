// $Id: ESMC_Comp.C,v 1.37 2008/08/26 20:46:49 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Comp.C"
//==============================================================================
//
// ESMC Component method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the common code for the public, pure C
// ESMC_GridComp and ESMC_CplComp structures.
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Comp.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Comp.h"
#include "ESMC_Clock.h"
#include "ESMCI_Clock.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Comp.C,v 1.37 2008/08/26 20:46:49 theurich Exp $";
//-----------------------------------------------------------------------------

extern "C" {

  
// initilalize global character constants to be used by user code written in C
const char *ESMC_SetInit         = "ESMF_Initialize";
const char *ESMC_SetRun          = "ESMF_Run";
const char *ESMC_SetFinal        = "ESMF_Finalize";
const char *ESMC_SetWriteRestart = "ESMF_WriteRestart";
const char *ESMC_SetReadRestart  = "ESMF_ReadRestart";


ESMC_GridComp ESMC_GridCompCreate(char *name, enum ESMC_GridCompType mtype,
  char *configFile, ESMC_Clock clock, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompESMC_GridCompCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_GridComp comp;
  
  // typecast into ESMCI types
  enum ESMCI::GridCompType mtypeArg = (enum ESMCI::GridCompType)mtype;
  ESMCI::Clock *clockp = (ESMCI::Clock *)clock.ptr;
  
  comp.ptr = (void *)
    ESMCI::GridComp::create(name, mtypeArg, configFile, clockp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    comp.ptr = NULL;
    return comp;  // bail out
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}


int ESMC_GridCompDestroy(ESMC_GridComp *comp){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompDestroy()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp->ptr);

  // call into ESMCI method  
  localrc = ESMCI::GridComp::destroy(compp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
  
  // invalidate pointer
  comp->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  


int ESMC_GridCompSetServices(ESMC_GridComp comp,
  void (*func)(ESMC_GridComp *, int *)){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompSetServices()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setServices((void(*)(ESMCI::Comp *, int*))func);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  


int ESMC_GridCompPrint(ESMC_GridComp comp, const char *options){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompPrint()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->print(options);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  


}; // extern "C"
