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
// ESMC_GridComp, ESMC_CplComp and ESMC_SciComp structures.
//
//-----------------------------------------------------------------------------
// include associated header files
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"
#include "ESMC_SciComp.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Comp.h"
#include "ESMC_Clock.h"
#include "ESMCI_Clock.h"
#include "ESMCI_FTable.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompCreate()"
ESMC_GridComp ESMC_GridCompCreate(const char *name,
  const char *configFile, ESMC_Clock clock, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_GridComp comp;
  
  // typecast into ESMCI types
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);
  
  comp.ptr = (void *)
    ESMCI::GridComp::create(name, configFile, clockp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    comp.ptr = NULL;  // invalidate
    return comp;  // bail out
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompDestroy()"
int ESMC_GridCompDestroy(ESMC_GridComp *comp){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // call into ESMCI method  
  localrc = ESMCI::GridComp::destroy((ESMCI::GridComp *)(comp->ptr));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // invalidate pointer
  comp->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompSetServices()"
int ESMC_GridCompSetServices(ESMC_GridComp comp,
  void (*func)(ESMC_GridComp, int *), int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setServices((void(*)(ESMCI::Comp *, int *))func, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompSetEntryPoint()"
int ESMC_GridCompSetEntryPoint(ESMC_GridComp comp, enum ESMC_Method method,
  void (*func)(ESMC_GridComp, ESMC_State, ESMC_State, ESMC_Clock *, int *),
  int phase){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  enum ESMCI::method methodArg = (enum ESMCI::method)method;
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setEntryPoint(methodArg,
    (void(*)(ESMCI::Comp *, ESMCI::State *, ESMCI::State *, ESMCI::Clock **,
    int *))func, phase);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompInitialize()"
int ESMC_GridCompInitialize(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->initialize(importStatep, exportStatep, clockp, phase,
    userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompRun()"
int ESMC_GridCompRun(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->run(importStatep, exportStatep, clockp, phase, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompFinalize()"
int ESMC_GridCompFinalize(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->finalize(importStatep, exportStatep, clockp, phase, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompGetInternalState()"
void *ESMC_GridCompGetInternalState(ESMC_GridComp comp, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  void *data = compp->getInternalState(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL;  // bail out
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return data;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompSetInternalState()"
int ESMC_GridCompSetInternalState(ESMC_GridComp comp, void *data){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setInternalState(data);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCompPrint()"
int ESMC_GridCompPrint(ESMC_GridComp comp){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::GridComp *compp = (ESMCI::GridComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompCreate()"
ESMC_CplComp ESMC_CplCompCreate(const char *name, const char *configFile, ESMC_Clock clock,
  int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_CplComp comp;
  
  // typecast into ESMCI types
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);
  
  comp.ptr = (void *)
    ESMCI::CplComp::create(name, configFile, clockp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    comp.ptr = NULL;  // invalidate
    return comp;  // bail out
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompDestroy()"
int ESMC_CplCompDestroy(ESMC_CplComp *comp){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // call into ESMCI method  
  localrc = ESMCI::CplComp::destroy((ESMCI::CplComp *)(comp->ptr));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // invalidate pointer
  comp->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompSetServices()"
int ESMC_CplCompSetServices(ESMC_CplComp comp,
  void (*func)(ESMC_CplComp, int *), int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setServices((void(*)(ESMCI::Comp *, int *))func, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompSetEntryPoint()"
int ESMC_CplCompSetEntryPoint(ESMC_CplComp comp, enum ESMC_Method method,
  void (*func)(ESMC_CplComp, ESMC_State, ESMC_State, ESMC_Clock *, int *),
  int phase){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  enum ESMCI::method methodArg = (enum ESMCI::method)method;
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setEntryPoint(methodArg,
    (void(*)(ESMCI::Comp *, ESMCI::State *, ESMCI::State *, ESMCI::Clock **,
    int *))func, phase);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompInitialize()"
int ESMC_CplCompInitialize(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->initialize(importStatep, exportStatep, clockp, phase, 
    userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompRun()"
int ESMC_CplCompRun(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->run(importStatep, exportStatep, clockp, phase, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompFinalize()"
int ESMC_CplCompFinalize(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);
  ESMCI::State *importStatep = (ESMCI::State *)(importState.ptr);
  ESMCI::State *exportStatep = (ESMCI::State *)(exportState.ptr);
  ESMCI::Clock *clockp = (ESMCI::Clock *)(clock.ptr);

  // call into ESMCI method  
  localrc = compp->finalize(importStatep, exportStatep, clockp, phase, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompGetInternalState()"
void *ESMC_CplCompGetInternalState(ESMC_CplComp comp, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);

  // call into ESMCI method  
  void *data = compp->getInternalState(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL;  // bail out
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return data;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompSetInternalState()"
int ESMC_CplCompSetInternalState(ESMC_CplComp comp, void *data){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->setInternalState(data);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CplCompPrint()"
int ESMC_CplCompPrint(ESMC_CplComp comp){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::CplComp *compp = (ESMCI::CplComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SciCompCreate()"
ESMC_SciComp ESMC_SciCompCreate(const char *name, int *rc)
{

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;           // local return code
  if (rc != NULL) 
    *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_SciComp comp;
  
  comp.ptr = (void *) ESMCI::SciComp::create(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    comp.ptr = NULL;  // invalidate
    return comp;  // bail out
  }
  
  // return successfully
  if (rc != NULL) 
    *rc = ESMF_SUCCESS;

  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SciCompDestroy()"
int ESMC_SciCompDestroy(ESMC_SciComp *comp)
{

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // call into ESMCI method  
  localrc = ESMCI::SciComp::destroy((ESMCI::SciComp *)(comp->ptr));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // invalidate pointer
  comp->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SciCompPrint()"
int ESMC_SciCompPrint(ESMC_SciComp comp)
{

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::SciComp *compp = (ESMCI::SciComp *)(comp.ptr);

  // call into ESMCI method  
  localrc = compp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


}; // extern "C"
