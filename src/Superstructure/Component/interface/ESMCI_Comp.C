// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Comp.C"
//==============================================================================
//
// ESMC Component method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the ESMCI::Comp interface to the Component
// class implemented in Fortran.
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_Comp.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMCI_FTable.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN_X(f_esmf_compexecute)(const ESMCI::Comp *comp,
    enum ESMCI::method *method,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *timeout, int *userRc, int *rc);
  
  void FTN_X(f_esmf_compgetcurrentphase)(const ESMCI::Comp *compp,
    int *currentPhase, int *rc);
  void FTN_X(f_esmf_compgettimeout)(const ESMCI::Comp *compp,
    int *timeout, int *rc);
  void FTN_X(f_esmf_compgetvminfo)(const ESMCI::Comp *compp, void **vm_info,
    int *rc);
  void FTN_X(f_esmf_compgetvm)(const ESMCI::Comp *compp, ESMCI::VM **vm,
    int *rc);
  void FTN_X(f_esmf_compgetvmparent)(const ESMCI::Comp *compp,
    ESMCI::VM **vmparent, int *rc);
  void FTN_X(f_esmf_compgetvmplan)(const ESMCI::Comp *compp,
    ESMCI::VMPlan **vmplan, int *rc);
  void FTN_X(f_esmf_compgetftable)(const ESMCI::Comp *compp,
    ESMCI::FTable **ftable, int *rc);
  void FTN_X(f_esmf_compgettunnel)(const ESMCI::Comp *comp, 
    ESMCI::CompTunnel **tunnel, int *rc);

  void FTN_X(f_esmf_gridcompcreate)(ESMCI::GridComp *comp, char const *name, 
    char const *configFile, ESMCI::Clock **clock, 
    int *rc, ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
  void FTN_X(f_esmf_gridcompdestroy)(ESMCI::GridComp *comp, int *rc);
  void FTN_X(f_esmf_gridcompinitialize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_gridcomprun)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_gridcompfinalize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_gridcompprint)(const ESMCI::GridComp *gcomp, int *rc);
  
  void FTN_X(f_esmf_cplcompcreate)(ESMCI::CplComp *comp, char const *name, 
    char const *configFile, ESMCI::Clock **clock, 
    int *rc, ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
  void FTN_X(f_esmf_cplcompdestroy)(ESMCI::CplComp *comp, int *rc);
  void FTN_X(f_esmf_cplcompinitialize)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_cplcomprun)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_cplcompfinalize)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, ESMC_BlockingFlag *blockingFlag, int *phase,
    int *userRc, int *rc);
  void FTN_X(f_esmf_cplcompprint)(const ESMCI::CplComp *gcomp, int *rc);

  void FTN_X(f_esmf_scicompcreate)(ESMCI::SciComp *comp, char const *name, 
    int *rc, ESMCI_FortranStrLenArg nlen);
  void FTN_X(f_esmf_scicompdestroy)(ESMCI::SciComp *comp, int *rc);
  void FTN_X(f_esmf_scicompprint)(const ESMCI::SciComp *comp, int *rc);
};
//==============================================================================

//==============================================================================
// ESMCI::Comp interfaces to be called from Fortran side
extern "C" {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getcompliancecheckdepth"
  void FTN_X(c_esmc_getcompliancecheckdepth)(int *depth, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMCI::Comp::getComplianceCheckerDepth(depth);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
} // extern "C"
//==============================================================================


namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:setServices()"
//BOPI
// !IROUTINE:  ESMCI::Comp:setServices
//
// !INTERFACE:
int Comp::setServices(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void (*func)(Comp *, int *),            // (in)
  int *userRc                             // (out)
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  FTable::setServices(this, (void(*)())func, userRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:setEntryPoint()"
//BOPI
// !IROUTINE:  ESMCI::Comp:setEntryPoint
//
// !INTERFACE:
int Comp::setEntryPoint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    enum method method,                           // in: method type
    void (*functionPtr)(Comp *, State *, State *, Clock **, int *), // in: 
    int phase                                           // in: phase
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", ESMC_CONTEXT, &rc);
    return rc;
  }

  char const *methodString = FTable::methodString(method);
  
  int slen = strlen(methodString);
  char *fname;
  FTable::newtrim(methodString, slen, &phase, NULL, &fname);
  
  localrc = ftable->setFuncPtr(fname, (void *)functionPtr, FT_VOIDP4INTP);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  delete[] fname;  // delete memory that "newtrim" allocated above
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getInternalState()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getInternalState
//
// !INTERFACE:
void *Comp::getInternalState(
//
// !RETURN VALUE:
//    void * to data
//
// !ARGUMENTS:
//
  int *rc                       // (out)
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, rc);
    return NULL;
  }
  
  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", ESMC_CONTEXT, rc);
    return NULL;
  }
  
  char const *name = "localdata";
  enum dtype dtype;
  void *data;
  
  localrc = ftable->getDataPtr(name, &data, &dtype);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return data;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:setInternalState()"
//BOPI
// !IROUTINE:  ESMCI::Comp:setInternalState
//
// !INTERFACE:
int Comp::setInternalState(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void *data                    // (in)
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  char const *name = "localdata";
  enum dtype dtype = DT_VOIDP;
  
  localrc = ftable->setDataPtr(name, &data, dtype);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:execute()"
//BOPI
// !IROUTINE:  ESMCI::Comp:execute
//
// !INTERFACE:
int Comp::execute(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    enum method method,
    State *importState,
    State *exportState,
    Clock *clock,
    ESMC_BlockingFlag blockingFlag,
    int phase,
    int timeout,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;
  
  FTN_X(f_esmf_compexecute)(this, &method, importState, exportState, &clock,
    &blockingFlag, &phase, &timeout, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getCurrentPhase()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getCurrentPhase
//
// !INTERFACE:
int Comp::getCurrentPhase(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    int *currentPhase
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetcurrentphase)(this, currentPhase, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getTimeout()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getTimeout
//
// !INTERFACE:
int Comp::getTimeout(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    int *timeout
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgettimeout)(this, timeout, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getVmInfo()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getVmInfo
//
// !INTERFACE:
int Comp::getVmInfo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    void **vm_info
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetvminfo)(this, vm_info, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getVm()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getVm
//
// !INTERFACE:
int Comp::getVm(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    VM **vm
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetvm)(this, vm, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getVmParent()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getVmParent
//
// !INTERFACE:
int Comp::getVmParent(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    VM **vmparent
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetvmparent)(this, vmparent, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getVmPlan()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getVmPlan
//
// !INTERFACE:
int Comp::getVmPlan(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    VMPlan **vmplan
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetvmplan)(this, vmplan, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getFTable()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getFTable
//
// !INTERFACE:
int Comp::getFTable(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    FTable **ftable
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgetftable)(this, ftable, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getTunnel()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getTunnel
//
// !INTERFACE:
int Comp::getTunnel(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    CompTunnel **tunnel
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_compgettunnel)(this, tunnel, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Comp:getComplianceCheckerDepth()"
//BOPI
// !IROUTINE:  ESMCI::Comp:getComplianceCheckerDepth
//
// !INTERFACE:
int Comp::getComplianceCheckerDepth(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    int *depth
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  char const *envVar = VM::getenv("ESMF_RUNTIME_COMPLIANCECHECK");
  if (envVar != NULL && depth != NULL){
    std::string value(envVar);
    // see if there is a depth specified in ESMF_RUNTIME_COMPLIANCECHECK
    int index;
    *depth=-1;
    index = value.find("depth=");
    if (index == std::string::npos)
      index = value.find("DEPTH=");
    if (index != std::string::npos){
      index += 6; // right after the equal sign
      int indexEnd = value.find_first_of(":", index);
      *depth = atoi(value.substr(index,indexEnd).c_str());
    }
    //printf("depth = %d\n", *depth);
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:create()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:create
//
// !INTERFACE:
GridComp *GridComp::create(
//
// !RETURN VALUE:
//    GridComp *
//
// !ARGUMENTS:
//
    char const *name, 
    char const *configFile,
    Clock *clock,
    int *rc
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  GridComp *comp = new GridComp;
  
  FTN_X(f_esmf_gridcompcreate)(comp, name,
    configFile, &clock, &localrc, strlen(name), strlen(configFile));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return comp;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:destroy()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:destroy
//
// !INTERFACE:
int GridComp::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    GridComp *comp
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (comp==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_gridcompdestroy)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  delete comp;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:initialize()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:initialize
//
// !INTERFACE:
int GridComp::initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;
  
  FTN_X(f_esmf_gridcompinitialize)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:run()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:run
//
// !INTERFACE:
int GridComp::run(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN_X(f_esmf_gridcomprun)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:finalize()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:finalize
//
// !INTERFACE:
int GridComp::finalize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;
  
  FTN_X(f_esmf_gridcompfinalize)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridComp:print()"
//BOPI
// !IROUTINE:  ESMCI::GridComp:print
//
// !INTERFACE:
int GridComp::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_gridcompprint)(this, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:create()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:create
//
// !INTERFACE:
CplComp *CplComp::create(
//
// !RETURN VALUE:
//    CplComp *
//
// !ARGUMENTS:
//
    char const *name, 
    char const *configFile,
    Clock *clock,
    int *rc
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  CplComp *comp = new CplComp;
  
  FTN_X(f_esmf_cplcompcreate)(comp, name, configFile, &clock, &localrc,
    strlen(name), strlen(configFile));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return comp;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:destroy()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:destroy
//
// !INTERFACE:
int CplComp::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    CplComp *comp
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (comp==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_cplcompdestroy)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  delete comp;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:initialize()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:initialize
//
// !INTERFACE:
int CplComp::initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN_X(f_esmf_cplcompinitialize)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:run()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:run
//
// !INTERFACE:
int CplComp::run(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN_X(f_esmf_cplcomprun)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:finalize()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:finalize
//
// !INTERFACE:
int CplComp::finalize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    State *importState,
    State *exportState,
    Clock *clock,
    int phase,
    int *userRc
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN_X(f_esmf_cplcompfinalize)(this, importState, exportState, &clock,
    &blockingFlag, &phase, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  if (userRc) *userRc = localUserRc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CplComp:print()"
//BOPI
// !IROUTINE:  ESMCI::CplComp:print
//
// !INTERFACE:
int CplComp::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  )const{
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_cplcompprint)(this, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::SciComp:create()"
//BOPI
// !IROUTINE:  ESMCI::SciComp:create
//
// !INTERFACE:
SciComp *SciComp::create(
//
// !RETURN VALUE:
//    SciComp *
//
// !ARGUMENTS:
//
    char const *name, 
    int *rc
  )
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
{
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc != NULL) 
    *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  SciComp *comp = new SciComp;
  
  FTN_X(f_esmf_scicompcreate)(comp, name, &localrc, strlen(name));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return comp;

  // return successfully
  if (rc != NULL) 
    *rc = ESMF_SUCCESS;

  return comp;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::SciComp:destroy()"
//BOPI
// !IROUTINE:  ESMCI::SciComp:destroy
//
// !INTERFACE:
int SciComp::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
    SciComp *comp
  )
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
{
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (comp == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_scicompdestroy)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  delete comp;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::SciComp:print()"
//BOPI
// !IROUTINE:  ESMCI::SciComp:print
//
// !INTERFACE:
int SciComp::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ) const
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
{
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (this==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid comp argument", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  FTN_X(f_esmf_scicompprint)(this, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
