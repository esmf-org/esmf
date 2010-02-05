// $Id: ESMCI_Comp.C,v 1.13.2.1 2010/02/05 20:03:54 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

// LogErr headers
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr

// include associated header file
#include "ESMCI_Comp.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMCI_FTable.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Comp.C,v 1.13.2.1 2010/02/05 20:03:54 svasquez Exp $";
//-----------------------------------------------------------------------------


//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN(f_esmf_gridcompcreate)(ESMCI::GridComp *comp, char const *name, 
    ESMCI::GridCompType *mtype, char const *configFile, ESMCI::Clock **clock, 
    int *rc, ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
  void FTN(f_esmf_gridcompdestroy)(ESMCI::GridComp *comp, int *rc);
  void FTN(f_esmf_gridcompinitialize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag, 
    int *userRc, int *rc);
  void FTN(f_esmf_gridcomprun)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag,
    int *userRc, int *rc);
  void FTN(f_esmf_gridcompfinalize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag,
    int *userRc, int *rc);
  void FTN(f_esmf_gridcompprint)(const ESMCI::GridComp *gcomp,
    char const *options, int *rc, ESMCI_FortranStrLenArg olen);
  
  void FTN(f_esmf_cplcompcreate)(ESMCI::CplComp *comp, char const *name, 
    char const *configFile, ESMCI::Clock **clock, 
    int *rc, ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
  void FTN(f_esmf_cplcompdestroy)(ESMCI::CplComp *comp, int *rc);
  void FTN(f_esmf_cplcompinitialize)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag,
    int *userRc, int *rc);
  void FTN(f_esmf_cplcomprun)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag,
    int *userRc, int *rc);
  void FTN(f_esmf_cplcompfinalize)(const ESMCI::CplComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock **clock, int *phase, ESMC_BlockingFlag *blockingFlag,
    int *userRc, int *rc);
  void FTN(f_esmf_cplcompprint)(const ESMCI::CplComp *gcomp,
    char const *options, int *rc, ESMCI_FortranStrLenArg olen);
};
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
      "- Not a valid comp argument", &rc);
    return rc;
  }

  FTable::setServices(this, (void(*)())func, userRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }

  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", &rc);
    return rc;
  }

  char const *methodString;
  switch(method){
  case ESMCI::SETINIT:
    methodString = "Initialize";
    break;
  case ESMCI::SETRUN:
    methodString = "Run";
    break;
  case ESMCI::SETFINAL:
    methodString = "Finalize";
    break;
  case ESMCI::SETWRITERESTART:
    methodString = "WriteRestart";
    break;
  case ESMCI::SETREADRESTART:
    methodString = "ReadRestart";
    break;
  case ESMCI::SETREGISTER:
    methodString = "Register";
    break;
  default:
    break;
  }
  
  int slen = strlen(methodString);
  char *fname;
  FTable::newtrim(methodString, slen, &phase, NULL, &fname);
  
  localrc = ftable->setFuncPtr(fname, (void *)functionPtr, FT_VOIDP4INTP);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) 
    return rc;

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
      "- Not a valid comp argument", rc);
    return NULL;
  }
  
  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", rc);
    return NULL;
  }
  
  char const *name = "localdata";
  enum dtype dtype;
  void *data;
  
  localrc = ftable->getDataPtr(name, &data, &dtype);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return NULL;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  FTable *ftable = **(FTable***)this;
  if (ftable==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid FTable pointer", &rc);
    return rc;
  }
  
  char const *name = "localdata";
  enum dtype dtype = DT_VOIDP;
  
  localrc = ftable->setDataPtr(name, &data, dtype);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
    enum GridCompType mtype,
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
  
  FTN(f_esmf_gridcompcreate)(comp, name, &mtype,
    configFile, &clock, &localrc, strlen(name), strlen(configFile));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return comp;

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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  FTN(f_esmf_gridcompdestroy)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;
  
  FTN(f_esmf_gridcompinitialize)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN(f_esmf_gridcomprun)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;
  
  FTN(f_esmf_gridcompfinalize)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
    char const *options
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  FTN(f_esmf_gridcompprint)(this, options, &localrc, strlen(options));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
  
  FTN(f_esmf_cplcompcreate)(comp, name, configFile, &clock, &localrc,
    strlen(name), strlen(configFile));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return comp;

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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  FTN(f_esmf_cplcompdestroy)(comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN(f_esmf_cplcompinitialize)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN(f_esmf_cplcomprun)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  ESMC_BlockingFlag blockingFlag = ESMF_VASBLOCKING;
  
  int localUserRc;
  if (userRc) localUserRc = *userRc;

  FTN(f_esmf_cplcompfinalize)(this, importState, exportState, &clock,
    &phase, &blockingFlag, &localUserRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
    char const *options
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
      "- Not a valid comp argument", &rc);
    return rc;
  }
  
  FTN(f_esmf_cplcompprint)(this, options, &localrc, strlen(options));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
