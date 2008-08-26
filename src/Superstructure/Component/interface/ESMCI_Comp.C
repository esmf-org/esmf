// $Id: ESMCI_Comp.C,v 1.4 2008/08/26 18:51:04 theurich Exp $
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
#include "ESMC_Start.h"
#include "ESMCI_FTable.h"

// LogErr headers
#include "ESMCI_LogErr.h"
#include "ESMC_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Comp.C,v 1.4 2008/08/26 18:51:04 theurich Exp $";
//-----------------------------------------------------------------------------

// prototypes for fortran interface routines
extern "C" {
  void FTN(f_esmf_gridcompcreate)(ESMCI::GridComp *comp, char *name, 
    ESMCI::GridCompType *mtype, char *configFile, ESMCI::Clock *clock, 
    int *rc, ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
  void FTN(f_esmf_gridcompdestroy)(ESMCI::GridComp *comp, int *rc);
  void FTN(f_esmf_gridcompprint)(const ESMCI::GridComp *gcomp,
    const char *options, int *rc, ESMCI_FortranStrLenArg olen);
  void FTN(f_esmf_gridcompinitialize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock *clock, int *phase, ESMC_BlockingFlag *blockingFlag, int *rc);
  void FTN(f_esmf_gridcomprun)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock *clock, int *phase, ESMC_BlockingFlag *blockingFlag, int *rc);
  void FTN(f_esmf_gridcompfinalize)(const ESMCI::GridComp *gcomp,
    ESMCI::State *importState, ESMCI::State *exportState, 
    ESMCI::Clock *clock, int *phase, ESMC_BlockingFlag *blockingFlag, int *rc);
};


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
  void (*func)(Comp *, int *)             // (in)
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

  ESMCI::FTable::setServices(this, (void(*)())func, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
  
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
    const char *functionType,                           // in: function type
    void (*functionPtr)(Comp *, State *, State *, Clock *), // in: function ptr
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

  int slen = strlen(functionType);
  char *tname = new char[slen+1];
  strcpy(tname, functionType);
  ESMCI::FTable::setTypedEP(this, tname, strlen(functionType),
    &phase, 0, FT_COMP2STAT, (void *)functionPtr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  delete [] tname;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


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
    char *name, 
    enum GridCompType mtype,
    char *configFile,
    ESMCI::Clock *clock,
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
    configFile, clock, &localrc, strlen(name), strlen(configFile));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return comp;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return comp;
}


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
    const char *options
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


} // namespace ESMCI
