// $Id: ESMCI_Comp.C,v 1.2 2008/08/26 05:15:11 theurich Exp $
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
static const char *const version = "$Id: ESMCI_Comp.C,v 1.2 2008/08/26 05:15:11 theurich Exp $";
//-----------------------------------------------------------------------------


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


} // namespace ESMCI
