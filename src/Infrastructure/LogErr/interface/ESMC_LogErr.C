// $Id: ESMC_LogErr.C,v 1.4.2.1 2010/02/05 19:58:55 svasquez Exp $
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
#define ESMC_FILENAME "ESMC_LogErr.C"
//==============================================================================
//
// ESMC LogErr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C LogErr methods declared
// in the companion file ESMC_LogErr.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_LogErr.h"

// include ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_LogErr.C,v 1.4.2.1 2010/02/05 19:58:55 svasquez Exp $";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_LogWrite(const char msg[], int msgtype){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_LogWrite()"

//TODO: Add context arguments as optional arguments

  // Call into ESMCI method
  if ( ESMC_LogDefault.Write(msg, msgtype) ){
     return ESMF_TRUE;
  }else{
     return ESMF_FALSE;
  }

} // ESMC_LogWrite

} // extern "C"
