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
#define ESMC_FILENAME "ESMC_Util.C"
//==============================================================================
//
// ESMC Interface method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Interface methods declared
// in the companion file ESMC_Util.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMC_Util.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMCI_Macros.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


extern "C" {

void FTN_X(f_esmf_utilversionprint) (ESMC_Logical *Flag, ESMC_Logical *versionFlag, int *rc);

int ESMC_UtilGetArgIndex(int argc, char * const argv[], const char *value, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;         // final return code
  
  int argIndex = -1;  // initialize
  
  int i;
  for (i=0; i<argc; i++)
    if (strcmp(argv[i], value)==0) break;
  
  if (i<argc) argIndex = i;
    
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return argIndex;
}  

void ESMC_UtilVersionPrint (int vFlag, int versionFlag, int *rc) {
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;         // final return code

  fflush (stdout);

  ESMC_Logical vf = (vFlag != 0) ? ESMF_TRUE:ESMF_FALSE;
  ESMC_Logical versionf= (versionFlag != 0) ? ESMF_TRUE:ESMF_FALSE;
  FTN_X (f_esmf_utilversionprint) (&vf, &versionf, rc);
}

}; // extern "C"
