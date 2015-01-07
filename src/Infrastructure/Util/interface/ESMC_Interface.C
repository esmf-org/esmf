// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Interface.C"
//==============================================================================
//
// ESMC Interface method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Interface methods declared
// in the companion file ESMC_Interface.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Interface.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"

#include <stdio.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_InterfaceIntSet(ESMC_InterfaceInt *interfaceIntArg,
  int *arrayArg, int lenArg){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  /*
  // this is a test to see if the data is passed in correctly
  printf("ESMC_InterfaceIntCreate - arrayArg = [");
  for (int i=0; i<lenArg; ++i)
    printf("%d,", arrayArg[i]);
  printf("], length = %d\n", lenArg);
  */
  
  ESMCI::InterfaceInt *ii = ((ESMCI::InterfaceInt *)(interfaceIntArg));
  
  ii->set(arrayArg, lenArg);

  /*
  // this is a test to see if the data is passed in correctly
  printf("ESMC_InterfaceIntCreate - arrayArg = [");
  for (int i=0; i<ii->extent[0]; ++i)
    printf("%d,", ii->array[i]);
  printf("], length = %d\n", ii->extent[0]);
  */

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

}; // extern "C"
