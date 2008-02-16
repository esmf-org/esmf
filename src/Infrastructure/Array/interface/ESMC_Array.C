// $Id: ESMC_Array.C,v 1.1 2008/02/16 00:36:05 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Array.C"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Array methods declared
// in the companion file ESMC_Array.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Array.h"

// include ESMF headers
#include "ESMCI_Array.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Array.C,v 1.1 2008/02/16 00:36:05 theurich Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_Array ESMC_ArrayCreate(void){
  ESMC_Array array;
  array.arrayPtr = NULL;  // this dummy prototype returns an invalidated ESMC_Array
  return array;
}

}; // extern "C"
