// $Id: ESMC_Array.h,v 1.102 2008/04/02 02:57:25 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Array_H
#define ESMC_Array_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Array - Public C interface to the ESMF Array class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Array class and declares method 
// signatures (prototypes).  The companion file {\tt ESMC\_Array.C} contains
// the definitions (full code bodies) for the Array methods.
//
//EOPI
//-----------------------------------------------------------------------------


#include "ESMC_ArraySpec.h"
#include "ESMC_DistGrid.h"

extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Array;

// Class API
ESMC_Array ESMC_ArrayCreate(ESMC_ArraySpec arrayspec, ESMC_DistGrid distgrid,
  char* name, int *rc); //TODO: complete this API
int ESMC_ArrayPrint(ESMC_Array array);
char* ESMC_ArrayGetName(ESMC_Array array, int *rc);
int ESMC_ArrayDestroy(ESMC_Array *array);


}; // extern "C"


#endif  // ESMC_Array_H
