// $Id: ESMC_Array.h,v 1.109.4.1 2010/02/05 19:52:06 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

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
#include "ESMC_Interface.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Array;

// Class API
ESMC_Array ESMC_ArrayCreate(ESMC_ArraySpec arrayspec, ESMC_DistGrid distgrid,
  char* name, int *rc);
int ESMC_ArrayDestroy(ESMC_Array *array);
int ESMC_ArrayPrint(ESMC_Array array);
const char *ESMC_ArrayGetName(ESMC_Array array, int *rc);
void *ESMC_ArrayGetPtr(ESMC_Array array, int localDe, int *rc);
int ESMC_ArraySetLWidth(ESMC_Array array,
   ESMC_InterfaceInt computationalLWidthArg);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Array_H
