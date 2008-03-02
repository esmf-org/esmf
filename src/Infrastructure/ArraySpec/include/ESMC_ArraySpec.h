// $Id: ESMC_ArraySpec.h,v 1.10 2008/03/02 04:49:52 theurich Exp $
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

#ifndef ESMC_ArraySpec_H
#define ESMC_ArraySpec_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_ArraySpec - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the public C ArraySpec interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_ArraySpec.C}
// contains the definitions (full code bodies) for the ArraySpec methods.
// 
//EOPI
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"

extern "C" {

// Class declaration type
typedef struct{
  // Allocate enough memory to store members in the Fortran side.
  // Adjust if members are added, rounding up to multiples of 64)
  char shallowMem[192];
}ESMC_ArraySpec;

// Class API
int ESMC_ArraySpecSet(ESMC_ArraySpec *arrayspec, int rank,
  ESMC_TypeKind typekind);
int ESMC_ArraySpecGet(ESMC_ArraySpec arrayspec, int *rank,
  ESMC_TypeKind *typekind);

}; // extern "C"

#endif  // ESMC_ArraySpec_H
