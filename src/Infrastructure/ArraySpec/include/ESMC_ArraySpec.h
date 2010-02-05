// $Id: ESMC_ArraySpec.h,v 1.15.2.1 2010/02/05 19:53:07 svasquez Exp $
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

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  // Allocate enough memory to store members in the Fortran side.
  // Adjust if members are added, rounding up to multiples of 64 byte
  char shallowMem[192];
}ESMC_ArraySpec;

// Class API
int ESMC_ArraySpecSet(ESMC_ArraySpec *arrayspec, int rank,
  enum ESMC_TypeKind typekind);
int ESMC_ArraySpecGet(ESMC_ArraySpec arrayspec, int *rank,
  enum ESMC_TypeKind *typekind);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_ArraySpec_H
