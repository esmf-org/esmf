// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
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
// ESMC_ArraySpec - uniform array specification for Fortran and C
//
// The code in this file defines the public C ArraySpec interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_ArraySpec.C}
// contains the definitions (full code bodies) for the ArraySpec methods.
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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArraySpecGet - Get values from an ArraySpec
//
// !INTERFACE:
int ESMC_ArraySpecGet(
  ESMC_ArraySpec arrayspec,         // in
  int *rank,                        // out
  enum ESMC_TypeKind_Flag *typekind // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Returns information about the contents of an {\tt ESMC\_ArraySpec}.
//
//  The arguments are:
//  \begin{description}
//  \item[arrayspec]
//    The {\tt ESMC\_ArraySpec} to query.
//  \item[rank]
//    Array rank (dimensionality - 1D, 2D, etc). Maximum allowed is 7D.
//  \item[typekind]
//    Array typekind. See section~\ref{const:ctypekind} for valid values.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArraySpecSet - Set values for an ArraySpec
//
// !INTERFACE:
int ESMC_ArraySpecSet(
  ESMC_ArraySpec *arrayspec,         // inout 
  int rank,                          // in
  enum ESMC_TypeKind_Flag typekind        // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set an Array specification - typekind, and rank.
//
//  The arguments are:
//  \begin{description}
//  \item[arrayspec]
//    The {\tt ESMC\_ArraySpec} to set.
//  \item[rank]
//    Array rank (dimensionality - 1D, 2D, etc). Maximum allowed is 7D.
//  \item[typekind]
//    Array typekind. See section~\ref{const:ctypekind} for valid values.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_ArraySpec_H
