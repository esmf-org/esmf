// $Id: ESMC_ArraySpec.h,v 1.9 2008/02/29 17:44:01 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF ArraySpec C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_ArraySpec_H
 #define ESMC_ArraySpec_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only


//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_ArraySpec - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C Array interfaces and declares method 
// signatures (prototypes).  The companion file ESMC\_ArraySpec.C contains
// the definitions (full code bodies) for the Array methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
#include "ESMC.h"
#include "ESMC_F90Interface.h"

struct ESMC_ArraySpec;

extern "C" {
// Prototypes of the F77 interface functions.
void FTN(f_esmf_arrayspecset)(ESMC_ArraySpec *arrayspec, int *rank,
                              ESMC_TypeKind *typekind, int *rc);

void FTN(f_esmf_arrayspecget)(ESMC_ArraySpec *arrayspec, int *rank,
                              ESMC_TypeKind *typekind, int *rc);

// ! class declaration type
struct ESMC_ArraySpec{

    // Allocate enough memory to store members in the Fortran side.
    // Adjust if members are added, rounding up to multiples of 64)
    char shallowMem[192];

//
//EOP
//-----------------------------------------------------------------------------

}; // end class ESMC_ArraySpec

  int ESMC_ArraySpecSet(ESMC_ArraySpec *arrayspec, 
                        int rank, 
                        ESMC_TypeKind typekind);

  int ESMC_ArraySpecGet(ESMC_ArraySpec arrayspec,
                        int *rank,
                        ESMC_TypeKind *typekind);

}; // extern "C"
 #endif  // ESMC_ArraySpec_H
