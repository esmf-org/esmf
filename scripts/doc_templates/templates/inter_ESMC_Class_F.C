// $Id: inter_ESMC_Class_F.C,v 1.2 2003/01/09 16:31:48 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_<Class>.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt <Class>} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

//     void FTN(c_esmc_<class>create)(ESMC_<Class> **ptr, int *status) {
//         *ptr = ESMC_<Class>Create(status);
//     }

//     void FTN(c_esmc_<class>destroy)(ESMC_<Class> **ptr, int *status) {
//         *status = ESMC_<Class>Destroy(*ptr);
//     }

//     void FTN(c_esmc_<class>print)(ESMC_<Class> **ptr, char *opts, int *status) {
//         *status = (*ptr)->ESMC_<Class>Print(opts);
//     }

};



