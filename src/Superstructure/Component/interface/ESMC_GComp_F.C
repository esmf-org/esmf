// $Id: ESMC_GComp_F.C,v 1.1 2003/01/07 21:38:17 nscollins Exp $
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
#include "ESMC_GComp.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt GComp} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_gcompcreate)(ESMC_GComp *ptr, int *status) {
         ptr = ESMC_GCompCreate(status);
     }

     void FTN(c_esmc_gcompdestroy)(ESMC_GComp *ptr, int *status) {
         *status = ESMC_GCompDestroy(ptr);
     }

     void FTN(c_esmc_gcompprint)(ESMC_GComp *ptr, char *opts, int *status) {
         *status = ptr->ESMC_GCompPrint(opts);
     }

};



