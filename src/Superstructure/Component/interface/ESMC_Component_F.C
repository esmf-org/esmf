// $Id: ESMC_Component_F.C,v 1.1 2003/01/07 21:38:15 nscollins Exp $
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
#include "ESMC_Component.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Component} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_componentcreate)(ESMC_Component *ptr, int *status) {
         ptr = ESMC_ComponentCreate(status);
     }

     void FTN(c_esmc_componentdestroy)(ESMC_Component *ptr, int *status) {
         *status = ESMC_ComponentDestroy(ptr);
     }

     void FTN(c_esmc_componentprint)(ESMC_Component *ptr, char *opts, int *status) {
         *status = ptr->ESMC_ComponentPrint(opts);
     }

};



