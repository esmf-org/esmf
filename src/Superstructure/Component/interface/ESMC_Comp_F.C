// $Id: ESMC_Comp_F.C,v 1.4 2003/02/18 17:00:59 nscollins Exp $
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
#include "ESMC_Comp.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which provides
//  callbacks to user-supplied functions instead of forcing them to 
//  have unique entry point names over the entire set of possible components.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_compregister)(ESMC_Comp **ptr, int type, void (func)(), 
                                   int *status) {
         //*status = (*ptr)->ESMC_CompRegister(type, func);
     }

     void FTN(c_esmc_compcall)(ESMC_Comp **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_CompCall(type, func);
     }

};


