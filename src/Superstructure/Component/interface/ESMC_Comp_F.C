// $Id: ESMC_Comp_F.C,v 1.6 2003/02/25 18:27:09 nscollins Exp $
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
// This file contains Fortran interface code to link F90 and C++.
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
// The {\tt Component} implementation language is Fortran 90, but the
// routines which register function and data addresses to be used later
// in callback code must be implemented in C++.  These routines here
// allow the F90 to call the C++ support routines.
//
// For the general C++ interfaces to the public entry points, see
// the file {\tt ESMF_Comp_C.F90}.
//
//
//EOP


// these interface subroutine names MUST be in lower case
extern "C" {

     // these first two have no leading c_ and are ESMF and not ESMC because 
     // they're to be called directly by F90 user code.  making them different
     // seems like an invitation to errors; also if they don't have the
     // leading c_ then we may run into naming conflicts with the real
     // C++ code which does the work (which was why the prefix was added 
     // in the first place).
     //
     // also note they CANNOT have prototypes in fortran because the routine 
     // types and data types are private/different for each call so there
     // is no correct prototype syntax which will work.

     void FTN(esmf_compsetroutine)(ESMC_Comp **ptr, int type, void (func)(), 
                                   int *status) {
         //*status = (*ptr)->ESMC_CompSetRoutine(type, func);
     }

     void FTN(esmf_compsetdataptr)(ESMC_Comp **ptr, int type, void (func)(), 
                                   int *status) {
         //*status = (*ptr)->ESMC_CompSetDataPtr(type, func);
     }

     // the rest of these routines follow the normal naming conventions and
     // are called from the framework internals.
  
     void FTN(c_esmc_comptablecreate)(ESMC_Comp **ptr, void *table, int *status) {
         *status = (*ptr)->ESMC_CompTableCreate(table);
     }
  
     void FTN(c_esmc_compcallroutine)(ESMC_Comp **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_CompCallRoutine(type, func);
         *status = ESMF_FAILURE;
     }

     void FTN(c_esmc_compgetroutine)(ESMC_Comp **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_CompGetRoutine(type, func);
         *status = ESMF_FAILURE;
     }

     void FTN(c_esmc_compgetdataptr)(ESMC_Comp **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_CompGetDataPtr(type, func);
         *status = ESMF_FAILURE;
     }

};


