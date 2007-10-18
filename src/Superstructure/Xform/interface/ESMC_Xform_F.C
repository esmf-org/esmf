// $Id: ESMC_Xform_F.C,v 1.2.8.3 2007/10/18 02:44:09 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
//==============================================================================
//
// This file contains interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_Xform.h"

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The {\tt Transform} implementation language is Fortran 90, but the
// routines which register function and data addresses to be used later
// in callback code must be implemented in C++.  These routines here
// allow the F90 to call the C++ support routines.
//
// For the general C++ interfaces to the public entry points, see
// the file {\tt ESMF_Xform_C.F90}.
//
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     // TODO: add the proper entry points here.
     //       these are just a best guess at this point.

     // and note again - these are *NOT* wrappers for externally visible
     // ESMF routines.  the bulk of the xform implementation is done in
     // F90, and only those methods which need to manipulate function or
     // data pointers are implemented in C++ and these are wrappers for
     // those calls.

     void FTN(c_esmc_xformcallroutine)(ESMC_Xform **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_XformCallRoutine(type, func);
     }

     void FTN(c_esmc_xformgetroutine)(ESMC_Xform **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_XformGetRoutine(type, func);
     }

     void FTN(c_esmc_xformgetdataptr)(ESMC_Xform **ptr, int type, int *status) {
         //*status = (*ptr)->ESMC_XformGetDataPtr(type, func);
     }

};



