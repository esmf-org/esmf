// $Id: ESMC_Clock_F.C,v 1.1 2003/03/14 05:12:26 eschwab Exp $
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
#include "ESMC_Clock.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Clock} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_clockcreate)(ESMC_Clock **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_ClockCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_clockdestroy)(ESMC_Clock **ptr, int *status) {
           *status = ESMC_ClockDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_clockinit)(ESMC_Clock **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_ClockInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_clockgetconfig)(ESMC_Clock **ptr, 
                                         ESMC_ClockConfig *config, int *status) {
           *status = (*ptr)->ESMC_ClockGetConfig(&config);
       }

       void FTN(c_esmc_clocksetconfig)(ESMC_Clock **ptr, 
                                         ESMC_ClockConfig *config, int *status) {
           *status = (*ptr)->ESMC_ClockSetConfig(config);
       }

       void FTN(c_esmc_clockget)(ESMC_Clock **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_ClockGet(&value);
       }

       void FTN(c_esmc_clockset)(ESMC_Clock **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_ClockSet(value);
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_ClockValidate(opts);
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_ClockPrint(opts);
       }

};


