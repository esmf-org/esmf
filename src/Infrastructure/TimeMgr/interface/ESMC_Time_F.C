// $Id: ESMC_Time_F.C,v 1.1 2003/03/14 05:12:43 eschwab Exp $
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
#include "ESMC_Time.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Time} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_timecreate)(ESMC_Time **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_TimeCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_timedestroy)(ESMC_Time **ptr, int *status) {
           *status = ESMC_TimeDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_timeinit)(ESMC_Time **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_TimeInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_timegetconfig)(ESMC_Time **ptr, 
                                         ESMC_TimeConfig *config, int *status) {
           *status = (*ptr)->ESMC_TimeGetConfig(&config);
       }

       void FTN(c_esmc_timesetconfig)(ESMC_Time **ptr, 
                                         ESMC_TimeConfig *config, int *status) {
           *status = (*ptr)->ESMC_TimeSetConfig(config);
       }

       void FTN(c_esmc_timeget)(ESMC_Time **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeGet(&value);
       }

       void FTN(c_esmc_timeset)(ESMC_Time **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeSet(value);
       }

       void FTN(c_esmc_timevalidate)(ESMC_Time **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimeValidate(opts);
       }

       void FTN(c_esmc_timeprint)(ESMC_Time **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimePrint(opts);
       }

};


