// $Id: ESMC_TimeInterval_F.C,v 1.2 2003/03/22 05:46:04 eschwab Exp $
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
#include "ESMC.h"
#include "ESMC_TimeInterval.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt TimeInterval} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_timeintervalcreate)(ESMC_TimeInterval **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_TimeIntervalCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_timeintervaldestroy)(ESMC_TimeInterval **ptr, int *status) {
           *status = ESMC_TimeIntervalDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_timeintervalinit)(ESMC_TimeInterval **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_timeintervalgetconfig)(ESMC_TimeInterval **ptr, 
                                         ESMC_TimeIntervalConfig *config, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalGetConfig(&config);
       }

       void FTN(c_esmc_timeintervalsetconfig)(ESMC_TimeInterval **ptr, 
                                         ESMC_TimeIntervalConfig *config, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalSetConfig(config);
       }

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeIntervalGet(&value);
       }

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeIntervalSet(value);
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalValidate(opts);
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalPrint(opts);
       }

};


