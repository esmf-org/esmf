// $Id: ESMC_Alarm_F.C,v 1.1 2003/03/14 05:12:12 eschwab Exp $
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
#include "ESMC_Alarm.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Alarm} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_alarmcreate)(ESMC_Alarm **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_AlarmCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_alarmdestroy)(ESMC_Alarm **ptr, int *status) {
           *status = ESMC_AlarmDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_alarminit)(ESMC_Alarm **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_AlarmInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_alarmgetconfig)(ESMC_Alarm **ptr, 
                                         ESMC_AlarmConfig *config, int *status) {
           *status = (*ptr)->ESMC_AlarmGetConfig(&config);
       }

       void FTN(c_esmc_alarmsetconfig)(ESMC_Alarm **ptr, 
                                         ESMC_AlarmConfig *config, int *status) {
           *status = (*ptr)->ESMC_AlarmSetConfig(config);
       }

       void FTN(c_esmc_alarmget)(ESMC_Alarm **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_AlarmGet(&value);
       }

       void FTN(c_esmc_alarmset)(ESMC_Alarm **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_AlarmSet(value);
       }

       void FTN(c_esmc_alarmvalidate)(ESMC_Alarm **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_AlarmValidate(opts);
       }

       void FTN(c_esmc_alarmprint)(ESMC_Alarm **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_AlarmPrint(opts);
       }

};


