// $Id: ESMC_Clock_F.C,v 1.5 2003/03/28 00:45:51 eschwab Exp $
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

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_clockinit)(ESMC_Clock *ptr,
                                  ESMC_TimeInterval *timeStep,
                                  ESMC_Time *startTime,
                                  ESMC_Time *stopTime,
                                  ESMC_Time *refTime,
                                  int *status) {
           *status = (ptr)->ESMC_ClockInit(timeStep, startTime, stopTime,
                                           refTime);
       }

       void FTN(c_esmc_clockadvance)(ESMC_Clock *ptr,
                                     ESMC_Alarm *ringingList,
                                     int *numRingingAlarms, int *status) {
           *status = (ptr)->ESMC_ClockAdvance(ringingList, numRingingAlarms);
       }

       void FTN(c_esmc_clockisstoptime)(ESMC_Clock *ptr, 
                                      int *esmf_clockIsStopTime, int *status) {
           *esmf_clockIsStopTime = (int) (ptr)->ESMC_ClockIsStopTime(status);
       }

       void FTN(c_esmc_clockgetadvancecount)(ESMC_Clock *ptr, 
                                             ESMF_IKIND_I8 *advanceCount,
                                             int *status) {
           *status = (ptr)->ESMC_ClockGetAdvanceCount(advanceCount);
       }

#if 0
       void FTN(c_esmc_clockget)(ESMC_Clock *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_ClockGet(&value);
       }

       void FTN(c_esmc_clockset)(ESMC_Clock *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_ClockSet(value);
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_ClockValidate(opts);
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_ClockPrint(opts);
       }
#endif
};
