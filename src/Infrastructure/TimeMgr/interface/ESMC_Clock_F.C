// $Id: ESMC_Clock_F.C,v 1.15 2003/08/29 05:31:58 eschwab Exp $
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
//  allows F90 to call C++ for supporting {\tt ESMC\_Clock} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_clocksetup)(ESMC_Clock *ptr,
                                   ESMC_TimeInterval *timeStep,
                                   ESMC_Time *startTime,
                                   ESMC_Time *stopTime,
                                   ESMC_Time *refTime,
                                   int *status) {
          int rc = (ptr)->ESMC_ClockSetup(timeStep, startTime, stopTime,
                                          refTime);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockset)(ESMC_Clock *ptr,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMF_IKIND_I8 *advanceCount,
                                 int *status) {
          int rc = (ptr)->ESMC_ClockSet(timeStep, startTime, stopTime,
                                        refTime, currTime, advanceCount);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockget)(ESMC_Clock *ptr,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMC_Time *prevTime,
                                 ESMC_Time *currSimTime,
                                 ESMC_Time *prevSimTime,
                                 ESMF_IKIND_I8 *advanceCount,
                                 int *numAlarms,
                                 int *status) {
          int rc = (ptr)->ESMC_ClockGet(timeStep, startTime, stopTime,
                                        refTime, currTime, prevTime,
                                        currSimTime, prevSimTime,
                                        advanceCount, numAlarms);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockaddalarm)(ESMC_Clock *ptr,
                                      ESMC_Alarm *alarm,
                                      int *status) {
          int rc = (ptr)->ESMC_ClockAddAlarm(alarm);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockgetalarm)(ESMC_Clock *ptr,
                                      int *i,
                                      ESMC_Alarm **alarm,
                                      int *status) {
          int rc = (ptr)->ESMC_ClockGetAlarm(*i, alarm);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockgetringingalarm)(ESMC_Clock *ptr,
                                             int *i,
                                             ESMC_Alarm **alarm,
                                             int *status) {
          int rc = (ptr)->ESMC_ClockGetRingingAlarm(*i, alarm);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockadvance)(ESMC_Clock *ptr,
                                     ESMC_TimeInterval *timeStep,
                                     int *numRingingAlarms, int *status) {
          int rc = (ptr)->ESMC_ClockAdvance(timeStep, numRingingAlarms);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockisstoptime)(ESMC_Clock *ptr, 
                                      int *esmf_clockIsStopTime, int *status) {
          *esmf_clockIsStopTime = (int) (ptr)->ESMC_ClockIsStopTime(status);
       }

       void FTN(c_esmc_clocksynctorealtime)(ESMC_Clock *ptr, int *status) {
          int rc = (ptr)->ESMC_ClockSyncToRealTime();
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockreadrestart)(ESMC_Clock *ptr, 
                                         ESMC_TimeInterval *timeStep,
                                         ESMC_Time *startTime,
                                         ESMC_Time *stopTime,
                                         ESMC_Time *refTime,
                                         ESMC_Time *currTime,
                                         ESMC_Time *prevTime,
                                         ESMF_IKIND_I8 *advanceCount,
                                         int *numAlarms,
                                         ESMC_Alarm *alarmList[],
                                         int *status) {
          int rc = (ptr)->ESMC_ClockReadRestart(timeStep, startTime,
                                                stopTime, refTime,
                                                currTime, prevTime,
                                                *advanceCount, *numAlarms,
                                                alarmList);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockwriterestart)(ESMC_Clock *ptr, 
                                          ESMC_TimeInterval *timeStep,
                                          ESMC_Time *startTime,
                                          ESMC_Time *stopTime,
                                          ESMC_Time *refTime,
                                          ESMC_Time *currTime,
                                          ESMC_Time *prevTime,
                                          ESMF_IKIND_I8 *advanceCount,
                                          int *numAlarms,
                                          ESMC_Alarm *alarmList[],
                                          int *status) {
          int rc = (ptr)->ESMC_ClockWriteRestart(timeStep, startTime,
                                                 stopTime, refTime,
                                                 currTime, prevTime,
                                                 advanceCount, numAlarms, 
                                                 alarmList);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->ESMC_ClockValidate(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock *ptr, const char *options,
                                   int *status) {
          int rc = (ptr)->ESMC_ClockPrint(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }
};
