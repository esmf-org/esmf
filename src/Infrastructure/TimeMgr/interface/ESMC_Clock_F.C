// $Id: ESMC_Clock_F.C,v 1.11 2003/06/07 00:41:59 eschwab Exp $
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

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_clockset)(ESMC_Clock *ptr,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_Time *refTime,
                                 int *status) {
           *status = (ptr)->ESMC_ClockSet(timeStep, startTime, stopTime,
                                          refTime);
       }

       void FTN(c_esmc_clockaddalarm)(ESMC_Clock *ptr,
                                      ESMC_Alarm *alarm,
                                      int *status) {
           *status = (ptr)->ESMC_ClockAddAlarm(alarm);
       }

       void FTN(c_esmc_clockgetalarmlist)(ESMC_Clock *ptr,
                                          ESMC_Alarm **alarmList,
                                          int *numAlarms, int *status) {
           *status = (ptr)->ESMC_ClockGetAlarmList(alarmList, numAlarms);
       }

       void FTN(c_esmc_clockgetnumalarms)(ESMC_Clock *ptr,
                                          int *numAlarms, int *status) {
           *status = (ptr)->ESMC_ClockGetNumAlarms(numAlarms);
       }

       void FTN(c_esmc_clocksynctowallclock)(ESMC_Clock *ptr, int *status) {
           *status = (ptr)->ESMC_ClockSyncToWallClock();
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

       void FTN(c_esmc_clockgettimestep)(ESMC_Clock *ptr, 
                                         ESMC_TimeInterval *timeStep,
                                         int *status) {
           *status = (ptr)->ESMC_ClockGetTimeStep(timeStep);
       }

       void FTN(c_esmc_clocksettimestep)(ESMC_Clock *ptr, 
                                         ESMC_TimeInterval *timeStep,
                                         int *status) {
           *status = (ptr)->ESMC_ClockSetTimeStep(timeStep);
       }

       void FTN(c_esmc_clockgetcurrtime)(ESMC_Clock *ptr, 
                                         ESMC_Time *currTime,
                                         int *status) {
           *status = (ptr)->ESMC_ClockGetCurrTime(currTime);
       }

       void FTN(c_esmc_clocksetcurrtime)(ESMC_Clock *ptr, 
                                         ESMC_Time *currTime,
                                         int *status) {
           *status = (ptr)->ESMC_ClockSetCurrTime(currTime);
       }

       void FTN(c_esmc_clockgetstarttime)(ESMC_Clock *ptr, 
                                          ESMC_Time *startTime,
                                          int *status) {
           *status = (ptr)->ESMC_ClockGetStartTime(startTime);
       }

       void FTN(c_esmc_clockgetstoptime)(ESMC_Clock *ptr, 
                                         ESMC_Time *stopTime,
                                         int *status) {
           *status = (ptr)->ESMC_ClockGetStopTime(stopTime);
       }

       void FTN(c_esmc_clockgetreftime)(ESMC_Clock *ptr, 
                                        ESMC_Time *refTime,
                                        int *status) {
           *status = (ptr)->ESMC_ClockGetRefTime(refTime);
       }

       void FTN(c_esmc_clockgetprevtime)(ESMC_Clock *ptr, 
                                         ESMC_Time *prevTime,
                                         int *status) {
           *status = (ptr)->ESMC_ClockGetPrevTime(prevTime);
       }

       void FTN(c_esmc_clockgetcurrsimtime)(ESMC_Clock *ptr, 
                                            ESMC_TimeInterval *currSimTime,
                                            int *status) {
           *status = (ptr)->ESMC_ClockGetCurrSimTime(currSimTime);
       }

       void FTN(c_esmc_clockgetprevsimtime)(ESMC_Clock *ptr, 
                                            ESMC_TimeInterval *prevSimTime,
                                            int *status) {
           *status = (ptr)->ESMC_ClockGetPrevSimTime(prevSimTime);
       }

       void FTN(c_esmc_clockread)(ESMC_Clock *ptr, 
                                  ESMC_TimeInterval *timeStep,
                                  ESMC_Time *startTime,
                                  ESMC_Time *stopTime,
                                  ESMC_Time *refTime,
                                  ESMC_Time *currTime,
                                  ESMC_Time *prevTime,
                                  ESMF_IKIND_I8 *advanceCount,
                                  ESMC_Alarm *alarmList[],
                                  int *numAlarms, int *status) {
           *status = (ptr)->ESMC_ClockRead(timeStep, startTime,
                                           stopTime, refTime,
                                           currTime, prevTime,
                                           *advanceCount, alarmList,
                                           *numAlarms);
       }

       void FTN(c_esmc_clockwrite)(ESMC_Clock *ptr, 
                                   ESMC_TimeInterval *timeStep,
                                   ESMC_Time *startTime,
                                   ESMC_Time *stopTime,
                                   ESMC_Time *refTime,
                                   ESMC_Time *currTime,
                                   ESMC_Time *prevTime,
                                   ESMF_IKIND_I8 *advanceCount,
                                   ESMC_Alarm *alarmList[],
                                   int *numAlarms, int *status) {
           *status = (ptr)->ESMC_ClockWrite(timeStep, startTime,
                                            stopTime, refTime,
                                            currTime, prevTime,
                                            advanceCount, alarmList,
                                            numAlarms);
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock *ptr, const char *opts,
                                      int *status) {
           *status = (ptr)->ESMC_ClockValidate(opts);
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock *ptr, const char *opts,
                                   int *status) {
           *status = (ptr)->ESMC_ClockPrint(opts);
       }
};
