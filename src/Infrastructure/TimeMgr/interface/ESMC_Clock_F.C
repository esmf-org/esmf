// $Id: ESMC_Clock_F.C,v 1.41.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <ESMCI_F90Interface.h>
#include <ESMC_Clock.h>
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Clock} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_clockcreatenew)(ESMC_Clock **ptr,
                                       int *nameLen,
                                       const char *name,
                                       ESMC_TimeInterval *timeStep,
                                       ESMC_Time *startTime,
                                       ESMC_Time *stopTime,
                                       ESMC_TimeInterval *runDuration,
                                       int *runTimeStepCount,
                                       ESMC_Time *refTime,
                                       int *status) {
          *ptr = ESMC_ClockCreate(
                                           *nameLen,   // always present 
                                                       //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                            timeStep,   // required
                                            startTime,  // required
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(runDuration),
                    ESMC_NOT_PRESENT_FILTER(runTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockcreatecopy)(ESMC_Clock **ptr,
                                        ESMC_Clock **clock,
                                        int *status) {
          *ptr = ESMC_ClockCreate(
                                           *clock,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockdestroy)(ESMC_Clock **ptr, int *status) {
          int rc = ESMC_ClockDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockset)(ESMC_Clock **ptr,
                                 int *nameLen,
                                 const char *name,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_TimeInterval *runDuration,
                                 int *runTimeStepCount,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMC_I8 *advanceCount,
                                 ESMC_Direction *direction,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockSet(
                                           *nameLen,   // always present 
                                                       //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                    ESMC_NOT_PRESENT_FILTER(timeStep),
                    ESMC_NOT_PRESENT_FILTER(startTime),
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(runDuration),
                    ESMC_NOT_PRESENT_FILTER(runTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(currTime),
                    ESMC_NOT_PRESENT_FILTER(advanceCount),
                    ESMC_NOT_PRESENT_FILTER(direction) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockget)(ESMC_Clock **ptr,
                                 int *nameLen,
                                 int *tempNameLen,
                                 char *tempName,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_TimeInterval *runDuration,
                                 ESMC_R8 *runTimeStepCount,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMC_Time *prevTime,
                                 ESMC_TimeInterval *currSimTime,
                                 ESMC_TimeInterval *prevSimTime,
                                 ESMC_Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 int *timeZone,
                                 ESMC_I8 *advanceCount,
                                 int *alarmCount,
                                 ESMC_Direction *direction,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockGet(
			                  // always present internal arguments.
                                           *nameLen,
                                            tempNameLen,
                                            tempName,
                    ESMC_NOT_PRESENT_FILTER(timeStep),
                    ESMC_NOT_PRESENT_FILTER(startTime),
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(runDuration),
                    ESMC_NOT_PRESENT_FILTER(runTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(currTime),
                    ESMC_NOT_PRESENT_FILTER(prevTime),
                    ESMC_NOT_PRESENT_FILTER(currSimTime),
                    ESMC_NOT_PRESENT_FILTER(prevSimTime),
                    ESMC_NOT_PRESENT_FILTER(calendar),
                    ESMC_NOT_PRESENT_FILTER(calendarType),
                    ESMC_NOT_PRESENT_FILTER(timeZone),
                    ESMC_NOT_PRESENT_FILTER(advanceCount),
                    ESMC_NOT_PRESENT_FILTER(alarmCount),
                    ESMC_NOT_PRESENT_FILTER(direction) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for ringingAlarmList() size > 1
       void FTN(c_esmc_clockadvance2)(ESMC_Clock **ptr,
                                   ESMC_TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   char *ringingAlarmList2ndElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockAdvance(
                     ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ringingAlarmList1stElementPtr,
                                                   // ringingAlarmList present,
                                             ringingAlarmList2ndElementPtr,
                                                   // size > 1
                                            *sizeofRingingAlarmList,
                                             // always present internal argument
                     ESMC_NOT_PRESENT_FILTER(ringingAlarmCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for ringingAlarmList() size == 1
       void FTN(c_esmc_clockadvance1)(ESMC_Clock **ptr,
                                   ESMC_TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockAdvance(
                     ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ringingAlarmList1stElementPtr,
                                                   // ringingAlarmList present,
                                             ESMC_NULL_POINTER,
                                                   // size == 1
                                            *sizeofRingingAlarmList,
                                             // always present internal argument
                     ESMC_NOT_PRESENT_FILTER(ringingAlarmCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for ringingAlarmList() size == 0 (missing) 
       void FTN(c_esmc_clockadvance0)(ESMC_Clock **ptr,
                                   ESMC_TimeInterval *timeStep,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockAdvance(
                     ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NULL_POINTER,
                                             ESMC_NULL_POINTER,
                                                   // ringingAlarmList missing
                                            *sizeofRingingAlarmList,
                                             // always present internal argument
                     ESMC_NOT_PRESENT_FILTER(ringingAlarmCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockisstoptime)(ESMC_Clock **ptr, 
                                      int *esmf_clockIsStopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTime = (int) (*ptr)->ESMC_ClockIsStopTime(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockstoptimeenable)(ESMC_Clock **ptr,
                                            ESMC_Time *stopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockStopTimeEnable(
                                         ESMC_NOT_PRESENT_FILTER(stopTime) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockstoptimedisable)(ESMC_Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockStopTimeDisable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockisstoptimeenabled)(ESMC_Clock **ptr, 
                               int *esmf_clockIsStopTimeEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTimeEnabled =
                         (int) (*ptr)->ESMC_ClockIsStopTimeEnabled(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockisdone)(ESMC_Clock **ptr, 
                                    int *esmf_clockIsDone, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsDone = (int) (*ptr)->ESMC_ClockIsDone(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockisreverse)(ESMC_Clock **ptr, 
                                    int *esmf_clockIsReverse, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsReverse = (int) (*ptr)->ESMC_ClockIsReverse(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockgetnexttime)(ESMC_Clock **ptr,
                                 ESMC_Time *nextTime,
                                 ESMC_TimeInterval *timeStep,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockGetNextTime(
                                                 nextTime,  // required
                         ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockgetalarm)(ESMC_Clock **ptr,
                                      int *nameLen,
                                      char *name,
                                      ESMC_Alarm **alarm,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockGetAlarm(*nameLen, name, alarm);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size > 1
       void FTN(c_esmc_clockgetalarmlist2)(ESMC_Clock **ptr,
                                           ESMC_AlarmListType *type,
                                           char *AlarmList1stElementPtr,
                                           char *AlarmList2ndElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           ESMC_TimeInterval *timeStep,
                                           int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockGetAlarmList(*type,
                                                  AlarmList1stElementPtr,
                                                  AlarmList2ndElementPtr,
                                                 *sizeofAlarmList,
                                                  alarmCount,
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size == 1
       void FTN(c_esmc_clockgetalarmlist1)(ESMC_Clock **ptr,
                                           ESMC_AlarmListType *type,
                                           char *AlarmList1stElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           ESMC_TimeInterval *timeStep,
                                           int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockGetAlarmList(*type,
                                                  AlarmList1stElementPtr,
                                                  ESMC_NULL_POINTER,
                                                 *sizeofAlarmList,
                                                  alarmCount,
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clocksynctorealtime)(ESMC_Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockSyncToRealTime();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockeq)(ESMC_Clock **clock1, ESMC_Clock **clock2,
                                   int *esmf_clockEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockEQ)
          *esmf_clockEQ = (int) (**clock1 == **clock2);
       }

       void FTN(c_esmc_clockne)(ESMC_Clock **clock1, ESMC_Clock **clock2,
                                   int *esmf_clockNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockNE)
          *esmf_clockNE = (int) (**clock1 != **clock2);
       }

       void FTN(c_esmc_clockreadrestart)(ESMC_Clock **ptr, int *nameLen,
                                         const char *name,
                                         ESMC_IOSpec *iospec,   
                                         int *status) {    
          *ptr = ESMC_ClockReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(iospec),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockwriterestart)(ESMC_Clock **ptr,
                                          ESMC_IOSpec *iospec,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockWriteRestart(
                          ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockValidate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock **ptr, const char *options,
                                   int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_ClockPrint(
                   ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
};
