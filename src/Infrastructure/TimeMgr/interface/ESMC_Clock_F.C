// $Id: ESMC_Clock_F.C,v 1.18 2003/10/22 01:06:28 eschwab Exp $
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
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_clockcreate)(ESMC_Clock **ptr,
                                    int *nameLen,
                                    const char *name,
                                    ESMC_TimeInterval *timeStep,
                                    ESMC_Time *startTime,
                                    ESMC_Time *stopTime,
                                    ESMC_Time *refTime,
                                    int *status) {
          *ptr = ESMC_ClockCreate(
                    *nameLen,   // always present internal argument.

                    ((void*) name     == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                    timeStep,   // required
                    startTime,  // required

                    ((void*) stopTime == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : stopTime),
                    ((void*) refTime  == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : refTime),
                    ((void*) status   == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_clockdestroy)(ESMC_Clock **ptr, int *status) {
          int rc = ESMC_ClockDestroy(*ptr);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockset)(ESMC_Clock **ptr,
                                 int *nameLen,
                                 const char *name,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMF_KIND_I8 *advanceCount,
                                 int *status) {
          int rc = (*ptr)->ESMC_ClockSet(
                 *nameLen,   // always present internal argument.

                 ((void*) name         == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : name),
                 ((void*) timeStep     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : timeStep),
                 ((void*) startTime    == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : startTime),
                 ((void*) stopTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : stopTime),
                 ((void*) refTime      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : refTime),
                 ((void*) currTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : currTime),
                 ((void*) advanceCount == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : advanceCount) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockget)(ESMC_Clock **ptr,
                                 int *nameLen,
                                 int *tempNameLen,
                                 char *tempName,
                                 ESMC_TimeInterval *timeStep,
                                 ESMC_Time *startTime,
                                 ESMC_Time *stopTime,
                                 ESMC_Time *refTime,
                                 ESMC_Time *currTime,
                                 ESMC_Time *prevTime,
                                 ESMC_Time *currSimTime,
                                 ESMC_Time *prevSimTime,
                                 ESMF_KIND_I8 *advanceCount,
                                 int *numAlarms,
                                 int *status) {
          int rc = (*ptr)->ESMC_ClockGet(
                 *nameLen,      // always present internal argument.

                 tempNameLen,   // always present internal argument.

                 ((void*) tempName     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : tempName),
                 ((void*) timeStep     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : timeStep),
                 ((void*) startTime    == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : startTime),
                 ((void*) stopTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : stopTime),
                 ((void*) refTime      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : refTime),
                 ((void*) currTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : currTime),
                 ((void*) prevTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : prevTime),
                 ((void*) currSimTime  == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : currSimTime),
                 ((void*) prevSimTime  == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : prevSimTime),
                 ((void*) advanceCount == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : advanceCount),
                 ((void*) numAlarms    == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : numAlarms) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockadvance)(ESMC_Clock **ptr,
                                   ESMC_TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   char *ringingAlarmList2ndElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *numRingingAlarms, int *status) {
          int rc = (*ptr)->ESMC_ClockAdvance(
             ((void*) timeStep         == (void*)ESMC_BAD_POINTER ?
                            ESMC_NULL_POINTER : timeStep),
             ((void*) ringingAlarmList1stElementPtr == (void*)ESMC_BAD_POINTER ?
                            ESMC_NULL_POINTER : ringingAlarmList1stElementPtr),
             ((void*) ringingAlarmList2ndElementPtr == (void*)ESMC_BAD_POINTER ?
                            ESMC_NULL_POINTER : ringingAlarmList2ndElementPtr),
             sizeofRingingAlarmList,  // always present internal argument.

             ((void*) numRingingAlarms == (void*)ESMC_BAD_POINTER ?
                            ESMC_NULL_POINTER : numRingingAlarms) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockisstoptime)(ESMC_Clock **ptr, 
                                      int *esmf_clockIsStopTime, int *status) {
          *esmf_clockIsStopTime = (int) (*ptr)->ESMC_ClockIsStopTime(
                       ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                 ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_clockgetnexttime)(ESMC_Clock **ptr,
                                 ESMC_Time *nextTime,
                                 ESMC_TimeInterval *timeStep,
                                 int *status) {
          int rc = (*ptr)->ESMC_ClockGetNextTime(
                 nextTime,  // required

                 ((void*) timeStep     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : timeStep) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockgetalarm)(ESMC_Clock **ptr,
                                      int *nameLen,
                                      char *name,
                                      ESMC_Alarm **alarm,
                                      int *status) {
          int rc = (*ptr)->ESMC_ClockGetAlarm(*nameLen, name, alarm);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockgetalarmlist)(ESMC_Clock **ptr,
                                          ESMC_AlarmListType *type,
                                          char *AlarmList1stElementPtr,
                                          char *AlarmList2ndElementPtr,
                                          int *sizeofAlarmList,
                                          int *numAlarms,
                                          ESMC_TimeInterval *timeStep,
                                          int *status) {
          int rc = (*ptr)->ESMC_ClockGetAlarmList(*type,
                                                  AlarmList1stElementPtr,
                                                  AlarmList2ndElementPtr,
                                                  sizeofAlarmList,
                                                  numAlarms,
                          ((void*) timeStep == (void*)ESMC_BAD_POINTER ?
                              ESMC_NULL_POINTER : timeStep));
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clocksynctorealtime)(ESMC_Clock **ptr, int *status) {
          int rc = (*ptr)->ESMC_ClockSyncToRealTime();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockeq)(ESMC_Clock **clock1, ESMC_Clock **clock2,
                                   int *esmf_clockEQ) {
           *esmf_clockEQ = (int) (**clock1 == **clock2);
       }

       void FTN(c_esmc_clockreadrestart)(ESMC_Clock **ptr, 
                                         ESMC_TimeInterval *timeStep,
                                         ESMC_Time *startTime,
                                         ESMC_Time *stopTime,
                                         ESMC_Time *refTime,
                                         ESMC_Time *currTime,
                                         ESMC_Time *prevTime,
                                         ESMF_KIND_I8 *advanceCount,
                                         int *numAlarms,
                                         ESMC_Alarm *alarmList[],
                                         int *status) {
          int rc = (*ptr)->ESMC_ClockReadRestart(timeStep, startTime,
                                                stopTime, refTime,
                                                currTime, prevTime,
                                                *advanceCount, *numAlarms,
                                                alarmList);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockwriterestart)(ESMC_Clock **ptr, 
                                          ESMC_TimeInterval *timeStep,
                                          ESMC_Time *startTime,
                                          ESMC_Time *stopTime,
                                          ESMC_Time *refTime,
                                          ESMC_Time *currTime,
                                          ESMC_Time *prevTime,
                                          ESMF_KIND_I8 *advanceCount,
                                          int *numAlarms,
                                          ESMC_Alarm *alarmList[],
                                          int *status) {
          int rc = (*ptr)->ESMC_ClockWriteRestart(timeStep, startTime,
                                                 stopTime, refTime,
                                                 currTime, prevTime,
                                                 advanceCount, numAlarms, 
                                                 alarmList);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockvalidate)(ESMC_Clock **ptr, const char *options,
                                      int *status) {
          int rc = (*ptr)->ESMC_ClockValidate(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_clockprint)(ESMC_Clock **ptr, const char *options,
                                   int *status) {
          int rc = (*ptr)->ESMC_ClockPrint(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
