// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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
#include <cstdio>

#include "ESMCI_F90Interface.h"
#include "ESMCI_Clock.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMCI\_Clock} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN_X(c_esmc_clockcreatenew)(Clock **ptr,
                                       int *nameLen,
                                       const char *name,
                                       TimeInterval *timeStep,
                                       Time *startTime,
                                       Time *stopTime,
                                       TimeInterval *runDuration,
                                       int *runTimeStepCount,
                                       Time *refTime,
                                       int *status,
                                       ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_ClockCreate(
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

       void FTN_X(c_esmc_clockcreatecopy)(Clock **ptr,
                                        Clock **clock,
                                        int *status) {
          *ptr = ESMCI_ClockCreate(
                                           *clock,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockdestroy)(Clock **ptr, int *status) {
          int rc = ESMCI_ClockDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockset)(Clock **ptr,
                                 int *nameLen,
                                 const char *name,
                                 TimeInterval *timeStep,
                                 Time *startTime,
                                 Time *stopTime,
                                 TimeInterval *runDuration,
                                 int *runTimeStepCount,
                                 Time *refTime,
                                 Time *currTime,
                                 ESMC_I8 *advanceCount,
                                 ESMC_Direction *direction,
                                 int *status,
                                 ESMCI_FortranStrLenArg name_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::set(
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

       void FTN_X(c_esmc_clockget)(Clock **ptr,
                                 int *nameLen,
                                 int *tempNameLen,
                                 char *tempName,
                                 TimeInterval *timeStep,
                                 Time *startTime,
                                 Time *stopTime,
                                 TimeInterval *runDuration,
                                 ESMC_R8 *runTimeStepCount,
                                 Time *refTime,
                                 Time *currTime,
                                 Time *prevTime,
                                 TimeInterval *currSimTime,
                                 TimeInterval *prevSimTime,
                                 Calendar **calendar,
                                 ESMC_CalKind_Flag *calkindflag,
                                 int *timeZone,
                                 ESMC_I8 *advanceCount,
                                 int *alarmCount,
                                 ESMC_Direction *direction,
                                 int *status,
                                 ESMCI_FortranStrLenArg tempName_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::get(
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
                    ESMC_NOT_PRESENT_FILTER(calkindflag),
                    ESMC_NOT_PRESENT_FILTER(timeZone),
                    ESMC_NOT_PRESENT_FILTER(advanceCount),
                    ESMC_NOT_PRESENT_FILTER(alarmCount),
                    ESMC_NOT_PRESENT_FILTER(direction) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for ringingAlarmList() size > 1
       void FTN_X(c_esmc_clockadvance2)(Clock **ptr,
                                   TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   char *ringingAlarmList2ndElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status,
                                   ESMCI_FortranStrLenArg raList1El_l,
                                   ESMCI_FortranStrLenArg raList2El_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::advance(
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
       void FTN_X(c_esmc_clockadvance1)(Clock **ptr,
                                   TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status,
                                   ESMCI_FortranStrLenArg raListEl_1) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::advance(
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
       void FTN_X(c_esmc_clockadvance0)(Clock **ptr,
                                   TimeInterval *timeStep,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::advance(
                     ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NULL_POINTER,
                                             ESMC_NULL_POINTER,
                                                   // ringingAlarmList missing
                                            *sizeofRingingAlarmList,
                                             // always present internal argument
                     ESMC_NOT_PRESENT_FILTER(ringingAlarmCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockisstoptime)(Clock **ptr,
                                      int *esmf_clockIsStopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTime = (int) (*ptr)->Clock::isStopTime(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockstoptimeenable)(Clock **ptr,
                                            Time *stopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::stopTimeEnable(
                                         ESMC_NOT_PRESENT_FILTER(stopTime) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockstoptimedisable)(Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::stopTimeDisable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockisstoptimeenabled)(Clock **ptr,
                               int *esmf_clockIsStopTimeEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTimeEnabled =
                         (int) (*ptr)->Clock::isStopTimeEnabled(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockisdone)(Clock **ptr,
                                    int *esmf_clockIsDone, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsDone = (int) (*ptr)->Clock::isDone(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockisreverse)(Clock **ptr,
                                    int *esmf_clockIsReverse, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsReverse = (int) (*ptr)->Clock::isReverse(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockgetnexttime)(Clock **ptr,
                                 Time *nextTime,
                                 TimeInterval *timeStep,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::getNextTime(
                                                 nextTime,  // required
                         ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockgetalarm)(Clock **ptr,
                                      int *alarmnameLen,
                                      char *alarmname,
                                      Alarm **alarm,
                                      int *status,
                                      ESMCI_FortranStrLenArg name_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::getAlarm(*alarmnameLen, alarmname, alarm);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockgetalarmlist3)(Clock **ptr,
                                           ESMC_AlarmList_Flag *alarmlistflag,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           TimeInterval *timeStep,
                                           int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::getAlarmList(*alarmlistflag,
                                                  ESMC_NULL_POINTER,
                                                  ESMC_NULL_POINTER,
                                                 *sizeofAlarmList,
                          ESMC_NOT_PRESENT_FILTER(alarmCount),
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size > 1
       void FTN_X(c_esmc_clockgetalarmlist2)(Clock **ptr,
                                           ESMC_AlarmList_Flag *alarmlistflag,
                                           char *AlarmList1stElementPtr,
                                           char *AlarmList2ndElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           TimeInterval *timeStep,
                                           int *status,
                                           ESMCI_FortranStrLenArg AlList1El_l,
                                           ESMCI_FortranStrLenArg AlList2El_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::getAlarmList(*alarmlistflag,
                                                  AlarmList1stElementPtr,
                                                  AlarmList2ndElementPtr,
                                                 *sizeofAlarmList,
                          ESMC_NOT_PRESENT_FILTER(alarmCount),
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size == 1
       void FTN_X(c_esmc_clockgetalarmlist1)(Clock **ptr,
                                           ESMC_AlarmList_Flag *alarmlistflag,
                                           char *AlarmList1stElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           TimeInterval *timeStep,
                                           int *status,
                                           ESMCI_FortranStrLenArg AlListEl_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::getAlarmList(*alarmlistflag,
                                                  AlarmList1stElementPtr,
                                                  ESMC_NULL_POINTER,
                                                 *sizeofAlarmList,
                          ESMC_NOT_PRESENT_FILTER(alarmCount),
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clocksynctorealtime)(Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::syncToRealTime();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockeq)(Clock **clock1, Clock **clock2,
                                   int *esmf_clockEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockEQ)
          *esmf_clockEQ = (int) (**clock1 == **clock2);
       }

       void FTN_X(c_esmc_clockne)(Clock **clock1, Clock **clock2,
                                   int *esmf_clockNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockNE)
          *esmf_clockNE = (int) (**clock1 != **clock2);
       }

       void FTN_X(c_esmc_clockreadrestart)(Clock **ptr, int *nameLen,
                                         const char *name,
                                         int *status,
                                         ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_ClockReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_clockwriterestart)(Clock **ptr,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::writeRestart();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockvalidate)(Clock **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::validate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_clockprint)(Clock **ptr, const char *options,
                                   int *status,
                                   ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Clock::print(
                   ESMC_NOT_PRESENT_FILTER(options) );
          fflush (stdout);
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

} // namespace ESMCI
