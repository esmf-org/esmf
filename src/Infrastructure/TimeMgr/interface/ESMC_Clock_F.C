// $Id: ESMC_Clock_F.C,v 1.48 2008/06/26 02:08:16 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
#include <ESMCI_Clock.h>
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

       void FTN(c_esmc_clockcreatenew)(ESMCI::Clock **ptr,
                                       int *nameLen,
                                       const char *name,
                                       ESMCI::TimeInterval *timeStep,
                                       ESMCI::Time *startTime,
                                       ESMCI::Time *stopTime,
                                       ESMCI::TimeInterval *runDuration,
                                       int *runTimeStepCount,
                                       ESMCI::Time *refTime,
                                       int *status) {
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

       void FTN(c_esmc_clockcreatecopy)(ESMCI::Clock **ptr,
                                        ESMCI::Clock **clock,
                                        int *status) {
          *ptr = ESMCI_ClockCreate(
                                           *clock,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockdestroy)(ESMCI::Clock **ptr, int *status) {
          int rc = ESMCI_ClockDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockset)(ESMCI::Clock **ptr,
                                 int *nameLen,
                                 const char *name,
                                 ESMCI::TimeInterval *timeStep,
                                 ESMCI::Time *startTime,
                                 ESMCI::Time *stopTime,
                                 ESMCI::TimeInterval *runDuration,
                                 int *runTimeStepCount,
                                 ESMCI::Time *refTime,
                                 ESMCI::Time *currTime,
                                 ESMC_I8 *advanceCount,
                                 ESMC_Direction *direction,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::set(
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

       void FTN(c_esmc_clockget)(ESMCI::Clock **ptr,
                                 int *nameLen,
                                 int *tempNameLen,
                                 char *tempName,
                                 ESMCI::TimeInterval *timeStep,
                                 ESMCI::Time *startTime,
                                 ESMCI::Time *stopTime,
                                 ESMCI::TimeInterval *runDuration,
                                 ESMC_R8 *runTimeStepCount,
                                 ESMCI::Time *refTime,
                                 ESMCI::Time *currTime,
                                 ESMCI::Time *prevTime,
                                 ESMCI::TimeInterval *currSimTime,
                                 ESMCI::TimeInterval *prevSimTime,
                                 ESMCI::Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 int *timeZone,
                                 ESMC_I8 *advanceCount,
                                 int *alarmCount,
                                 ESMC_Direction *direction,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::get(
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
       void FTN(c_esmc_clockadvance2)(ESMCI::Clock **ptr,
                                   ESMCI::TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   char *ringingAlarmList2ndElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::advance(
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
       void FTN(c_esmc_clockadvance1)(ESMCI::Clock **ptr,
                                   ESMCI::TimeInterval *timeStep,
                                   char *ringingAlarmList1stElementPtr,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::advance(
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
       void FTN(c_esmc_clockadvance0)(ESMCI::Clock **ptr,
                                   ESMCI::TimeInterval *timeStep,
                                   int *sizeofRingingAlarmList,
                                   int *ringingAlarmCount, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::advance(
                     ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NULL_POINTER,
                                             ESMC_NULL_POINTER,
                                                   // ringingAlarmList missing
                                            *sizeofRingingAlarmList,
                                             // always present internal argument
                     ESMC_NOT_PRESENT_FILTER(ringingAlarmCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockisstoptime)(ESMCI::Clock **ptr, 
                                      int *esmf_clockIsStopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTime = (int) (*ptr)->ESMCI::Clock::isStopTime(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockstoptimeenable)(ESMCI::Clock **ptr,
                                            ESMCI::Time *stopTime, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::stopTimeEnable(
                                         ESMC_NOT_PRESENT_FILTER(stopTime) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockstoptimedisable)(ESMCI::Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::stopTimeDisable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockisstoptimeenabled)(ESMCI::Clock **ptr, 
                               int *esmf_clockIsStopTimeEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsStopTimeEnabled =
                         (int) (*ptr)->ESMCI::Clock::isStopTimeEnabled(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockisdone)(ESMCI::Clock **ptr, 
                                    int *esmf_clockIsDone, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsDone = (int) (*ptr)->ESMCI::Clock::isDone(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockisreverse)(ESMCI::Clock **ptr, 
                                    int *esmf_clockIsReverse, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_clockIsReverse = (int) (*ptr)->ESMCI::Clock::isReverse(
                                            ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockgetnexttime)(ESMCI::Clock **ptr,
                                 ESMCI::Time *nextTime,
                                 ESMCI::TimeInterval *timeStep,
                                 int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::getNextTime(
                                                 nextTime,  // required
                         ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockgetalarm)(ESMCI::Clock **ptr,
                                      int *nameLen,
                                      char *name,
                                      ESMCI::Alarm **alarm,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::getAlarm(*nameLen, name, alarm);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size > 1
       void FTN(c_esmc_clockgetalarmlist2)(ESMCI::Clock **ptr,
                                           ESMC_AlarmListType *type,
                                           char *AlarmList1stElementPtr,
                                           char *AlarmList2ndElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           ESMCI::TimeInterval *timeStep,
                                           int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::getAlarmList(*type,
                                                  AlarmList1stElementPtr,
                                                  AlarmList2ndElementPtr,
                                                 *sizeofAlarmList,
                                                  alarmCount,
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for alarmList() size == 1
       void FTN(c_esmc_clockgetalarmlist1)(ESMCI::Clock **ptr,
                                           ESMC_AlarmListType *type,
                                           char *AlarmList1stElementPtr,
                                           int *sizeofAlarmList,
                                           int *alarmCount,
                                           ESMCI::TimeInterval *timeStep,
                                           int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::getAlarmList(*type,
                                                  AlarmList1stElementPtr,
                                                  ESMC_NULL_POINTER,
                                                 *sizeofAlarmList,
                                                  alarmCount,
                          ESMC_NOT_PRESENT_FILTER(timeStep) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clocksynctorealtime)(ESMCI::Clock **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::syncToRealTime();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockeq)(ESMCI::Clock **clock1, ESMCI::Clock **clock2,
                                   int *esmf_clockEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockEQ)
          *esmf_clockEQ = (int) (**clock1 == **clock2);
       }

       void FTN(c_esmc_clockne)(ESMCI::Clock **clock1, ESMCI::Clock **clock2,
                                   int *esmf_clockNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*clock1, *clock2, esmf_clockNE)
          *esmf_clockNE = (int) (**clock1 != **clock2);
       }

       void FTN(c_esmc_clockreadrestart)(ESMCI::Clock **ptr, int *nameLen,
                                         const char *name,
                                         ESMC_IOSpec *iospec,   
                                         int *status) {    
          *ptr = ESMCI_ClockReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(iospec),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_clockwriterestart)(ESMCI::Clock **ptr,
                                          ESMC_IOSpec *iospec,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::writeRestart(
                          ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockvalidate)(ESMCI::Clock **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::validate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_clockprint)(ESMCI::Clock **ptr, const char *options,
                                   int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Clock::print(
                   ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

} // namespace ESMCI
