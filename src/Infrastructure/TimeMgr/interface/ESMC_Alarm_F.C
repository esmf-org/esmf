// $Id: ESMC_Alarm_F.C,v 1.16 2003/10/22 01:03:48 eschwab Exp $
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
#include "ESMC_Alarm.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Alarm} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_alarmcreate)(ESMC_Alarm **ptr, int *nameLen, 
                                    const char *name, ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                bool *enabled, bool *sticky, int *status) {
          *ptr = ESMC_AlarmCreate(
                 *nameLen,  // always present internal argument.

                 ((void*) name         == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : name),
                 *clock,    // required.

                 ((void*) ringTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : ringTime),
                 ((void*) ringInterval == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : ringInterval),
                 ((void*) stopTime     == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : stopTime),
                 ((void*) ringDuration == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : ringDuration),
                 ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                   ESMC_NULL_POINTER : nRingDurationTimeSteps),
                 ((void*) refTime      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : refTime),
                 ((void*) enabled      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : enabled),
                 ((void*) sticky       == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : sticky),
                 ((void*) status       == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmdestroy)(ESMC_Alarm **ptr, int *status) {
          int rc = ESMC_AlarmDestroy(*ptr);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmset)(ESMC_Alarm **ptr, int *nameLen, 
                                 const char *name, ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                bool *ringing, bool *enabled, bool *sticky,
                int *status) {
          int rc = (*ptr)->ESMC_AlarmSet(
                 *nameLen,  // always present internal argument.

                 ((void*) name          == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : name),
                 ((void*) clock         == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : clock),
                 ((void*) ringTime      == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringTime),
                 ((void*) ringInterval  == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringInterval),
                 ((void*) stopTime      == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : stopTime),
                 ((void*) ringDuration  == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringDuration),
                 ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                   ESMC_NULL_POINTER : nRingDurationTimeSteps),
                 ((void*) refTime       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : refTime),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmget)(ESMC_Alarm **ptr, int *nameLen, 
                                 int *tempNameLen, char *tempName,
                                 ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_Time *prevRingTime, 
                ESMC_TimeInterval *ringInterval, ESMC_Time *stopTime,
                ESMC_TimeInterval *ringDuration, int *nRingDurationTimeSteps,
                int *nTimeStepsRinging, ESMC_Time *ringBegin,
                ESMC_Time *refTime, bool *ringing, bool *ringingOnPrevTimeStep,
                bool *enabled, bool *sticky, int *status) {
          int rc = (*ptr)->ESMC_AlarmGet(
                 *nameLen,      // always present internal argument.

                 tempNameLen,   // always present internal argument.

                 ((void*) tempName      == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : tempName),
                 ((void*) clock         == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : clock),
                 ((void*) ringTime      == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringTime),
                 ((void*) prevRingTime  == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : prevRingTime),
                 ((void*) ringInterval  == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringInterval),
                 ((void*) stopTime      == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : stopTime),
                 ((void*) ringDuration  == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringDuration),
                 ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                   ESMC_NULL_POINTER : nRingDurationTimeSteps),
                 ((void*) nTimeStepsRinging      == (void*)ESMC_BAD_POINTER ?
                                   ESMC_NULL_POINTER : nTimeStepsRinging),
                 ((void*) ringBegin     == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringBegin),
                 ((void*) refTime       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : refTime),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) ringingOnPrevTimeStep == (void*)ESMC_BAD_POINTER ?
                                   ESMC_NULL_POINTER : ringingOnPrevTimeStep),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmenable)(ESMC_Alarm **ptr, int *status) {
          int rc = (*ptr)->ESMC_AlarmEnable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmdisable)(ESMC_Alarm **ptr, int *status) {
          int rc = (*ptr)->ESMC_AlarmDisable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmisenabled)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsEnabled, int *status) {
           *esmf_alarmIsEnabled = (int) (*ptr)->ESMC_AlarmIsEnabled(
            ((void*) status == (void*)ESMC_BAD_POINTER ?
                                      ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmringeron)(ESMC_Alarm **ptr, int *status) {
          int rc = (*ptr)->ESMC_AlarmRingerOn();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmringeroff)(ESMC_Alarm **ptr, int *status) {
          int rc = (*ptr)->ESMC_AlarmRingerOff();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmisringing)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsRinging, int *status) {
           *esmf_alarmIsRinging = (int) (*ptr)->ESMC_AlarmIsRinging(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmwillringnext)(ESMC_Alarm **ptr, 
                ESMC_TimeInterval *timeStep, int *esmf_alarmWillRingNext,
                int *status) {
           *esmf_alarmWillRingNext = (int) (*ptr)->ESMC_AlarmWillRingNext(
                   ((void*) timeStep == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : timeStep),
                   ((void*) status   == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmwasprevringing)(ESMC_Alarm **ptr, 
                int *esmf_alarmWasPrevRinging, int *status) {
           *esmf_alarmWasPrevRinging = (int) (*ptr)->ESMC_AlarmWasPrevRinging(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmsticky)(ESMC_Alarm **ptr, int *status) {
          int rc = (*ptr)->ESMC_AlarmSticky();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmnotsticky)(ESMC_Alarm **ptr,
                                          ESMC_TimeInterval *ringDuration, 
                                          int *nRingDurationTimeSteps,
                                          int *status) {
          int rc = (*ptr)->ESMC_AlarmNotSticky(
            ((void*) ringDuration           == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : ringDuration),
            ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : nRingDurationTimeSteps) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmissticky)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsSticky, int *status) {
           *esmf_alarmIsSticky = (int) (*ptr)->ESMC_AlarmIsSticky(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmeq)(ESMC_Alarm **alarm1, ESMC_Alarm **alarm2,
                                   int *esmf_alarmEQ) {
           *esmf_alarmEQ = (int) (**alarm1 == **alarm2);
       }

       void FTN(c_esmc_alarmreadrestart)(ESMC_Alarm **ptr,
                                         ESMC_Clock **clock,
                                         ESMC_TimeInterval *ringInterval,
                                         ESMC_TimeInterval *ringDuration,
                                         ESMC_Time *ringTime,
                                         ESMC_Time *prevRingTime,
                                         ESMC_Time *stopTime,
                                         ESMC_Time *ringBegin,
                                         ESMC_Time *refTime,
                                         int  *nRingDurationTimeSteps,
                                         int  *nTimeStepsRinging,
                                         bool *ringing,
                                         bool *enabled,
                                         bool *sticky,
                                         int  *status) {
          int rc = (*ptr)->ESMC_AlarmReadRestart(clock, ringInterval, 
                                                ringDuration, ringTime,
                                                prevRingTime, stopTime,
                                                ringBegin, refTime,
                                                *nRingDurationTimeSteps,
                                                *nTimeStepsRinging,
                                                *ringing, *enabled, *sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmwriterestart)(ESMC_Alarm **ptr,
                                         ESMC_Clock **clock,
                                         ESMC_TimeInterval *ringInterval,
                                         ESMC_TimeInterval *ringDuration,
                                         ESMC_Time *ringTime,
                                         ESMC_Time *prevRingTime,
                                         ESMC_Time *stopTime,
                                         ESMC_Time *ringBegin,
                                         ESMC_Time *refTime,
                                         int  *nRingDurationTimeSteps,
                                         int  *nTimeStepsRinging,
                                         bool *ringing,
                                         bool *enabled,
                                         bool *sticky,
                                         int  *status) {
          int rc = (*ptr)->ESMC_AlarmWriteRestart(clock, ringInterval, 
                                                 ringDuration, ringTime,
                                                 prevRingTime, stopTime,
                                                 ringBegin, refTime,
                                                 nRingDurationTimeSteps,
                                                 nTimeStepsRinging,
                                                 ringing, enabled, sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmvalidate)(ESMC_Alarm **ptr, const char *options,
                                         int *status) {
          int rc = (*ptr)->ESMC_AlarmValidate(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmprint)(ESMC_Alarm **ptr, const char *options,
                                      int *status) {
          int rc = (*ptr)->ESMC_AlarmPrint(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
