// $Id: ESMC_Alarm_F.C,v 1.15 2003/09/12 01:58:03 eschwab Exp $
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

//------------------------------------------------------------------------------
// alarm object API version
//------------------------------------------------------------------------------

       void FTN(c_esmc_alarmobjsetup)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                int *id, bool *enabled, bool *sticky, int *status) {
          int rc = (ptr)->ESMC_AlarmSetup(
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
                 ((void*) id           == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : id),
                 ((void*) enabled      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : enabled),
                 ((void*) sticky       == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjset)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                int *id, bool *ringing, bool *enabled, bool *sticky,
                int *status) {
          int rc = (ptr)->ESMC_AlarmSet(
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
                 ((void*) id            == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : id),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
         (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjget)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_Time *prevRingTime, 
                ESMC_TimeInterval *ringInterval, ESMC_Time *stopTime,
                ESMC_TimeInterval *ringDuration, int *nRingDurationTimeSteps,
                int *nTimeStepsRinging, ESMC_Time *ringBegin,
                ESMC_Time *refTime, int *id, bool *ringing, bool *enabled,
                bool *sticky, int *status) {
          int rc = (ptr)->ESMC_AlarmGet(
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
                 ((void*) id            == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : id),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjenable)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmEnable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjdisable)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmDisable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjisenabled)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsEnabled, int *status) {
           *esmf_alarmIsEnabled = (int) (ptr)->ESMC_AlarmIsEnabled(
            ((void*) status == (void*)ESMC_BAD_POINTER ?
                                      ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmobjringeron)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmRingerOn();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjringeroff)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmRingerOff();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjisringing)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsRinging, int *status) {
           *esmf_alarmIsRinging = (int) (ptr)->ESMC_AlarmIsRinging(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmobjsticky)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmSticky();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjnotsticky)(ESMC_Alarm *ptr,
                                          ESMC_TimeInterval *ringDuration, 
                                          int *nRingDurationTimeSteps,
                                          int *status) {
          int rc = (ptr)->ESMC_AlarmNotSticky(
            ((void*) ringDuration           == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : ringDuration),
            ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : nRingDurationTimeSteps) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjissticky)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsSticky, int *status) {
           *esmf_alarmIsSticky = (int) (ptr)->ESMC_AlarmIsSticky(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmobjeq)(ESMC_Alarm *alarm1, ESMC_Alarm *alarm2,
                                   int *esmf_alarmEQ) {
           *esmf_alarmEQ = (int) (*alarm1 == *alarm2);
       }

       void FTN(c_esmc_alarmobjreadrestart)(ESMC_Alarm *ptr,
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
                                            int  *id,
                                            int  *status) {
          int rc = (ptr)->ESMC_AlarmReadRestart(ringInterval, ringDuration, 
                                                ringTime, prevRingTime, 
                                                stopTime, ringBegin,
                                                refTime,
                                                *nRingDurationTimeSteps,
                                                *nTimeStepsRinging, *id,
                                                *ringing, *enabled, *sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjwriterestart)(ESMC_Alarm *ptr,
                                             ESMC_TimeInterval *ringInterval,
                                             ESMC_TimeInterval *ringDuration,
                                             ESMC_Time *ringTime,
                                             ESMC_Time *prevRingTime,
                                             ESMC_Time *stopTime,
                                             ESMC_Time *ringBegin,
                                             ESMC_Time *refTime,
                                             int  *nRingDurationTimeSteps,
                                             int  *nTimeStepsRinging,
                                             int  *id,
                                             bool *ringing,
                                             bool *enabled,
                                             bool *sticky,
                                             int  *status) {
          int rc = (ptr)->ESMC_AlarmWriteRestart(ringInterval, ringDuration, 
                                                 ringTime, prevRingTime, 
                                                 stopTime, ringBegin,
                                                 refTime,
                                                 nRingDurationTimeSteps,
                                                 nTimeStepsRinging,
                                                 id, ringing, enabled, sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjvalidate)(ESMC_Alarm *ptr, const char *options,
                                         int *status) {
          int rc = (ptr)->ESMC_AlarmValidate(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmobjprint)(ESMC_Alarm *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->ESMC_AlarmPrint(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

//------------------------------------------------------------------------------
// alarm pointer API version
//------------------------------------------------------------------------------

       void FTN(c_esmc_alarmptrsetup)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                int *id, bool *enabled, bool *sticky, int *status) {
          int rc = (ptr)->ESMC_AlarmSetup(
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
                 ((void*) id           == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : id),
                 ((void*) enabled      == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : enabled),
                 ((void*) sticky       == (void*)ESMC_BAD_POINTER ?
                                           ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrset)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *nRingDurationTimeSteps, ESMC_Time *refTime,
                int *id, bool *ringing, bool *enabled, bool *sticky,
                int *status) {
          int rc = (ptr)->ESMC_AlarmSet(
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
                 ((void*) id            == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : id),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
         (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrget)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_Time *prevRingTime, 
                ESMC_TimeInterval *ringInterval, ESMC_Time *stopTime,
                ESMC_TimeInterval *ringDuration, int *nRingDurationTimeSteps,
                int *nTimeStepsRinging, ESMC_Time *ringBegin,
                ESMC_Time *refTime, int *id, bool *ringing, bool *enabled,
                bool *sticky, int *status) {
          int rc = (ptr)->ESMC_AlarmGet(
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
                 ((void*) id            == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : id),
                 ((void*) ringing       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : ringing),
                 ((void*) enabled       == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : enabled),
                 ((void*) sticky        == (void*)ESMC_BAD_POINTER ?
                                            ESMC_NULL_POINTER : sticky) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrenable)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmEnable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrdisable)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmDisable();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrisenabled)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsEnabled, int *status) {
           *esmf_alarmIsEnabled = (int) (ptr)->ESMC_AlarmIsEnabled(
            ((void*) status == (void*)ESMC_BAD_POINTER ?
                                      ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmptrringeron)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmRingerOn();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrringeroff)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmRingerOff();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrisringing)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsRinging, int *status) {
           *esmf_alarmIsRinging = (int) (ptr)->ESMC_AlarmIsRinging(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmptrsticky)(ESMC_Alarm *ptr, int *status) {
          int rc = (ptr)->ESMC_AlarmSticky();
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrnotsticky)(ESMC_Alarm *ptr,
                                          ESMC_TimeInterval *ringDuration, 
                                          int *nRingDurationTimeSteps,
                                          int *status) {
          int rc = (ptr)->ESMC_AlarmNotSticky(
            ((void*) ringDuration           == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : ringDuration),
            ((void*) nRingDurationTimeSteps == (void*)ESMC_BAD_POINTER ?
                                  ESMC_NULL_POINTER : nRingDurationTimeSteps) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrissticky)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsSticky, int *status) {
           *esmf_alarmIsSticky = (int) (ptr)->ESMC_AlarmIsSticky(
                        ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_alarmptreq)(ESMC_Alarm *alarm1, ESMC_Alarm *alarm2,
                                   int *esmf_alarmEQ) {
           *esmf_alarmEQ = (int) (*alarm1 == *alarm2);
       }

       void FTN(c_esmc_alarmptrreadrestart)(ESMC_Alarm *ptr,
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
                                            int  *id,
                                            int  *status) {
          int rc = (ptr)->ESMC_AlarmReadRestart(ringInterval, ringDuration, 
                                                ringTime, prevRingTime, 
                                                stopTime, ringBegin,
                                                refTime,
                                                *nRingDurationTimeSteps,
                                                *nTimeStepsRinging, *id,
                                                *ringing, *enabled, *sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrwriterestart)(ESMC_Alarm *ptr,
                                             ESMC_TimeInterval *ringInterval,
                                             ESMC_TimeInterval *ringDuration,
                                             ESMC_Time *ringTime,
                                             ESMC_Time *prevRingTime,
                                             ESMC_Time *stopTime,
                                             ESMC_Time *ringBegin,
                                             ESMC_Time *refTime,
                                             int  *nRingDurationTimeSteps,
                                             int  *nTimeStepsRinging,
                                             int  *id,
                                             bool *ringing,
                                             bool *enabled,
                                             bool *sticky,
                                             int  *status) {
          int rc = (ptr)->ESMC_AlarmWriteRestart(ringInterval, ringDuration, 
                                                 ringTime, prevRingTime, 
                                                 stopTime, ringBegin,
                                                 refTime,
                                                 nRingDurationTimeSteps,
                                                 nTimeStepsRinging,
                                                 id, ringing, enabled, sticky);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrvalidate)(ESMC_Alarm *ptr, const char *options,
                                         int *status) {
          int rc = (ptr)->ESMC_AlarmValidate(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_alarmptrprint)(ESMC_Alarm *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->ESMC_AlarmPrint(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
