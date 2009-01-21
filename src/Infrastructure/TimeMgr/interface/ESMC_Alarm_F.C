// $Id: ESMC_Alarm_F.C,v 1.30.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Alarm.h>
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

       void FTN(c_esmc_alarmcreatenew)(ESMC_Alarm **ptr, int *nameLen, 
                const char *name, ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *ringTimeStepCount, ESMC_Time *refTime,
                bool *enabled, bool *sticky, int *status) {
          *ptr = ESMC_AlarmCreate(          
                                           *nameLen, // always present
                                                     //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                           *clock,    // required.
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(ringDuration),
                    ESMC_NOT_PRESENT_FILTER(ringTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmcreatecopy)(ESMC_Alarm **ptr,
                                        ESMC_Alarm **alarm,
                                        int *status) {
          *ptr = ESMC_AlarmCreate(
                                           *alarm,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmdestroy)(ESMC_Alarm **ptr, int *status) {
          int rc = ESMC_AlarmDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmset)(ESMC_Alarm **ptr, int *nameLen, 
                                 const char *name, ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, ESMC_TimeInterval *ringDuration, 
                int *ringTimeStepCount, ESMC_Time *refTime,
                bool *ringing, bool *enabled, bool *sticky,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmSet(
                                           *nameLen, // always present
                                                     //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                    ESMC_NOT_PRESENT_FILTER(clock),
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(ringDuration),
                    ESMC_NOT_PRESENT_FILTER(ringTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(ringing),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmget)(ESMC_Alarm **ptr, int *nameLen, 
                                 int *tempNameLen, char *tempName,
                                 ESMC_Clock **clock,
                ESMC_Time *ringTime, ESMC_Time *prevRingTime, 
                ESMC_TimeInterval *ringInterval, ESMC_Time *stopTime,
                ESMC_TimeInterval *ringDuration, int *ringTimeStepCount,
                int *timeStepRingingCount, ESMC_Time *ringBegin,
                ESMC_Time *ringEnd, ESMC_Time *refTime, bool *ringing,
                bool *ringingOnPrevTimeStep, bool *enabled, bool *sticky,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmGet(
			                 // always presnet internal arguments
                                           *nameLen, 
                                            tempNameLen,
                                            tempName,
                    ESMC_NOT_PRESENT_FILTER(clock),
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(prevRingTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(stopTime),
                    ESMC_NOT_PRESENT_FILTER(ringDuration),
                    ESMC_NOT_PRESENT_FILTER(ringTimeStepCount),
                    ESMC_NOT_PRESENT_FILTER(timeStepRingingCount),
                    ESMC_NOT_PRESENT_FILTER(ringBegin),
                    ESMC_NOT_PRESENT_FILTER(ringEnd),
                    ESMC_NOT_PRESENT_FILTER(refTime),
                    ESMC_NOT_PRESENT_FILTER(ringing),
                    ESMC_NOT_PRESENT_FILTER(ringingOnPrevTimeStep),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmenable)(ESMC_Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmEnable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmdisable)(ESMC_Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmDisable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmisenabled)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsEnabled = (int) (*ptr)->ESMC_AlarmIsEnabled(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmringeron)(ESMC_Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmRingerOn();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmringeroff)(ESMC_Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmRingerOff();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmisringing)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsRinging = (int) (*ptr)->ESMC_AlarmIsRinging(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwillringnext)(ESMC_Alarm **ptr, 
                ESMC_TimeInterval *timeStep, int *esmf_alarmWillRingNext,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWillRingNext = (int) (*ptr)->ESMC_AlarmWillRingNext(
                                             ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwasprevringing)(ESMC_Alarm **ptr, 
                int *esmf_alarmWasPrevRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWasPrevRinging = (int) (*ptr)->ESMC_AlarmWasPrevRinging(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmsticky)(ESMC_Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmSticky();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmnotsticky)(ESMC_Alarm **ptr,
                                          ESMC_TimeInterval *ringDuration, 
                                          int *ringTimeStepCount,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmNotSticky(
                       ESMC_NOT_PRESENT_FILTER(ringDuration),
                       ESMC_NOT_PRESENT_FILTER(ringTimeStepCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmissticky)(ESMC_Alarm **ptr, 
                int *esmf_alarmIsSticky, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsSticky = (int) (*ptr)->ESMC_AlarmIsSticky(
                                         ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmeq)(ESMC_Alarm **alarm1, ESMC_Alarm **alarm2,
                                   int *esmf_alarmEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmEQ)
          *esmf_alarmEQ = (int) (**alarm1 == **alarm2);
       }

       void FTN(c_esmc_alarmne)(ESMC_Alarm **alarm1, ESMC_Alarm **alarm2,
                                   int *esmf_alarmNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmNE)
          *esmf_alarmNE = (int) (**alarm1 != **alarm2);
       }

       void FTN(c_esmc_alarmreadrestart)(ESMC_Alarm **ptr, int *nameLen,
                                         const char *name,
                                         ESMC_IOSpec *iospec,
                                         int *status) {
          *ptr = ESMC_AlarmReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(iospec),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwriterestart)(ESMC_Alarm **ptr,
                                          ESMC_IOSpec *iospec,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmWriteRestart(
                          ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmvalidate)(ESMC_Alarm **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmValidate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmprint)(ESMC_Alarm **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_AlarmPrint(
                   ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
};
