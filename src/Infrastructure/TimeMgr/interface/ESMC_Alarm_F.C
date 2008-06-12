// $Id: ESMC_Alarm_F.C,v 1.35 2008/06/12 18:08:21 rosalind Exp $
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
#include <ESMCI_Alarm.h>
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

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_alarmcreatenew)(ESMCI::Alarm **ptr, int *nameLen, 
                const char *name, ESMC_Clock **clock,
                ESMCI::Time *ringTime, ESMCI::TimeInterval *ringInterval,
                ESMCI::Time *stopTime, ESMCI::TimeInterval *ringDuration, 
                int *ringTimeStepCount, ESMCI::Time *refTime,
                bool *enabled, bool *sticky, int *status) {
          *ptr = ESMCI_alarmCreate(          
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

       void FTN(c_esmc_alarmcreatecopy)(ESMCI::Alarm **ptr,
                                        ESMCI::Alarm **alarm,
                                        int *status) {
          *ptr = ESMCI_alarmCreate(
                                           *alarm,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmdestroy)(ESMCI::Alarm **ptr, int *status) {
          int rc = ESMCI_alarmDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmset)(ESMCI::Alarm **ptr, int *nameLen, 
                                 const char *name, ESMC_Clock **clock,
                ESMCI::Time *ringTime, ESMCI::TimeInterval *ringInterval,
                ESMCI::Time *stopTime, ESMCI::TimeInterval *ringDuration, 
                int *ringTimeStepCount, ESMCI::Time *refTime,
                bool *ringing, bool *enabled, bool *sticky,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::set(
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

       void FTN(c_esmc_alarmget)(ESMCI::Alarm **ptr, int *nameLen, 
                                 int *tempNameLen, char *tempName,
                                 ESMC_Clock **clock,
                ESMCI::Time *ringTime, ESMCI::Time *prevRingTime, 
                ESMCI::TimeInterval *ringInterval, ESMCI::Time *stopTime,
                ESMCI::TimeInterval *ringDuration, int *ringTimeStepCount,
                int *timeStepRingingCount, ESMCI::Time *ringBegin,
                ESMCI::Time *ringEnd, ESMCI::Time *refTime, bool *ringing,
                bool *ringingOnPrevTimeStep, bool *enabled, bool *sticky,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::get(
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

       void FTN(c_esmc_alarmenable)(ESMCI::Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::enable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmdisable)(ESMCI::Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::disable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmisenabled)(ESMCI::Alarm **ptr, 
                int *esmf_alarmIsEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsEnabled = (int) (*ptr)->ESMCI::Alarm::isEnabled(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmringeron)(ESMCI::Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::ringerOn();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmringeroff)(ESMCI::Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::ringerOff();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmisringing)(ESMCI::Alarm **ptr, 
                int *esmf_alarmIsRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsRinging = (int) (*ptr)->ESMCI::Alarm::isRinging(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwillringnext)(ESMCI::Alarm **ptr, 
                ESMCI::TimeInterval *timeStep, int *esmf_alarmWillRingNext,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWillRingNext = (int) (*ptr)->ESMCI::Alarm::willRingNext(
                                             ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwasprevringing)(ESMCI::Alarm **ptr, 
                int *esmf_alarmWasPrevRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWasPrevRinging = (int) (*ptr)->ESMCI::Alarm::wasPrevRinging(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmsticky)(ESMCI::Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::setToSticky();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmnotsticky)(ESMCI::Alarm **ptr,
                                          ESMCI::TimeInterval *ringDuration, 
                                          int *ringTimeStepCount,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::notSticky(
                       ESMC_NOT_PRESENT_FILTER(ringDuration),
                       ESMC_NOT_PRESENT_FILTER(ringTimeStepCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmissticky)(ESMCI::Alarm **ptr, 
                int *esmf_alarmIsSticky, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsSticky = (int) (*ptr)->ESMCI::Alarm::isSticky(
                                         ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmeq)(ESMCI::Alarm **alarm1, ESMCI::Alarm **alarm2,
                                   int *esmf_alarmEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmEQ)
          *esmf_alarmEQ = (int) (**alarm1 == **alarm2);
       }

       void FTN(c_esmc_alarmne)(ESMCI::Alarm **alarm1, ESMCI::Alarm **alarm2,
                                   int *esmf_alarmNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmNE)
          *esmf_alarmNE = (int) (**alarm1 != **alarm2);
       }

       void FTN(c_esmc_alarmreadrestart)(ESMCI::Alarm **ptr, int *nameLen,
                                         const char *name,
                                         ESMC_IOSpec *iospec,
                                         int *status) {
          *ptr = ESMCI_alarmReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(iospec),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_alarmwriterestart)(ESMCI::Alarm **ptr,
                                          ESMC_IOSpec *iospec,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::writeRestart(
                          ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmvalidate)(ESMCI::Alarm **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::validate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_alarmprint)(ESMCI::Alarm **ptr, const char *options,
                                      int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMCI::Alarm::print(
                   ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

}  // namespace ESMCI
