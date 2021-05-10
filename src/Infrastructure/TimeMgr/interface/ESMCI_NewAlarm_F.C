// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
#include "ESMCI_NewAlarm.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_NewAlarm} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN_X(c_esmc_newalarmcreatenew)(NewAlarm **ptr, int *nameLen,
                const char *name, Clock **clock,
                Time *ringTime, TimeInterval *ringInterval,
                Time *stopTime, TimeInterval *ringDuration,
                int *ringTimeStepCount, Time *refTime,
                bool *enabled, bool *sticky, int *status,
                ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_newalarmCreate(
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

       void FTN_X(c_esmc_newalarmcreatecopy)(NewAlarm **ptr,
                                        NewAlarm **newalarm,
                                        int *status) {
          *ptr = ESMCI_newalarmCreate(
                                           *newalarm,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmdestroy)(NewAlarm **ptr, int *status) {
          int rc = ESMCI_newalarmDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmset)(NewAlarm **ptr, int *nameLen,
                                 const char *name, Clock **clock,
                Time *ringTime, TimeInterval *ringInterval,
                Time *stopTime, TimeInterval *ringDuration,
                int *ringTimeStepCount, Time *refTime,
                bool *ringing, bool *enabled, bool *sticky,
                int *status,
                ESMCI_FortranStrLenArg name_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::set(
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

       void FTN_X(c_esmc_newalarmget)(NewAlarm **ptr, int *nameLen,
                                 int *tempNameLen, char *tempName,
                                 Clock **clock,
                Time *ringTime, Time *prevRingTime,
                TimeInterval *ringInterval, Time *stopTime,
                TimeInterval *ringDuration, int *ringTimeStepCount,
                int *timeStepRingingCount, Time *ringBegin,
                Time *ringEnd, Time *refTime, bool *ringing,
                bool *ringingOnPrevTimeStep, bool *enabled, bool *sticky,
                int *status,
                ESMCI_FortranStrLenArg tempName_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::get(
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

       void FTN_X(c_esmc_newalarmenable)(NewAlarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::enable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmdisable)(NewAlarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::disable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmisenabled)(NewAlarm **ptr,
                int *esmf_newalarmIsEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_newalarmIsEnabled = (int) (*ptr)->NewAlarm::isEnabled(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmringeron)(NewAlarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::ringerOn();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmringeroff)(NewAlarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::ringerOff();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmisringing)(NewAlarm **ptr,
                int *esmf_newalarmIsRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_newalarmIsRinging = (int) (*ptr)->NewAlarm::isRinging(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmwillringnext)(NewAlarm **ptr,
                TimeInterval *timeStep, int *esmf_newalarmWillRingNext,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_newalarmWillRingNext = (int) (*ptr)->NewAlarm::willRingNext(
                                             ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmwasprevringing)(NewAlarm **ptr,
                int *esmf_newalarmWasPrevRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_newalarmWasPrevRinging = (int) (*ptr)->NewAlarm::wasPrevRinging(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmsticky)(NewAlarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::setToSticky();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmnotsticky)(NewAlarm **ptr,
                                          TimeInterval *ringDuration,
                                          int *ringTimeStepCount,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::notSticky(
                       ESMC_NOT_PRESENT_FILTER(ringDuration),
                       ESMC_NOT_PRESENT_FILTER(ringTimeStepCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmissticky)(NewAlarm **ptr,
                int *esmf_newalarmIsSticky, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_newalarmIsSticky = (int) (*ptr)->NewAlarm::isSticky(
                                         ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmeq)(NewAlarm **newalarm1, NewAlarm **newalarm2,
                                   int *esmf_newalarmEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*newalarm1, *newalarm2, esmf_newalarmEQ)
          *esmf_newalarmEQ = (int) (**newalarm1 == **newalarm2);
       }

       void FTN_X(c_esmc_newalarmne)(NewAlarm **newalarm1, NewAlarm **newalarm2,
                                   int *esmf_newalarmNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*newalarm1, *newalarm2, esmf_newalarmNE)
          *esmf_newalarmNE = (int) (**newalarm1 != **newalarm2);
       }

       void FTN_X(c_esmc_newalarmreadrestart)(NewAlarm **ptr, int *nameLen,
                                         const char *name,
                                         int *status,
                                         ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_newalarmReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_newalarmwriterestart)(NewAlarm **ptr,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::writeRestart();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmvalidate)(NewAlarm **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::validate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_newalarmprint)(NewAlarm **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->NewAlarm::print(
                   ESMC_NOT_PRESENT_FILTER(options) );
          fflush (stdout);
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

}  // namespace ESMCI
