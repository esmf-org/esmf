// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
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
#include "ESMCI_Alarm.h"
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

       void FTN_X(c_esmc_alarmcreatenew)(Alarm **ptr, int *nameLen,
                const char *name, Clock **clock,
                Time *ringTime, TimeInterval *ringInterval,
                bool *enabled, bool *sticky, int *status,
                ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_alarmCreate(
                                           *nameLen, // always present
                                                     //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                           *clock,    // required.
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmcreatecopy)(Alarm **ptr,
                                        Alarm **alarm,
                                        int *status) {
          *ptr = ESMCI_alarmCreate(
                                           *alarm,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmdestroy)(Alarm **ptr, int *status) {
          int rc = ESMCI_alarmDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmset)(Alarm **ptr, int *nameLen,
                                 const char *name, Clock **clock,
                Time *ringTime, TimeInterval *ringInterval,
                bool *ringing, bool *enabled, bool *sticky,
                int *status,
                ESMCI_FortranStrLenArg name_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::set(
                                           *nameLen, // always present
                                                     //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                    ESMC_NOT_PRESENT_FILTER(clock),
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(ringing),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

              int     get(int            nameLen,
                      int               *tempNameLen,
                      char              *tempName=0,
                      Clock            **clock=0,
                      Time              *ringTime=0,
                      TimeInterval      *ringInterval=0,
                      bool              *ringing=0,
                      bool              *enabled=0,  // (TMG 4.1, 4.7)
                      bool              *sticky=0, 
                      bool              *ringerIsOn=0);
       void FTN_X(c_esmc_alarmget)(Alarm **ptr, int *nameLen,
                                 int *tempNameLen, char *tempName,
                                 Clock **clock,
                Time *ringTime, 
                TimeInterval *ringInterval,
                bool *ringing,
                bool *enabled, bool *sticky, bool *ringerIsOn, 
                int *status,
                ESMCI_FortranStrLenArg tempName_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          //if(sticky != 0) printf("ptr address %x => %d \n", sticky, (int)*sticky);
          int rc = (*ptr)->Alarm::get(
                                         // always presnet internal arguments
                                           *nameLen,
                                            tempNameLen,
                                            tempName,
                    ESMC_NOT_PRESENT_FILTER(clock),
                    ESMC_NOT_PRESENT_FILTER(ringTime),
                    ESMC_NOT_PRESENT_FILTER(ringInterval),
                    ESMC_NOT_PRESENT_FILTER(ringing),
                    ESMC_NOT_PRESENT_FILTER(enabled),
                    ESMC_NOT_PRESENT_FILTER(sticky),
                    ESMC_NOT_PRESENT_FILTER(ringerIsOn) );
          if(sticky != 0) {
            if(*sticky) *(int *)sticky = 1;
            else *(int *)sticky = 0;
            //printf("ptr address %x => %d \n", sticky, *(int *)sticky);
          }
          if(ringing != 0) {
            if(*ringing) *(int *)ringing = 1;
            else *(int *)ringing = 0;
            //printf("ptr address %x => %d \n", ringing, *(int *)ringing);
          }
          if(enabled != 0) {
            if(*enabled) *(int *)enabled = 1;
            else *(int *)enabled = 0;
          }
          if(ringerIsOn != 0) {
            if(*ringerIsOn) *(int *)ringerIsOn = 1;
            else *(int *)ringerIsOn = 0;
          }
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmenable)(Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::enable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmdisable)(Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::disable();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmisenabled)(Alarm **ptr,
                int *esmf_alarmIsEnabled, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsEnabled = (int) (*ptr)->Alarm::isEnabled(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmringeron)(Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::ringerOn();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmringeroff)(Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::ringerOff();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmisringing)(Alarm **ptr,
                int *esmf_alarmIsRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsRinging = (int) (*ptr)->Alarm::isRinging(
                                           ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmwillringnext)(Alarm **ptr,
                TimeInterval *timeStep, int *esmf_alarmWillRingNext,
                int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWillRingNext = (int) (*ptr)->Alarm::willRingNext(
                                             ESMC_NOT_PRESENT_FILTER(timeStep),
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmwasprevringing)(Alarm **ptr,
                int *esmf_alarmWasPrevRinging, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmWasPrevRinging = (int) (*ptr)->Alarm::wasPrevRinging(
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmsticky)(Alarm **ptr, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::setToSticky();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmnotsticky)(Alarm **ptr,
                                          TimeInterval *ringDuration,
                                          int *ringTimeStepCount,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::notSticky(
                       ESMC_NOT_PRESENT_FILTER(ringDuration),
                       ESMC_NOT_PRESENT_FILTER(ringTimeStepCount) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmissticky)(Alarm **ptr,
                int *esmf_alarmIsSticky, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_alarmIsSticky = (int) (*ptr)->Alarm::isSticky(
                                         ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmeq)(Alarm **alarm1, Alarm **alarm2,
                                   int *esmf_alarmEQ) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmEQ)
          *esmf_alarmEQ = (int) (**alarm1 == **alarm2);
       }

       void FTN_X(c_esmc_alarmne)(Alarm **alarm1, Alarm **alarm2,
                                   int *esmf_alarmNE) {
          ESMF_CHECK_BINARY_OPERATOR_POINTERS(*alarm1, *alarm2, esmf_alarmNE)
          *esmf_alarmNE = (int) (**alarm1 != **alarm2);
       }

       void FTN_X(c_esmc_alarmreadrestart)(Alarm **ptr, int *nameLen,
                                         const char *name,
                                         int *status,
                                         ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_alarmReadRestart(
                                           *nameLen,  // always present
                                                      //   internal argument.
                                            name,     // required.
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_alarmwriterestart)(Alarm **ptr,
                                          int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::writeRestart();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmvalidate)(Alarm **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::validate(
                      ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_alarmprint)(Alarm **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->Alarm::print(
                   ESMC_NOT_PRESENT_FILTER(options) );
          fflush (stdout);
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

}  // namespace ESMCI
