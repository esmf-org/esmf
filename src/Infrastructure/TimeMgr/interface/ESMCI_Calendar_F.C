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
#include "ESMCI_Calendar.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Calendar} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN_X(c_esmc_calendarinitialize)(ESMC_CalKind_Flag *calkindflag,
                                           int *status) {
          int rc =
                ESMCI_CalendarInitialize(
                   (ESMC_CalKind_Flag *)ESMC_NOT_PRESENT_FILTER(calkindflag));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarfinalize)(int *status) {
          int rc = ESMCI_CalendarFinalize();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarcreatebuiltin)(Calendar    **ptr,
                                              int               *nameLen,
                                              const char        *name,
                                              ESMC_CalKind_Flag *calkindflag,
                                              int *status,
                                              ESMCI_FortranStrLenArg name_l) {
          *ptr = ESMCI_CalendarCreate(
                                           *nameLen,      // always present
                                                          //  internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                           *calkindflag, // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       // for daysPerMonth present
       void FTN_X(c_esmc_calendarcreatecustom1)(Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status,
                                    ESMCI_FortranStrLenArg name_l) {
           *ptr = ESMCI_CalendarCreate(
                                           *nameLen,    // always present
                                                        //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                            daysPerMonth,   // daysPerMonth
                                                            //   present
                                           *monthsPerYear,  // always present
                                                          // internal argument.
                    ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                    ESMC_NOT_PRESENT_FILTER(daysPerYear),
                    ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                    ESMC_NOT_PRESENT_FILTER(daysPerYearDd),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       // for daysPerMonth missing
       void FTN_X(c_esmc_calendarcreatecustom0)(Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status,
                                    ESMCI_FortranStrLenArg name_l) {
           *ptr = ESMCI_CalendarCreate(
                                           *nameLen,    // always present
                                                        //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                            ESMC_NULL_POINTER,  // daysPerMonth
                                                                //   missing
                                           *monthsPerYear,  // always present
                                                          // internal argument.
                    ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                    ESMC_NOT_PRESENT_FILTER(daysPerYear),
                    ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                    ESMC_NOT_PRESENT_FILTER(daysPerYearDd),
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_calendarcreatecopy)(Calendar **ptr,
                                           Calendar **calendar,
                                           int *status) {
          *ptr = ESMCI_CalendarCreate(
                            *calendar,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_calendardestroy)(Calendar **ptr, int *status) {
          int rc = ESMCI_CalendarDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarsetbuiltin)(Calendar **ptr,
                                           int *nameLen,
                                           const char *name,
                                           ESMC_CalKind_Flag *calkindflag,
                                           int *status,
                                           ESMCI_FortranStrLenArg name_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::set(
                                             *nameLen,   // always present
                                                         //   internal argument.
                      ESMC_NOT_PRESENT_FILTER(name),
                                             *calkindflag);  // required
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarsetdefaultcal)(Calendar **calendar,
                                              int *status) {
          int rc =
                ESMCI_CalendarSetDefault(
                          (Calendar **)ESMC_NOT_PRESENT_FILTER(calendar));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarsetdefaultkind)(ESMC_CalKind_Flag *calkindflag,
                                               int *status) {
          int rc =
                ESMCI_CalendarSetDefault(
                   (ESMC_CalKind_Flag *)ESMC_NOT_PRESENT_FILTER(calkindflag));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for daysPerMonth present
       void FTN_X(c_esmc_calendarsetcustom1)(Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status,
                                    ESMCI_FortranStrLenArg name_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::set(
                                            *nameLen,    // always present
                                                         //   internal argument.
                     ESMC_NOT_PRESENT_FILTER(name),
                                             daysPerMonth,   // daysPerMonth
                                                             //   present
                                            *monthsPerYear,  // always present
                                                           // internal argument.
                     ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                     ESMC_NOT_PRESENT_FILTER(daysPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDd) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       // for daysPerMonth missing
       void FTN_X(c_esmc_calendarsetcustom0)(Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status,
                                    ESMCI_FortranStrLenArg name_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::set(
                                            *nameLen,    // always present
                                                         //   internal argument.
                     ESMC_NOT_PRESENT_FILTER(name),
                                             ESMC_NULL_POINTER,  // daysPerMonth
                                                                 //   missing
                                            *monthsPerYear,  // always present
                                                           // internal argument.
                     ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                     ESMC_NOT_PRESENT_FILTER(daysPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDd) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       // for daysPerMonth present
       void FTN_X(c_esmc_calendarget1)(Calendar **ptr,
                                    int *nameLen,
                                    int *tempNameLen,
                                    char *tempName,
                                    ESMC_CalKind_Flag *calkindflag,
                                    int          *daysPerMonth,
                                    int          *sizeofDaysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *secondsPerYear,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd,
                                    int *status,
                                    ESMCI_FortranStrLenArg tempName_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::get(
                                         // always present internal arguments.
                                            *nameLen,
                                             tempNameLen,
                                             tempName,
                     ESMC_NOT_PRESENT_FILTER(calkindflag),
                                             daysPerMonth, // always present

                                            *sizeofDaysPerMonth, // always
                                                                 //  present
                                                                 //  internal
                                                                 //  argument.
                     ESMC_NOT_PRESENT_FILTER(monthsPerYear),
                     ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                     ESMC_NOT_PRESENT_FILTER(secondsPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDd) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       // for daysPerMonth missing
       void FTN_X(c_esmc_calendarget0)(Calendar **ptr,
                                    int *nameLen,
                                    int *tempNameLen,
                                    char *tempName,
                                    ESMC_CalKind_Flag *calkindflag,
                                    int          *sizeofDaysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *secondsPerYear,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd,
                                    int *status,
                                    ESMCI_FortranStrLenArg tempName_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::get(
                                         // always present interval arguments.
                                            *nameLen,
                                             tempNameLen,
                                             tempName,
                     ESMC_NOT_PRESENT_FILTER(calkindflag),
                                             ESMC_NULL_POINTER,
                                                        // daysPerMonth missing
                                            *sizeofDaysPerMonth, // always
                                                                 //  present
                                                                 //  internal
                                                                 //  argument.
                     ESMC_NOT_PRESENT_FILTER(monthsPerYear),
                     ESMC_NOT_PRESENT_FILTER(secondsPerDay),
                     ESMC_NOT_PRESENT_FILTER(secondsPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYear),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDn),
                     ESMC_NOT_PRESENT_FILTER(daysPerYearDd) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarisleapyeari4)(Calendar **ptr,
                                   ESMC_I4 *yy,
                                   int *esmf_calendarIsLeapYear, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_calendarIsLeapYear = (int) (*ptr)->Calendar::isLeapYear(
                                             (ESMC_I8) *yy,
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_calendarisleapyeari8)(Calendar **ptr,
                                   ESMC_I8 *yy,
                                   int *esmf_calendarIsLeapYear, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_calendarIsLeapYear = (int) (*ptr)->Calendar::isLeapYear(
                                             *yy,
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_calendareq)(Calendar **calendar1,
                                   Calendar **calendar2,
                                   int *esmf_calendarEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar1, *calendar2,
                                               esmf_calendarEQ)
           *esmf_calendarEQ = (int) (**calendar1 == **calendar2);
       }

       void FTN_X(c_esmc_calendarkindeq)(ESMC_CalKind_Flag *calkindflag1,
                                       ESMC_CalKind_Flag *calkindflag2,
                                       int *esmf_calendarKindEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calkindflag1, *calkindflag2,
                                               esmf_calendarKindEQ)
           *esmf_calendarKindEQ = (int) (*calkindflag1 == *calkindflag2);
       }

       void FTN_X(c_esmc_calendarcalandkindeq)(Calendar **calendar,
                                             ESMC_CalKind_Flag *calkindflag,
                                             int *esmf_calendarCalAndKindEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar, *calkindflag,
                                               esmf_calendarCalAndKindEQ)
           *esmf_calendarCalAndKindEQ = (int) (**calendar == *calkindflag);
       }

       void FTN_X(c_esmc_calendarkindandcaleq)(ESMC_CalKind_Flag *calkindflag,
                                             Calendar **calendar,
                                             int *esmf_calendarKindAndCalEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calkindflag, *calendar,
                                               esmf_calendarKindAndCalEQ)
           *esmf_calendarKindAndCalEQ = (int) (**calendar == *calkindflag);
       }

       void FTN_X(c_esmc_calendarne)(Calendar **calendar1,
                                   Calendar **calendar2,
                                   int *esmf_calendarNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar1, *calendar2,
                                                           esmf_calendarNE)
           *esmf_calendarNE = (int) (**calendar1 != **calendar2);
       }

       void FTN_X(c_esmc_calendarkindne)(ESMC_CalKind_Flag *calkindflag1,
                                       ESMC_CalKind_Flag *calkindflag2,
                                       int *esmf_calendarKindNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calkindflag1, *calkindflag2,
                                               esmf_calendarKindNE)
           *esmf_calendarKindNE = (int) (*calkindflag1 != *calkindflag2);
       }

       void FTN_X(c_esmc_calendarcalandkindne)(Calendar **calendar,
                                             ESMC_CalKind_Flag *calkindflag,
                                             int *esmf_calendarCalAndKindNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar, *calkindflag,
                                               esmf_calendarCalAndKindNE)
           *esmf_calendarCalAndKindNE = (int) (**calendar != *calkindflag);
       }

       void FTN_X(c_esmc_calendarkindandcalne)(ESMC_CalKind_Flag *calkindflag,
                                             Calendar **calendar,
                                             int *esmf_calendarKindAndCalNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calkindflag, *calendar,
                                               esmf_calendarKindAndCalNE)
           *esmf_calendarKindAndCalNE = (int) (**calendar != *calkindflag);
       }

       void FTN_X(c_esmc_calendarreadrestart)(Calendar **ptr, int *nameLen,
                                            const char *name,
                                            int *status,
                                            ESMCI_FortranStrLenArg name_l) {
           *ptr = ESMCI_CalendarReadRestart(
                                          *nameLen,  // always present
                                                    //   internal argument.
                                           name,     // required.
                   ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_calendarwriterestart)(Calendar **ptr,
                                             int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::writeRestart();
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarvalidate)(Calendar **ptr,
                                         const char *options,
                                         int *status,
                                         ESMCI_FortranStrLenArg options_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::validate(
                          ESMC_NOT_PRESENT_FILTER(options) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_calendarprint)(Calendar **ptr, const char *options,
                                      int *status,
                                      ESMCI_FortranStrLenArg options_l) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->Calendar::print(
                       ESMC_NOT_PRESENT_FILTER(options) );
           fflush (stdout);
           if (ESMC_PRESENT(status)) *status = rc;
       }
};

} // namespace ESMCI
