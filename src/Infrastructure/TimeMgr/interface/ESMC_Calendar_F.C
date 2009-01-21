// $Id: ESMC_Calendar_F.C,v 1.41.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Calendar.h>
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


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_calendarinitialize)(ESMC_CalendarType *calendarType, 
                                           int *status) {
          int rc =
                ESMC_CalendarInitialize(
                   (ESMC_CalendarType *)ESMC_NOT_PRESENT_FILTER(calendarType));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarfinalize)(int *status) {
          int rc = ESMC_CalendarFinalize();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarcreatebuiltin)(ESMC_Calendar    **ptr,
                                              int               *nameLen,
                                              const char        *name,
                                              ESMC_CalendarType *calendarType, 
                                              int *status) {
          *ptr = ESMC_CalendarCreate(
                                           *nameLen,      // always present 
                                                          //  internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                           *calendarType, // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       // for daysPerMonth present
       void FTN(c_esmc_calendarcreatecustom1)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status) {
           *ptr = ESMC_CalendarCreate(
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
       void FTN(c_esmc_calendarcreatecustom0)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status) {
           *ptr = ESMC_CalendarCreate(
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

       void FTN(c_esmc_calendarcreatecopy)(ESMC_Calendar **ptr,
                                           ESMC_Calendar **calendar,
                                           int *status) {
          *ptr = ESMC_CalendarCreate(
                            *calendar,   // required
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_calendardestroy)(ESMC_Calendar **ptr, int *status) {
          int rc = ESMC_CalendarDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarsetbuiltin)(ESMC_Calendar **ptr,
                                           int *nameLen,
                                           const char *name,
                                           ESMC_CalendarType *calendarType, 
                                           int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
                                             *nameLen,   // always present
                                                         //   internal argument.
                      ESMC_NOT_PRESENT_FILTER(name),
                                             *calendarType);  // required
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarsetdefaultcal)(ESMC_Calendar **calendar, 
                                              int *status) {
          int rc =
                ESMC_CalendarSetDefault(
                          (ESMC_Calendar **)ESMC_NOT_PRESENT_FILTER(calendar));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarsetdefaulttype)(ESMC_CalendarType *calendarType, 
                                               int *status) {
          int rc =
                ESMC_CalendarSetDefault(
                   (ESMC_CalendarType *)ESMC_NOT_PRESENT_FILTER(calendarType));
          if (ESMC_PRESENT(status)) *status = rc;
       }

       // for daysPerMonth present
       void FTN(c_esmc_calendarsetcustom1)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
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
       void FTN(c_esmc_calendarsetcustom0)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd, int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
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
       void FTN(c_esmc_calendarget1)(ESMC_Calendar **ptr,
                                    int *nameLen,
                                    int *tempNameLen,
                                    char *tempName,
                                    ESMC_CalendarType *calendarType,
                                    int          *daysPerMonth,
                                    int          *sizeofDaysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *secondsPerYear,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd,
                                    int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarGet(
			                 // always present internal arguments.
                                            *nameLen,
                                             tempNameLen,
                                             tempName,
                     ESMC_NOT_PRESENT_FILTER(calendarType),
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
       void FTN(c_esmc_calendarget0)(ESMC_Calendar **ptr,
                                    int *nameLen,
                                    int *tempNameLen,
                                    char *tempName,
                                    ESMC_CalendarType *calendarType,
                                    int          *sizeofDaysPerMonth,
                                    int          *monthsPerYear,
                                    ESMC_I4 *secondsPerDay,
                                    ESMC_I4 *secondsPerYear,
                                    ESMC_I4 *daysPerYear,
                                    ESMC_I4 *daysPerYearDn,
                                    ESMC_I4 *daysPerYearDd,
                                    int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarGet(
			                 // always present interval arguments.
                                            *nameLen,
                                             tempNameLen,
                                             tempName,
                     ESMC_NOT_PRESENT_FILTER(calendarType),
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

       void FTN(c_esmc_calendarisleapyeari4)(ESMC_Calendar **ptr, 
                                   ESMC_I4 *yy,
                                   int *esmf_calendarIsLeapYear, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_calendarIsLeapYear = (int) (*ptr)->ESMC_CalendarIsLeapYear(
                                             (ESMC_I8) *yy,
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_calendarisleapyeari8)(ESMC_Calendar **ptr, 
                                   ESMC_I8 *yy_i8,
                                   int *esmf_calendarIsLeapYear, int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          *esmf_calendarIsLeapYear = (int) (*ptr)->ESMC_CalendarIsLeapYear(
                                             *yy_i8,
                                             ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_calendareq)(ESMC_Calendar **calendar1,
                                   ESMC_Calendar **calendar2,
                                   int *esmf_calendarEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar1, *calendar2,
                                               esmf_calendarEQ)
           *esmf_calendarEQ = (int) (**calendar1 == **calendar2);
       }

       void FTN(c_esmc_calendartypeeq)(ESMC_CalendarType *calendarType1,
                                       ESMC_CalendarType *calendarType2,
                                       int *esmf_calendarTypeEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendarType1, *calendarType2,
                                               esmf_calendarTypeEQ)
           *esmf_calendarTypeEQ = (int) (*calendarType1 == *calendarType2);
       }

       void FTN(c_esmc_calendarcalandtypeeq)(ESMC_Calendar **calendar,
                                             ESMC_CalendarType *calendarType,
                                             int *esmf_calendarCalAndTypeEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar, *calendarType,
                                               esmf_calendarCalAndTypeEQ)
           *esmf_calendarCalAndTypeEQ = (int) (**calendar == *calendarType);
       }

       void FTN(c_esmc_calendartypeandcaleq)(ESMC_CalendarType *calendarType,
                                             ESMC_Calendar **calendar,
                                             int *esmf_calendarTypeAndCalEQ) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendarType, *calendar,
                                               esmf_calendarTypeAndCalEQ)
           *esmf_calendarTypeAndCalEQ = (int) (**calendar == *calendarType);
       }

       void FTN(c_esmc_calendarne)(ESMC_Calendar **calendar1,
                                   ESMC_Calendar **calendar2,
                                   int *esmf_calendarNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar1, *calendar2,
                                                           esmf_calendarNE)
           *esmf_calendarNE = (int) (**calendar1 != **calendar2);
       }

       void FTN(c_esmc_calendartypene)(ESMC_CalendarType *calendarType1,
                                       ESMC_CalendarType *calendarType2,
                                       int *esmf_calendarTypeNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendarType1, *calendarType2,
                                               esmf_calendarTypeNE)
           *esmf_calendarTypeNE = (int) (*calendarType1 != *calendarType2);
       }

       void FTN(c_esmc_calendarcalandtypene)(ESMC_Calendar **calendar,
                                             ESMC_CalendarType *calendarType,
                                             int *esmf_calendarCalAndTypeNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar, *calendarType,
                                               esmf_calendarCalAndTypeNE)
           *esmf_calendarCalAndTypeNE = (int) (**calendar != *calendarType);
       }

       void FTN(c_esmc_calendartypeandcalne)(ESMC_CalendarType *calendarType,
                                             ESMC_Calendar **calendar,
                                             int *esmf_calendarTypeAndCalNE) {
           ESMF_CHECK_BINARY_OPERATOR_POINTERS(*calendar, *calendarType,
                                               esmf_calendarTypeAndCalNE)
           *esmf_calendarTypeAndCalNE = (int) (**calendar != *calendarType);
       }

       void FTN(c_esmc_calendarreadrestart)(ESMC_Calendar **ptr, int *nameLen,
                                            const char *name,
                                            ESMC_IOSpec *iospec,
                                            int *status) {
           *ptr = ESMC_CalendarReadRestart(
                                          *nameLen,  // always present
                                                    //   internal argument.
                                           name,     // required.
                   ESMC_NOT_PRESENT_FILTER(iospec),
                   ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN(c_esmc_calendarwriterestart)(ESMC_Calendar **ptr,
                                             ESMC_IOSpec *iospec,
                                             int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarWriteRestart(
                              ESMC_NOT_PRESENT_FILTER(iospec) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarvalidate)(ESMC_Calendar **ptr,
                                         const char *options,
                                         int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarValidate(
                          ESMC_NOT_PRESENT_FILTER(options) );
           if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_calendarprint)(ESMC_Calendar **ptr, const char *options,
                                      int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarPrint(
                       ESMC_NOT_PRESENT_FILTER(options) );
           if (ESMC_PRESENT(status)) *status = rc;
       }
};
