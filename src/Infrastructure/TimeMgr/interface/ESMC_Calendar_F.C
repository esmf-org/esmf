// $Id: ESMC_Calendar_F.C,v 1.29 2004/02/18 01:45:45 eschwab Exp $
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
#include <ESMC.h>
#include <ESMC_F90Interface.h>
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

       void FTN(c_esmc_calendarcreatenew)(ESMC_Calendar    **ptr,
                                          int               *nameLen,
                                          const char        *name,
                                          ESMC_CalendarType *calendarType, 
                                          int *status) {
          *ptr = ESMC_CalendarCreate(
                            *nameLen,      // always present internal argument.

                    ((void*) name   == (void*)ESMC_BAD_POINTER ?
                                              ESMC_NULL_POINTER : name),

                            *calendarType, // required

                    ((void*) status == (void*)ESMC_BAD_POINTER ?
                                              ESMC_NULL_POINTER : status) );
       }

       // for daysPerMonth present
       void FTN(c_esmc_calendarcreatecustom1)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           *ptr = ESMC_CalendarCreate(
                      *nameLen,          // always present internal argument.

              ((void*) name           == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                       daysPerMonth,     // daysPerMonth present

                      *monthsPerYear,    // always present internal argument.

              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerDay),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd),
              ((void*) status         == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : status) );
       }

       // for daysPerMonth missing
       void FTN(c_esmc_calendarcreatecustom0)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           *ptr = ESMC_CalendarCreate(
                      *nameLen,          // always present internal argument.

              ((void*) name           == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                       ESMC_NULL_POINTER,  // daysPerMonth missing

                      *monthsPerYear,      // always present internal argument.

              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerDay),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd),
              ((void*) status         == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_calendarcreatecopy)(ESMC_Calendar **ptr,
                                           ESMC_Calendar **calendar,
                                           int *status) {
          *ptr = ESMC_CalendarCreate(
                            *calendar,   // required

                    ((void*) status == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_calendardestroy)(ESMC_Calendar **ptr, int *status) {
          int rc = ESMC_CalendarDestroy(ptr);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarsetnew)(ESMC_Calendar **ptr,
                                       int *nameLen,
                                       const char *name,
                                       ESMC_CalendarType *calendarType, 
                                       int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
                         *nameLen,   // always present internal argument.

                 ((void*) name         == (void*)ESMC_BAD_POINTER ?
                                                 ESMC_NULL_POINTER : name),
                 *calendarType);     // required
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       // for daysPerMonth present
       void FTN(c_esmc_calendarsetcustom1)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *daysPerMonth,
                                    int          *monthsPerYear,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
                      *nameLen,          // always present internal argument.

              ((void*) name           == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                       daysPerMonth,     // daysPerMonth present

                      *monthsPerYear,    // always present internal argument.

              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerDay),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       // for daysPerMonth missing
       void FTN(c_esmc_calendarsetcustom0)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarSet(
                      *nameLen,          // always present internal argument.

              ((void*) name           == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                       ESMC_NULL_POINTER, // daysPerMonth missing

                      *monthsPerYear,     // always present internal argument.

              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerDay),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
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
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *secondsPerYear,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd,
                                    int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarGet(
                      *nameLen,      // always present internal argument.

                       tempNameLen,  // always present internal argument.

                       tempName,     // always present internal argument.

              ((void*) calendarType   == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : calendarType),
                       daysPerMonth,        // always present

                      *sizeofDaysPerMonth,  // always present internal argument

              ((void*) monthsPerYear  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : monthsPerYear),
              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : secondsPerDay),
              ((void*) secondsPerYear == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerYear),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       // for daysPerMonth missing
       void FTN(c_esmc_calendarget0)(ESMC_Calendar **ptr,
                                    int *nameLen,
                                    int *tempNameLen,
                                    char *tempName,
                                    ESMC_CalendarType *calendarType,
                                    int          *sizeofDaysPerMonth,
                                    int          *monthsPerYear,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *secondsPerYear,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd,
                                    int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarGet(
                      *nameLen,      // always present internal argument.

                       tempNameLen,  // always present internal argument.

                       tempName,     // always present internal argument.

              ((void*) calendarType   == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : calendarType),
                       ESMC_NULL_POINTER,   // daysPerMonth missing

                      *sizeofDaysPerMonth,  // always present internal argument

              ((void*) monthsPerYear  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : monthsPerYear),
              ((void*) secondsPerDay  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : secondsPerDay),
              ((void*) secondsPerYear == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : secondsPerYear),
              ((void*) daysPerYear    == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYear),
              ((void*) daysPerYearDn  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDn),
              ((void*) daysPerYearDd  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerYearDd) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
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

       void FTN(c_esmc_calendarreadrestart)(ESMC_Calendar **ptr, int *nameLen,
                                            const char *name,
                                            ESMC_IOSpec *iospec,
                                            int *status) {
          *ptr = ESMC_CalendarReadRestart(
                 *nameLen,  // always present internal argument.
                 name,      // required.
                 ((void*)iospec == (void*)ESMC_BAD_POINTER ?      
                                                  ESMC_NULL_POINTER : iospec),
                 ((void*)status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_calendarwriterestart)(ESMC_Calendar **ptr,
                                             ESMC_IOSpec *iospec,
                                             int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->ESMC_CalendarWriteRestart(
              ((void*)iospec == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : iospec) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;  
       }

       void FTN(c_esmc_calendarvalidate)(ESMC_Calendar **ptr,
                                         const char *options,
                                         int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarValidate(
               ((void*)options == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : options) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarprint)(ESMC_Calendar **ptr, const char *options,
                                      int *status) {
           ESMF_CHECK_POINTER(*ptr, status)
           int rc = (*ptr)->ESMC_CalendarPrint(
               ((void*)options == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : options) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
