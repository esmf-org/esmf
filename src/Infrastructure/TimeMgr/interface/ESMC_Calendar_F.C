// $Id: ESMC_Calendar_F.C,v 1.24 2004/02/02 19:14:08 eschwab Exp $
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
#include "ESMC_Calendar.h"
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
                                          ESMC_CalendarType *type, 
                                          int *status) {
          *ptr = ESMC_CalendarCreate(
                            *nameLen,   // always present internal argument.

                    ((void*) name   == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                            *type,      // required

                    ((void*) status == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : status) );
       }

       void FTN(c_esmc_calendarcreatecustom)(ESMC_Calendar **ptr,
                                    int          *nameLen,
                                    const char   *name,
                                    int          *monthsPerYear,
                                    int          *daysPerMonth,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           *ptr = ESMC_CalendarCreate(
                      *nameLen,          // always present internal argument.

              ((void*) name           == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : name),
                       monthsPerYear,    // always present internal argument.

              ((void*) daysPerMonth   == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerMonth),
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
          int rc = ESMC_CalendarDestroy(*ptr);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarsetnew)(ESMC_Calendar **ptr,
                                       ESMC_CalendarType *type, 
                                       int *status) {
           int rc = (*ptr)->ESMC_CalendarSet(*type);
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarsetcustom)(ESMC_Calendar **ptr,
                                    int          *monthsPerYear,
                                    int          *daysPerMonth,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           int rc = (*ptr)->ESMC_CalendarSet(
                       monthsPerYear,    // always present internal argument.

              ((void*) daysPerMonth   == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerMonth),
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

       void FTN(c_esmc_calendarget)(ESMC_Calendar **ptr,
                                    ESMC_CalendarType *type,
                                    int          *monthsPerYear,
                                    int          *daysPerMonth,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *secondsPerYear,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           int rc = (*ptr)->ESMC_CalendarGet(
              ((void*) type           == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : type),
              ((void*) monthsPerYear  == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : monthsPerYear),
              ((void*) daysPerMonth   == (void*)ESMC_BAD_POINTER ?
                                          ESMC_NULL_POINTER : daysPerMonth),
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
          int rc = (*ptr)->ESMC_CalendarWriteRestart(
              ((void*)iospec == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : iospec) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;  
       }

       void FTN(c_esmc_calendarvalidate)(ESMC_Calendar **ptr,
                                         const char *options,
                                         int *status) {
           int rc = (*ptr)->ESMC_CalendarValidate(
               ((void*)options == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : options) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarprint)(ESMC_Calendar **ptr, const char *options,
                                      int *status) {
           int rc = (*ptr)->ESMC_CalendarPrint(
               ((void*)options == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : options) );
           if (status != ESMC_NULL_POINTER &&
               (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
