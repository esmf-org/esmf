// $Id: ESMC_Calendar_F.C,v 1.14 2003/09/04 18:57:56 cdeluca Exp $
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
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_calendarset)(ESMC_Calendar *ptr,
                                    ESMC_CalendarType *type, 
                                    int *status) {
           int rc = (ptr)->ESMC_CalendarSet(*type);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarsetgeneric)(ESMC_Calendar *ptr,
                                    int           *daysPerMonth,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           int rc = (ptr)->ESMC_CalendarSetGeneric(daysPerMonth,
                                                   *secondsPerDay,
                                                   *daysPerYear,
                                                   *daysPerYearDn,
                                                   *daysPerYearDd);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarget)(ESMC_Calendar *ptr,
                                    ESMC_CalendarType *type,
                                    int           *daysPerMonth,
                                    ESMF_KIND_I4 *secondsPerDay,
                                    ESMF_KIND_I4 *secondsPerYear,
                                    ESMF_KIND_I4 *daysPerYear,
                                    ESMF_KIND_I4 *daysPerYearDn,
                                    ESMF_KIND_I4 *daysPerYearDd, int *status) {
           int rc = (ptr)->ESMC_CalendarGet(type,
                                            daysPerMonth,
                                            secondsPerDay,
                                            secondsPerYear,
                                            daysPerYear,
                                            daysPerYearDn,
                                            daysPerYearDd);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }
       void FTN(c_esmc_calendarreadrestart)(ESMC_Calendar *ptr,
                                            ESMC_CalendarType *type,
                                            int           *daysPerMonth,
                                            ESMF_KIND_I4 *secondsPerDay,
                                            ESMF_KIND_I4 *secondsPerYear,
                                            ESMF_KIND_I4 *daysPerYear,
                                            ESMF_KIND_I4 *daysPerYearDn,
                                            ESMF_KIND_I4 *daysPerYearDd,
                                            int *status) {
           int rc = (ptr)->ESMC_CalendarReadRestart(*type, daysPerMonth,
                                                    *secondsPerDay,
                                                    *secondsPerYear,
                                                    *daysPerYear,
                                                    *daysPerYearDn,
                                                    *daysPerYearDd);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarwriterestart)(ESMC_Calendar *ptr,
                                             ESMC_CalendarType *type,
                                             int           *daysPerMonth,
                                             ESMF_KIND_I4 *secondsPerDay,
                                             ESMF_KIND_I4 *secondsPerYear,
                                             ESMF_KIND_I4 *daysPerYear,
                                             ESMF_KIND_I4 *daysPerYearDn, 
                                             ESMF_KIND_I4 *daysPerYearDd,
                                             int *status) {
           int rc = (ptr)->ESMC_CalendarWriteRestart(type, daysPerMonth,
                                                     secondsPerDay,
                                                     secondsPerYear,
                                                     daysPerYear,
                                                     daysPerYearDn,
                                                     daysPerYearDd);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarvalidate)(ESMC_Calendar *ptr,
                                         const char *options,
                                         int *status) {
           int rc = (ptr)->ESMC_CalendarValidate(options);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_calendarprint)(ESMC_Calendar *ptr, const char *options,
                                      int *status) {
           int rc = (ptr)->ESMC_CalendarPrint(options);
           if (status != ESMC_NULL_POINTER) *status = rc;
       }
};
