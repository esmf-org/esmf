// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Time.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Time} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN_X(c_esmc_timeset)(Time *ptr,
                                ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                int *mm, int *dd,
                                ESMC_I4 *d,  ESMC_I8 *d_i8,
                                ESMC_I4 *h,  ESMC_I4 *m,
                                ESMC_I4 *s,  ESMC_I8 *s_i8,
                                ESMC_I4 *ms, ESMC_I4 *us,
                                ESMC_I4 *ns,
                                ESMC_R8 *d_r8,  ESMC_R8 *h_r8,
                                ESMC_R8 *m_r8,  ESMC_R8 *s_r8,
                                ESMC_R8 *ms_r8, ESMC_R8 *us_r8,
                                ESMC_R8 *ns_r8,
                                ESMC_I4 *sN, ESMC_I8 *sN_i8,
                                ESMC_I4 *sD, ESMC_I8 *sD_i8,
                                Calendar **calendar,
                                ESMC_CalKind_Flag *calkindflag,
                                int *timeZone,
                                int *status) {
          int rc = (ptr)->Time::set(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(dd),
                       ESMC_NOT_PRESENT_FILTER(d),
                       ESMC_NOT_PRESENT_FILTER(d_i8),
                       ESMC_NOT_PRESENT_FILTER(h),
                       ESMC_NOT_PRESENT_FILTER(m),
                       ESMC_NOT_PRESENT_FILTER(s),
                       ESMC_NOT_PRESENT_FILTER(s_i8),
                       ESMC_NOT_PRESENT_FILTER(ms),
                       ESMC_NOT_PRESENT_FILTER(us),
                       ESMC_NOT_PRESENT_FILTER(ns),
                       ESMC_NOT_PRESENT_FILTER(d_r8),
                       ESMC_NOT_PRESENT_FILTER(h_r8),
                       ESMC_NOT_PRESENT_FILTER(m_r8),
                       ESMC_NOT_PRESENT_FILTER(s_r8),
                       ESMC_NOT_PRESENT_FILTER(ms_r8),
                       ESMC_NOT_PRESENT_FILTER(us_r8),
                       ESMC_NOT_PRESENT_FILTER(ns_r8),
                       ESMC_NOT_PRESENT_FILTER(sN),
                       ESMC_NOT_PRESENT_FILTER(sN_i8),
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(sD_i8),
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NOT_PRESENT_FILTER(timeZone) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeget)(Time *ptr,
                              ESMC_I4 *yy, ESMC_I8 *yy_i8,
                              int *mm, int *dd,
                              ESMC_I4 *d,  ESMC_I8 *d_i8,
                              ESMC_I4 *h,  ESMC_I4 *m,
                              ESMC_I4 *s,  ESMC_I8 *s_i8,
                              ESMC_I4 *ms, ESMC_I4 *us,
                              ESMC_I4 *ns,
                              ESMC_R8 *d_r8,  ESMC_R8 *h_r8,
                              ESMC_R8 *m_r8,  ESMC_R8 *s_r8,
                              ESMC_R8 *ms_r8, ESMC_R8 *us_r8,
                              ESMC_R8 *ns_r8,
                              ESMC_I4 *sN, ESMC_I8 *sN_i8,
                              ESMC_I4 *sD, ESMC_I8 *sD_i8,
                              Calendar **calendar, 
                              ESMC_CalKind_Flag *calkindflag, 
                              int *timeZone,
                              int *timeStringLen, int *tempTimeStringLen, 
                              char *tempTimeString,
                              int *timeStringLenISOFrac, 
                              int *tempTimeStringLenISOFrac, 
                              char *tempTimeStringISOFrac,
                              int *dayOfWeek,
                              Time *midMonth,
                              ESMC_I4 *dayOfYear,
                              ESMC_R8 *dayOfYear_r8,
                              TimeInterval *dayOfYear_intvl,
                              int *status,
                              ESMCI_FortranStrLenArg tempTime_l,
                              ESMCI_FortranStrLenArg tempTimeISOFrac_l) {
          int rc = (ptr)->Time::get(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(dd),
                       ESMC_NOT_PRESENT_FILTER(d),
                       ESMC_NOT_PRESENT_FILTER(d_i8),
                       ESMC_NOT_PRESENT_FILTER(h),
                       ESMC_NOT_PRESENT_FILTER(m),
                       ESMC_NOT_PRESENT_FILTER(s),
                       ESMC_NOT_PRESENT_FILTER(s_i8),
                       ESMC_NOT_PRESENT_FILTER(ms),
                       ESMC_NOT_PRESENT_FILTER(us),
                       ESMC_NOT_PRESENT_FILTER(ns),
                       ESMC_NOT_PRESENT_FILTER(d_r8),
                       ESMC_NOT_PRESENT_FILTER(h_r8),
                       ESMC_NOT_PRESENT_FILTER(m_r8),
                       ESMC_NOT_PRESENT_FILTER(s_r8),
                       ESMC_NOT_PRESENT_FILTER(ms_r8),
                       ESMC_NOT_PRESENT_FILTER(us_r8),
                       ESMC_NOT_PRESENT_FILTER(ns_r8),
                       ESMC_NOT_PRESENT_FILTER(sN),
                       ESMC_NOT_PRESENT_FILTER(sN_i8),
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(sD_i8),
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NOT_PRESENT_FILTER(timeZone),
                                          // always present internal arguments
                                              *timeStringLen,
                                               tempTimeStringLen, 
                                               tempTimeString,
                                              *timeStringLenISOFrac,
                                               tempTimeStringLenISOFrac, 
                                               tempTimeStringISOFrac,
                       ESMC_NOT_PRESENT_FILTER(dayOfWeek),
                       ESMC_NOT_PRESENT_FILTER(midMonth),
                       ESMC_NOT_PRESENT_FILTER(dayOfYear),
                       ESMC_NOT_PRESENT_FILTER(dayOfYear_r8),
                       ESMC_NOT_PRESENT_FILTER(dayOfYear_intvl) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeisleapyear)(Time *ptr,
                                       int *esmf_timeIsLeapYear,
                                       int *status) {
           *esmf_timeIsLeapYear =
                 (int) (ptr)->Time::isLeapYear(
                              ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_timeissamecalendar)(Time *ptr, Time *time,
                                           int *esmf_timeIsSameCalendar,
                                           int *status) {
           *esmf_timeIsSameCalendar =
                 (int) (ptr)->Time::isSameCalendar(time, 
                              ESMC_NOT_PRESENT_FILTER(status) );
       }

       void FTN_X(c_esmc_timesynctorealtime)(Time *ptr,
                                           int *status) {                 
          int rc = (ptr)->Time::syncToRealTime();      
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeinc)(Time *time,
                                TimeInterval *timeinterval,
                                Time *esmf_baseTimeInc) {
           *esmf_baseTimeInc = (*time + *timeinterval);
       }

       void FTN_X(c_esmc_timedec)(Time *time,
                                TimeInterval *timeinterval,
                                Time *esmf_baseTimeDec) {
           *esmf_baseTimeDec = (*time - *timeinterval);
       }

       void FTN_X(c_esmc_timediff)(Time *time1,
                                 Time *time2,
                                 TimeInterval *esmf_timeDiff) {
           *esmf_timeDiff = (*time1 - *time2);
       }

       void FTN_X(c_esmc_timereadrestart)(Time *ptr, int *nameLen,
                                        const char *name,
                                        int *status,
                                        ESMCI_FortranStrLenArg name_l) {
          int rc = (ptr)->Time::readRestart(
                                               *nameLen,  // always present 
                                                          //  internal argument.
                                                name);    // required.
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timewriterestart)(Time *ptr, 
                                         int *status) {
          int rc = (ptr)->Time::writeRestart();
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timevalidate)(Time *ptr, const char *options,
                                     int *status,
                                     ESMCI_FortranStrLenArg options_l) {
          int rc = (ptr)->Time::validate(
                    ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeprint)(Time *ptr, const char *options,
                                  int *status,
                                  ESMCI_FortranStrLenArg options_l) {
          int rc = (ptr)->Time::print(
                 ESMC_NOT_PRESENT_FILTER(options) );
          fflush (stdout);
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

}  // namespace ESMCI
