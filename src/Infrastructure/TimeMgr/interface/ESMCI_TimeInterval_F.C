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
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Fraction.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_TimeInterval} class
//  functions.  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN_X(c_esmc_timeintervalsetdur)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 int *status) {
          int rc = (ptr)->TimeInterval::set(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalsetdurstart)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 Time *startTime,
                                 int *status) {
          int rc = (ptr)->TimeInterval::set(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalsetdurcal)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 int *status) {
          int rc = (ptr)->TimeInterval::set(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalsetdurcaltyp)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 ESMC_CalKind_Flag *calkindflag,
                                 int *status) {
          int rc = (ptr)->TimeInterval::set(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calkindflag) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalgetdur)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 Time *startTime,
                                 Calendar **calendar,
                                 ESMC_CalKind_Flag *calkindflag,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status,
                                 ESMCI_FortranStrLenArg tempTime_l,
                                 ESMCI_FortranStrLenArg tempTimeISOFrac_l) {
          int rc = (ptr)->TimeInterval::get(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                                          // always present internal arguments
                                              *timeStringLen,
                                               tempTimeStringLen,
                                               tempTimeString,
                                              *timeStringLenISOFrac,
                                               tempTimeStringLenISOFrac,
                                               tempTimeStringISOFrac);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalgetdurstart)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 Time *startTime,
                                 Calendar **calendar,
                                 ESMC_CalKind_Flag *calkindflag,
                                 Time *startTimeIn,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status,
                                 ESMCI_FortranStrLenArg tempTime_l,
                                 ESMCI_FortranStrLenArg tempTimeISOFrac_l) {
          int rc = (ptr)->TimeInterval::get(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NOT_PRESENT_FILTER(startTimeIn),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                                          // always present internal arguments
                                              *timeStringLen,
                                               tempTimeStringLen,
                                               tempTimeString,
                                              *timeStringLenISOFrac,
                                               tempTimeStringLenISOFrac,
                                               tempTimeStringISOFrac);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalgetdurcal)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 Time *startTime,
                                 Calendar **calendar,
                                 ESMC_CalKind_Flag *calkindflag,
                                 Calendar **calendarIn,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status,
                                 ESMCI_FortranStrLenArg tempTime_l,
                                 ESMCI_FortranStrLenArg tempTimeISOFrac_l) {
          int rc = (ptr)->TimeInterval::get(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendarIn),
                       ESMC_NULL_POINTER,
                                          // always present internal arguments
                                              *timeStringLen,
                                               tempTimeStringLen,
                                               tempTimeString,
                                              *timeStringLenISOFrac,
                                               tempTimeStringLenISOFrac,
                                               tempTimeStringISOFrac);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalgetdurcaltyp)(TimeInterval *ptr,
                                 ESMC_I4 *yy, ESMC_I8 *yy_i8,
                                 ESMC_I4 *mm, ESMC_I8 *mm_i8,
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
                                 Time *startTime,
                                 Calendar **calendar,
                                 ESMC_CalKind_Flag *calkindflag,
                                 ESMC_CalKind_Flag *calkindflagIn,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status,
                                 ESMCI_FortranStrLenArg tempTime_l,
                                 ESMCI_FortranStrLenArg tempTimeISOFrac_l) {
          int rc = (ptr)->TimeInterval::get(
                       ESMC_NOT_PRESENT_FILTER(yy),
                       ESMC_NOT_PRESENT_FILTER(yy_i8),
                       ESMC_NOT_PRESENT_FILTER(mm),
                       ESMC_NOT_PRESENT_FILTER(mm_i8),
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calkindflag),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calkindflagIn),
                                          // always present internal arguments
                                              *timeStringLen,
                                               tempTimeStringLen,
                                               tempTimeString,
                                              *timeStringLenISOFrac,
                                               tempTimeStringLenISOFrac,
                                               tempTimeStringISOFrac);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalabsvalue)(TimeInterval *ptr,
                                    TimeInterval *timeintervalAbsValue) {
           *timeintervalAbsValue = (ptr)->TimeInterval::absValue();
       }

       void FTN_X(c_esmc_timeintervalnegabsvalue)(TimeInterval *ptr,
                                  TimeInterval *timeintervalNegAbsValue) {
           *timeintervalNegAbsValue = (ptr)->TimeInterval::negAbsValue();
       }

       void FTN_X(c_esmc_timeintervalrquot)(TimeInterval *timeinterval1,
                                          TimeInterval *timeinterval2,
                                          ESMC_R8 *timeintervalRQuot) {
           *timeintervalRQuot = (*timeinterval1 / *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalquoti)(TimeInterval *timeinterval,
                                        ESMC_I4 *divisor,
                                        TimeInterval *timeintervalQuotI) {
           *timeintervalQuotI = (*timeinterval / *divisor);
       }

       void FTN_X(c_esmc_timeintervalquotr)(TimeInterval *timeinterval,
                                        ESMC_R8 *divisor,
                                        TimeInterval *timeintervalQuotR) {
           *timeintervalQuotR = (*timeinterval / *divisor);
       }

       void FTN_X(c_esmc_timeintervalfquot)(TimeInterval *timeinterval1,
                                          TimeInterval *timeinterval2,
                                          Fraction *timeintervalFQuot) {
           *timeintervalFQuot =
                        timeinterval1->TimeInterval::div(*timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalremainder)(TimeInterval *timeinterval1,
                                              TimeInterval *timeinterval2,
                                    TimeInterval *timeintervalRemainder) {
           *timeintervalRemainder = (*timeinterval1 % *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalprodti)(TimeInterval *timeinterval,
                                        ESMC_I4 *multiplier,
                                        TimeInterval *timeintervalProdTI) {
           *timeintervalProdTI = (*timeinterval * *multiplier);
       }

       void FTN_X(c_esmc_timeintervalprodit)(ESMC_I4 *multiplier,
                                        TimeInterval *timeinterval,
                                        TimeInterval *timeintervalProdIT) {
           *timeintervalProdIT = (*multiplier * *timeinterval);
       }

       void FTN_X(c_esmc_timeintervalprodtf)(TimeInterval *timeinterval,
                                        Fraction *multiplier,
                                        TimeInterval *timeintervalProdTF) {
           *timeintervalProdTF = (*timeinterval * *multiplier);
       }

       void FTN_X(c_esmc_timeintervalprodft)(Fraction *multiplier,
                                        TimeInterval *timeinterval,
                                        TimeInterval *timeintervalProdFT) {
           *timeintervalProdFT = (*multiplier * *timeinterval);
       }

       void FTN_X(c_esmc_timeintervalprodtr)(TimeInterval *timeinterval,
                                        ESMC_R8 *multiplier,
                                        TimeInterval *timeintervalProdTR) {
           *timeintervalProdTR = (*timeinterval * *multiplier);
       }

       void FTN_X(c_esmc_timeintervalprodrt)(ESMC_R8 *multiplier,
                                        TimeInterval *timeinterval,
                                        TimeInterval *timeintervalProdRT) {
           *timeintervalProdRT = (*multiplier * *timeinterval);
       }

       void FTN_X(c_esmc_timeintervalsum)(TimeInterval *timeinterval1,
                                 TimeInterval *timeinterval2,
                                 TimeInterval *esmf_timeintervalSum) {
           *esmf_timeintervalSum = (*timeinterval1 + *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervaldiff)(TimeInterval *timeinterval1,
                                 TimeInterval *timeinterval2,
                                 TimeInterval *esmf_timeintervalDiff) {
           *esmf_timeintervalDiff = (*timeinterval1 - *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalnegate)(TimeInterval *timeinterval,
                                 TimeInterval *esmf_timeintervalNegate) {
           *esmf_timeintervalNegate = (-(*timeinterval));
       }

       //
       // overloaded comparison operators
       //

       void FTN_X(c_esmc_timeintervaleq)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalEQ) {
           *esmf_timeintervalEQ = (int) (*timeinterval1 == *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalne)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalNE) {
           *esmf_timeintervalNE = (int) (*timeinterval1 != *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervallt)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalLT) {
           *esmf_timeintervalLT = (int) (*timeinterval1 < *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalgt)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalGT) {
           *esmf_timeintervalGT = (int) (*timeinterval1 > *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalle)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalLE) {
           *esmf_timeintervalLE = (int) (*timeinterval1 <= *timeinterval2);
       }

       void FTN_X(c_esmc_timeintervalge)(TimeInterval *timeinterval1,
                                       TimeInterval *timeinterval2,
                                       int *esmf_timeintervalGE) {
           *esmf_timeintervalGE = (int) (*timeinterval1 >= *timeinterval2);
       }


       void FTN_X(c_esmc_timeintervalreadrestart)(TimeInterval *ptr,
                                                int *nameLen,
                                                const char *name,
                                                int *status,
                                                ESMCI_FortranStrLenArg name_l) {
          int rc = (ptr)->TimeInterval::readRestart(
                                        *nameLen,  // always present
                                                   //   internal argument.
                                         name);    // required.

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalwriterestart)(TimeInterval *ptr,
                                                 int *status) {
          int rc = (ptr)->TimeInterval::writeRestart();

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalvalidate)(TimeInterval *ptr,
                                             const char *options, int *status,
                                             ESMCI_FortranStrLenArg options_l) {
          int rc = (ptr)->TimeInterval::validate(
                            ESMC_NOT_PRESENT_FILTER(options) );

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN_X(c_esmc_timeintervalprint)(TimeInterval *ptr,
                                          const char *options, int *status,
                                          ESMCI_FortranStrLenArg options_l) {
          int rc = (ptr)->TimeInterval::print(
                         ESMC_NOT_PRESENT_FILTER(options) );

          fflush (stdout);
          if (ESMC_PRESENT(status)) *status = rc;
       }
};

}  // namespace ESMCI
