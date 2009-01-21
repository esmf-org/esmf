// $Id: ESMC_TimeInterval_F.C,v 1.42.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_TimeInterval.h>
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


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_timeintervalsetdur)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 int *status) {
          int rc = (ptr)->ESMC_TimeIntervalSet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER, 
                       ESMC_NULL_POINTER, 
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalsetdurstart)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Time *startTime,
                                 int *status) {
          int rc = (ptr)->ESMC_TimeIntervalSet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalsetdurcal)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Calendar **calendar,
                                 int *status) {
          int rc = (ptr)->ESMC_TimeIntervalSet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NULL_POINTER );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalsetdurcaltyp)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_CalendarType *calendarType,
                                 int *status) {
          int rc = (ptr)->ESMC_TimeIntervalSet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendarType) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalgetdur)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Time *startTime,
                                 ESMC_Calendar **calendar,
                                 ESMC_CalendarType *calendarType,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalGet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar), 
                       ESMC_NOT_PRESENT_FILTER(calendarType), 
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

       void FTN(c_esmc_timeintervalgetdurstart)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Time *startTime,
                                 ESMC_Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 ESMC_Time *startTimeIn,
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalGet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar), 
                       ESMC_NOT_PRESENT_FILTER(calendarType), 
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

       void FTN(c_esmc_timeintervalgetdurcal)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Time *startTime,
                                 ESMC_Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 ESMC_Calendar **calendarIn, 
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalGet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar), 
                       ESMC_NOT_PRESENT_FILTER(calendarType), 
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

       void FTN(c_esmc_timeintervalgetdurcaltyp)(ESMC_TimeInterval *ptr,
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
                                 ESMC_I4 *sN, ESMC_I4 *sD,
                                 ESMC_Time *startTime,
                                 ESMC_Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 ESMC_CalendarType *calendarTypeIn, 
                                 int *timeStringLen, int *tempTimeStringLen,
                                 char *tempTimeString,
                                 int *timeStringLenISOFrac,
                                 int *tempTimeStringLenISOFrac,
                                 char *tempTimeStringISOFrac, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalGet(
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
                       ESMC_NOT_PRESENT_FILTER(sD),
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendar), 
                       ESMC_NOT_PRESENT_FILTER(calendarType), 
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NULL_POINTER,
                       ESMC_NOT_PRESENT_FILTER(calendarTypeIn),
                                          // always present internal arguments
                                              *timeStringLen,
	                                       tempTimeStringLen,
                                               tempTimeString,
                                              *timeStringLenISOFrac,
	                                       tempTimeStringLenISOFrac,
                                               tempTimeStringISOFrac);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalabsvalue)(ESMC_TimeInterval *ptr,
                                    ESMC_TimeInterval *timeintervalAbsValue) {
           *timeintervalAbsValue = (ptr)->ESMC_TimeIntervalAbsValue();
       }

       void FTN(c_esmc_timeintervalnegabsvalue)(ESMC_TimeInterval *ptr,
                                  ESMC_TimeInterval *timeintervalNegAbsValue) {
           *timeintervalNegAbsValue = (ptr)->ESMC_TimeIntervalNegAbsValue();
       }

       void FTN(c_esmc_timeintervalrquot)(ESMC_TimeInterval *timeinterval1,
                                          ESMC_TimeInterval *timeinterval2,
                                          ESMC_R8 *timeintervalRQuot) {
           *timeintervalRQuot = (*timeinterval1 / *timeinterval2);
       }

       void FTN(c_esmc_timeintervalquoti)(ESMC_TimeInterval *timeinterval,
                                        ESMC_I4 *divisor,
                                        ESMC_TimeInterval *timeintervalQuotI) {
           *timeintervalQuotI = (*timeinterval / *divisor);
       }

       void FTN(c_esmc_timeintervalquotr)(ESMC_TimeInterval *timeinterval,
                                        ESMC_R8 *divisor,
                                        ESMC_TimeInterval *timeintervalQuotR) {
           *timeintervalQuotR = (*timeinterval / *divisor);
       }

       void FTN(c_esmc_timeintervalfquot)(ESMC_TimeInterval *timeinterval1,
                                          ESMC_TimeInterval *timeinterval2,
                                          ESMC_Fraction *timeintervalFQuot) {
           *timeintervalFQuot = 
                        timeinterval1->ESMC_TimeIntervalDiv(*timeinterval2);
       }

       void FTN(c_esmc_timeintervalremainder)(ESMC_TimeInterval *timeinterval1,
                                              ESMC_TimeInterval *timeinterval2,
                                    ESMC_TimeInterval *timeintervalRemainder) {
           *timeintervalRemainder = (*timeinterval1 % *timeinterval2);
       }

       void FTN(c_esmc_timeintervalprodti)(ESMC_TimeInterval *timeinterval,
                                        ESMC_I4 *multiplier,
                                        ESMC_TimeInterval *timeintervalProdTI) {
           *timeintervalProdTI = (*timeinterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodit)(ESMC_I4 *multiplier,
                                        ESMC_TimeInterval *timeinterval,
                                        ESMC_TimeInterval *timeintervalProdIT) {
           *timeintervalProdIT = (*multiplier * *timeinterval);
       }

       void FTN(c_esmc_timeintervalprodtf)(ESMC_TimeInterval *timeinterval,
                                        ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeintervalProdTF) {
           *timeintervalProdTF = (*timeinterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodft)(ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeinterval,
                                        ESMC_TimeInterval *timeintervalProdFT) {
           *timeintervalProdFT = (*multiplier * *timeinterval);
       }

       void FTN(c_esmc_timeintervalprodtr)(ESMC_TimeInterval *timeinterval,
                                        ESMC_R8 *multiplier,
                                        ESMC_TimeInterval *timeintervalProdTR) {
           *timeintervalProdTR = (*timeinterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodrt)(ESMC_R8 *multiplier,
                                        ESMC_TimeInterval *timeinterval,
                                        ESMC_TimeInterval *timeintervalProdRT) {
           *timeintervalProdRT = (*multiplier * *timeinterval);
       }

       void FTN(c_esmc_timeintervalsum)(ESMC_TimeInterval *timeinterval1,
                                 ESMC_TimeInterval *timeinterval2,
                                 ESMC_TimeInterval *esmf_timeintervalSum) {
           *esmf_timeintervalSum = (*timeinterval1 + *timeinterval2);
       }

       void FTN(c_esmc_timeintervaldiff)(ESMC_TimeInterval *timeinterval1,
                                 ESMC_TimeInterval *timeinterval2,
                                 ESMC_TimeInterval *esmf_timeintervalDiff) {
           *esmf_timeintervalDiff = (*timeinterval1 - *timeinterval2);
       }

       void FTN(c_esmc_timeintervalnegate)(ESMC_TimeInterval *timeinterval,
                                 ESMC_TimeInterval *esmf_timeintervalNegate) {
           *esmf_timeintervalNegate = (-(*timeinterval));
       }

       //
       // overloaded comparison operators
       //

       void FTN(c_esmc_timeintervaleq)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalEQ) {
           *esmf_timeintervalEQ = (int) (*timeinterval1 == *timeinterval2);
       }

       void FTN(c_esmc_timeintervalne)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalNE) {
           *esmf_timeintervalNE = (int) (*timeinterval1 != *timeinterval2);
       }

       void FTN(c_esmc_timeintervallt)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalLT) {
           *esmf_timeintervalLT = (int) (*timeinterval1 < *timeinterval2);
       }

       void FTN(c_esmc_timeintervalgt)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalGT) {
           *esmf_timeintervalGT = (int) (*timeinterval1 > *timeinterval2);
       }

       void FTN(c_esmc_timeintervalle)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalLE) {
           *esmf_timeintervalLE = (int) (*timeinterval1 <= *timeinterval2);
       }

       void FTN(c_esmc_timeintervalge)(ESMC_TimeInterval *timeinterval1,
                                       ESMC_TimeInterval *timeinterval2,
                                       int *esmf_timeintervalGE) {
           *esmf_timeintervalGE = (int) (*timeinterval1 >= *timeinterval2);
       }


       void FTN(c_esmc_timeintervalreadrestart)(ESMC_TimeInterval *ptr,
                                                int *nameLen,
                                                const char *name,
                                                ESMC_IOSpec *iospec,
                                                int *status) {
          int rc = (ptr)->ESMC_TimeIntervalReadRestart(
                                        *nameLen,  // always present
                                                   //   internal argument.
                                         name,     // required.
                 ESMC_NOT_PRESENT_FILTER(iospec) );

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalwriterestart)(ESMC_TimeInterval *ptr,
                                                 ESMC_IOSpec *iospec,
                                                 int *status) {
          int rc = (ptr)->ESMC_TimeIntervalWriteRestart(
                                ESMC_NOT_PRESENT_FILTER(iospec) );

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval *ptr,
                                             const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalValidate(
                            ESMC_NOT_PRESENT_FILTER(options) );

          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval *ptr,
                                          const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalPrint(
                         ESMC_NOT_PRESENT_FILTER(options) );

          if (ESMC_PRESENT(status)) *status = rc;
       }
};
