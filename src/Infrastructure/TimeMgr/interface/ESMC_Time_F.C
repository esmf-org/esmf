// $Id: ESMC_Time_F.C,v 1.22 2004/01/16 00:31:23 eschwab Exp $
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
#include "ESMC_TimeInterval.h"
#include "ESMC_Time.h"
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


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_timeset)(ESMC_Time *ptr,
                                ESMF_KIND_I4 *yr, ESMF_KIND_I8 *yr_i8,
                                int *mm, int *dd,
                                ESMF_KIND_I4 *d,  ESMF_KIND_I8 *d_i8,
                                ESMF_KIND_I4 *h,  ESMF_KIND_I4 *m,
                                ESMF_KIND_I4 *s,  ESMF_KIND_I8 *s_i8,
                                ESMF_KIND_I4 *ms, ESMF_KIND_I4 *us,
                                ESMF_KIND_I4 *ns,
                                ESMF_KIND_R8 *d_r8,  ESMF_KIND_R8 *h_r8,
                                ESMF_KIND_R8 *m_r8,  ESMF_KIND_R8 *s_r8,
                                ESMF_KIND_R8 *ms_r8, ESMF_KIND_R8 *us_r8,
                                ESMF_KIND_R8 *ns_r8,
                                ESMF_KIND_I4 *sN, ESMF_KIND_I4 *sD,
                                ESMC_Calendar *calendar, int *timeZone,
                                int *status) {
          int rc = (ptr)->ESMC_TimeSet(
                    ((void*)yr       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : yr),
                    ((void*)yr_i8    == (void*)ESMC_BAD_POINTER ? 
                                               ESMC_NULL_POINTER : yr_i8),
                    ((void*)mm       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : mm),
                    ((void*)dd       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : dd),
                    ((void*)d        == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : d),
                    ((void*)d_i8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : d_i8),
                    ((void*)h        == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : h),
                    ((void*)m        == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : m),
                    ((void*)s        == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : s),
                    ((void*)s_i8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : s_i8),
                    ((void*)ms       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : ms),
                    ((void*)us       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : us),
                    ((void*)ns       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : ns),
                    ((void*)d_r8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : d_r8),
                    ((void*)h_r8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : h_r8),
                    ((void*)m_r8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : m_r8),
                    ((void*)s_r8     == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : s_r8),
                    ((void*)ms_r8    == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : ms_r8),
                    ((void*)us_r8    == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : us_r8),
                    ((void*)ns_r8    == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : ns_r8),
                    ((void*)sN       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : sN),
                    ((void*)sD       == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : sD),
                    ((void*)calendar == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : calendar),
                    ((void*)timeZone == (void*)ESMC_BAD_POINTER ?
                                               ESMC_NULL_POINTER : timeZone) );

          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeget)(ESMC_Time *ptr,
                                ESMF_KIND_I4 *yr, ESMF_KIND_I8 *yr_i8,
                                int *mm, int *dd,
                                ESMF_KIND_I4 *d,  ESMF_KIND_I8 *d_i8,
                                ESMF_KIND_I4 *h,  ESMF_KIND_I4 *m,
                                ESMF_KIND_I4 *s,  ESMF_KIND_I8 *s_i8,
                                ESMF_KIND_I4 *ms, ESMF_KIND_I4 *us,
                                ESMF_KIND_I4 *ns,
                                ESMF_KIND_R8 *d_r8,  ESMF_KIND_R8 *h_r8,
                                ESMF_KIND_R8 *m_r8,  ESMF_KIND_R8 *s_r8,
                                ESMF_KIND_R8 *ms_r8, ESMF_KIND_R8 *us_r8,
                                ESMF_KIND_R8 *ns_r8,
                                ESMF_KIND_I4 *sN, ESMF_KIND_I4 *sD,
                                ESMC_Calendar *calendar, int *timeZone,
                                char *timeString, int *dayOfWeek,
                                ESMC_Time *midMonth,
                                ESMF_KIND_I4 *dayOfYear,
                                ESMF_KIND_R8 *dayOfYear_r8,
                                ESMC_TimeInterval *dayOfYear_intvl,
                                int *status) {

          int rc = (ptr)->ESMC_TimeGet(
                 ((void*)yr    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : yr),
                 ((void*)yr_i8 == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : yr_i8),
                 ((void*)mm    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : mm),
                 ((void*)dd    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : dd),
                 ((void*)d     == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : d),
                 ((void*)d_i8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : d_i8),
                 ((void*)h     == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : h),
                 ((void*)m     == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : m),
                 ((void*)s     == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : s),
                 ((void*)s_i8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : s_i8),
                 ((void*)ms    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : ms),
                 ((void*)us    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : us),
                 ((void*)ns    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : ns),
                 ((void*)d_r8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : d_r8),
                 ((void*)h_r8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : h_r8),
                 ((void*)m_r8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : m_r8),
                 ((void*)s_r8  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : s_r8),
                 ((void*)ms_r8 == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : ms_r8),
                 ((void*)us_r8 == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : us_r8),
                 ((void*)ns_r8 == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : ns_r8),
                 ((void*)sN    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : sN),
                 ((void*)sD    == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : sD),
                 ((void*)calendar   == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : calendar),
                 ((void*)timeZone   == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : timeZone),
                 ((void*)timeString == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : timeString),
                 ((void*)dayOfWeek  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : dayOfWeek),
                 ((void*)midMonth   == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : midMonth),
                 ((void*)dayOfYear  == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : dayOfYear),
                 ((void*)dayOfYear_r8 == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : dayOfYear_r8),
                 ((void*)dayOfYear_intvl == (void*)ESMC_BAD_POINTER ?
                                         ESMC_NULL_POINTER : dayOfYear_intvl) );

          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeissamecalendar)(ESMC_Time *ptr, ESMC_Time *time,
                                           int *esmf_timeIsSameCalendar,
                                           int *status) {
           *esmf_timeIsSameCalendar =
                 (int) (ptr)->ESMC_TimeIsSameCalendar(time, 
                         ((void*)status == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : status));
       }

       void FTN(c_esmc_timesynctorealtime)(ESMC_Time *ptr,
                                           int *status) {                 
          int rc = (ptr)->ESMC_TimeSyncToRealTime();      
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeinc)(ESMC_Time *time,
                                ESMC_TimeInterval *timeInterval,
                                ESMC_Time *esmf_baseTimeInc) {
           *esmf_baseTimeInc = (*time + *timeInterval);
       }

       void FTN(c_esmc_timedec)(ESMC_Time *time,
                                ESMC_TimeInterval *timeInterval,
                                ESMC_Time *esmf_baseTimeDec) {
           *esmf_baseTimeDec = (*time - *timeInterval);
       }

       void FTN(c_esmc_timediff)(ESMC_Time *time1,
                                 ESMC_Time *time2,
                                 ESMC_TimeInterval *esmf_timeDiff) {
           *esmf_timeDiff = (*time1 - *time2);
       }

       void FTN(c_esmc_timereadrestart)(ESMC_Time *ptr, int *nameLen,
                                        const char *name,
                                        ESMC_IOSpec *iospec,   
                                        int *status) {
          int rc = (ptr)->ESMC_TimeReadRestart(
                 *nameLen,  // always present internal argument.
                 name,      // required.
                 ((void*)iospec == (void*)ESMC_BAD_POINTER ?       
                                                  ESMC_NULL_POINTER : iospec) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timewriterestart)(ESMC_Time *ptr, 
                                         ESMC_IOSpec *iospec,
                                         int *status) {
          int rc = (ptr)->ESMC_TimeWriteRestart(
              ((void*)iospec == (void*)ESMC_BAD_POINTER ?
                                                  ESMC_NULL_POINTER : iospec) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timevalidate)(ESMC_Time *ptr, const char *options,
                                     int *status) {
          int rc = (ptr)->ESMC_TimeValidate(
                      ((void*)options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeprint)(ESMC_Time *ptr, const char *options,
                                  int *status) {
          int rc = (ptr)->ESMC_TimePrint(
                      ((void*)options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
