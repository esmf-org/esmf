// $Id: ESMC_Time_F.C,v 1.18 2003/08/29 05:31:58 eschwab Exp $
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
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_timeset)(ESMC_Time *ptr,
                                ESMF_IKIND_I4 *yr, ESMF_IKIND_I8 *yr_i8,
                                int *mm, int *dd,
                                ESMF_IKIND_I4 *d,  ESMF_IKIND_I8 *d_i8,
                                ESMF_IKIND_I4 *h,  ESMF_IKIND_I4 *m,
                                ESMF_IKIND_I4 *s,  ESMF_IKIND_I8 *s_i8,
                                ESMF_IKIND_I4 *ms, ESMF_IKIND_I4 *us,
                                ESMF_IKIND_I4 *ns,
                                ESMF_IKIND_R8 *d_r8,  ESMF_IKIND_R8 *h_r8,
                                ESMF_IKIND_R8 *m_r8,  ESMF_IKIND_R8 *s_r8,
                                ESMF_IKIND_R8 *ms_r8, ESMF_IKIND_R8 *us_r8,
                                ESMF_IKIND_R8 *ns_r8,
                                ESMF_IKIND_I4 *sN, ESMF_IKIND_I4 *sD,
                                ESMC_Calendar *calendar, int *timeZone,
                                int *status) {
          int rc = (ptr)->ESMC_TimeSet(yr, yr_i8, mm, dd, d, d_i8, h, m,
                                         s, s_i8, ms, us, ns, d_r8, h_r8, m_r8,
                                         s_r8, ms_r8, us_r8, ns_r8,
                                         sN, sD, calendar, timeZone);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeget)(ESMC_Time *ptr,
                                ESMF_IKIND_I4 *yr, ESMF_IKIND_I8 *yr_i8,
                                int *mm, int *dd,
                                ESMF_IKIND_I4 *d,  ESMF_IKIND_I8 *d_i8,
                                ESMF_IKIND_I4 *h,  ESMF_IKIND_I4 *m,
                                ESMF_IKIND_I4 *s,  ESMF_IKIND_I8 *s_i8,
                                ESMF_IKIND_I4 *ms, ESMF_IKIND_I4 *us,
                                ESMF_IKIND_I4 *ns,
                                ESMF_IKIND_R8 *d_r8,  ESMF_IKIND_R8 *h_r8,
                                ESMF_IKIND_R8 *m_r8,  ESMF_IKIND_R8 *s_r8,
                                ESMF_IKIND_R8 *ms_r8, ESMF_IKIND_R8 *us_r8,
                                ESMF_IKIND_R8 *ns_r8,
                                ESMF_IKIND_I4 *sN, ESMF_IKIND_I4 *sD,
                                ESMC_Calendar *calendar, int *timeZone,
                                char *timeString, int *dayOfWeek,
                                int *dayOfMonth, ESMC_Time *midMonth,
                                ESMF_IKIND_I4 *dayOfYear,
                                ESMF_IKIND_R8 *dayOfYear_r8,
                                ESMC_TimeInterval *dayOfYear_intvl,
                                int *status) {
          int rc = (ptr)->ESMC_TimeGet(yr, yr_i8, mm, dd, d, d_i8, h, m,
                                         s, s_i8, ms, us, ns, d_r8, h_r8, m_r8,
                                         s_r8, ms_r8, us_r8, ns_r8,
                                         sN, sD, calendar, timeZone,
                                         timeString, dayOfWeek, dayOfMonth,
                                         midMonth, dayOfYear, dayOfYear_r8,
                                         dayOfYear_intvl);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeissamecalendar)(ESMC_Time *ptr, ESMC_Time *time,
                                           int *esmf_timeIsSameCalendar,
                                           int *status) {
           *esmf_timeIsSameCalendar =
                                   (ptr)->ESMC_TimeIsSameCalendar(time, status);
       }

       void FTN(c_esmc_timesynctorealtime)(ESMC_Time *ptr,
                                           int *status) {                 
          int rc = (ptr)->ESMC_TimeSyncToRealTime();      
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timereadrestart)(ESMC_Time *ptr, ESMF_IKIND_I8 *s,
                                        ESMF_IKIND_I4 *sN, ESMF_IKIND_I4 *sD,
                                        ESMC_Calendar *calendar, int *timeZone,
                                        int *status) {
          int rc = (ptr)->ESMC_TimeReadRestart(*s, *sN, *sD, calendar,
                                               *timeZone);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timewriterestart)(ESMC_Time *ptr, ESMF_IKIND_I8 *s,
                                         ESMF_IKIND_I4 *sN, ESMF_IKIND_I4 *sD,
                                         ESMC_Calendar *calendar, int *timeZone,
                                         int *status) {
          int rc = (ptr)->ESMC_TimeWriteRestart(s, sN, sD, calendar, timeZone);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timevalidate)(ESMC_Time *ptr, const char *options,
                                     int *status) {
          int rc = (ptr)->ESMC_TimeValidate(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeprint)(ESMC_Time *ptr, const char *options,
                                  int *status) {
          int rc = (ptr)->ESMC_TimePrint(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }
};
