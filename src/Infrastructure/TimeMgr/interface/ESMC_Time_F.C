// $Id: ESMC_Time_F.C,v 1.8 2003/04/05 01:53:48 eschwab Exp $
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
#include "ESMC_Time.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Time} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_timeinit)(ESMC_Time *ptr, int *YR, int *MM, int *DD,
                                 int *D, int *H, int *M, ESMF_IKIND_I8 *S,
                                 int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd,
                                 ESMC_Calendar *cal, int *tz, int *status) {
           *status = (ptr)->ESMC_TimeInit(YR, MM, DD, D, H, M, S, MS,
                                          US, NS, d_, h_, m_, s_, ms_,
                                          us_, ns_, Sn, Sd, cal, tz);
       }

       void FTN(c_esmc_timeget)(ESMC_Time *ptr, int *YR, int *MM, int *DD,
                                int *D, int *H, int *M, ESMF_IKIND_I8 *S,
                                int *MS, int *US, int *NS,
                                double *d_, double *h_, double *m_,
                                double *s_, double *ms_, double *us_,
                                double *ns_, int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_TimeGet(YR, MM, DD, D, H, M, S, MS,
                                         US, NS, d_, h_, m_, s_, ms_,
                                         us_, ns_, Sn, Sd);
       }

       void FTN(c_esmc_timeset)(ESMC_Time *ptr, int *YR, int *MM, int *DD,
                                int *D, int *H, int *M, ESMF_IKIND_I8 *S,
                                int *MS, int *US, int *NS,
                                double *d_, double *h_, double *m_,
                                double *s_, double *ms_, double *us_,
                                double *ns_, int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_TimeSet(YR, MM, DD, D, H, M, S, MS,
                                         US, NS, d_, h_, m_, s_, ms_,
                                         us_, ns_, Sn, Sd);
       }

       void FTN(c_esmc_timegetcalendar)(ESMC_Time *ptr,
                                        ESMC_Calendar *calendar, int *status) {
           *status = (ptr)->ESMC_TimeGetCalendar(calendar);
       }

       void FTN(c_esmc_timesetcalendar)(ESMC_Time *ptr,
                                        ESMC_Calendar *calendar, int *status) {
           *status = (ptr)->ESMC_TimeSetCalendar(calendar);
       }

       void FTN(c_esmc_timeissamecal)(ESMC_Time *ptr, ESMC_Time *time,
                                      int *esmf_timeIsSameCal, int *status) {
           *esmf_timeIsSameCal = (ptr)->ESMC_TimeIsSameCal(time, status);
       }

       void FTN(c_esmc_timegettimezone)(ESMC_Time *ptr,
                                        int *timezone, int *status) {  
           *status = (ptr)->ESMC_TimeGetTimeZone(timezone);
       }

       void FTN(c_esmc_timesettimezone)(ESMC_Time *ptr,
                                        int *timezone, int *status) {  
           *status = (ptr)->ESMC_TimeSetTimeZone(*timezone);
       }

       void FTN(c_esmc_timegetstring)(ESMC_Time *ptr,
                                      char *timestring, int *status) { 
           *status = (ptr)->ESMC_TimeGetString(timestring);
       }

       void FTN(c_esmc_timegetdayofyear)(ESMC_Time *ptr,
                                         double *dayofyear, int *status) {
           *status = (ptr)->ESMC_TimeGetDayOfYear(dayofyear);
       }

       void FTN(c_esmc_timegetdayofweek)(ESMC_Time *ptr,
                                         int *dayofweek, int *status) {   
           *status = (ptr)->ESMC_TimeGetDayOfWeek(dayofweek);
       }

       void FTN(c_esmc_timegetdayofmonth)(ESMC_Time *ptr,
                                          int *dayofmonth, int *status) {
           *status = (ptr)->ESMC_TimeGetDayOfMonth(dayofmonth);
       }

       void FTN(c_esmc_timegetmidmonth)(ESMC_Time *ptr,
                                        ESMC_Time *midmonth, int *status) {  
           *status = (ptr)->ESMC_TimeGetMidMonth(midmonth);
       }

       void FTN(c_esmc_timegetrealtime)(ESMC_Time *ptr,
                                        int *status) {                 
           *status = (ptr)->ESMC_TimeGetRealTime();      
       }

       void FTN(c_esmc_timeread)(ESMC_Time *ptr, ESMF_IKIND_I8 *S,
                                 int *Sn, int *Sd, ESMC_Calendar *cal,
                                 int *timeZone, int *status) {
           *status = (ptr)->ESMC_Time::ESMC_Read(*S, *Sn, *Sd, cal, *timeZone);
       }

       void FTN(c_esmc_timewrite)(ESMC_Time *ptr, ESMF_IKIND_I8 *S,
                                  int *Sn, int *Sd, ESMC_Calendar *cal,
                                 int *timeZone, int *status) {
           *status = (ptr)->ESMC_Time::ESMC_Write(S, Sn, Sd, cal, timeZone);
       }

       void FTN(c_esmc_timevalidate)(ESMC_Time *ptr, const char *opts,
                                     int *status) {
           *status = (ptr)->ESMC_Time::ESMC_Validate(opts);
       }

       void FTN(c_esmc_timeprint)(ESMC_Time *ptr, const char *opts,
                                  int *status) {
           *status = (ptr)->ESMC_Time::ESMC_Print(opts);
       }
};
