// $Id: ESMC_Time_F.C,v 1.16 2003/07/25 19:36:08 eschwab Exp $
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

       void FTN(c_esmc_timeget)(ESMC_Time *ptr,
                                int *YR, ESMF_IKIND_I8 *YRl,
                                int *MM, int *DD,
                                int *D, ESMF_IKIND_I8 *Dl,
                                int *H, int *M,
                                int *S, ESMF_IKIND_I8 *Sl,
                                int *MS, int *US, int *NS,
                                double *d_, double *h_, double *m_,
                                double *s_, double *ms_, double *us_,
                                double *ns_, int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_TimeGet(YR, YRl, MM, DD, D, Dl, H, M, S, Sl,
                                         MS, US, NS, d_, h_, m_, s_,
                                         ms_, us_, ns_, Sn, Sd);
       }

       void FTN(c_esmc_timeset)(ESMC_Time *ptr,
                                int *YR, ESMF_IKIND_I8 *YRl,
                                int *MM, int *DD,
                                int *D, ESMF_IKIND_I8 *Dl,
                                int *H, int *M,
                                int *S, ESMF_IKIND_I8 *Sl,
                                int *MS, int *US, int *NS,
                                double *d_, double *h_, double *m_,
                                double *s_, double *ms_, double *us_,
                                double *ns_, int *Sn, int *Sd,
                                ESMC_Calendar *cal, int *tz, int *status) {
           *status = (ptr)->ESMC_TimeSet(YR, YRl, MM, DD, D, Dl, H, M, S, Sl,
                                         MS, US, NS, d_, h_, m_, s_,
                                         ms_, us_, ns_, Sn, Sd, cal, tz);
       }

       void FTN(c_esmc_timegetcalendarcopy)(ESMC_Time *ptr,
                                        ESMC_Calendar *calendar, int *status) {
           *status = (ptr)->ESMC_TimeGetCalendar(calendar);
       }

       void FTN(c_esmc_timegetcalendarptr)(ESMC_Time *ptr,
                                        ESMC_Calendar **calendar, int *status) {
           *status = (ptr)->ESMC_TimeGetCalendar(calendar);
       }

       void FTN(c_esmc_timesetcalendarptr)(ESMC_Time *ptr,
                                        ESMC_Calendar *calendar, int *status) {
           *status = (ptr)->ESMC_TimeSetCalendar(calendar);
       }

       void FTN(c_esmc_timesetcalendarptrptr)(ESMC_Time *ptr,
                                        ESMC_Calendar **calendar, int *status) {
           *status = (ptr)->ESMC_TimeSetCalendar(*calendar);
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

       void FTN(c_esmc_timegetdayofyeardouble)(ESMC_Time *ptr,
                                               double *dayofyear, int *status) {
           *status = (ptr)->ESMC_TimeGetDayOfYear(dayofyear);
       }

       void FTN(c_esmc_timegetdayofyearinteger)(ESMC_Time *ptr,
                                                int *dayofyear, int *status) {
           *status = (ptr)->ESMC_TimeGetDayOfYear(dayofyear);
       }

       void FTN(c_esmc_timegetdayofyeartimeint)(ESMC_Time *ptr,
                                                ESMC_TimeInterval *dayofyear,
                                                int *status) {
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

       void FTN(c_esmc_timereadrestart)(ESMC_Time *ptr, ESMF_IKIND_I8 *S,
                                        int *Sn, int *Sd, ESMC_Calendar *cal,
                                        int *timeZone, int *status) {
       //    *status = (ptr)->ESMC_TimeReadRestart(*S, *Sn, *Sd, cal, *timeZone);
       }

       void FTN(c_esmc_timewriterestart)(ESMC_Time *ptr, ESMF_IKIND_I8 *S,
                                         int *Sn, int *Sd, ESMC_Calendar *cal,
                                         int *timeZone, int *status) {
        //   *status = (ptr)->ESMC_TimeWriteRestart(S, Sn, Sd, cal, timeZone);
       }

       void FTN(c_esmc_timevalidate)(ESMC_Time *ptr, const char *opts,
                                     int *status) {
           *status = (ptr)->ESMC_TimeValidate(opts);
       }

       void FTN(c_esmc_timeprint)(ESMC_Time *ptr, const char *opts,
                                  int *status) {
           *status = (ptr)->ESMC_TimePrint(opts);
       }
};
