// $Id: ESMC_TimeInterval_F.C,v 1.31 2004/04/20 23:22:35 eschwab Exp $
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

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval *ptr,
                                 ESMF_KIND_I4 *yy, ESMF_KIND_I8 *yy_i8,
                                 ESMF_KIND_I4 *mm, ESMF_KIND_I8 *mm_i8,
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
                                 ESMC_Time *startTime, ESMC_Time *endTime,
                                 ESMC_Calendar **calendar,
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
                       ESMC_NOT_PRESENT_FILTER(startTime),
                       ESMC_NOT_PRESENT_FILTER(endTime),
                       ESMC_NOT_PRESENT_FILTER(calendar),
                       ESMC_NOT_PRESENT_FILTER(calendarType) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval *ptr,
                                 ESMF_KIND_I4 *yy, ESMF_KIND_I8 *yy_i8,
                                 ESMF_KIND_I4 *mm, ESMF_KIND_I8 *mm_i8,
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
                                 ESMC_Time *startTime, ESMC_Time *endTime,
                                 ESMC_Calendar **calendar, 
                                 ESMC_CalendarType *calendarType, 
                                 ESMC_Time *startTimeIn, ESMC_Time *endTimeIn,
                                 ESMC_Calendar **calendarIn, 
                                 ESMC_CalendarType *calendarTypeIn, 
                                 char *timeString, int *status) {
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
                       ESMC_NOT_PRESENT_FILTER(endTime),
                       ESMC_NOT_PRESENT_FILTER(calendar), 
                       ESMC_NOT_PRESENT_FILTER(calendarType), 
                       ESMC_NOT_PRESENT_FILTER(startTimeIn),
                       ESMC_NOT_PRESENT_FILTER(endTimeIn),
                       ESMC_NOT_PRESENT_FILTER(calendarIn),
                       ESMC_NOT_PRESENT_FILTER(calendarTypeIn),
                       ESMC_NOT_PRESENT_FILTER(timeString) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_timeintervalabsvalue)(ESMC_TimeInterval *ptr,
                                    ESMC_TimeInterval *timeIntervalAbsValue) {
           *timeIntervalAbsValue = (ptr)->ESMC_TimeIntervalAbsValue();
       }

       void FTN(c_esmc_timeintervalnegabsvalue)(ESMC_TimeInterval *ptr,
                                  ESMC_TimeInterval *timeIntervalNegAbsValue) {
           *timeIntervalNegAbsValue = (ptr)->ESMC_TimeIntervalNegAbsValue();
       }

       void FTN(c_esmc_timeintervalrquot)(ESMC_TimeInterval *timeInterval1,
                                          ESMC_TimeInterval *timeInterval2,
                                          ESMF_KIND_R8 *timeIntervalRQuot) {
           *timeIntervalRQuot = (*timeInterval1 / *timeInterval2);
       }

       void FTN(c_esmc_timeintervalquoti)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_I4 *divisor,
                                        ESMC_TimeInterval *timeIntervalQuotI) {
           *timeIntervalQuotI = (*timeInterval / *divisor);
       }

       void FTN(c_esmc_timeintervalquotr)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_R8 *divisor,
                                        ESMC_TimeInterval *timeIntervalQuotR) {
           *timeIntervalQuotR = (*timeInterval / *divisor);
       }

       void FTN(c_esmc_timeintervalfquot)(ESMC_TimeInterval *timeInterval1,
                                          ESMC_TimeInterval *timeInterval2,
                                          ESMC_Fraction *timeIntervalFQuot) {
           *timeIntervalFQuot = 
                        timeInterval1->ESMC_TimeIntervalDiv(*timeInterval2);
       }

       void FTN(c_esmc_timeintervalremainder)(ESMC_TimeInterval *timeInterval1,
                                              ESMC_TimeInterval *timeInterval2,
                                    ESMC_TimeInterval *timeIntervalRemainder) {
           *timeIntervalRemainder = (*timeInterval1 % *timeInterval2);
       }

       void FTN(c_esmc_timeintervalprodti)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_I4 *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdTI) {
           *timeIntervalProdTI = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodit)(ESMF_KIND_I4 *multiplier,
                                        ESMC_TimeInterval *timeInterval,
                                        ESMC_TimeInterval *timeIntervalProdIT) {
           *timeIntervalProdIT = (*multiplier * *timeInterval);
       }

       void FTN(c_esmc_timeintervalprodtf)(ESMC_TimeInterval *timeInterval,
                                        ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdTF) {
           *timeIntervalProdTF = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodft)(ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeInterval,
                                        ESMC_TimeInterval *timeIntervalProdFT) {
           *timeIntervalProdFT = (*multiplier * *timeInterval);
       }

       void FTN(c_esmc_timeintervalprodtr)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_R8 *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdTR) {
           *timeIntervalProdTR = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodrt)(ESMF_KIND_R8 *multiplier,
                                        ESMC_TimeInterval *timeInterval,
                                        ESMC_TimeInterval *timeIntervalProdRT) {
           *timeIntervalProdRT = (*multiplier * *timeInterval);
       }

       void FTN(c_esmc_timeintervalsum)(ESMC_TimeInterval *timeInterval1,
                                 ESMC_TimeInterval *timeInterval2,
                                 ESMC_TimeInterval *esmf_timeIntervalSum) {
           *esmf_timeIntervalSum = (*timeInterval1 + *timeInterval2);
       }

       void FTN(c_esmc_timeintervaldiff)(ESMC_TimeInterval *timeInterval1,
                                 ESMC_TimeInterval *timeInterval2,
                                 ESMC_TimeInterval *esmf_timeIntervalDiff) {
           *esmf_timeIntervalDiff = (*timeInterval1 - *timeInterval2);
       }


       //
       // overloaded comparison operators
       //

       void FTN(c_esmc_timeintervaleq)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalEQ) {
           *esmf_timeIntervalEQ = (int) (*timeInterval1 == *timeInterval2);
       }

       void FTN(c_esmc_timeintervalne)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalNE) {
           *esmf_timeIntervalNE = (int) (*timeInterval1 != *timeInterval2);
       }

       void FTN(c_esmc_timeintervallt)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalLT) {
           *esmf_timeIntervalLT = (int) (*timeInterval1 < *timeInterval2);
       }

       void FTN(c_esmc_timeintervalgt)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalGT) {
           *esmf_timeIntervalGT = (int) (*timeInterval1 > *timeInterval2);
       }

       void FTN(c_esmc_timeintervalle)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalLE) {
           *esmf_timeIntervalLE = (int) (*timeInterval1 <= *timeInterval2);
       }

       void FTN(c_esmc_timeintervalge)(ESMC_TimeInterval *timeInterval1,
                                       ESMC_TimeInterval *timeInterval2,
                                       int *esmf_timeIntervalGE) {
           *esmf_timeIntervalGE = (int) (*timeInterval1 >= *timeInterval2);
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
