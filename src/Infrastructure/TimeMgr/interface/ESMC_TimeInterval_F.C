// $Id: ESMC_TimeInterval_F.C,v 1.21 2003/09/12 01:58:03 eschwab Exp $
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
                                 ESMF_KIND_I4 *mo, ESMF_KIND_I8 *mo_i8,
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
                                 int *status) {

          int rc = (ptr)->ESMC_TimeIntervalSet(
                    ((void*) yy    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : yy),
                    ((void*) yy_i8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : yy_i8),
                    ((void*) mo    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : mo),
                    ((void*) mo_i8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : mo_i8),
                    ((void*) d     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d),
                    ((void*) d_i8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d_i8),
                    ((void*) h     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : h),
                    ((void*) m     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : m),
                    ((void*) s     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s),
                    ((void*) s_i8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s_i8),
                    ((void*) ms    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ms),
                    ((void*) us    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : us),
                    ((void*) ns    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ns),
                    ((void*) d_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d_r8),
                    ((void*) h_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : h_r8),
                    ((void*) m_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : m_r8),
                    ((void*) s_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s_r8),
                    ((void*) ms_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ms_r8),
                    ((void*) us_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : us_r8),
                    ((void*) ns_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ns_r8),
                    ((void*) sN    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : sN),
                    ((void*) sD    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : sD) );

          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval *ptr,
                                 ESMF_KIND_I4 *yy, ESMF_KIND_I8 *yy_i8,
                                 ESMF_KIND_I4 *mo, ESMF_KIND_I8 *mo_i8,
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
                                 char *timeString, int *status) {

          int rc = (ptr)->ESMC_TimeIntervalGet(
                    ((void*) yy    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : yy),
                    ((void*) yy_i8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : yy_i8),
                    ((void*) mo    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : mo),
                    ((void*) mo_i8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : mo_i8),
                    ((void*) d     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d),
                    ((void*) d_i8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d_i8),
                    ((void*) h     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : h),
                    ((void*) m     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : m),
                    ((void*) s     == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s),
                    ((void*) s_i8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s_i8),
                    ((void*) ms    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ms),
                    ((void*) us    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : us),
                    ((void*) ns    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ns),
                    ((void*) d_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : d_r8),
                    ((void*) h_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : h_r8),
                    ((void*) m_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : m_r8),
                    ((void*) s_r8  == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : s_r8),
                    ((void*) ms_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ms_r8),
                    ((void*) us_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : us_r8),
                    ((void*) ns_r8 == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : ns_r8),
                    ((void*) sN    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : sN),
                    ((void*) sD    == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : sD),
                    ((void*) timeString == (void*)ESMC_BAD_POINTER ?
                                             ESMC_NULL_POINTER : timeString) );

          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;

       }

       void FTN(c_esmc_timeintervalabsvalue)(ESMC_TimeInterval *ptr,
                                    ESMC_TimeInterval *timeIntervalAbsValue) {
           *timeIntervalAbsValue = (ptr)->ESMC_TimeIntervalAbsValue();
       }

       void FTN(c_esmc_timeintervalnegabsvalue)(ESMC_TimeInterval *ptr,
                                  ESMC_TimeInterval *timeIntervalNegAbsValue) {
           *timeIntervalNegAbsValue = (ptr)->ESMC_TimeIntervalNegAbsValue();
       }

       void FTN(c_esmc_timeintervalfquot)(ESMC_TimeInterval *timeInterval1,
                                          ESMC_TimeInterval *timeInterval2,
                                          ESMC_Fraction *timeIntervalFQuot) {
           *timeIntervalFQuot = 
                        timeInterval1->ESMC_TimeIntervalDiv(*timeInterval2);
       }

       void FTN(c_esmc_timeintervalrquot)(ESMC_TimeInterval *timeInterval1,
                                          ESMC_TimeInterval *timeInterval2,
                                          ESMF_KIND_R8 *timeIntervalRQuot) {
           *timeIntervalRQuot = (*timeInterval1 / *timeInterval2);
       }

       void FTN(c_esmc_timeintervalremainder)(ESMC_TimeInterval *timeInterval1,
                                              ESMC_TimeInterval *timeInterval2,
                                    ESMC_TimeInterval *timeIntervalRemainder) {
           *timeIntervalRemainder = (*timeInterval1 % *timeInterval2);
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

       void FTN(c_esmc_timeintervalprodi)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_I4 *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdI) {
           *timeIntervalProdI = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodf)(ESMC_TimeInterval *timeInterval,
                                        ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdF) {
           *timeIntervalProdF = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodr)(ESMC_TimeInterval *timeInterval,
                                        ESMF_KIND_R8 *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdR) {
           *timeIntervalProdR = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalreadrestart)(ESMC_TimeInterval *ptr,
                                           ESMF_KIND_I8 *s,
                                           ESMF_KIND_I4 *sN, ESMF_KIND_I4 *sD,
                                           ESMF_KIND_I8 *yy, ESMF_KIND_I8 *mo,
                                           ESMF_KIND_I8 *d, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalReadRestart(*s, *sN, *sD,
                                                       *yy, *mo, *d);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalwriterestart)(ESMC_TimeInterval *ptr,
                                           ESMF_KIND_I8 *s, 
                                           ESMF_KIND_I4 *sN, ESMF_KIND_I4 *sD,
                                           ESMF_KIND_I8 *yy, ESMF_KIND_I8 *mo,
                                           ESMF_KIND_I8 *d, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalWriteRestart(s, sN, sD, yy, mo, d);
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval *ptr,
                                             const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalValidate(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval *ptr,
                                          const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalPrint(
                     ((void*) options == (void*)ESMC_BAD_POINTER ?
                                                ESMC_NULL_POINTER : options) );
          if (status != ESMC_NULL_POINTER &&
              (void*)status != (void*)ESMC_BAD_POINTER) *status = rc;
       }
};
