// $Id: ESMC_TimeInterval_F.C,v 1.20 2003/09/04 18:57:56 cdeluca Exp $
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
//  functions.
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
          int rc = (ptr)->ESMC_TimeIntervalSet(yy, yy_i8, mo, mo_i8, d, d_i8,
                                               h, m, s, s_i8, ms, us, ns,
                                               d_r8, h_r8, m_r8, s_r8,
                                               ms_r8, us_r8, ns_r8, sN, sD);
          if (status != ESMC_NULL_POINTER) *status = rc;
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
          int rc = (ptr)->ESMC_TimeIntervalGet(yy, yy_i8, mo, mo_i8, d, d_i8, 
                                               h, m, s, s_i8, ms, us, ns,
                                               d_r8, h_r8, m_r8, s_r8,
                                               ms_r8, us_r8, ns_r8, sN, sD,
                                               timeString);
          if (status != ESMC_NULL_POINTER) *status = rc;
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
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalwriterestart)(ESMC_TimeInterval *ptr,
                                           ESMF_KIND_I8 *s, 
                                           ESMF_KIND_I4 *sN, ESMF_KIND_I4 *sD,
                                           ESMF_KIND_I8 *yy, ESMF_KIND_I8 *mo,
                                           ESMF_KIND_I8 *d, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalWriteRestart(s, sN, sD, yy, mo, d);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval *ptr,
                                             const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalValidate(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval *ptr,
                                          const char *options, int *status) {
          int rc = (ptr)->ESMC_TimeIntervalPrint(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }
};
