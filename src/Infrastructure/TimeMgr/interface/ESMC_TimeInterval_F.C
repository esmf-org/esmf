// $Id: ESMC_TimeInterval_F.C,v 1.18 2003/07/25 19:58:26 eschwab Exp $
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

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval *ptr,
                                 int *YY, ESMF_IKIND_I8 *YYl,
                                 int *MO, ESMF_IKIND_I8 *MOl,
                                 int *D, ESMF_IKIND_I8 *Dl,
                                 int *H, int *M,
                                 int *S, ESMF_IKIND_I8 *Sl,
                                 int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_TimeIntervalGet(YY, YYl, MO, MOl, D, Dl, 
                                                 H, M, S, Sl, MS, US, NS,
                                                 d_, h_, m_, s_, ms_, us_, ns_,
                                                 Sn, Sd);
       }

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval *ptr,
                                 int *YY, ESMF_IKIND_I8 *YYl,
                                 int *MO, ESMF_IKIND_I8 *MOl,
                                 int *D, ESMF_IKIND_I8 *Dl,
                                 int *H, int *M,
                                 int *S, ESMF_IKIND_I8 *Sl,
                                 int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_TimeIntervalSet(YY, YYl, MO, MOl, D, Dl,
                                                 H, M, S, Sl, MS, US, NS,
                                                 d_, h_, m_, s_, ms_, us_, ns_,
                                                 Sn, Sd);
       }

       void FTN(c_esmc_timeintervalgetstring)(ESMC_TimeInterval *ptr,
                                              char *timeString, int *status) {
           *status = (ptr)->ESMC_TimeIntervalGetString(timeString);
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
                                          double *timeIntervalRQuot) {
           *timeIntervalRQuot = (*timeInterval1 / *timeInterval2);
       }

       void FTN(c_esmc_timeintervalremainder)(ESMC_TimeInterval *timeInterval1,
                                              ESMC_TimeInterval *timeInterval2,
                                    ESMC_TimeInterval *timeIntervalRemainder) {
           *timeIntervalRemainder = (*timeInterval1 % *timeInterval2);
       }

       void FTN(c_esmc_timeintervalquoti)(ESMC_TimeInterval *timeInterval,
                                        int *divisor,
                                        ESMC_TimeInterval *timeIntervalQuotI) {
           *timeIntervalQuotI = (*timeInterval / *divisor);
       }

       void FTN(c_esmc_timeintervalquotr)(ESMC_TimeInterval *timeInterval,
                                        double *divisor,
                                        ESMC_TimeInterval *timeIntervalQuotR) {
           *timeIntervalQuotR = (*timeInterval / *divisor);
       }

       void FTN(c_esmc_timeintervalprodi)(ESMC_TimeInterval *timeInterval,
                                        int *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdI) {
           *timeIntervalProdI = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodf)(ESMC_TimeInterval *timeInterval,
                                        ESMC_Fraction *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdF) {
           *timeIntervalProdF = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalprodr)(ESMC_TimeInterval *timeInterval,
                                        double *multiplier,
                                        ESMC_TimeInterval *timeIntervalProdR) {
           *timeIntervalProdR = (*timeInterval * *multiplier);
       }

       void FTN(c_esmc_timeintervalreadrestart)(ESMC_TimeInterval *ptr,
                                           ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                                           ESMF_IKIND_I8 *YY, ESMF_IKIND_I8 *MO,
                                           ESMF_IKIND_I8 *D, int *status) {
           *status = (ptr)->ESMC_TimeIntervalReadRestart(*S,  *Sn, *Sd,
                                                         *YY, *MO, *D);
       }

       void FTN(c_esmc_timeintervalwriterestart)(ESMC_TimeInterval *ptr,
                                           ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                                           ESMF_IKIND_I8 *YY, ESMF_IKIND_I8 *MO,
                                           ESMF_IKIND_I8 *D, int *status) {
           *status = (ptr)->ESMC_TimeIntervalWriteRestart(S, Sn, Sd, YY, MO, D);
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval *ptr,
                                             const char *opts, int *status) {
           *status = (ptr)->ESMC_TimeIntervalValidate(opts);
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval *ptr,
                                          const char *opts, int *status) {
           *status = (ptr)->ESMC_TimeIntervalPrint(opts);
       }
};
