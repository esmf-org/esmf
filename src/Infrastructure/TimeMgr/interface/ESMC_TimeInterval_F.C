// $Id: ESMC_TimeInterval_F.C,v 1.9 2003/04/15 16:47:40 eschwab Exp $
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
//  allows F90 to call C++ for supporting {\tt TimeInterval} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_timeintervalinit)(ESMC_TimeInterval *ptr,
                                 int *YY, int *MO, int *D, int *H, int *M,
                                 ESMF_IKIND_I8 *S, int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd,
                                 ESMC_Calendar *cal, int *status) {
           *status = (ptr)->ESMC_TimeIntervalInit(YY, MO, D, H, M, S, MS,
                                                  US, NS, d_, h_, m_, s_, ms_,
                                                  us_, ns_, Sn, Sd, cal);
       }

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval *ptr,
                                 int *YY, int *MO, int *D, int *H, int *M,
                                 ESMF_IKIND_I8 *S, int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd,
                                 int *status) {
           *status = (ptr)->ESMC_TimeIntervalGet(YY, MO, D, H, M, S, MS,
                                                 US, NS, d_, h_, m_, s_, ms_,
                                                 us_, ns_, Sn, Sd);
       }

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval *ptr,
                                 int *YY, int *MO, int *D, int *H, int *M,
                                 ESMF_IKIND_I8 *S, int *MS, int *US, int *NS,
                                 double *d_, double *h_, double *m_,
                                 double *s_, double *ms_, double *us_,
                                 double *ns_, int *Sn, int *Sd,
                                 int *status) {
           *status = (ptr)->ESMC_TimeIntervalSet(YY, MO, D, H, M, S, MS,
                                                 US, NS, d_, h_, m_, s_, ms_,
                                                 us_, ns_, Sn, Sd);
       }

       void FTN(c_esmc_timeintervalread)(ESMC_TimeInterval *ptr,
                                         ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                                         ESMC_Calendar *cal, int *status) {
           *status = (ptr)->ESMC_TimeIntervalRead(*S, *Sn, *Sd, cal);   
       }

       void FTN(c_esmc_timeintervalwrite)(ESMC_TimeInterval *ptr,
                                          ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                                          ESMC_Calendar *cal, int *status) {
           *status = (ptr)->ESMC_TimeIntervalWrite(S, Sn, Sd, cal);
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
