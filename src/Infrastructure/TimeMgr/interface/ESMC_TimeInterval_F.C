// $Id: ESMC_TimeInterval_F.C,v 1.5 2003/03/28 00:45:51 eschwab Exp $
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

#if 0
       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_TimeIntervalGet(&value);
       }

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_TimeIntervalSet(value);
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_TimeIntervalValidate(opts);
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_TimeIntervalPrint(opts);
       }
#endif

};
