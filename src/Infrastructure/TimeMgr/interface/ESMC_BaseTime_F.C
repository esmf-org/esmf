// $Id: ESMC_BaseTime_F.C,v 1.7 2003/04/02 17:24:54 eschwab Exp $
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
#include <ESMC_BaseTime.h>
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt BaseTime} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_basetimeinit)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *s,
                                     int *sn, int *sd, int *status) {
           *status = (ptr)->ESMC_BaseTimeInit(*s, *sn, *sd);
       }

       //
       // integer get/sets
       //

       void FTN(c_esmc_basetimeget_d)(ESMC_BaseTime *ptr, 
                                      int *d, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_D(d);
       }

       void FTN(c_esmc_basetimeset_d)(ESMC_BaseTime *ptr, 
                                      int *d, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_D(*d);
       }

       void FTN(c_esmc_basetimeget_h)(ESMC_BaseTime *ptr, 
                                      int *h, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_H(h);
       }

       void FTN(c_esmc_basetimeset_h)(ESMC_BaseTime *ptr, 
                                      int *h, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_H(*h);
       }

       void FTN(c_esmc_basetimeget_m)(ESMC_BaseTime *ptr, 
                                      int *m, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_M(m);
       }

       void FTN(c_esmc_basetimeset_m)(ESMC_BaseTime *ptr, 
                                      int *m, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_M(*m);
       }

       void FTN(c_esmc_basetimeget_s)(ESMC_BaseTime *ptr, 
                                      int *s, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_S(s);
       }

       void FTN(c_esmc_basetimeset_s)(ESMC_BaseTime *ptr, 
                                      int *s, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_S(*s);
       }

       void FTN(c_esmc_basetimeget_ms)(ESMC_BaseTime *ptr, 
                                      int *ms, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_MS(ms);
       }

       void FTN(c_esmc_basetimeset_ms)(ESMC_BaseTime *ptr, 
                                      int *ms, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_MS(*ms);
       }

       void FTN(c_esmc_basetimeget_us)(ESMC_BaseTime *ptr, 
                                      int *us, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_US(us);
       }

       void FTN(c_esmc_basetimeset_us)(ESMC_BaseTime *ptr, 
                                      int *us, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_US(*us);
       }

       void FTN(c_esmc_basetimeget_ns)(ESMC_BaseTime *ptr, 
                                      int *ns, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_NS(ns);
       }

       void FTN(c_esmc_basetimeset_ns)(ESMC_BaseTime *ptr, 
                                      int *ns, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_NS(*ns);
       }

       //
       // floating point get/sets
       //

       void FTN(c_esmc_basetimeget_d_f)(ESMC_BaseTime *ptr, 
                                        double *d, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_d(d);
       }

       void FTN(c_esmc_basetimeset_d_f)(ESMC_BaseTime *ptr, 
                                        double *d, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_d(*d);
       }

       void FTN(c_esmc_basetimeget_h_f)(ESMC_BaseTime *ptr, 
                                        double *h, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_h(h);
       }

       void FTN(c_esmc_basetimeset_h_f)(ESMC_BaseTime *ptr, 
                                        double *h, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_h(*h);
       }

       void FTN(c_esmc_basetimeget_m_f)(ESMC_BaseTime *ptr, 
                                        double *m, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_m(m);
       }

       void FTN(c_esmc_basetimeset_m_f)(ESMC_BaseTime *ptr, 
                                        double *m, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_m(*m);
       }

       void FTN(c_esmc_basetimeget_s_f)(ESMC_BaseTime *ptr, 
                                        double *s, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_s(s);
       }

       void FTN(c_esmc_basetimeset_s_f)(ESMC_BaseTime *ptr, 
                                        double *s, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_s(*s);
       }

       void FTN(c_esmc_basetimeget_ms_f)(ESMC_BaseTime *ptr, 
                                        double *ms, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_ms(ms);
       }

       void FTN(c_esmc_basetimeset_ms_f)(ESMC_BaseTime *ptr, 
                                        double *ms, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_ms(*ms);
       }

       void FTN(c_esmc_basetimeget_us_f)(ESMC_BaseTime *ptr, 
                                        double *us, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_us(us);
       }

       void FTN(c_esmc_basetimeset_us_f)(ESMC_BaseTime *ptr, 
                                        double *us, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_us(*us);
       }

       void FTN(c_esmc_basetimeget_ns_f)(ESMC_BaseTime *ptr, 
                                        double *ns, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet_ns(ns);
       }

       void FTN(c_esmc_basetimeset_ns_f)(ESMC_BaseTime *ptr, 
                                        double *ns, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet_ns(*ns);
       }

       //
       // overloaded comparison operators
       //

       void FTN(c_esmc_basetimeeq)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeEQ) {
           *esmf_baseTimeEQ = (int) (*baseTime1 == *baseTime2);
       }

       void FTN(c_esmc_basetimene)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeNE) {
           *esmf_baseTimeNE = (int) (*baseTime1 != *baseTime2);
       }

       void FTN(c_esmc_basetimelt)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeLT) {
           *esmf_baseTimeLT = (int) (*baseTime1 < *baseTime2);
       }

       void FTN(c_esmc_basetimegt)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeGT) {
           *esmf_baseTimeGT = (int) (*baseTime1 > *baseTime2);
       }

       void FTN(c_esmc_basetimele)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeLE) {
           *esmf_baseTimeLE = (int) (*baseTime1 <= *baseTime2);
       }

       void FTN(c_esmc_basetimege)(ESMC_BaseTime *baseTime1,
                                   ESMC_BaseTime *baseTime2,
                                   int *esmf_baseTimeGE) {
           *esmf_baseTimeGE = (int) (*baseTime1 >= *baseTime2);
       }

       //
       // overloaded increment/decrement
       //

       void FTN(c_esmc_basetimesum)(ESMC_BaseTime *baseTime1,
                                    ESMC_BaseTime *baseTime2,
                                    ESMC_BaseTime *esmf_baseTimeSum) {
           *esmf_baseTimeSum = (*baseTime1 + *baseTime2);
       }

       void FTN(c_esmc_basetimediff)(ESMC_BaseTime *baseTime1,
                                     ESMC_BaseTime *baseTime2,
                                     ESMC_BaseTime *esmf_baseTimeDiff) {
           *esmf_baseTimeDiff = (*baseTime1 - *baseTime2);
       }

       //
       // read, write, validate and print
       //

       void FTN(c_esmc_basetimeread)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *S,
                                     int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_BaseTime::ESMC_Read(*S, *Sn, *Sd);
       }

       void FTN(c_esmc_basetimewrite)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *S,
                                      int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_BaseTime::ESMC_Write(S, Sn, Sd);
       }

       void FTN(c_esmc_basetimevalidate)(ESMC_BaseTime *ptr, const char *opts,
                                         int *status) {
           *status = (ptr)->ESMC_BaseTime::ESMC_Validate(opts);
       }

       void FTN(c_esmc_basetimeprint)(ESMC_BaseTime *ptr, const char *opts,
                                      int *status) {
           *status = (ptr)->ESMC_BaseTime::ESMC_Print(opts);
       }
};
