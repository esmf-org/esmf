// $Id: ESMC_BaseTime_F.C,v 1.10 2003/04/27 19:30:30 eschwab Exp $
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

       void FTN(c_esmc_basetimeget)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *s,
                                    int *sn, int *sd, int *status) {
           *status = (ptr)->ESMC_BaseTimeGet(s, sn, sd);
       }

       void FTN(c_esmc_basetimeset)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *s,
                                    int *sn, int *sd, int *status) {
           *status = (ptr)->ESMC_BaseTimeSet(*s, *sn, *sd);
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
           *status = (ptr)->ESMC_BaseTimeRead(*S, *Sn, *Sd);
       }

       void FTN(c_esmc_basetimewrite)(ESMC_BaseTime *ptr, ESMF_IKIND_I8 *S,
                                      int *Sn, int *Sd, int *status) {
           *status = (ptr)->ESMC_BaseTimeWrite(S, Sn, Sd);
       }

       void FTN(c_esmc_basetimevalidate)(ESMC_BaseTime *ptr, const char *opts,
                                         int *status) {
           *status = (ptr)->ESMC_BaseTimeValidate(opts);
       }

       void FTN(c_esmc_basetimeprint)(ESMC_BaseTime *ptr, const char *opts,
                                      int *status) {
           *status = (ptr)->ESMC_BaseTimePrint(opts);
       }
};
