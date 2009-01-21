// $Id: ESMC_BaseTime_F.C,v 1.16.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
#include <ESMC_BaseTime.h>
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_BaseTime} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

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
};
