// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
#include "ESMCI_BaseTime.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_BaseTime} class functions.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       //
       // overloaded comparison operators
       //

       void FTN_X(c_esmc_basetimeeq)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeEQ) {
           *esmf_baseTimeEQ = (int) (*baseTime1 == *baseTime2);
       }

       void FTN_X(c_esmc_basetimene)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeNE) {
           *esmf_baseTimeNE = (int) (*baseTime1 != *baseTime2);
       }

       void FTN_X(c_esmc_basetimelt)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeLT) {
           *esmf_baseTimeLT = (int) (*baseTime1 < *baseTime2);
       }

       void FTN_X(c_esmc_basetimegt)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeGT) {
           *esmf_baseTimeGT = (int) (*baseTime1 > *baseTime2);
       }

       void FTN_X(c_esmc_basetimele)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeLE) {
           *esmf_baseTimeLE = (int) (*baseTime1 <= *baseTime2);
       }

       void FTN_X(c_esmc_basetimege)(BaseTime *baseTime1,
                                   BaseTime *baseTime2,
                                   int *esmf_baseTimeGE) {
           *esmf_baseTimeGE = (int) (*baseTime1 >= *baseTime2);
       }

       //
       // overloaded increment/decrement
       //

       void FTN_X(c_esmc_basetimesum)(BaseTime *baseTime1,
                                    BaseTime *baseTime2,
                                    BaseTime *esmf_baseTimeSum) {
           *esmf_baseTimeSum = (*baseTime1 + *baseTime2);
       }

       void FTN_X(c_esmc_basetimediff)(BaseTime *baseTime1,
                                     BaseTime *baseTime2,
                                     BaseTime *esmf_baseTimeDiff) {
           *esmf_baseTimeDiff = (*baseTime1 - *baseTime2);
       }
};

}  // namespace ESMCI
