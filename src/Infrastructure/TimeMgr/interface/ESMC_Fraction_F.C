// $Id: ESMC_Fraction_F.C,v 1.9 2003/08/29 05:31:58 eschwab Exp $
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
#include "ESMC_Fraction.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Fraction} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

#if 0
       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_fractionset)(ESMC_Fraction *ptr, int *arg1, int *arg2,
                                                  int *arg3, int *status) {
          int rc = (ptr)->ESMC_FractionSet(*arg1, *arg2, *arg3);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionget)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->ESMC_FractionGet(&value);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionset)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->ESMC_FractionSet(value);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionreadrestart)(ESMC_Fraction *ptr, int *arg1,
                                            int *arg2, int *arg3, int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_ReadRestart(*arg1, *arg2, *arg3);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionwriterestart)(ESMC_Fraction *ptr, int *arg1,
                                            int *arg2, int *arg3, int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_WriteRestart(arg1, arg2, arg3);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionvalidate)(ESMC_Fraction *ptr,
                                         const char *options,
                                         int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_Validate(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }

       void FTN(c_esmc_fractionprint)(ESMC_Fraction *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_Print(options);
          if (status != ESMC_NULL_POINTER) *status = rc;
       }
#endif
};
