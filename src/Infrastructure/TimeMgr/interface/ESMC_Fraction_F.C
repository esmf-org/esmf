// $Id: ESMC_Fraction_F.C,v 1.7 2003/06/07 00:41:59 eschwab Exp $
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
           *status = (ptr)->ESMC_FractionSet(*arg1, *arg2, *arg3);
       }

       void FTN(c_esmc_fractionget)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_FractionGet(&value);
       }

       void FTN(c_esmc_fractionset)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
           *status = (ptr)->ESMC_FractionSet(value);
       }

       void FTN(c_esmc_fractionread)(ESMC_Fraction *ptr, int *arg1, int *arg2,
                                     int *arg3, int *status) {
           *status = (ptr)->ESMC_Fraction::ESMC_Read(*arg1, *arg2, *arg3);
       }

       void FTN(c_esmc_fractionwrite)(ESMC_Fraction *ptr, int *arg1, int *arg2,
                                      int *arg3, int *status) {
           *status = (ptr)->ESMC_Fraction::ESMC_Write(arg1, arg2, arg3);
       }

       void FTN(c_esmc_fractionvalidate)(ESMC_Fraction *ptr, const char *opts,
                                         int *status) {
           *status = (ptr)->ESMC_Fraction::ESMC_Validate(opts);
       }

       void FTN(c_esmc_fractionprint)(ESMC_Fraction *ptr, const char *opts,
                                      int *status) {
           *status = (ptr)->ESMC_Fraction::ESMC_Print(opts);
       }
#endif
};
