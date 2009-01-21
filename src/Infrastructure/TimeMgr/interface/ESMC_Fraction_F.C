// $Id: ESMC_Fraction_F.C,v 1.19.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMCI_F90Interface.h>
#include <ESMC_Fraction.h>
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Fraction} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

// TODO: remove this once this file contains real code.  otherwise ranlib
// moans about no visible symbols.  sigh.
static int fred;


// the interface subroutine names MUST be in lower case
extern "C" {

#if 0
       void FTN(c_esmc_fractionset)(ESMC_Fraction *ptr, int *arg1, int *arg2,
                                                  int *arg3, int *status) {
          int rc = (ptr)->ESMC_FractionSet(*arg1, *arg2, *arg3);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionget)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->ESMC_FractionGet(&value);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionset)(ESMC_Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->ESMC_FractionSet(value);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionreadrestart)(ESMC_Fraction *ptr, int *nameLen,
                                            const char *name,
                                            ESMC_IOSpec *iospec,
                                            int *status) {
          int rc = (ptr)->ESMC_FractionReadRestart(
                                                   *nameLen,  // always present
                                                              //   internal
                                                              //   argument.
                                                    name,     // required.
                            ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionwriterestart)(ESMC_Fraction *ptr,
                                             ESMC_IOSpec *iospec,
                                             int *status) {
          int rc = (ptr)->ESMC_FractionWriteRestart(
                            ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionvalidate)(ESMC_Fraction *ptr,
                                         const char *options,
                                         int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_Validate(
                               ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionprint)(ESMC_Fraction *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->ESMC_Fraction::ESMC_Print(
                            ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
#endif
};
