// $Id: ESMCI_Fraction_F.C,v 1.2.4.1 2010/02/05 20:01:04 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include <ESMCI_Fraction.h>
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

namespace ESMCI{

// TODO: remove this once this file contains real code.  otherwise ranlib
// moans about no visible symbols.  sigh.
static int fred;


// the interface subroutine names MUST be in lower case
extern "C" {

#if 0
       void FTN(c_esmc_fractionset)(Fraction *ptr, int *arg1, int *arg2,
                                             int *arg3, int *status) {
          int rc = (ptr)->set(*arg1, *arg2, *arg3);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionget)(Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->get(&value);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionset)(Fraction *ptr, 
                                         <value> *value, int *status} {
          int rc = (ptr)->set(value);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionreadrestart)(Fraction *ptr, int *nameLen,
                                            const char *name,
                                            ESMC_IOSpec *iospec,
                                            int *status) {
          int rc = (ptr)->readRestart(*nameLen,  // always present
                                                 //   internal
                                                 //   argument.
                                       name,     // required.
                            ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionwriterestart)(Fraction *ptr,
                                             ESMC_IOSpec *iospec,
                                             int *status) {
          int rc = (ptr)->writeRestart(
                            ESMC_NOT_PRESENT_FILTER(iospec) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionvalidate)(Fraction *ptr,
                                         const char *options,
                                         int *status) {
          int rc = (ptr)->validate(ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

       void FTN(c_esmc_fractionprint)(Fraction *ptr, const char *options,
                                      int *status) {
          int rc = (ptr)->print(ESMC_NOT_PRESENT_FILTER(options) );
          if (ESMC_PRESENT(status)) *status = rc;
       }
#endif
};

}  // namespace ESMCI
