// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C functions used to verify that
// Fortran optional argument handling works consistently with Fortran
// TS 29113.
//
//-------------------------------------------------------------------------
//
#include "ESMCI.h"

#include <cstddef>

extern "C" {

// Return ESMF_TRUE or ESMF_FALSE depending on whether the argument is
// present or not.

void FTN_X(esmc_present_test)(int *arg, ESMC_Logical *present) {
  if (arg == NULL)
    *present = ESMF_FALSE;
  else
    *present = ESMF_TRUE;
}

}
