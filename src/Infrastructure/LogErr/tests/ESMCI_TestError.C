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
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C++ methods used to verify that strings
// can be passed correctly between F90 and C++.
//
//-------------------------------------------------------------------------
//
#define ESMC_FILENAME "ESMCI_TestError.C"

#include <stdio.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"

extern "C" {

#undef  ESMC_METHOD
#define ESMC_METHOD "c_error()"
  void FTN_X(c_error)(int *rc){
    *rc = ESMC_LogDefault.Write("C Error", ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
  }

}
