// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// first file to be included in all other files.


// common macros and constants
#include "ESMC_Macros.h"

// shared macros between fortran and C++.  this must come first before
// the system dependent file below.  also shared error messages.
#include "ESMF_Macros.inc"
#include "ESMF_ErrReturnCodes.inc"

// system dependent #defines from build process
// first those which apply uniformly to F90 and C++, then those
// specifically needed for C++ code.
#include "ESMF_Conf.inc"
#include "ESMC_Conf.h"


