// $Id: ESMC_Start.h,v 1.1 2004/04/23 17:31:05 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// first file to be included in all other files.


// common macros and constants
#include "ESMC_Macros.h"

// shared macros between fortran and C++.  this must come first before
// the system dependent file below.
#include "ESMF_Macros.inc"

// system dependent #defines from build process
#include "ESMC_Conf.h"


