// $Id: ESMC_Start.h,v 1.5 2004/05/20 11:41:03 nscollins Exp $
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
#include "ESMC_F90Interface.h"

// shared macros between fortran and C++.  this must come first before
// the system dependent file below.  also shared error messages.
#include "ESMF_Macros.inc"
#include "ESMF_ErrReturnCodes.inc"

// if we decide users should get the logerr macros by default, include
// this here.  otherwise users can include it or not as they choose.
// #include "ESMF_LogConstants.inc"
// #include "ESMF_LogMacros.inc"

// system dependent #defines from build process
#include "ESMC_Conf.h"


