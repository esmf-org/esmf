#if 0
$Id: ESMF.h,v 1.6.2.2 2009/01/21 21:25:24 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.

Central F90 include file which includes other files.
#endif

#if 0
Most of the previous contents of this file have been moved into the
base class fortran module. User code gets access to parameter definitions,
derived types, and interfaces by using the ESMF_Mod module in their code.
Macros useful to the framework, or system-dependent compile time defines
can go here.  THIS FILE CANNOT CONTAIN ANYTHING THAT IS REQUIRED BY USER 
CODE BECAUSE WE AGREED TO NOT FORCE F90 USER SOURCE TO BE PREPROCESSED.  

All ESMF source code does get preprocessed and user code has the option.  
So this should only include optional things which are of benefit to ESMF
source or are value-add to user-code but not critical.  

No ESMF examples should include ESMF.h, but ESMF unit and systems test
do need to include ESMF.h so they can use the test macros for
uniform error messages and exit codes.
#endif

#ifndef ESMF_H
#define ESMF_H

#include "ESMF_Macros.inc"
#define _FROM_FORTRAN 1
#include "ESMC_Conf.h"
#include "ESMF_Conf.inc"
#include "ESMF_LogConstants.inc"
#include "ESMF_LogMacros.inc"
#include "ESMF_ErrReturnCodes.inc"
#include "ESMF_InitMacros.inc"
#endif
