#if 0
$Id: ESMC_LogErr.inc,v 1.6 2004/05/17 19:25:40 eschwab Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.


ESMF Error include file of constants for both FORTRAN or C++.
Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.


#endif

#include "ESMF_ErrReturnCodes.inc"
#include "ESMF_LogConstants.inc"

#ifndef PARCH_IRIX64

#define ESMC_LogWrite(...) \
ESMC_LogWrite(__LINE__,__FILE__,__VA_ARGS__)

#define ESMC_LogFoundError(rc,...) \
ESMC_LogFoundError(rc,__LINE__,__FILE__,__VA_ARGS__)

#endif

