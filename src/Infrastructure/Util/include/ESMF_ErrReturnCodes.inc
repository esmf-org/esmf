#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2016, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.


ESMF Error include file of constants for both FORTRAN or C++.
Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.

#endif

#ifndef ESMF_ERRRETURNCODES_INC
#define ESMF_ERRRETURNCODES_INC

#if 0
Special macro for use in ESM[F,C]_LogMsgFoundError(), e.g.
  ESMF_LogFoundError(rcToCheck, ESMF_ERR_PASSTHRU. rcToReturn)
#endif
#define ESMF_ERR_PASSTHRU msg="Internal subroutine call returned Error"
#define ESMCI_ERR_PASSTHRU    "Internal subroutine call returned Error"

#if 0
Return codes are defined here; corresponding error messages are defined in
ESMC_LogErrMsgs.C
#endif

#if 0
These are here for convenient reference and possible future definition;
they are currently defined in ESMF_UtilTypes.F90 and ESMC_Macros.h
//BOP
\begin{verbatim}
=====================================================
Success/Failure Return codes for both Fortran and C++
=====================================================

#define  ESMF_SUCCESS             0
#define  ESMF_FAILURE            -1
#endif

#include "ESMC_ReturnCodes.h"

#if 0
\end{verbatim}
//EOP
#endif

#endif
