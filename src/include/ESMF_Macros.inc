#if 0
$Id: ESMF_Macros.inc,v 1.4 2003/02/21 21:06:15 jwolfe Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.

These lines prevent this file from being read more than once if it
ends up being included multiple times.
#endif

#ifndef ESMF_MACROS_INC
#define ESMF_MACROS_INC

#if 0
!BOP
-------------------------------------------------------------------------

 !DESCRIPTION:

 Utility macros and constants usable ESMF-wide in both F90 and C++

 (all lines below between the !BOP and !EOP markers will be included in
  the automated document processing.)

-------------------------------------------------------------------------
!EOP
#endif

#define ESMF_SUCCESS  0
#define ESMF_FAILURE -1

#define ESMF_MAXSTR 128
#define ESMF_MAXDIM 7
#define ESMF_MAXDECOMPDIM 2
#define ESMF_MAXGRIDDIM 2

#endif 
