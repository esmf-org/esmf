#if 0
$Id: ESMF_Version.inc,v 1.1 2003/03/04 19:51:22 nscollins Exp $

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

#ifndef ESMF_VERSION_INC
#define ESMF_VERSION_INC

#if 0
!BOP
!-------------------------------------------------------------------------
!
!DESCRIPTION:
!
! Version numbers usable ESMF-wide in both F90 and C++
!
!-------------------------------------------------------------------------
!EOP
#endif

#define ESMF_MAJOR_VERSION 0
#define ESMF_MINOR_VERSION 0
#define ESMF_REVISION      1

#define ESMF_VERSION_STRING "0.0.1"

#endif 
