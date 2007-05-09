#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.11 2007/05/09 21:22:52 theurich Exp $"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2007, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_linux

#ifdef ESMF_LOWERCASE_SINGLEUNDERSCORE 
#define FTN(func) func##_
#endif
#ifdef ESMF_LOWERCASE_DOUBLEUNDERSCORE 
#define FTN(func) func##__
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 28
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if Sia64_32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 28
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if Sia64_64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 40
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 8
#endif
#if Sx86_64_32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 28
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if Sx86_64_small
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 40
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 8
#endif
#if Sx86_64_medium
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 40
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 8
#endif

#endif
