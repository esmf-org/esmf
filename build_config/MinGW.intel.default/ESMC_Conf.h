#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.4 2009/01/22 06:04:00 w6ws Exp $"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_mingw

#define FTN(func) func

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 36 
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if Sx86_64_32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 36
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if defined (Sx86_64_small) || defined (S64)
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 72
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif
#if Sx86_64_medium
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 144
#define ESMF_F90_PTR_PLUS_RANK 48
#define ESMC_POINTER_SIZE 8
#endif

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// On Windows, 'long' is always 32 bits.  So conditional code to
// select between 'int' and 'long long' is needed.
#if defined (S32)
typedef int ESMCI_FortranStrLenArg;
#elif defined (Sx86_64_32)
typedef int ESMCI_FortranStrLenArg;
#elif defined (Sx86_64_small) || defined (S64)
typedef long long ESMCI_FortranStrLenArg;
#elif defined (Sx86_64_medium)
typedef long long ESMCI_FortranStrLenArg;
#else
#error Can't typedef ESMCI_FortranStrLenArg
#endif

#endif

#endif
