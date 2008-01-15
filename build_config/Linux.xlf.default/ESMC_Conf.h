#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.9 2008/01/15 20:54:31 w6ws Exp $"
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

#define FTN(func) func

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
typedef int ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 32
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 56
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif

#if 0
Special defines necessary on this platform:
#endif

#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 
#endif
#if !defined(_XOPEN_SOURCE_EXTENDED)
#define _XOPEN_SOURCE_EXTENDED 1
#endif
#define _ALL_SOURCE   

#endif
