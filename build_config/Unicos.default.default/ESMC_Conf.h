#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.4.2.4 2008/01/19 03:58:33 theurich Exp $"
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
 
#define PARCH_unicos
#define ESMF_ARCH_NAME "unicos"

#define ESMC_HAVE_LIMITS_H
#define ESMC_HAVE_PWD_H 
#define ESMC_HAVE_STRING_H 
#define ESMC_HAVE_STROPTS_H 
#define ESMC_HAVE_MALLOC_H 
#define ESMC_HAVE_DRAND48 
#define ESMC_HAVE_GETDOMAINNAME 
#define ESMC_HAVE_UNAME 
#define ESMC_HAVE_UNISTD_H 
#define ESMC_HAVE_STDLIB_H
#define ESMC_HAVE_SYS_TIME_H 
#define ESMC_HAVE_SYS_UTSNAME_H
#define ESMC_USE_SHARED_MEMORY

#define ESMC_HAVE_OMP_THREADS 1

#define ESMC_HAVE_MPI 1

#define ESMC_SUBSTITUTE_CTRL_CHARS 1

#define ESMC_HAVE_FORTRAN_UNDERSCORE
#define FTN(func) func##_

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
typedef int ESMCI_FortranStrLenArg;
#endif

#define ESMC_SIZEOF_VOIDP 8
#define ESMC_SIZEOF_INT 4
#define ESMC_SIZEOF_DOUBLE 8

#define ESMC_HAVE_IRIXF90

#define ESMC_WORDS_BIGENDIAN 1

#define ESMC_HAVE_MEMMOVE

#define ESMC_HAVE_DOUBLE_ALIGN
#define ESMC_HAVE_DOUBLE_ALIGN_MALLOC

#define ESMC_HAVE_MEMALIGN

#define ESMC_HAVE_FAST_MPI_WTIME

#define ESMC_USE_DBX_DEBUGGER
#define ESMC_HAVE_SYS_RESOURCE_H
#define ESMC_RESTRICT __restrict

#define ESMC_HAVE_RTLD_GLOBAL 1

#define ESMC_CAN_SLEEP_AFTER_ERROR

#define ESMC_HAVE_4ARG_SIGNAL_HANDLER

#define ESMC_USE_KBYTES_FOR_SIZE
#define ESMC_USE_P_FOR_DEBUGGER

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMC_HAVE_PCL 1
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 44
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#undef ESMC_HAVE_PCL 
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 72
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif


#endif
