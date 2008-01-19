#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.3.2.5 2008/01/19 03:58:33 theurich Exp $"
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
#define ESMF_ARCH_NAME "linux"

#define ESMC_USE_READ_REAL_TIME

#define ESMC_HAVE_LIMITS_H
#define ESMC_HAVE_STROPTS_H 
#define ESMC_HAVE_SEARCH_H 
#define ESMC_HAVE_PWD_H 
#define ESMC_HAVE_STDLIB_H
#define ESMC_HAVE_STRING_H 
#define ESMC_HAVE_STRINGS_H 
#define ESMC_HAVE_MALLOC_H 
#define ESMC_HAVE_DRAND48  
#define ESMC_HAVE_GETDOMAINNAME 
#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 
#endif
#define ESMC_HAVE_UNISTD_H 
#define ESMC_HAVE_SYS_TIME_H 
#define ESMC_HAVE_UNAME 
#if !defined(_XOPEN_SOURCE_EXTENDED)
#define _XOPEN_SOURCE_EXTENDED 1
#endif
#define _ALL_SOURCE   
#define ESMC_HAVE_BROKEN_REQUEST_FREE 
#define ESMC_HAVE_STRINGS_H
#define ESMC_HAVE_DOUBLE_ALIGN_MALLOC

#undef ESMC_HAVE_PTHREADS 
#define ESMC_HAVE_MPI 1

#define ESMC_SUBSTITUTE_CTRL_CHARS 1

#define FTN(func) func

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// XLF changes between 32- and 64-bit ABIs, so use long.
typedef long ESMCI_FortranStrLenArg;
#endif

#define ESMC_HAVE_XLF90

#define ESMC_PREFER_BZERO

#define ESMC_HAVE_READLINK
#define ESMC_HAVE_MEMMOVE

#define ESMC_HAVE_PRAGMA_DISJOINT
#define ESMC_RESTRICT __restrict

#define ESMC_USE_DBX_DEBUGGER
#define ESMC_HAVE_SYS_RESOURCE_H

#define ESMC_SIZEOF_INT 4
#define ESMC_SIZEOF_DOUBLE 8

#define ESMC_WORDS_BIGENDIAN 1
#define ESMC_NEED_SOCKET_PROTO
#define ESMC_HAVE_ACCEPT_SIZE_T

#define ESMC_HAVE_SYS_UTSNAME_H

#define ESMC_HAVE_SLEEP_RETURNS_EARLY
#define ESMC_USE_KBYTES_FOR_SIZE

#define ESMC_USE_A_FOR_DEBUGGER

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMC_SIZEOF_VOIDP 4
#define ESMF_F90_PTR_BASE_SIZE 32
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMC_SIZEOF_VOIDP 8
#define ESMF_F90_PTR_BASE_SIZE 56
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif

#endif
