#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.3.2.3 2007/10/18 02:41:50 cdeluca Exp $"
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

#define PARCH_alpha
#define ESMF_ARCH_NAME "alpha"
/*#define ESMC_USE_GETCLOCK*/

#define ESMC_HAVE_LIMITS_H
#define ESMC_HAVE_PWD_H 
#define ESMC_HAVE_STRING_H 
#define ESMC_HAVE_MALLOC_H 
#define ESMC_HAVE_STDLIB_H 
#define ESMC_HAVE_DRAND48  
#define ESMC_HAVE_GETDOMAINNAME  
#define ESMC_HAVE_UNISTD_H 
#define ESMC_HAVE_SYS_TIME_H 
#define ESMC_HAVE_UNAME  
#define ESMC_HAVE_FORTRAN_UNDERSCORE
#define FTN(func) func##_

#define ESMC_HAVE_OMP_THREADS 1
#define ESMC_HAVE_MPI 1
#define MF_SUBSTITUTE_CTRL_CHARS 1

#define ESMC_SUBSTITUTE_CTRL_CHARS 1

#define ESMC_HAVE_READLINK
#define ESMC_HAVE_MEMMOVE
#define ESMC_NEEDS_UTYPE_TYPEDEFS

#define ESMC_RESTRICT __restrict

#define ESMC_USE_DBX_DEBUGGER
#define ESMC_HAVE_SYS_RESOURCE_H

#define ESMC_SIZEOF_VOIDP 8
#define ESMC_SIZEOF_INT 4
#define ESMC_SIZEOF_DOUBLE 8

#define ESMC_USE_DYNAMIC_LIBRARIES 1
#define ESMC_USE_NONEXECUTABLE_SO 1

#define ESMC_NEED_SOCKET_PROTO
#define ESMC_HAVE_ENDIAN_H

#define ESMC_NEED_KILL_FOR_DEBUGGER
#define ESMC_USE_PID_FOR_DEBUGGER

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE xx
#define ESMF_F90_PTR_PLUS_RANK xx
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 64
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif

#define ESMF_NEEDS_MPI_LOGICAL8 1

#endif
