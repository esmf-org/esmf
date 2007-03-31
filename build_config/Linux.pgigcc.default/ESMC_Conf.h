#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.5 2007/03/31 05:50:43 cdeluca Exp $"
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

#define FTN(func) func##_

#if 0
PGI method for marking F90 "not present" optional arguments uses
the address of compiler-generated global array element pghpf_0_[8] for
non-character data.  However, PGI says check against entire array to be safe.
For character data, "not present" optional arguments are marked with the
address of global compiler-generated variable pghpf_0c_.
#endif
#if 0
This file is included by one F90 file (the test for F90 pointer size), so
try to protect the F90 file from C declarations.
#endif
#ifndef _FROM_FORTRAN
extern char pghpf_0_[];
extern char pghpf_0c_;
#define ESMC_PRESENT(arg) ( ! ( ( (char*)(arg) >=  pghpf_0_ && \
                                  (char*)(arg) <= &pghpf_0_[12] ) || \
		                  (char*)(arg) == &pghpf_0c_ ) )
#endif

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 72
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 4
#endif
#if S32_x86_64
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 72
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 88
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif
#if S64_x86_64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 88
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif

#endif
