#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.6.2.5 2009/01/21 21:25:18 cdeluca Exp $"
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

#define PARCH_sunos

#define FTN(func) func##_

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// Sun changes between 32- and 64-bit ABIs, so use long.
typedef long ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#ifndef ESMF_F90_PTR_BASE_SIZE
#define ESMF_F90_PTR_BASE_SIZE 20
#endif
#ifndef ESMF_F90_PTR_PLUS_RANK
#define ESMF_F90_PTR_PLUS_RANK 12
#endif
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#ifndef ESMF_F90_PTR_BASE_SIZE
#define ESMF_F90_PTR_BASE_SIZE 40
#endif
#ifndef ESMF_F90_PTR_PLUS_RANK
#define ESMF_F90_PTR_PLUS_RANK 24
#endif
#define ESMC_POINTER_SIZE 8
#endif

#endif
