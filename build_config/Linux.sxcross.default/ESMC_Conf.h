#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.5.2.1 2008/01/18 23:52:50 theurich Exp $"
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
 
#define PARCH_NECSX

#define FTN(func) func##_

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
typedef int ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if S32
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 44
#define ESMF_F90_PTR_PLUS_RANK 12
#define ESMC_POINTER_SIZE 4
#endif
#if S64
#define ESMF_IS_64BIT_MACHINE 1
#define ESMF_F90_PTR_BASE_SIZE 72
#define ESMF_F90_PTR_PLUS_RANK 24
#define ESMC_POINTER_SIZE 8
#endif

#endif
