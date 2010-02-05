#ifdef ESMC_RCS_HEADER
"$Id: ESMC_Conf.h,v 1.22.2.1 2010/02/05 20:22:27 svasquez Exp $"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2010, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_darwin

#ifdef ESMF_LOWERCASE_SINGLEUNDERSCORE 
#define FTN(func) func##_
#endif
#ifdef ESMF_LOWERCASE_DOUBLEUNDERSCORE 
#define FTN(func) func##__
#endif

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// Absoft changes between 32-bit and 64-bit ABIs, so use 'long'.
typedef long ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#ifdef S32
#define ESMC_POINTER_SIZE 4
#endif
#ifdef Sx86_64_32
#define ESMC_POINTER_SIZE 4
#endif
#ifdef Sx86_64_small
#define ESMC_POINTER_SIZE 8
#endif
#ifdef Sx86_64_medium
#define ESMC_POINTER_SIZE 8
#endif

#endif
