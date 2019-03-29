#ifdef ESMC_RCS_HEADER
"$Id$"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2019, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_mingw

#define FTN_X(func) func
#define FTNX(func) func

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// On Windows, 'long' is always 32 bits.  So conditional code to
// select between 'int' and 'long long' is needed.
#if defined (S32)
typedef int ESMCI_FortranStrLenArg;
#elif defined (Sx86_64_32)
typedef int ESMCI_FortranStrLenArg;
#elif (defined (Sx86_64_small) || defined (S64))
typedef long long ESMCI_FortranStrLenArg;
#elif defined (Sx86_64_medium)
typedef long long ESMCI_FortranStrLenArg;
#else
#error Can't typedef ESMCI_FortranStrLenArg
#endif
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#endif
