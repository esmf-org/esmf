#ifdef ESMC_RCS_HEADER
"$Id$"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2020, University Corporation for Atmospheric Research,
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
#define FTN_X(func) func##_
#define FTNX(func) func##_
#endif
#ifdef ESMF_LOWERCASE_DOUBLEUNDERSCORE 
#define FTN_X(func) func##__
#define FTNX(func) func##_
#endif

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
#if (__GNUC__ > 7)
#include <cstddef>
typedef size_t ESMCI_FortranStrLenArg;
#else
typedef int ESMCI_FortranStrLenArg;
#endif
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#endif
