#ifdef ESMC_RCS_HEADER
"$Id$"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2018, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_linux

#define FTN_X(func) func##_
#define FTNX(func) func##_

#if defined (__cplusplus)
#if 0
PGI v16.0 - onwards supports std::isfinite and friends.  This
is a c++-11 feature.
#endif
#if (ESMF_PGIVERSION_MAJOR < 16)
#define ESMF_PGI_NO_STDISFINITE
#endif

// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
#if (ESMF_PGIVERSION_MAJOR < 12)
typedef int ESMCI_FortranStrLenArg;
#else
#include <cstddef>
typedef size_t ESMCI_FortranStrLenArg;
#endif
#endif

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

#ifdef S32
#define ESMC_POINTER_SIZE 4
#endif
#ifdef S64
#define ESMC_POINTER_SIZE 8
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

#if 0
PGI before 17.9 could not handle the dynamic masking interfaces for different
typekinds
#endif
#if defined (ESMF_PGIVERSION_MAJOR)
#if (ESMF_PGIVERSION_MAJOR < 17) || ((ESMF_PGIVERSION_MAJOR == 17) && (ESMF_PGIVERSION_MINOR < 9))
#define ESMF_NO_DYNMASKOVERLOAD
#endif
#endif

#endif
