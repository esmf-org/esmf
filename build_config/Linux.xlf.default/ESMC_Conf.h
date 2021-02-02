#ifdef ESMC_RCS_HEADER
"$Id$"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2021, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_linux

#define FTN_X(func) func
#define FTNX(func) func

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// XLF changes between 32- and 64-bit ABIs, so use long.
typedef long ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#if 0
Special defines necessary on this platform:
#endif

#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 
#endif
#if !defined(_XOPEN_SOURCE_EXTENDED)
#define _XOPEN_SOURCE_EXTENDED 1
#endif
#define _ALL_SOURCE   

#endif
