#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2019, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
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
