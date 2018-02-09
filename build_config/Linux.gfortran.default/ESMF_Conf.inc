#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2018, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(__cplusplus)
#if (__GNUC__ == 4) && (__GNUC_MINOR__ < 5)
#error "The gfortran compiler must be version 4.5 or newer."
#endif

#if (__GNUC__ == 4) && (__GNUC_MINOR__ < 6)
#if 0
Gfortran prior to 4.6 does not support Fortran 2003 allocatable character string lengths.
#endif
#define ESMF_NO_F2003_ALLOC_STRING_LENS
#endif

#endif
