#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2021, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
The g95 compiler does not support Fortran 2003 allocatable character string lengths.
#endif

#define ESMF_NO_F2003_ALLOC_STRING_LENS

#if 0
The g95 compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword. Do not sequence some select derived
types to avoid issues.
#endif

#define ESMF_SEQUENCE_BUG 1

#if 0
Has F2003 C Interop, but not F2008 C_SIZEOF
#endif
#define C_SIZEOF SIZEOF

