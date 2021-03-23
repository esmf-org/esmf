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
Define the proper way to flush Fortran units.  Typically this will
be one of the following:

  * Fortran-2003 FLUSH statement (preferred and default)
  * Two argument FLUSH call
  * One argument FLUSH call

  Use F2003 FLUSH statement from 5.2-onwards NAG, because
  the library call has thread-safety problems.  If a pre-5.2
  compiler is being used, enable the following macro.
#endif
#if 0
#define ESMF_IOFlushMacro(unit,status) call flush (unit, status)
#endif

#if 0
NAG before v6.2, use sizeof work-around code instead of F2008 C_SIZEOF
#endif
#ifndef __NAG_COMPILER_RELEASE
#define ESMF_NO_C_SIZEOF
#else
#if (__NAG_COMPILER_RELEASE < 62)
#define ESMF_NO_C_SIZEOF
#endif
#endif

