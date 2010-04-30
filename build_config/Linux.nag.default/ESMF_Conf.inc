#if 0
$Id: ESMF_Conf.inc,v 1.7.4.2 2010/04/30 22:09:09 theurich Exp $

Earth System Modeling Framework
Copyright 2002-2010, University Corporation for Atmospheric Research,
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
  compiler is being used, enable the following macros.
#endif
#if 0
#define ESMF_NAG_UNIXIO_MODULE
#define ESMF_IOFlushMacro(unit,status) call flush (unit, status)
#endif
