#if 0
$Id: ESMF_Conf.inc,v 1.8.4.2 2010/05/06 05:49:52 theurich Exp $

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

  * Fortran-2003 FLUSH statement (preferred)
  * Two argument FLUSH call
  * One argument FLUSH call

#endif

#define ESMF_IOFlushMacro(unit,status) flush (unit, iostat=status)

#if 0
prevent use of esmf derived type initializers.  causes some compilers problems.
#endif

#define ESMF_NO_INITIALIZERS 1
