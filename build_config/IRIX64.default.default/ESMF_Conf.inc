#if 0
$Id: ESMF_Conf.inc,v 1.8.4.1 2010/02/05 20:22:33 svasquez Exp $

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

#define ESMF_IOFlushMacro(unit,status) call flush (unit, status)

#if 0
Define the proper way to flush Fortran units.  Choices include:

  ESMF_HAS_F2003_FLUSH (preferred)
  ESMF_HAS_1ARG_FLUSH
  ESMF_HAS_2ARG_FLUSH

#endif

#define ESMF_HAS_2ARG_FLUSH

