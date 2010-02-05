#if 0
$Id: ESMF_Conf.inc,v 1.9.4.1 2010/02/05 20:22:34 svasquez Exp $

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
the GNU G95 compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI and G95 only, do not sequence
the State derived types.
#endif

#define ESMF_SEQUENCE_BUG 0


