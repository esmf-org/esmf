#if 0
$Id: ESMF_Conf.inc,v 1.6 2008/10/14 21:30:31 w6ws Exp $

Earth System Modeling Framework
Copyright 2002-2008, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
Define the proper way to flush Fortran units.  Choices include:

  ESMF_HAS_F2003_FLUSH (preferred)
  ESMF_HAS_1ARG_FLUSH
  ESMF_HAS_2ARG_FLUSH

#endif

#define ESMF_HAS_F2003_FLUSH

#if 0
the GNU G95 compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI and G95 only, do not sequence
the State derived types.
#endif

#define ESMF_SEQUENCE_BUG 0


