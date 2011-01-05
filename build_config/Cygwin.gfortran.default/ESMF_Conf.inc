#if 0
$Id: ESMF_Conf.inc,v 1.9 2011/01/05 20:05:38 svasquez Exp $

Earth System Modeling Framework
Copyright 2002-2011, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
the GNU G95 compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI and G95 only, do not sequence
the State derived types.

#define ESMF_SEQUENCE_BUG 0
#endif


