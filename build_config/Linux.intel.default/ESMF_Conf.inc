#if 0
$Id: ESMF_Conf.inc,v 1.2 2004/03/24 17:59:30 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif


#if 0
the PGI compiler won't allow initializers.  on the theory that they
might be causing problems, don't include them in the intel build either.
the build is currently making the compiler core dump in the bundle code.
see if this helps.
#endif

#define ESMF_NO_INITIALIZERS 1

#if 0
the PGI compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  again, on the "things which
are hard for one compiler might be hard for another" theory, don't sequence
the forward references in states here either.
#endif

#define ESMF_SEQUENCE_BUG 1
