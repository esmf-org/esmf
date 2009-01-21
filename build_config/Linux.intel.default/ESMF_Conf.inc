#if 0
$Id: ESMF_Conf.inc,v 1.10.2.2 2009/01/21 21:25:18 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif


#if 0
Strict F90 compilers will not allow initializers.  Also, in the past when we 
have had problems with internal compiler errors, it has often helped if we 
turn off the derived type initializers.  At the moment the intel compiler 
seems to be ok with setting them, so i have removed the following #define, 
but if the problems resurface, try putting this back in.  
ditto for the sequence bug described below.
#endif

#if 0
-#-define ESMF_NO_INITIALIZERS 1
#endif

#if 0
the PGI compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  again, on the -- things which
are hard for one compiler might be hard for another --  theory, do not sequence
the forward references in states here either.   this is currently turned off
and seems to be working, but in case of problems, it is available to be
reactivated.
#endif

#if 0
-#-define ESMF_SEQUENCE_BUG 1
#endif


