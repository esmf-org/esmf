#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2016, University Corporation for Atmospheric Research,
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

#endif

#if (__INTEL_COMPILER < 1000)
#define ESMF_IOFlushMacro(unit,status) call flush (unit)
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


