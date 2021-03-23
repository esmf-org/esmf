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

  * Fortran-2003 FLUSH statement (preferred)
  * Two argument FLUSH call
  * One argument FLUSH call

#endif

#define ESMF_IOFlushMacro(unit,status) call flush_ (unit)

#if 0
Apparently some versions the AIX xlf 8.x compiler are unable to 
handle initializers for derived type contents.  If this is a problem
on your system, remove the #if 0/#endif lines and fix the #define line
before you compile the framework.
#endif

#if 0
-#-define ESMF_NO_INITIALIZERS 1
#endif

