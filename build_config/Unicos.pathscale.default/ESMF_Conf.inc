#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2018, University Corporation for Atmospheric Research,
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
Define the proper way to read the command line.  Absoft does not
have the Fortran-2003 command line intrinsics.  So use Posix calls.  

#endif

#define ESMF_NEEDSPXFGETARG

#if 0
in theory absoft supports initializers, but in practice with the number
of modules we include, it seems to cause problems when enabled.
#endif

#define ESMF_NO_INITIALIZERS 1


