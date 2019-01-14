#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2019, University Corporation for Atmospheric Research,
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

#define ESMF_IOFlushMacro(unit,status) call flush (unit)

#if 0
Define the proper way to read the command line.  Absoft does not
have the Fortran-2003, nor Posix calls.  So use getarg. 

#endif

#define ESMF_NEEDSGETARG

#if 0
Absoft up to version 9.0 does not seem to support F95 initializer feature
#endif

#define ESMF_NO_INITIALIZERS 1
