#if 0
$Id: ESMF_Conf.inc,v 1.8.4.1 2010/02/05 20:22:36 svasquez Exp $

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

#define ESMF_IOFlushMacro(unit,status) call flush (unit)

#if 0
the lahey compiler does not seem to pass the base address of the data
when an array is passed from fortran to a C++.  instead they provide
an intrinsic which does return the base address i am looking for.
the default macro is defined before, so undefine it before redefining it.
#endif

#undef  ESMF_DATA_ADDRESS
#define ESMF_DATA_ADDRESS(x)  POINTER(x)

