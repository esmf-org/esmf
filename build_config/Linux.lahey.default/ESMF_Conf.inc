#if 0
$Id: ESMF_Conf.inc,v 1.2 2004/03/04 17:02:16 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
the lahey compiler doesn't seem to pass the base address of the data
when an array is passed from fortran to a C++.  instead they provide
an intrinsic which does return the base address i'm looking for.
the default macro is defined before, so undefine it before redefining it.
#endif

#undef  ESMF_DATA_ADDRESS
#define ESMF_DATA_ADDRESS(x)  POINTER(x)

