#if 0
$Id: ESMF_Macros.inc,v 1.4 2004/03/04 17:01:41 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.

These lines prevent this file from being read more than once if it
ends up being included multiple times.
#endif

#ifndef ESMF_MACROS_INC
#define ESMF_MACROS_INC

#if 0
basic #defines for all architectures.  the Conf.h file is system
dependent and can override or redefine macros or constants which
are different on just a few systems.
#endif 

#if 0
how do you get to the base address of a fortran array, when you are
passing it from fortran to C++?  on most architectures you just use
the array.  but there are exceptions...
#endif 

#define ESMF_DATA_ADDRESS(x)    x


#if 0
the following macro is here for the frameworks own internal use.
#endif

#define ESMF_SRCLINE __FILE__, __LINE__

#endif 
