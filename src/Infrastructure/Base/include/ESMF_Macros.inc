#if 0
$Id: ESMF_Macros.inc,v 1.5 2004/05/14 08:12:55 nscollins Exp $

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
here is how to get the base address of a fortran array when you are
passing it from fortran to C++.  on most architectures you just use
the array.  but there are exceptions...
#endif 

#define ESMF_DATA_ADDRESS(x)    x


#if 0
the following macro is here for the frameworks own internal use.
(yes, the line above is missing a single quote for the possessive, but
unmatched quotes confuse some preprocessors with delusions of grammar.)
#endif

#define ESMF_SRCLINE __FILE__, __LINE__

#endif 

