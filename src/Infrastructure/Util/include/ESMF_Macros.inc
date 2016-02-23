#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2016, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.

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

#define ESMF_SRCLINE ESMF_FILENAME, __LINE__

#ifndef ESMF_FILENAME
#define ESMF_FILENAME __FILE__
#endif

#if 0
Macros to allow structured early exits from blocks of Fortran code.
These are typically when testing for errors.

Eventually these macros can be rewritten or replaced using the F2008
block/end block statements.
#endif

#if defined (_FROM_FORTRAN)
#define ESMF_BLOCK(ESMF_BLOCK_LABEL) ESMF_BLOCK_LABEL: do
#define ESMF_ENDBLOCK(ESMF_BLOCK_LABEL) if (.true.) exit ESMF_BLOCK_LABEL; end do ESMF_BLOCK_LABEL
#endif

#endif 

