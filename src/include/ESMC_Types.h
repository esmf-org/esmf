// $Id: ESMC_Types.h,v 1.3 2002/11/07 22:19:27 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMF_TYPE_H
#define ESMF_TYPE_H

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//  
// ESMF platform-independent data types.  Uses same architecture names
// from esmf/build.
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//
//-------------------------------------------------------------------------


// 32-bit machines/compilers
#if ESMF_IS_32BIT_MACHINE

   #define INT64  long long
   #define UINT64 unsigned long long
   #define INT32  long
   #define UINT32 unsigned long

// 64-bit machines/compilers
#elif ESMF_IS_64BIT_MACHINE

   #define INT64  long
   #define UINT64 unsigned long
   #define INT32  int
   #define UINT32 unsigned int

#endif // machine word length


// wrappers for how fortran calls into C code
#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FTN(func) func##_
#else
#define FTN(func) func
#endif


#endif // ESMF_TYPE_H

