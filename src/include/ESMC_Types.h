// $Id: ESMC_Types.h,v 1.1 2002/10/23 20:19:34 eschwab Exp $
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
#if ESMF_ARCH == linux_lf95 || ESMF_ARCH == linux_pgi ||
                               ESMF_ARCH == linux_gnupgf90 ||
                               ESMF_ARCH == IRIX ||
                               ESMF_ARCH == rs6000_sp ||
                               ESMF_ARCH == solaris ||
                               ESMF_ARCH == solaris_hpc
   #define INT64  long long
   #define UINT64 unsigned long long
   #define INT32  long
   #define UINT32 unsigned long

// 64-bit machines/compilers
#elif ESMF_ARCH == alpha || ESMF_ARCH == IRIX64 || ESMF_ARCH == rs6000_64

   #define INT64  long
   #define UINT64 unsigned long
   #define INT32  int
   #define UINT32 unsigned int

#endif // ESMF_TYPE_H
