// $Id: ESMC_Arch.h,v 1.1 2002/10/28 23:32:44 nscollins Exp $
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

#ifndef ESMF_ARCH_H
#define ESMF_ARCH_H

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//  
// ESMF platform-dependent settings.
//
//-------------------------------------------------------------------------

// 32-bit machines/compilers
#if (ESMF_ARCH == rs6000_sp)
#define ESMF_IS_32BIT_MACHINE 1
#define ESMF_HAS_INT32  1
#define ESMF_HAS_INT64  1
#endif

#if (ESMF_ARCH == IRIX)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == linux_gnupgf90)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == linux_lf95)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == linux_pgi)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == solaris)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == solaris_hpc)
#define ESMF_IS_32BIT_MACHINE 1
#endif

#if (ESMF_ARCH == alpha)
#define ESMF_IS_64BIT_MACHINE 1
#endif

#if (ESMF_ARCH == IRIX64)
#define ESMF_IS_64BIT_MACHINE 1
#endif

#if (ESMF_ARCH == rs6000_64)
#define ESMF_IS_64BIT_MACHINE 1
#endif


#endif // ESMF_ARCH_H
