// $Id: ESMC_Alloc.h,v 1.2 2002/12/07 00:00:39 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Alloc C++ declaration include file
//
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Alloc_H
#define ESMC_Alloc_H

#include "ESMC.h"

// !PRIVATE TYPES:

 // fortran interface declarations
extern "C" {

struct c_F90ptr {
   int pad[12];
};

   void FTN(f_esmf_allocate2dr4)(struct c_F90ptr *f90ptr, int *ni, int *nj, int *rc);
   void FTN(f_esmf_deallocate2dr4)(struct c_F90ptr *f90ptr, int *rc);
};

#endif  // ESMC_Alloc_H
