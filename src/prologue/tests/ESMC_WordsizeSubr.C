// $Id: ESMC_WordsizeSubr.C,v 1.1 2005/08/30 21:31:40 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2005, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C++ methods used to verify that strings
// can be passed correctly between F90 and C++.
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_WordsizeSubr.C"

#include <stdio.h>
#include <string.h>
#include "ESMC.h"

extern "C" {

// sizeof(short), sizeof(int), sizeof(long), sizeof(float), sizeof(double)

void FTN(c_ints)(int *i1, int *i2, int *i3, int *i4, int *i5, int *rc) {

      *i1 = sizeof(short);
      *i2 = sizeof(int);
      *i3 = sizeof(long long);
      *i4 = sizeof(float);
      *i5 = sizeof(double);

      printf("In C++: sizeof(short)  = %d\n", *i1);
      printf("In C++: sizeof(int)    = %d\n", *i2);
      printf("In C++: sizeof(long long)   = %d\n", *i3);
      printf("In C++: sizeof(float)  = %d\n", *i4);
      printf("In C++: sizeof(double) = %d\n", *i5);

      *rc = ESMF_SUCCESS;

}  // end of c_ints()

} // end of extern "C"




