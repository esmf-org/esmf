// $Id: ESMC_WordsizeSubr.C,v 1.8.2.2 2009/01/21 21:25:25 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
#include "ESMCI.h"

extern "C" {

// verify that we are using the same word sizes on each side of the language
// boundary - F90 and C++.

void FTN(c_ints)(int *i1, int *i2, int *i3, int *i4,
                 int *i5, int *i6, int *i7, int *rc) {

      printf("In C++: sizeof(char)     = %d\n", (int) sizeof(char));
      printf("In C++: sizeof(short)     = %d\n", (int) sizeof(short));
      printf("In C++: sizeof(int)       = %d\n", (int) sizeof(int));
      printf("In C++: sizeof(long)      = %d\n", (int) sizeof(long));
      printf("In C++: sizeof(long long) = %d\n", (int) sizeof(long long));
      printf("In C++: sizeof(float)     = %d\n", (int) sizeof(float));
      printf("In C++: sizeof(ESMC_R8)    = %d\n", (int) sizeof(ESMC_R8));
      printf("In C++: sizeof(pointer)   = %d\n", (int) sizeof(i1));

      *i1 = sizeof(ESMC_I1);
      *i2 = sizeof(ESMC_I2);
      *i3 = sizeof(ESMC_I4);
      *i4 = sizeof(ESMC_I8);
      *i5 = sizeof(ESMC_R4);
      *i6 = sizeof(ESMC_R8);
      *i7 = sizeof(ESMC_POINTER);

      printf("In C++: sizeof(ESMC_I1) = %d\n", *i1);
      printf("In C++: sizeof(ESMC_I2) = %d\n", *i2);
      printf("In C++: sizeof(ESMC_I4) = %d\n", *i3);
      printf("In C++: sizeof(ESMC_I8) = %d\n", *i4);
      printf("In C++: sizeof(ESMC_R4) = %d\n", *i5);
      printf("In C++: sizeof(ESMC_R8) = %d\n", *i6);
      printf("In C++: sizeof(ESMC_POINTER) = %d\n", *i7);

      *rc = ESMF_SUCCESS;

}  // end of c_ints()

} // end of extern "C"




