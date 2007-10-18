// $Id: ESMC_StringSubr.C,v 1.3.2.4 2007/10/18 02:44:12 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research,
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
 #define ESMC_FILENAME "ESMC_StringSubr.C"

#include <stdio.h>
#include <string.h>
#include <ESMC_Macros.h>
#include <ESMC_Conf.h>

typedef void (*FUNC)(int *, int *, int *, int *, int *);
typedef void (*FUNC2)(int *, int *, char *, int *, int *, int *, int);
typedef void (*FUNC3)(int *, char *, int *, char *, int *, int *, int *, int, int);

extern "C" {

void FTN(c_strings)(FUNC f90cb, FUNC2 f90cb2, FUNC3 f90cb3, 
                    int *i1, int *i2, char *fstr,
                    int *i3, int *i4, int *rc, int slen) {

      int ni1, ni2, ni3, ni4;
      int clen, clen2;
      static char *newstr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      static char *newstr2 = "0123456789abcdefghijklmnopqrstuvwxyz";
      char tbuf[8192];
      int result = ESMF_SUCCESS;
      int local_rc;


      printf("\n\n-- entering c_strings\n");

      strncpy(tbuf, fstr, slen);
      tbuf[slen] = '\0';
      printf(" %d %d %d %d %d\n", *i1, *i2, *i3, *i4, slen);
      printf(" strlen = %d, str='%s'\n", slen, tbuf);

      ni1 = 102;
      ni2 = 204;
      ni3 = 409;
      ni4 = 819;
  
      printf("\n\n-- ready to call f90int by callback with passed ints\n");
      (*f90cb)(i1, i2, i3, i4, &local_rc);
      printf("\n\n-- returned from call of f90int by callback with passed ints\n");
      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      printf("\n\n-- ready to call f90int by callback with local ints\n");
      (*f90cb)(&ni1, &ni2, &ni3, &ni4, &local_rc);
      printf("\n\n-- returned from call of f90int by callback with local ints\n");
      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 15;

      printf("\n\n-- ready to call f90string2 by callback, passing 15\n");
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, &local_rc, clen);
      printf("\n\n-- returned from call f90string2 by callback\n");

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 25;

      printf("\n\n-- ready to call f90string2 by callback, passing 25\n");
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, &local_rc, clen);
      printf("\n\n-- returned from call f90string2 by callback\n");

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 14;
      clen2 = 24;

      printf("\n\n-- ready to call f90string3 by callback, passing 14, 24\n");
      (*f90cb3)(&ni1, newstr, &ni2, newstr2, &ni3, &ni4, &local_rc, clen, clen2);
      printf("\n\n-- returned from call f90string3 by callback\n");

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      /* pass back final result */
      *rc = result;

      printf("\n\n-- leaving c_strings\n");

}  // end of c_strings()

} // end of extern "C"




