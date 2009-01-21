// $Id: ESMC_StringSubr.C,v 1.5.2.4 2009/01/21 21:25:25 cdeluca Exp $
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
 #define ESMC_FILENAME "ESMC_StringSubr.C"

#include <iostream>
using namespace std;

#include <string.h>
#include <ESMC_Macros.h>
#include <ESMC_Conf.h>

typedef void (*FUNC)(int *, int *, int *, int *, int *);
typedef void (*FUNC2)(int *, int *, char *, int *, int *, int *, ESMCI_FortranStrLenArg);
typedef void (*FUNC3)(int *, char *, int *, char *, int *, int *, int *, ESMCI_FortranStrLenArg, ESMCI_FortranStrLenArg);

extern "C" {

void FTN(c_strings)(FUNC f90cb, FUNC2 f90cb2, FUNC3 f90cb3, 
                    int *i1, int *i2, char *fstr,
                    int *i3, int *i4, int *rc, ESMCI_FortranStrLenArg slen) {

      int ni1, ni2, ni3, ni4;
      int clen, clen2;
      static char *newstr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      static char *newstr2 = "0123456789abcdefghijklmnopqrstuvwxyz";
      char tbuf[8192];
      int result = ESMF_SUCCESS;
      int local_rc;


      cout << "\n\n-- entering c_strings" << endl;

      strncpy(tbuf, fstr, slen);
      tbuf[slen] = '\0';
      cout << *i1 << " " << *i2 << " " << *i3 << " " << *i4 << endl;
      cout << " strlen = " << slen << ", str='" << tbuf << "'" << endl;

      ni1 = 102;
      ni2 = 204;
      ni3 = 409;
      ni4 = 819;
  
      cout << "\n\n-- ready to call f90int by callback with passed ints" << endl;
      (*f90cb)(i1, i2, i3, i4, &local_rc);
      cout << "\n\n-- returned from call of f90int by callback with passed ints" << endl;
      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      cout << "\n\n-- ready to call f90int by callback with local ints" << endl;
      (*f90cb)(&ni1, &ni2, &ni3, &ni4, &local_rc);
      cout << "\n\n-- returned from call of f90int by callback with local ints" << endl;
      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 15;

      cout << "\n\n-- ready to call f90string2 by callback, passing 15" << endl;
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, &local_rc, clen);
      cout << "\n\n-- returned from call f90string2 by callback" << endl;

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 25;

      cout << "\n\n-- ready to call f90string2 by callback, passing 25" << endl;
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, &local_rc, clen);
      cout << "\n\n-- returned from call f90string2 by callback" << endl;

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      clen = 14;
      clen2 = 24;

      cout << "\n\n-- ready to call f90string3 by callback, passing 14, 24" << endl;
      (*f90cb3)(&ni1, newstr, &ni2, newstr2, &ni3, &ni4, &local_rc, clen, clen2);
      cout << "\n\n-- returned from call f90string3 by callback" << endl;

      if (local_rc == ESMF_FAILURE) result = ESMF_FAILURE;

      /* pass back final result */
      *rc = result;

      cout << "\n\n-- leaving c_strings" << endl;

}  // end of c_strings()

} // end of extern "C"




