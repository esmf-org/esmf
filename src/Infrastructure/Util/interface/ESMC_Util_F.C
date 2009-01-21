// $Id: ESMC_Util_F.C,v 1.4.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Util method interface (from F90 to C++) file
#define ESMF_FILENAME "ESMC_Util_F.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Util methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"  // will this work?

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Util_F.C,v 1.4.2.2 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// String routines
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringSerialize - Serialize String object 
//
// !INTERFACE:
      void FTN(c_esmc_stringserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc,                  // out - return code
      int clen) {               // in, hidden - string length
// 
// !DESCRIPTION:
//     Serialize the contents of a string object.
//
//EOPI

  char *cp;

  if (!string) {
    //printf("uninitialized String object\n");
    ESMC_LogDefault.ESMC_LogWrite("String object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  int fixedpart = clen + 1;
  if ((*length - *offset) < fixedpart) {
         
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                             "Buffer too short to add a String object", rc);
       return;
 
      //buffer = (char *)realloc((void *)buffer,
      //                         *length + 2*fixedpart + byte_count);
      //*length += 2 * fixedpart;
  }

  cp = buf + *offset;
  memcpy(cp, string, clen);
  cp += clen;
  
  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringSerialize


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringDeserialize - Deserialize String object 
//
// !INTERFACE:
      void FTN(c_esmc_stringdeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      int *rc,                  // out - return code
      int clen) {               // in, hidden - string length
// 
// !DESCRIPTION:
//     Deserialize the contents of a base object.
//
//EOPI

  char *cp;

  cp = buf + *offset;
  memcpy(string, cp, clen);
  cp += clen;
  
  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringDeserialize




//-----------------------------------------------------------------------------

void FTN(c_pointerprint)(void **ptr){
  printf("ESMF_PointerPrint: %p\n", *ptr);
}

} // extern "C"
