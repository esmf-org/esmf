// $Id: ESMC_LogErr_F.C,v 1.2 2004/04/28 20:14:23 cpboulder Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Base method interface (from F90 to C++) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Base methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC_LogErr.h"
#include "ESMC_Base.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_LogErr_F.C,v 1.2 2004/04/28 20:14:23 cpboulder Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMC_Base routine interfaces
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  LogErr Methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_LogInitialize - initialize global Error Log
//
// !INTERFACE:
      void FTN(c_esmc_loginitialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *filename,           // in - F90 filename, non-null terminated string
      int nlen,                 // hidden/in - strlen count for filename
      int *rc){                 // out - return code
// 
// !DESCRIPTION:
//     Initialize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS: 

  char *fname = NULL;

  *rc = ESMF_FAILURE;
  // copy and convert F90 strings to null terminated ones
  if (filename && (nlen > 0) && (filename[0] != '\0')) {
      fname = ESMC_F90toCstring(filename, nlen);
      if (!fname) {
          delete [] fname;
          return;
      }
  } 
  strcpy(nameLogErrFile,fname);
  if (fname)  delete [] fname;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_Loginitialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_LogFinalize - Finalize global Error Log
//
// !INTERFACE:
      void FTN(c_esmc_logfinalize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      int *rc){                 // out - return code
// 
// !DESCRIPTION:
//     Finalize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS: 
  
  *rc = ESMF_FAILURE;
  if (nameLogErrFile) delete [] nameLogErrFile;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_Logfinalize

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


} // extern "C"
