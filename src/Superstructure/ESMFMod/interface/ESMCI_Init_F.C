// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C++ helper methods
// 
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include "ESMCI_Macros.h"
#include <cstring>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id$";
//-----------------------------------------------------------------------------
namespace ESMCI {

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Init helper routines
//
//

// non-method functions
void FTN_X(c_esmc_initget_esmf_comm) (
                           char *esmf_comm_str,
                           ESMCI_FortranStrLenArg esmf_comm_str_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_initget_esmf_comm()"

#define XSTR(X) STR(X)
#define STR(X) #X
  const char* esmf_comm = XSTR(ESMF_COMM);
  int esmf_comm_l = strlen (esmf_comm);
  if (esmf_comm_l <= esmf_comm_str_l) {
    memcpy (esmf_comm_str, esmf_comm, esmf_comm_l);
    memset (esmf_comm_str+esmf_comm_l, ' ', esmf_comm_str_l - esmf_comm_l);
  } else
    memcpy (esmf_comm_str, esmf_comm, esmf_comm_str_l);
}


//-----------------------------------------------------------------------------

} // extern "C"

} // namespace ESMCI
