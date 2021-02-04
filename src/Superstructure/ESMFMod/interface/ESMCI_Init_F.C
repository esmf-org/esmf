// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"

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

// Tricky macros to get a macro value into a string
#define XSTR(X) STR(X)
#define STR(X) #X

// non-method function
void FTN_X(c_esmc_initget_build_datetime) (
    char *esmf_date_str,       // out - build date
    char *esmf_time_str,       // out - build time
    int *rc,                   // out - return code
    ESMCI_FortranStrLenArg esmf_date_str_l,
    ESMCI_FortranStrLenArg esmf_time_str_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_initget_esmf_build_datetime()"

  const char* esmf_date = XSTR(__DATE__);
  int localrc = ESMC_CtoF90string (esmf_date, esmf_date_str, esmf_date_str_l);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

  const char* esmf_time = XSTR(__TIME__);
  localrc = ESMC_CtoF90string (esmf_time, esmf_time_str, esmf_time_str_l);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
}

// non-method function
void FTN_X(c_esmc_initget_esmf_comm) (
    char *esmf_comm_str,       // out - value of macro ESMF_COMM
    int *rc,                   // out - return code
    ESMCI_FortranStrLenArg esmf_comm_str_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_initget_esmf_comm()"

  const char* esmf_comm = XSTR(ESMF_COMM);
  int localrc = ESMC_CtoF90string (esmf_comm, esmf_comm_str, esmf_comm_str_l);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
}

// non-method function
void FTN_X(c_esmc_initget_esmf_dir) (
    char *esmf_dir_str,       // out - value of macro ESMF_DIR
    int *rc,                  // out - return code
    ESMCI_FortranStrLenArg esmf_dir_str_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_initget_esmf_dir()"

#define XSTR(X) STR(X)
#define STR(X) #X
  const char* esmf_dir = XSTR(ESMF_DIR);
  int localrc = ESMC_CtoF90string (esmf_dir, esmf_dir_str, esmf_dir_str_l);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
}


//-----------------------------------------------------------------------------

} // extern "C"

} // namespace ESMCI
