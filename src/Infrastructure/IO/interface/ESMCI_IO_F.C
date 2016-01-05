// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_IO_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <string>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_IO.h"
#include "ESMCI_LogErr.h"

using namespace std;

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt IO} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN_X(c_esmc_iocreate)(ESMCI::IO ** ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iocreate()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::IO::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
                                      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      ESMC_NOT_PRESENT_FILTER(rc))) {
      return;
    }
    if (rc != NULL) {
      *rc = localrc;
    }
  }

  void FTN_X(c_esmc_iodestroy)(ESMCI::IO **ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iodestroy()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = ESMCI::IO::destroy(ptr);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
    if (rc != NULL) {
      *rc = localrc;
    }
  }

  void FTN_X(c_esmc_ioclear)(ESMCI::IO **ptr) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ioclear()"
    // call into C++
    (*ptr)->clear();
  }

  void FTN_X(c_esmc_ioaddarray)(ESMCI::IO **ptr, ESMCI::Array **array,
                                char *opt_variableName, int *len_variableName,
                                int *rc,
                                ESMCI_FortranStrLenArg varname_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ioaddarray()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    string varName;
    if (*len_variableName > 0)
       varName = string (opt_variableName, *len_variableName);
    // call into C++
    localrc = (*ptr)->addArray(*array, varName.c_str());
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
    if (rc != NULL) {
      *rc = localrc;
    }
  }

  void FTN_X(c_esmc_iowrite)(ESMCI::IO **ptr,
                             char *file, int *len_file,
                             ESMC_IOFmt_Flag *opt_iofmt,
                             ESMC_Logical *opt_overwrite,
                             ESMC_FileStatus_Flag *opt_status,
                             int *timeslice,
                             char *schema, int *len_schema, int *rc,
                             ESMCI_FortranStrLenArg file_l,
                             ESMCI_FortranStrLenArg schema_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iowrite()"
    // Initialize return code; assume routine not implemented
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    string fileName (file, *len_file);

    ESMC_IOFmt_Flag iofmt = ESMF_IOFMT_NETCDF;             // default
    bool overwrite = false;                                // default
    ESMC_FileStatus_Flag status = ESMC_FILESTATUS_UNKNOWN; // default

    if (ESMC_NOT_PRESENT_FILTER(opt_iofmt) != ESMC_NULL_POINTER) {
      iofmt = *opt_iofmt;
    }
    if ((ESMC_NOT_PRESENT_FILTER(opt_overwrite) != ESMC_NULL_POINTER) &&
        (*opt_overwrite == ESMF_TRUE)) {
      overwrite = true;
    }
    if (ESMC_NOT_PRESENT_FILTER(opt_status) != ESMC_NULL_POINTER) {
      status = *opt_status;
    }

    if (*len_schema > 0) {
      // We don't yet support the schema argument.
      ESMC_LogDefault.Write("Schema argument not yet supported, ignoring",
                            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }

    // Call into the actual C++ method
    localrc = (*ptr)->write(fileName.c_str(), iofmt, overwrite, status, timeslice);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_ioread)(ESMCI::IO **ptr,
                            char *file, int *len_file,
                            ESMC_IOFmt_Flag *opt_iofmt,
                            int *opt_timeslice,
                            char *schema, int *len_schema, int *rc,
                            ESMCI_FortranStrLenArg file_l,
                            ESMCI_FortranStrLenArg schema_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ioread()"
    // Initialize return code; assume routine not implemented
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variables
    string fileName (file, *len_file);
    ESMC_IOFmt_Flag iofmt = ESMF_IOFMT_NETCDF; // default
    int timeslice = 0; // default


    if (ESMC_NOT_PRESENT_FILTER(opt_iofmt) != ESMC_NULL_POINTER) {
      iofmt = *opt_iofmt;
    }
    if (ESMC_NOT_PRESENT_FILTER(opt_timeslice) != ESMC_NULL_POINTER) {
      timeslice = *opt_timeslice;
    }
    if (*len_schema > 0) {
      // We don't yet support the schema argument.
      ESMC_LogDefault.Write("Schema argument not yet supported, ignoring",
                            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }

    // Call into the actual C++ method
    localrc = (*ptr)->read(fileName.c_str(), iofmt, &timeslice);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

#undef  ESMC_METHOD
}
