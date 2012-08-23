// $Id: ESMCI_IO_F.C,v 1.4 2012/08/23 19:29:47 gold2718 Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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
#include <cstring>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"

#include "ESMCI_IO.h"

#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMCI_LogMacros.inc"

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

  void FTN_X(c_esmc_iodestroy)(ESMCI::IO **ptr, int *rc){
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

  void FTN_X(c_esmc_ioaddarray)(ESMCI::IO **ptr, ESMCI::Array **array,
                                char *opt_variableName, int *len_variableName,
                                int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ioaddarray()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    char varName[ESMF_MAXSTR + 1];
    int len_varName = *len_variableName;
    if (len_varName > ESMF_MAXSTR) {
      ESMC_LogDefault.Write("Variable name length > ESMF_MAXSTR",
                            ESMC_LOG_WARN, ESMC_CONTEXT);
      len_varName = ESMF_MAXSTR;
    } else if (len_varName < 0) {
      ESMC_LogDefault.Write("Negative variable name length",
                            ESMC_LOG_WARN, ESMC_CONTEXT);
      len_varName = 0;
    }
    if (len_varName > 0) {
      // The user passed in the optional variable name
      strncpy(varName, opt_variableName, len_varName);
    } else {
      varName[0] = '\0';
    }
    // call into C++
    localrc = (*ptr)->addArray(*array, varName);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
    if (rc != NULL) {
      *rc = localrc;
    }
  }

  void FTN_X(c_esmc_iowrite)(ESMCI::IO **ptr,
                             char *file, int *len_file,
                             ESMC_IOFmtFlag *opt_iofmt,
                             ESMC_Logical *opt_appendflag,
                             int *opt_timeslice, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iowrite()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    char fileName[ESMF_MAXSTR + 1];
    int len_fileName = *len_file;
    ESMC_IOFmtFlag iofmt = ESMF_IOFMT_NETCDF; // default
    bool append = false; // default
    int timeslice = 0; // default

    if (len_fileName > ESMF_MAXSTR) {
      ESMC_LogDefault.Write("File name length > ESMF_MAXSTR",
                            ESMC_LOG_WARN, ESMC_CONTEXT);
      len_fileName = ESMF_MAXSTR;
    } else if (len_fileName < 0) {
      ESMC_LogDefault.Write("Negative file name length",
                            ESMC_LOG_WARN, ESMC_CONTEXT);
      len_fileName = 0;
    }
    if (len_fileName > 0) {
      // The user passed in the optional variable name
      strncpy(fileName, file, len_fileName);
    } else {
      fileName[0] = '\0';
    }
    if (ESMC_NOT_PRESENT_FILTER(opt_iofmt) != ESMC_NULL_POINTER) {
      iofmt = *opt_iofmt;
    }
    if ((ESMC_NOT_PRESENT_FILTER(opt_appendflag) != ESMC_NULL_POINTER) &&
        (*opt_appendflag == ESMF_TRUE)) {
      append = true;
    }
    if (ESMC_NOT_PRESENT_FILTER(opt_timeslice) != ESMC_NULL_POINTER) {
      timeslice = *opt_timeslice;
    }
    // Call into the actual C++ method
    localrc = (*ptr)->write(fileName, &iofmt, append, &timeslice);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

#if 0
  void FTN_X(c_esmc_ioread)(ESMCI::IO **ptr, ESMCI::Array **array,
                            char *file, char *variableName,
    int *timeslice, ESMC_IOFmtFlag *iofmt, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayread()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::read(
      *array, file, variableName, timeslice, iofmt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
#endif

#undef  ESMC_METHOD
}
