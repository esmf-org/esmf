//$Id: ESMC_ErrMsgs.C,v 1.1 2004/05/14 23:33:36 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
// 
//-------------------------------------------------------------------------
//
// Array of general error message strings, as generic as possible, to allow
// prepending or appending with user strings.

static const char *errMsg[] = {
// Error message                                        Return Code
// -----------------------------------------------      ----------------------
  "Invalid object ",                                  // ESMF_RC_OBJ_OBJ
  "Object SetDefault method not called ",             // ESMF_RC_OBJ_INIT
  "Object Create method not called ",                 // ESMF_RC_OBJ_CREATE
  "Corrupted ESMF object detected ",                  // ESMF_RC_OBJ_COR
  "Object is in wrong state ",                        // ESMF_RC_OBJ_WRONGSTATE

  "Invalid argument ",                                // ESMF_RC_ARG_BAD
  "Argument rank is not required size ",              // ESMF_RC_ARG_RANK
  "Argument sizes do not match ",                     // ESMF_RC_ARG_SIZE
  "Value unrecognized or out of range ",              // ESMF_RC_ARG_VALUE
  "Two arguments not allowed to be the same ",        // ESMF_RC_ARG_DUP
  "Two arguments must be same object type ",          // ESMF_RC_ARG_SAMETYPE
  "Two arguments must have the same communicators ",  // ESMF_RC_ARG_SAMECOMM
  "Arguments are incompatible ",                      // ESMF_RC_ARG_INCOMP
  "Argument contains invalid ESMF object ",           // ESMF_RC_ARG_CORRUPT
  "Wrong argument specified ",                        // ESMF_RC_ARG_WRONG
  "Input argument out of range ",                     // ESMF_RC_ARG_OUTOFRANGE
  "Unrecognized option string ",                      // ESMF_RC_ARG_OPT

  "Operation not yet supported ",                     // ESMF_RC_NOT_IMPL

  "Unable to open file ",                             // ESMF_RC_FILE_OPEN
  "Unable to create file ",                           // ESMF_RC_FILE_CREATE
  "Unable to read from file ",                        // ESMF_RC_FILE_READ
  "Unable to write to file ",                         // ESMF_RC_FILE_WRITE
  "Unexpected data in file ",                         // ESMF_RC_FILE_UNEXPECTED
  "Unable to close file ",                            // ESMF_RC_FILE_CLOSE
  "Instrumented region is still active ",             // ESMF_RC_FILE_ACTIVE

  "Value cannot be a NULL pointer ",                  // ESMF_RC_PTR_NULL
  "Invalid pointer ",                                 // ESMF_RC_PTR_BAD
  "Pointer must already be allocated ",               // ESMF_RC_PTR_NOTALLOC
  "Pointer must not already be allocated ",           // ESMF_RC_PTR_ISALLOC
  "Unable to allocate requested memory ",             // ESMF_RC_MEM
  "Memory corrupted ",                                // ESMF_RC_MEMC

  "Name already exists ",                             // ESMF_RC_DUP_NAME
  "Name too long, must be less than ESMF_MAXSTR ",    // ESMF_RC_LONG_NAME
  "String too long, must be less than ESMF_MAXSTR ",  // ESMF_RC_LONG_STR
  "Cannot copy non-existent object ",                 // ESMF_RC_COPY_FAIL
  "Cannot divide by zero ",                           // ESMF_RC_DIV_ZERO
  "Cannot get value ",                                // ESMF_RC_CANNOT_GET
  "Cannot set value ",                                // ESMF_RC_CANNOT_SET
  "Not found ",                                       // ESMF_RC_NOT_FOUND
  "Not valid " ,                                      // ESMF_RC_NOT_VALID

  "Internal error: List overflow ",                   // ESMF_RC_INTNRL_LIST
  "Internal error: Inconsistent data ",               // ESMF_RC_INTNRL_INCONS
  "Internal error: Unknown error ",                   // ESMF_RC_INTNRL_BAD

  "System call error ",                               // ESMF_RC_SYS
  "Resource is busy ",                                // ESMF_RC_BUSY
  "Error in library called by ESMF "                  // ESMF_RC_LIB
};
