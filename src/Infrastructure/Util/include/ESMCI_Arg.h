// $Id: ESMCI_Arg.h,v 1.2.2.1 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC optional arguments internal include file
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMCI_Arg_H
#define ESMCI_Arg_H

// Bring in the public interface (this includes stdarg.h)
#include <ESMC_Arg.h>

// Optional argument list data type.
#define ESMCI_ArgList  va_list

// Internal macro to initialize optional argument list processing.
//   AP: optional argument list
//   LAST: last fixed argument (before the optional argument list)
#define ESMCI_ArgStart(AP,LAST)  va_start(AP,LAST)

// Internal macro to finalize optional argument list processing.
//   AP: optional argument list
#define ESMCI_ArgEnd(AP)  va_end(AP)

// Internal macro to return optional argument identifier.
//   AP: optional argument list
#define ESMCI_ArgGetID(AP)  (ESMCI_ArgID)va_arg(AP,ESMCI_ArgID)

// Internal type specific macros to return optional argument values.
// Arguments corresponding to the variable argument list specified by ",..."
// in a function prototype always undergo the following argument conversions:
// bool or char or short are converted to int, and float is converted to double.
// Macros for standard C types (non-pointer):
#define ESMCI_ArgGetChar(AP)           (char)va_arg(AP,int)
#define ESMCI_ArgGetShort(AP)         (short)va_arg(AP,int)
#define ESMCI_ArgGetInt(AP)             (int)va_arg(AP,int)
#define ESMCI_ArgGetLong(AP)           (long)va_arg(AP,long)
#define ESMCI_ArgGetLongLong(AP)  (long long)va_arg(AP,long long)
#define ESMCI_ArgGetFloat(AP)         (float)va_arg(AP,double)
#define ESMCI_ArgGetDouble(AP)       (double)va_arg(AP,double)
// Macros for defined ESMC types (non-pointer):
#define ESMCI_ArgGetI1(AP)          (ESMC_I1)va_arg(AP,int)
#define ESMCI_ArgGetI2(AP)          (ESMC_I2)va_arg(AP,int)
#define ESMCI_ArgGetI4(AP)          (ESMC_I4)va_arg(AP,int)
#define ESMCI_ArgGetI8(AP)          (ESMC_I8)va_arg(AP,ESMC_I8)
#define ESMCI_ArgGetR8(AP)          (ESMC_R8)va_arg(AP,double)
#define ESMCI_ArgGetR4(AP)          (ESMC_R4)va_arg(AP,double)
// Macro for strings:
#define ESMCI_ArgGetString(AP)               va_arg(AP,char*)
// Macro for all non-converted types
#define ESMCI_ArgGet(AP,TYPE)                va_arg(AP,TYPE)


#if 0
//-----------------------------------------------------------------------------
// Sample class public header file (ESMC_Xclass.h)
//-----------------------------------------------------------------------------

// Optional argument identifier list for the ESMC_Xclass API.
enum {
  ESMCI_XclassArgAoptID = ESMCI_ArgBaseID,  // ESMC_I1
  ESMCI_XclassArgBoptID,                    // ESMC_I4
  ESMCI_XclassArgCoptID,                    // ESMC_R4
  ESMCI_XclassArgDoptID,                    // ESMC_R8
  ESMCI_XclassArgEoptID,                    // ESMC_Fred
};

// Argument expansion macros for the ESMC_Xclass API.
#define ESMC_XclassArgAopt(ARG)  ESMCI_Arg(ESMCI_XclassArgAoptID,ARG)
#define ESMC_XclassArgBopt(ARG)  ESMCI_Arg(ESMCI_XclassArgBoptID,ARG)
#define ESMC_XclassArgCopt(ARG)  ESMCI_Arg(ESMCI_XclassArgCoptID,ARG)
#define ESMC_XclassArgDopt(ARG)  ESMCI_Arg(ESMCI_XclassArgDoptID,ARG)
#define ESMC_XclassArgEopt(ARG)  ESMCI_Arg(ESMCI_XclassArgEoptID,ARG)

// ESMC_Xclass prototypes
int ESMC_XclassFunc( int ifix, ... );

//-----------------------------------------------------------------------------
#endif


#if 0
//-----------------------------------------------------------------------------
// Sample class function with optional argument list parsing (ESMC_Xclass.C)
//-----------------------------------------------------------------------------
#include "ESMC_Xclass.h"
#include "ESMCI_Xclass.h"

int ESMC_XclassFunc( int ifix, ... ) {
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  ESMC_I1   aOpt;
  ESMC_I4   bOpt;
  ESMC_R4   cOpt;
  ESMC_R8   dOpt;
  ESMC_Fred eOpt;

   // Initialize return code; assume rutine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,ifix);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_XclassArgAoptID:
        ESMCI_ArgGetI1(argPtr);
        break;
      case ESMCI_XclassArgBoptID:
        ESMCI_ArgGetI4(argPtr);
        break;
      case ESMCI_XclassArgCoptID:
        ESMCI_ArgGetR4(argPtr);
        break;
      case ESMCI_XclassArgDoptID:
        ESMCI_ArgGetR8(argPtr);
        break;
      case ESMCI_XclassArgEoptID:
        ESMCI_ArgGet(argPtr,ESMC_Fred);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list:
  ESMCI_ArgStart(argPtr,ifix);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_XclassArgAoptID:
        aOpt = ESMCI_ArgGetI1(argPtr);
        break;
      case ESMCI_XclassArgBoptID:
        bOpt = ESMCI_ArgGetI4(argPtr);
        break;
      case ESMCI_XclassArgCoptID:
        cOpt = ESMCI_ArgGetR4(argPtr);
        break;
      case ESMCI_XclassArgDoptID:
        dOpt = ESMCI_ArgGetR8(argPtr);
        break;
      case ESMCI_XclassArgEoptID:
        eOpt = ESMCI_ArgGet(argPtr,ESMC_Fred);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  <...function code goes here...>

  return rc;
}

//-----------------------------------------------------------------------------
#endif

//-----------------------------------------------------------------------------

#endif  // ESMCI_Arg_H
