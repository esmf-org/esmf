// $Id: ESMC_Arg.h,v 1.1 2007/04/13 05:16:55 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC optional arguments include file
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMC_Arg_H
#define ESMC_Arg_H

// Bring in the variable argument list macros.
#include <stdarg.h>

// Optional argument list data type.
#define ESMCI_ArgList  va_list

// Optional argument identifier datatype.
typedef int ESMCI_ArgID;

// The global optional argument identifier list.
// ESMCI_ArgBaseID is the starting argument identifier for local optional
// argument lists and must be the last identifier listed in the global
// identifier list.
enum {
  ESMCI_ArgLastID       = 0,
  ESMCI_ArgBaseID
};

// Function to initialize optional argument list processing.
//   ARG_LIST: optional argument list
//   LAST_FIXED: last fixed argument (before the optional argument list)
#define ESMCI_ArgStart(ARG_LIST,LAST_FIXED)  va_start(ARG_LIST,LAST_FIXED)

// Function to finalize optional argument list processing.
//   ARG_LIST: optional argument list
#define ESMCI_ArgEnd(ARG_LIST)  va_end(ARG_LIST)

// Function to return optional argument identifier.
//   ARG_LIST: optional argument list
#define ESMCI_ArgGetID(ARG_LIST)  va_arg(ARG_LIST,ESMCI_ArgID)

// Functions to return optional argument values.
// Arguments corresponding to the variable argument list specified by ",..."
// in a function prototype always undergo the following argument conversions.
//   char or short or bool are converted to int
//   float is converted to double
// Functions for standard C types (non-pointer):
#define ESMCI_ArgGetChar(ARG_LIST)              (char)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetShort(ARG_LIST)            (short)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetInt(ARG_LIST)                (int)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetLong(ARG_LIST)              (long)va_arg(ARG_LIST,long)
#define ESMCI_ArgGetLongLong(ARG_LIST)     (long long)va_arg(ARG_LIST,long long)
#define ESMCI_ArgGetFloat(ARG_LIST)            (float)va_arg(ARG_LIST,double)
#define ESMCI_ArgGetDouble(ARG_LIST)          (double)va_arg(ARG_LIST,double)
// Functions for defined ESMC types (non-pointer):
#define ESMCI_ArgGetI1(ARG_LIST)             (ESMC_I1)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetI2(ARG_LIST)             (ESMC_I2)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetI4(ARG_LIST)             (ESMC_I4)va_arg(ARG_LIST,int)
#define ESMCI_ArgGetI8(ARG_LIST)             (ESMC_I8)va_arg(ARG_LIST,ESMC_I8)
#define ESMCI_ArgGetR8(ARG_LIST)             (ESMC_R8)va_arg(ARG_LIST,double)
#define ESMCI_ArgGetR4(ARG_LIST)             (ESMC_R4)va_arg(ARG_LIST,double)
// Functions for pointer types:
#define ESMCI_ArgGetPtr(ARG_LIST,ARG_TYPE)  (ARG_TYPE)va_arg(ARG_LIST,ARG_TYPE)


//-----------------------------------------------------------------------------
// User interface
//-----------------------------------------------------------------------------

// Macro that casts optional argument id's to be type-appropriate for
// passing to functions.
#define ESMCI_Arg(ARG_ID) ((ESMCI_ArgID)ARG_ID)

// Convenience macro to indicate the end of an optional argument list.
#define ESMC_ArgLast ESMCI_Arg(ESMCI_ArgLastID)

//-----------------------------------------------------------------------------

#endif  // ESMC_Arg_H
