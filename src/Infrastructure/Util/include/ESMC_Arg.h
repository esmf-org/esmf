// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
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

// Optional argument identifier datatype.
typedef int ESMCI_ArgID;

// The global optional argument identifier list.
// ESMCI_ArgBaseID is the starting argument identifier for local optional
// argument lists and must be the last identifier listed in the global
// identifier list.  ESMCI_ArgLastID is the global identifier for terminating
// the optional argument list.
enum {
  ESMCI_ArgLastID       = 11235813,
  ESMCI_ArgBaseID       = 1
};

// Macro for casting an optional argument into the appropriate sequence for
// passing to functions.  Each class will use this internal macro in its public
// header to declare user macros for the class specific optional arguments.
#define ESMCI_Arg(ID,ARG)  ((ESMCI_ArgID)ID),(ARG)

// Convenience macro to indicate the end of an optional argument list.
#define ESMC_ArgLast (ESMCI_ArgLastID)

//-----------------------------------------------------------------------------

#endif  // ESMC_Arg_H
