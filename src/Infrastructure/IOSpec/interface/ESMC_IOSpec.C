// $Id: ESMC_IOSpec.C,v 1.7.4.1 2010/02/05 19:58:12 svasquez Exp $
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC IOSpec method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ IOSpec methods.
// This is a shallow class, and any changes to either the Fortran or C++
// should be mirrored in the other language.
//
//-----------------------------------------------------------------------------
//

// for printf
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC_Base.h"
#include "ESMC_IOSpec.h"

// in this case, no body for file, all in class
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_IOSpec.C,v 1.7.4.1 2010/02/05 19:58:12 svasquez Exp $";
//-----------------------------------------------------------------------------

//

// One single (private) symbol to shut up some of the compilers which 
// warn about a .o file with no contents.  remove this when real
// interfaces are added.
static ESMC_IOSpec* default_IOS;

