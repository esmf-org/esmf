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

//-------------------------------------------------------------------------
// (all lines below between the !BOP and EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_SciComp_H
#define ESMC_SciComp_H

//-----------------------------------------------------------------------------
// ESMC_SciComp - Public C interface to the ESMF SciComp class
//
// The code in this file defines the public C SciComp class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_SciComp.C} contains
// the definitions (full code bodies) for the SciComp methods.
//-----------------------------------------------------------------------------


#include "ESMC_Clock.h"
#include "ESMC_State.h"

#ifdef __cplusplus
extern "C" 
{
#endif

// Class declaration type
typedef struct
{
  void *ptr;
} ESMC_SciComp;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_SciCompCreate - Create a Science Component
//
// !INTERFACE:
ESMC_SciComp ESMC_SciCompCreate(
  const char *name,                    // in 
  int *rc                              // out
);
// !RETURN VALUE:
//  Newly created ESMC_SciComp object.
//
// !DESCRIPTION:
//
//  This interface creates an {\tt ESMC\_SciComp} object. 
//
//  The arguments are:
//  \begin{description}
//  \item[name]
//    Name of the newly-created {\tt ESMC\_SciComp}.
//  \item[{[rc]}]
//   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_SciCompDestroy - Destroy a Science Component
//
// !INTERFACE:
int ESMC_SciCompDestroy(
  ESMC_SciComp *comp               // inout
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_SciComp}.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    Release all resources associated with this {\tt ESMC\_SciComp} and mark
//    the object as invalid. It is an error to pass this object into any other
//    routines after being destroyed. 
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_SciCompPrint - Print the contents of a SciComp
//
// !INTERFACE:
int ESMC_SciCompPrint(
  ESMC_SciComp comp     // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Prints information about an {\tt ESMC\_SciComp} to {\tt stdout}.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_SciComp} object.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------


#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_SciComp_H
