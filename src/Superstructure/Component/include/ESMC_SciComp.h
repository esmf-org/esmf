// $Id: ESMC_SciComp.h,v 1.2 2012/09/19 20:35:22 ksaint Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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
//  This interface creates an {\tt ESMC\_SciComp} object. By default, a
//  separate VM context will be created for each component.  This implies
//  creating a new MPI communicator and allocating additional memory to
//  manage the VM resources.
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
// !IROUTINE: ESMC_SciCompGetInternalState - Get the Internal State of a Science Component
//
// !INTERFACE:
void *ESMC_SciCompGetInternalState(
  ESMC_SciComp comp,            // in
  int *rc                       // out
);
// !RETURN VALUE:
//  Pointer to private data block that is stored in the internal state.
//
// !DESCRIPTION:
//
//  Available to be called by an {\tt ESMC\_SciComp} at any time after 
//  {\tt ESMC\_SciCompSetInternalState} has been called. 
//  When running multiple instantiations of an 
//  {\tt ESMC\_SciComp}, for example during ensemble runs, it may be simpler 
//  to maintain private data specific to each run with private data blocks. A 
//  corresponding {\tt ESMC\_SciCompSetInternalState} call sets the data
//  pointer to this block, and this call retrieves the data pointer. 
//
//  Only the {\em last} data block set via {\tt ESMC\_SciCompSetInternalState}
//  will be accessible. 
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_SciComp} object.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_SciCompSetInternalState - Set the Internal State of a Science Component
//
// !INTERFACE:
int ESMC_SciCompSetInternalState(
  ESMC_SciComp comp,           // inout
  void *data                    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Available to be called by an {\tt ESMC\_SciComp} at any time.
//  When running multiple instantiations of an {\tt ESMC\_SciComp}, 
//  for example during ensemble runs, it may be simpler to maintain private 
//  data specific to each run with private data blocks.  A corresponding 
//  {\tt ESMC\_SciCompGetInternalState} call retrieves the data pointer.
//   
//  Only the {\em last} data block set via
//  {\tt ESMC\_SciCompSetInternalState} will be accessible.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_SciComp} object.
//  \item[data]
//    Pointer to private data block to be stored.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------


#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_SciComp_H
