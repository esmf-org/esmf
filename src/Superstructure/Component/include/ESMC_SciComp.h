// $Id: ESMC_SciComp.h,v 1.1 2012/09/07 18:38:44 ksaint Exp $
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
// signatures (prototypes).  The companion file {\tt ESMC\_Comp.C} contains
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
// !IROUTINE: ESMC_SciCompCreate - Create a Gridded Component
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
//  \item[mtype]
//   {\tt ESMC\_SciComp} model type, where models includes {\tt ESMF\_ATM},
//   {\tt ESMF\_LAND}, {\tt ESMF\_OCEAN}, {\tt ESMF\_SEAICE}, {\tt ESMF\_RIVER},
//   and {\tt ESMF\_GRIDCOMPTYPE\_UNKNOWN}. Note that this has no meaning to the
//   framework, it is an annotation for user code to query. See section
//   \ref{opt:gridcomptype} for a complete list of valid types. 
//  \item[configFile]
//   The filename of an {\tt ESMC\_Config} format file. If specified, this file
//   is opened an {\tt ESMC\_Config}  configuration object is created for the
//   file, and attached to the new component. 
//  \item[clock]
//   Component-specific {\tt ESMC\_Clock}. This clock is available to be queried
//   and updated by the new {\tt ESMC\_SciComp} as it chooses. This should not
//   be the parent component clock, which should be maintained and passed down
//   to the initialize/run/finalize routines separately. 
//  \item[{[rc]}]
//   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_SciCompDestroy - Destroy a Gridded Component
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
// !IROUTINE: ESMC_SciCompGetInternalState - Get the Internal State of a Gridded Component
//
// !INTERFACE:
void *ESMC_SciCompGetInternalState(
  ESMC_SciComp comp,           // in
  int *rc                       // out
);
// !RETURN VALUE:
//  Pointer to private data block that is stored in the internal state.
//
// !DESCRIPTION:
//
//  Available to be called by an {\tt ESMC\_SciComp} at any time after 
//  {\tt ESMC\_SciCompSetInternalState} has been called. Since init, run, and
//  finalize must be separate subroutines, data that they need to share in 
//  common can either be global data, or can be allocated in a private data
//  block and the address of that block can be registered with the framework 
//  and retrieved by this call. When running multiple instantiations of an 
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
// !IROUTINE: ESMC_SciCompSetInternalState - Set the Internal State of a Gridded Component
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
//  Available to be called by an {\tt ESMC\_SciComp} at any time, but
//  expected to be most useful when called during the registration process, 
//  or initialization. Since init, run, and finalize must be separate
//  subroutines, data that they need to share in common can either be global
//  data, or can be allocated in a private data block and the address of that 
//  block can be registered with the framework and retrieved by subsequent
//  calls.
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
