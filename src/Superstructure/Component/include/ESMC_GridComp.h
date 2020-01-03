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

#ifndef ESMC_GridComp_H
#define ESMC_GridComp_H

//-----------------------------------------------------------------------------
// ESMC_GridComp - Public C interface to the ESMF GridComp class
//
// The code in this file defines the public C GridComp class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Comp.C} contains
// the definitions (full code bodies) for the GridComp methods.
//-----------------------------------------------------------------------------


#include "ESMC_Clock.h"
#include "ESMC_State.h"

#ifdef __cplusplus
extern "C" {
#endif

enum ESMC_Method { ESMF_METHOD_INITIALIZE=1, ESMF_METHOD_RUN,
  ESMF_METHOD_FINALIZE, ESMF_METHOD_WRITERESTART, ESMF_METHOD_READRESTART,
  ESMF_METHOD_SERVICELOOP };

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_GridComp;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompCreate - Create a Gridded Component
//
// !INTERFACE:
ESMC_GridComp ESMC_GridCompCreate(
  const char *name,                    // in 
  const char *configFile,              // in
  ESMC_Clock clock,                    // in
  int *rc                              // out
);
// !RETURN VALUE:
//  Newly created ESMC_GridComp object.
//
// !DESCRIPTION:
//
//  This interface creates an {\tt ESMC\_GridComp} object. By default, a
//  separate VM context will be created for each component.  This implies
//  creating a new MPI communicator and allocating additional memory to
//  manage the VM resources.
//
//  The arguments are:
//  \begin{description}
//  \item[name]
//    Name of the newly-created {\tt ESMC\_GridComp}.
//  \item[configFile]
//   The filename of an {\tt ESMC\_Config} format file. If specified, this file
//   is opened an {\tt ESMC\_Config}  configuration object is created for the
//   file, and attached to the new component. 
//  \item[clock]
//   Component-specific {\tt ESMC\_Clock}. This clock is available to be queried
//   and updated by the new {\tt ESMC\_GridComp} as it chooses. This should not
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
// !IROUTINE: ESMC_GridCompDestroy - Destroy a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompDestroy(
  ESMC_GridComp *comp               // inout
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_GridComp}.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    Release all resources associated with this {\tt ESMC\_GridComp} and mark
//    the object as invalid. It is an error to pass this object into any other
//    routines after being destroyed. 
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompFinalize - Finalize a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompFinalize(
  ESMC_GridComp comp,           // inout
  ESMC_State importState,       // inout
  ESMC_State exportState,       // inout 
  ESMC_Clock clock,             // in
  int phase,                    // in
  int *userRc                   // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Call the associated user finalize code for a GridComp.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    {\tt ESMC\_GridComp} to call finalize routine for.
//  \item[importState]
//    {\tt ESMC\_State} containing import data for coupling.
//  \item[exportState]
//    {\tt ESMC\_State} containing export data for coupling.
//  \item[clock]
//    External {\tt ESMC\_Clock} for passing in time information. This is 
//    generally the parent component's clock, and will be treated as read-only
//    by the child component. The child component can maintain a private clock
//    for its own internal time computations.
//  \item[phase]
//    Component providers must document whether each of their routines are 
//    {\tt single-phase} or {\tt multi-phase}. Single-phase routines require 
//    only one invocation to complete their work. Multi-phase routines provide
//    multiple subroutines to accomplish the work, accommodating components
//    which must complete part of their work, return to the caller and allow 
//    other processing to occur, and then continue the original operation. 
//    For multiple-phase child components, this is the integer phase number to
//    be invoked. For single-phase child components this argument must be 1.
//  \item[{[userRc]}]
//    Return code set by {\tt userRoutine} before returning.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompGetInternalState - Get the Internal State of a Gridded Component
//
// !INTERFACE:
void *ESMC_GridCompGetInternalState(
  ESMC_GridComp comp,           // in
  int *rc                       // out
);
// !RETURN VALUE:
//  Pointer to private data block that is stored in the internal state.
//
// !DESCRIPTION:
//
//  Available to be called by an {\tt ESMC\_GridComp} at any time after 
//  {\tt ESMC\_GridCompSetInternalState} has been called. Since init, run, and
//  finalize must be separate subroutines, data that they need to share in 
//  common can either be global data, or can be allocated in a private data
//  block and the address of that block can be registered with the framework 
//  and retrieved by this call. When running multiple instantiations of an 
//  {\tt ESMC\_GridComp}, for example during ensemble runs, it may be simpler 
//  to maintain private data specific to each run with private data blocks. A 
//  corresponding {\tt ESMC\_GridCompSetInternalState} call sets the data
//  pointer to this block, and this call retrieves the data pointer. 
//
//  Only the {\em last} data block set via {\tt ESMC\_GridCompSetInternalState}
//  will be accessible. 
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_GridComp} object.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompInitialize - Initialize a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompInitialize(
  ESMC_GridComp comp,           // inout
  ESMC_State importState,       // inout
  ESMC_State exportState,       // inout 
  ESMC_Clock clock,             // in
  int phase,                    // in
  int *userRc                   // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Call the associated user initialization code for a GridComp.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    {\tt ESMC\_GridComp} to call initialize routine for.
//  \item[importState]
//    {\tt ESMC\_State} containing import data for coupling.
//  \item[exportState]
//    {\tt ESMC\_State} containing export data for coupling.
//  \item[clock]
//    External {\tt ESMC\_Clock} for passing in time information. This is 
//    generally the parent component's clock, and will be treated as read-only
//    by the child component. The child component can maintain a private clock
//    for its own internal time computations.
//  \item[phase]
//    Component providers must document whether each of their routines are 
//    {\tt single-phase} or {\tt multi-phase}. Single-phase routines require 
//    only one invocation to complete their work. Multi-phase routines provide
//    multiple subroutines to accomplish the work, accommodating components
//    which must complete part of their work, return to the caller and allow 
//    other processing to occur, and then continue the original operation. 
//    For multiple-phase child components, this is the integer phase number to
//    be invoked. For single-phase child components this argument must be 1.
//  \item[{[userRc]}]
//    Return code set by {\tt userRoutine} before returning.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompPrint - Print the contents of a GridComp
//
// !INTERFACE:
int ESMC_GridCompPrint(
  ESMC_GridComp comp     // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Prints information about an {\tt ESMC\_GridComp} to {\tt stdout}.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_GridComp} object.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompRun - Run a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompRun(
  ESMC_GridComp comp,           // inout
  ESMC_State importState,       // inout
  ESMC_State exportState,       // inout 
  ESMC_Clock clock,             // in
  int phase,                    // in
  int *userRc                   // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Call the associated user run code for a GridComp.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    {\tt ESMC\_GridComp} to call run routine for.
//  \item[importState]
//    {\tt ESMC\_State} containing import data for coupling.
//  \item[exportState]
//    {\tt ESMC\_State} containing export data for coupling.
//  \item[clock]
//    External {\tt ESMC\_Clock} for passing in time information. This is 
//    generally the parent component's clock, and will be treated as read-only
//    by the child component. The child component can maintain a private clock
//    for its own internal time computations.
//  \item[phase]
//    Component providers must document whether each of their routines are 
//    {\tt single-phase} or {\tt multi-phase}. Single-phase routines require 
//    only one invocation to complete their work. Multi-phase routines provide
//    multiple subroutines to accomplish the work, accommodating components
//    which must complete part of their work, return to the caller and allow 
//    other processing to occur, and then continue the original operation. 
//    For multiple-phase child components, this is the integer phase number to
//    be invoked. For single-phase child components this argument must be 1.
//  \item[{[userRc]}]
//    Return code set by {\tt userRoutine} before returning.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompSetEntryPoint - Set user routine as entry point for standard Component method
//
// !INTERFACE:
int ESMC_GridCompSetEntryPoint(
  ESMC_GridComp comp,                                               // in
  enum ESMC_Method method,                                          // in
  void (*userRoutine)                                               // in
    (ESMC_GridComp, ESMC_State, ESMC_State, ESMC_Clock *, int *),
  int phase                                                         // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Registers a user-supplied {\tt userRoutine} as the entry point for one of 
//  the predefined Component methods. After this call the {\tt userRoutine} 
//  becomes accessible via the standard Component method API.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_GridComp} object. 
//  \item[method]
//  \begin{sloppypar}
//    One of a set of predefined Component methods 
//    - e.g. {\tt ESMF\_METHOD\_INITIALIZE}, {\tt ESMF\_METHOD\_RUN},
//    {\tt ESMF\_METHOD\_FINALIZE}. See section~\ref{const:cmethod}
//    for a complete list of valid method options. 
//  \end{sloppypar}
//  \item[userRoutine]
//    The user-supplied subroutine to be associated for this Component 
//    {\tt method}. This subroutine does not have to be public. 
//  \item[phase]
//    The phase number for multi-phase methods.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompSetInternalState - Set the Internal State of a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompSetInternalState(
  ESMC_GridComp comp,           // inout
  void *data                    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Available to be called by an {\tt ESMC\_GridComp} at any time, but
//  expected to be most useful when called during the registration process, 
//  or initialization. Since init, run, and finalize must be separate
//  subroutines, data that they need to share in common can either be global
//  data, or can be allocated in a private data block and the address of that 
//  block can be registered with the framework and retrieved by subsequent
//  calls.
//  When running multiple instantiations of an {\tt ESMC\_GridComp}, 
//  for example during ensemble runs, it may be simpler to maintain private 
//  data specific to each run with private data blocks.  A corresponding 
//  {\tt ESMC\_GridCompGetInternalState} call retrieves the data pointer.
//   
//  Only the {\em last} data block set via
//  {\tt ESMC\_GridCompSetInternalState} will be accessible.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    An {\tt ESMC\_GridComp} object.
//  \item[data]
//    Pointer to private data block to be stored.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompSetServices - Call user routine to register GridComp methods
//
// !INTERFACE:
int ESMC_GridCompSetServices(
  ESMC_GridComp comp,                           // in
  void (*userRoutine)(ESMC_GridComp, int *),    // in
  int *userRc                                   // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Call into user provided {\tt userRoutine} which is responsible for setting
//  Component's Initialize(), Run() and Finalize() services.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//    Gridded Component. 
//  \item[userRoutine]
//    Routine to be called.
//  \item[userRc]
//    Return code set by {\tt userRoutine} before returning.
//  \end{description}
//  
//  The Component writer must supply a subroutine with the exact interface shown
//  above for the {\tt userRoutine} argument.
//
//  The {\tt userRoutine}, when called by the framework, must make successive
//  calls to {\tt ESMC\_GridCompSetEntryPoint()} to preset callback routines for
//  standard Component Initialize(), Run() and Finalize() methods. 
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_GridComp_H
