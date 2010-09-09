// $Id: ESMC_Comp.h,v 1.50 2010/09/09 23:07:39 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Comp_H
#define ESMC_Comp_H

//-----------------------------------------------------------------------------
// !CLASS:  ESMC_Comp - Public C interface to the ESMF Comp class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Comp class and declares global
// variables to be used in user code written in C.
//
//-----------------------------------------------------------------------------


#include "ESMC_Clock.h"
#include "ESMC_State.h"

#ifdef __cplusplus
extern "C" {
#endif

enum ESMC_GridCompType { ESMF_ATM=1, ESMF_LAND, ESMF_OCEAN, ESMF_SEAICE, 
  ESMF_RIVER, ESMF_GRIDCOMPTYPE_UNKNOWN };

enum ESMC_Method { ESMF_SETINIT=1, ESMF_SETRUN, ESMF_SETFINAL,
  ESMF_SETWRITERESTART, ESMF_SETREADRESTART };

// Class declaration type
typedef void* ESMC_GridComp;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridComp - Create a Gridded Component
//
// !INTERFACE:
ESMC_GridComp ESMC_GridCompCreate(
  const char *name,			// in 
  enum ESMC_GridCompType mtype,        // in
  const char *configFile, 		// in
  ESMC_Clock clock, 			// in
  int *rc				// out
);
// !RETURN VALUE:
//  Newly created ESMC_GridComp
//
// !DESCRIPTION:
//
//  Create a {\tt ESMC\_GridComp} object.
//
//  The arguments are:
//  \begin{description}
//  \item[name]
//    Name of the newly-created {\tt ESMC\_GridComp}.
//  \item[mtype]
//   {\tt ESMC\_GridComp} model type, where models includes {\tt ESMF\_ATM}, {\tt ESMF\_LAND}, {\tt ESMF\_OCEAN}, {\tt ESMF\_SEAICE}, {\tt ESMF\_RIVER}, and {\tt ESMF\_GRIDCOMPTYPE\_UNKNOWN}. Note that this has no meaning to the framework, it is an annotation for user code to query. See section ?? for a complete list of valid types. 
//  \item[configFile]
//  The filename of an {\tt ESMF\_Config} format file. If specified, this file is opened an {\tt ESMF\_Config}  configuration object is created for the file, and attached to the new component. 
//  \item[clock]
//   Component-specific {\tt ESMF\_Clock}. This clock is available to be queried and updated by the new {\tt ESMF\_GridComp} as it chooses. This should not be the parent component clock, which should be maintained and passed down to the initialize/run/finalize routines separately. 
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompDestroy - Destroy a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompDestroy(
  ESMC_GridComp *comp		// in
);
// !RETURN VALUE:
//  Return code; equals ESMF\_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_GridComp}.
//
//  The argument is:
//  \begin{description}
//  \item[comp]
//  Release all resources associated with this {\tt ESMC\_GridComp}  and mark the object as invalid. It is an error to pass this object into any other routines after being destroyed. 
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompSetServices - Call user routine to register GridComp methods
//
// !INTERFACE:
int ESMC_GridCompSetServices(
  ESMC_GridComp comp, 			// in
  void (*func)(ESMC_GridComp,		// in
  int *					// in
  ), 	
  int *userRc				// out
);
// !RETURN VALUE:
//  Return code; equals ESMF\_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
// Call into user provided {\tt userRoutine} which is responsible for for setting Component's Initialize(), Run() and Finalize() services.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//  Gridded Component. 
//  \item[userRoutine]
//  Routine to be called.
//  \item[userRc]
//  Return code set by {\tt userRoutine} before returning.
//  
//  The Component writer must supply a subroutine with the exact interface shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine} must not be declared as optional, and the types, intent and order must match.
//
//The {\tt userRoutine}, when called by the framework, must make successive calls to {\tt ESMC\_GridCompSetEntryPoint()} to preset callback routines for standard Component Initialize(), Run() and Finalize() methods. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompSetEntryPoint - Set user routine as entry point for standard Component method
//
// !INTERFACE:
int ESMC_GridCompSetEntryPoint(
  ESMC_GridComp comp, 			// in
  enum ESMC_Method method,		// in
  void (*func)(ESMC_GridComp, 		// in
  ESMC_State, 				// in	must not be optional
  ESMC_State, 				// in	must not be optional
  ESMC_Clock *,				// in	must not be optional
  int *					// in   must not be optional
  ),
  int phase				// in
);
//
// !RETURN VALUE:
//  Return code; equals ESMF\_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
// Registers a user-supplied {\tt userRoutine} as the entry point for one of the predefined Component methods. After this call the {\tt userRoutine} becomes accessible via the standard Component method API.
//
//  The arguments are:
//  \begin{description}
//  \item[comp]
//   An {\tt ESMC\_GridComp} object. 
//  \item[method]
//   One of a set of predefined Component methods - e.g. {\tt ESMF\_SETINIT}, {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}. See section ?? for a complete list of valid method options. 
//   \item[userRoutine]
//    The user-supplied subroutine to be associated for this Component {\tt method}. This subroutine does not have to be public. 
//   \item[phase]
//    The phase number for multi-phase methods. For single phase methods the {\tt phase} argument can be omitted. The default setting is 1.
//  \end{description}
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompInitialize - Initialize a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompInitialize(
  ESMC_GridComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompRun - Run a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompRun(
  ESMC_GridComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompFinalize - Finalize a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompFinalize(
  ESMC_GridComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridGetInternalState - Get the Internal State of a Gridded Component
//
// !INTERFACE:
void *ESMC_GridCompGetInternalState(
  ESMC_GridComp comp, 
  int *rc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridSetInternalState - Set the Internal State of a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompSetInternalState(
  ESMC_GridComp comp, 
  void *data);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCompPrint - Set the Internal State of a Gridded Component
//
// !INTERFACE:
int ESMC_GridCompPrint(
  ESMC_GridComp comp, 
  const char *options);
//EOP
//-----------------------------------------------------------------------------
// Class declaration type
typedef void* ESMC_CplComp;

// Class API
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompCreate - Create a Coupler Component
//
// !INTERFACE:
ESMC_CplComp ESMC_CplCompCreate(
  const char *name, 
  const char *configFile, 
  ESMC_Clock clock,
  int *rc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompDestroy - Destroy a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompDestroy(
  ESMC_CplComp *comp);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompDestroy - Destroy a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompSetServices(ESMC_CplComp comp, 
  void (*func)(ESMC_CplComp, int *), int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompSetEntryPoint - Set the Entry point of a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompSetEntryPoint(
  ESMC_CplComp comp, 
  enum ESMC_Method method,
  void (*func)(ESMC_CplComp, 
  ESMC_State, 
  ESMC_State, 
  ESMC_Clock *, 
  int *),
  int phase);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompInitialize - Initialize a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompInitialize(
  ESMC_CplComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompRun - Run a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompRun(
  ESMC_CplComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompFinalize - Finalize a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompFinalize(
  ESMC_CplComp comp, 
  ESMC_State importState,
  ESMC_State exportState, 
  ESMC_Clock clock, 
  int phase, 
  int *userRc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompGetInternalState - Get the internal State of a Coupler Component
//
// !INTERFACE:
void *ESMC_CplCompGetInternalState(
  ESMC_CplComp comp, 
  int *rc);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompSetInternalState - Set the internal State of a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompSetInternalState(
  ESMC_CplComp comp, 
  void *data);
//EOP
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompPrint - Print a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompPrint(
  ESMC_CplComp comp, 
  const char *options);
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Comp_H
