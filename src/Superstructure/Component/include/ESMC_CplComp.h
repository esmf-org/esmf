// $Id: ESMC_CplComp.h,v 1.23 2010/09/15 23:10:00 theurich Exp $
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

#ifndef ESMC_CplComp_H
#define ESMC_CplComp_H

//-----------------------------------------------------------------------------
// ESMC_CplComp - Public C interface to the ESMF CplComp class
//
// The code in this file defines the public C CplComp class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Comp.C} contains
// the definitions (full code bodies) for the CplComp methods.
//-----------------------------------------------------------------------------


#include "ESMC_GridComp.h"

#ifdef __cplusplus
extern "C" {
#endif

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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompDestroy - Destroy a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompDestroy(
  ESMC_CplComp *comp);
//EOP
//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CplCompSetServices - Destroy a Coupler Component
//
// !INTERFACE:
int ESMC_CplCompSetServices(ESMC_CplComp comp, 
  void (*func)(ESMC_CplComp, int *), int *userRc);
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_CplComp_H
