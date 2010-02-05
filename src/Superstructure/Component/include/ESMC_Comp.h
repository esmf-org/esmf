// $Id: ESMC_Comp.h,v 1.47.2.1 2010/02/05 20:03:54 svasquez Exp $
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
//BOPI
// !CLASS:  ESMC_Comp - Public C interface to the ESMF Comp class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Comp class and declares global
// variables to be used in user code written in C.
//
//EOPI
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
ESMC_GridComp ESMC_GridCompCreate(char *name, enum ESMC_GridCompType mtype,
  char *configFile, ESMC_Clock clock, int *rc);
int ESMC_GridCompDestroy(ESMC_GridComp *comp);
int ESMC_GridCompSetServices(ESMC_GridComp comp, 
  void (*func)(ESMC_GridComp, int *), int *userRc);
int ESMC_GridCompSetEntryPoint(ESMC_GridComp comp, enum ESMC_Method method,
  void (*func)(ESMC_GridComp, ESMC_State, ESMC_State, ESMC_Clock *, int *),
  int phase);
int ESMC_GridCompInitialize(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
int ESMC_GridCompRun(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
int ESMC_GridCompFinalize(ESMC_GridComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
void *ESMC_GridCompGetInternalState(ESMC_GridComp comp, int *rc);
int ESMC_GridCompSetInternalState(ESMC_GridComp comp, void *data);
int ESMC_GridCompPrint(ESMC_GridComp comp, const char *options);

// Class declaration type
typedef void* ESMC_CplComp;

// Class API
ESMC_CplComp ESMC_CplCompCreate(char *name, char *configFile, ESMC_Clock clock,
  int *rc);
int ESMC_CplCompDestroy(ESMC_CplComp *comp);
int ESMC_CplCompSetServices(ESMC_CplComp comp, 
  void (*func)(ESMC_CplComp, int *), int *userRc);
int ESMC_CplCompSetEntryPoint(ESMC_CplComp comp, enum ESMC_Method method,
  void (*func)(ESMC_CplComp, ESMC_State, ESMC_State, ESMC_Clock *, int *),
  int phase);
int ESMC_CplCompInitialize(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
int ESMC_CplCompRun(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
int ESMC_CplCompFinalize(ESMC_CplComp comp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock clock, int phase, int *userRc);
void *ESMC_CplCompGetInternalState(ESMC_CplComp comp, int *rc);
int ESMC_CplCompSetInternalState(ESMC_CplComp comp, void *data);
int ESMC_CplCompPrint(ESMC_CplComp comp, const char *options);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Comp_H
