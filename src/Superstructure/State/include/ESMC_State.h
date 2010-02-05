// $Id: ESMC_State.h,v 1.28.2.1 2010/02/05 20:04:51 svasquez Exp $
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
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_State_H
#define ESMC_State_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - C interface to the F90 State object
//
// !DESCRIPTION:
//
// The code in this file defines the public C State
// 
//EOP
//-----------------------------------------------------------------------------

#include "ESMC_Array.h"
#include "ESMC_Field.h"

#ifdef __cplusplus
extern "C" {
#endif

// class declaration type
typedef void* ESMC_State;

// Class API
ESMC_State ESMC_StateCreate(char *name, int *rc);
int ESMC_StateDestroy(ESMC_State *state);
int ESMC_StatePrint(ESMC_State state);
int ESMC_StateAddArray(ESMC_State state, ESMC_Array array);
int ESMC_StateAddField(ESMC_State state, ESMC_Field field);
int ESMC_StateGetArray(ESMC_State state, char *name, ESMC_Array *array);
int ESMC_StateGetField(ESMC_State state, char *name, ESMC_Field *field);

#ifdef __cplusplus
} // extern "C"
#endif

// !PRIVATE TYPES:

typedef enum ESMC_StateType {
      ESMC_StateImport=1, ESMC_StateExport,
      ESMC_StateImpExp, ESMC_StateUnknown } ESMC_StateType;

typedef enum ESMC_Objtype { FieldBundle=1, Field=2, Array=3 } ESMC_Objtype;
typedef enum ESMC_Needed { Needed=1, NotNeeded=2 } ESMC_Needed;
typedef enum ESMC_Ready { ReadyToRead=1, ReadyToWrite=2 } ESMC_Ready;


#endif  // ESMC_State_H
