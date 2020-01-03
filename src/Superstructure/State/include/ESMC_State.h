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
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_State_H
#define ESMC_State_H

//-----------------------------------------------------------------------------
// ESMC_State - C interface to the F90 State object
//
// The code in this file defines the public C State
// 
//-----------------------------------------------------------------------------

#include "ESMC_Array.h"
#include "ESMC_Field.h"

#ifdef __cplusplus
extern "C" {
#endif

// class declaration type
typedef struct{
  void *ptr;
}ESMC_State;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateAddArray - Add an Array object to a State
//
// !INTERFACE:
int ESMC_StateAddArray(
  ESMC_State state,   // in
  ESMC_Array array    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Add an Array object to a {\tt ESMC\_State} object.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State object.
//  \item[array]
//    The Array object to be included within the State.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateAddField - Add a Field object to a State
//
// !INTERFACE:
int ESMC_StateAddField(
  ESMC_State state,   // in
  ESMC_Field field    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Add an Array object to a {\tt ESMC\_State} object.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State object.
//  \item[array]
//    The Array object to be included within the State.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateCreate - Create an Array
//
// !INTERFACE:
ESMC_State ESMC_StateCreate(
  const char *name,  // in
  int *rc            // out
);
// !RETURN VALUE:
//  Newly created ESMC_State object.
//
// !DESCRIPTION:
//
//  Create an {\tt ESMC\_State} object.
//
//  The arguments are:
//  \begin{description}
//  \item[{[name]}]
//    The name for the State object. If not specified, i.e. NULL,
//    a default unique name will be generated: "StateNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[rc]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateDestroy - Destroy a State
//
// !INTERFACE:
int ESMC_StateDestroy(
  ESMC_State *state    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Destroy a {\tt ESMC\_State} object.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State to be destroyed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateGetArray - Obtains an Array object from a State
//
// !INTERFACE:
int ESMC_StateGetArray(
  ESMC_State state,    // in
  const char *name,    // in
  ESMC_Array *array    // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Obtain a pointer to an {\tt ESMC\_Array} object contained within
//  a State.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State object.
//  \item[name]
//    The name of the desired Array object.
//  \item[array]
//    A pointer to the Array object.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StateGetField - Obtains a Field object from a State
//
// !INTERFACE:
int ESMC_StateGetField(
  ESMC_State state,    // in
  const char *name,    // in
  ESMC_Field *field    // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Obtain a pointer to a {\tt ESMC\_Field} object contained within
//  a State.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State object.
//  \item[name]
//    The name of the desired Field object.
//  \item[array]
//    A pointer to the Field object.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_StatePrint - Print the contents of a State
//
// !INTERFACE:
int ESMC_StatePrint(
  ESMC_State state    // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Prints the contents of a {\tt ESMC\_State} object.
//
//  The arguments are:
//  \begin{description}
//  \item[state]
//    The State to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

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
