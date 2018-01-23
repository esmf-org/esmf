// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_State.C"
//==============================================================================
//
// ESMC State method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C State methods declared
// in the companion file ESMC_State.h
//
//-----------------------------------------------------------------------------
// associated header file
#include "ESMC_State.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_State.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: State object
//
// !DESCRIPTION:
//  State class which provides interfaces to the Fortran implementation
//    of States.
//EOP
//-----------------------------------------------------------------------------

// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the State routines
//
//

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateCreate()"
//BOP
// !IROUTINE:  ESMC_StateCreate - Create a new State
//

// !INTERFACE:
      ESMC_State ESMC_StateCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_State
//
// !ARGUMENTS:
      const char *name,    // in - state name
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new State.
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

    ESMC_State state;

    // call into ESMCI method 
    state.ptr = (void *)ESMCI::State::create(name, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      state.ptr = NULL; // invalidate
      return state; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return state;

 } // end ESMC_StateCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateAddArray()"
//BOP
// !IROUTINE:  ESMC_StateAddArray - Add an array to this state
//
// !INTERFACE:
      int ESMC_StateAddArray(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ESMC_State state,    // in - state
      ESMC_Array array){       // in - array being added
//
// !DESCRIPTION:
//      Add an array to an existing state
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = ((ESMCI::State*)state.ptr)->addArray((ESMCI::Array*)array.ptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_StateAddArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateAddField()"
//BOP
// !IROUTINE:  ESMC_StateAddField - Add a field to this state
//
// !INTERFACE:
      int ESMC_StateAddField(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ESMC_State state,     // in - state
      ESMC_Field field){    // in - field being added
//
// !DESCRIPTION:
//      Add a Field to an existing state
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = ((ESMCI::State*)state.ptr)->addField((ESMCI::Field*)(field.ptr));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

  } // end ESMC_StateAddField

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateGetArray()"
//BOP
// !IROUTINE:  ESMC_StateGetArray - Get an array to this state
//
// !INTERFACE:
      int ESMC_StateGetArray(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ESMC_State state,    // in - state
      const char* arrayName,    // in - name of Array to get
      ESMC_Array *array){       // out - array to get
//
// !DESCRIPTION:
//      Get an array to an existing state
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = ((ESMCI::State*)state.ptr)->getArray(arrayName,
      (ESMCI::Array**)&(array->ptr));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

  } // end ESMC_StateGetArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateGetField()"
//BOP
// !IROUTINE:  ESMC_StateGetField - Get an Field to this state
//
// !INTERFACE:
      int ESMC_StateGetField(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ESMC_State state,    // in - state
      const char* fieldName,    // in - name of Field to get
      ESMC_Field *field){       // out - Field to get
//
// !DESCRIPTION:
//      Get a Field to an existing state
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = ((ESMCI::State*)state.ptr)->getField(fieldName,
      (ESMCI::Field**)&(field->ptr));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

  } // end ESMC_StateGetArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StatePrint()"
//BOP
// !IROUTINE:  ESMC_StatePrint - print the internal data for a State
//
// !INTERFACE:
      int ESMC_StatePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_State state) {    // in - state object to print
//
// !DESCRIPTION:
//      ESMC routine which prints the internal data of a state
//      via an ESMCI::State::print routine.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_State.h)
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = ((ESMCI::State*)state.ptr)->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

  } // end ESMC_StatePrint


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateDestroy()"
//BOP
// !IROUTINE:  ESMC_StateDestroy - free a State created with Create
//
// !INTERFACE:
      int ESMC_StateDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_State *state) {    // in - state object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a State object previously allocated
//      via an ESMC\_StateCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_State.h)
//
//EOP
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    
    localrc = ESMCI::State::destroy((ESMCI::State*)(state->ptr));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return localrc;

    // invalidate pointer
    state->ptr = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
 }  // end ESMC_StateDestroy

}; // extern "C"
