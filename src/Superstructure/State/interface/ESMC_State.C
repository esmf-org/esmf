// $Id: ESMC_State.C,v 1.29.2.1 2010/02/05 20:04:52 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
//

// associated header file
#include "ESMC_State.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
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
static const char *const version = "$Id: ESMC_State.C,v 1.29.2.1 2010/02/05 20:04:52 svasquez Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the State routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StateCreate()"
//BOP
// !IROUTINE:  ESMC_StateCreate - Create a new State
//

extern "C" {
// !INTERFACE:
      ESMC_State ESMC_StateCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_State
//
// !ARGUMENTS:
      char *name,          // in - state name
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new State.
//
//EOP
   //Local variables
    int localrc;

    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    localrc = ESMF_RC_NOT_IMPL;

    ESMC_State state = NULL;  // initialize

    // Invoque the C++ interface
    state = (void *)ESMCI::State::create(name, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return state; // bail out

    rc = &localrc;
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
      //local variables
      int rc;
      int localrc;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;
      
      localrc = ((ESMCI::State*)state)->addArray((ESMCI::Array*)array.ptr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,&rc))
        return localrc;

      rc = localrc;
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
      //local variables
      int rc;
      int localrc;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;
      
      localrc = ((ESMCI::State*)state)->addField((ESMCI::Field*)field);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,&rc))
        return localrc;

      rc = localrc;
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
      char* arrayName,     // in - name of Array to get
      ESMC_Array *array){       // out - array to get
//
// !DESCRIPTION:
//      Get an array to an existing state
//
//EOP
      //local variables
      int rc;
      int localrc;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;

      localrc = ((ESMCI::State*)state)->getArray(arrayName,
        (ESMCI::Array**)&(array->ptr));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,&rc))
        return localrc;

      rc = localrc;
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
      char* fieldName,     // in - name of Field to get
      ESMC_Field *field){       // out - Field to get
//
// !DESCRIPTION:
//      Get a Field to an existing state
//
//EOP
      //local variables
      int rc;
      int localrc;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;

      localrc = ((ESMCI::State*)state)->getField(fieldName,
        (ESMCI::Field**)field);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,&rc))
        return localrc;

      rc = localrc;
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
      ESMC_State state) {    // in - state object to destroy
//
// !DESCRIPTION:
//      ESMC routine which prints the internal data of a state
//      via an ESMCI::State::print routine.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_State.h)
//
//EOP
// !REQUIREMENTS:

    int rc, localrc;

    // Invoque the C++ interface
    localrc = ((ESMCI::State*)state)->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return localrc;

    return rc = localrc;

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
// !REQUIREMENTS:  

    int rc, localrc;
    
   
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::State *statep = (ESMCI::State *)(*state);

    // Invoque the C++ interface
    localrc = ESMCI::State::destroy(statep);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return localrc;

    // invalidate pointer
    state = NULL;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
 }  // end ESMC_StateDestroy

}; // extern "C"
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateConstruct - fill in an already allocated State
//
// !INTERFACE:
     // int ESMC_State::ESMC_StateConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
     // void) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated State object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_StateDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_StateCreate, which calls
//      ESMC\_StateConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
   // int rc;

    // Initialize return code; assume routine not implemented
    //rc = ESMC_RC_NOT_IMPL;


    //return rc;

 //} // end ESMC_StateConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateDestruct - release resources associated w/a State
//
// !INTERFACE:
     // int ESMC_State::ESMC_StateDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF\_StateConstruct, does any additional cleanup before the
//      original State object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC\_StateDestroy, which calls
//      ESMC\_StateDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    //int rc;

    // Initialize return code; assume routine not implemented
    //rc = ESMC_RC_NOT_IMPL;

    //return rc;

// } // end ESMC_StateDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateGet<Value> - get <Value> for a State
//
// !INTERFACE:
      //int ESMC_State::ESMC_StateGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of State member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
//  int rc;
//
//  // Initialize return code; assume routine not implemented
//  rc = ESMC_RC_NOT_IMPL;

    //return rc;

 //} // end ESMC_StateGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateSet<Value> - set <Value> for a State
//
// !INTERFACE:
      //int ESMC_State::ESMC_StateSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the State member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
//  int rc;
//
//  // Initialize return code; assume routine not implemented
//  rc = ESMC_RC_NOT_IMPL;

    //return rc;

 //} // end ESMC_StateSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateValidate - internal consistency check for a State
//
// !INTERFACE:
      //int ESMC_State::ESMC_StateValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a State is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    //int rc;

    // Initialize return code; assume routine not implemented
    //rc = ESMC_RC_NOT_IMPL;

    //return rc;

// } // end ESMC_StateValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StatePrint - print contents of a State
//
// !INTERFACE:
     // int ESMC_State::ESMC_StatePrint(
//
// !RETURN VALUE:
//    int error return code
//
      //const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a State.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    //int rc;

    // Initialize return code; assume routine not implemented
    //rc = ESMC_RC_NOT_IMPL;

    //return rc;

// } // end ESMC_StatePrint

//-----------------------------------------------------------------------------
