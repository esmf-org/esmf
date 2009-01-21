// $1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC State method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt State} methods declared
// in the companion file {\tt ESMC\_State.h}.  These are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include "ESMC_Start.h"
#include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: State object
//
// !DESCRIPTION:
//  State class which provides interfaces to the Fortran implementation
//    of States.
//EOP
//-----------------------------------------------------------------------------

 // associated class definition file
 #include <ESMC_State.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_State.C,v 1.11.2.2 2009/01/21 21:25:25 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the State routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateCreate - Create a new State
//
// !INTERFACE:
      ESMC_State *ESMC_StateCreate(
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
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_State.h)
//
//EOP
    ESMC_State *state = NULL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_statecreate)(state, name, rc);

    return state;

 } // end ESMC_StateCreate

//-----------------------------------------------------------------------------
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

    int rc;
   
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_statedestroy)(state, &rc);

    return rc;

 } // end ESMC_StateDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateConstruct - fill in an already allocated State
//
// !INTERFACE:
      int ESMC_State::ESMC_StateConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
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
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;


    return rc;

 } // end ESMC_StateConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StateDestruct - release resources associated w/a State
//
// !INTERFACE:
      int ESMC_State::ESMC_StateDestruct(void) {
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
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_StateDestruct

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
      int ESMC_State::ESMC_StateValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
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
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_StateValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_StatePrint - print contents of a State
//
// !INTERFACE:
      int ESMC_State::ESMC_StatePrint(
//
// !RETURN VALUE:
//    int error return code
//
      const char *options) const {     //  in - print options
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
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_StatePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_State - native C++ constructor
//
// !INTERFACE:
      ESMC_State::ESMC_State(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    

 } // end ESMC_State

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_State - native C++ destructor
//
// !INTERFACE:
      ESMC_State::~ESMC_State(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    

 } // end ~ESMC_State
