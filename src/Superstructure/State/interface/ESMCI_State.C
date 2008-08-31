//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_State.C"
//==============================================================================
//
// ESMC State method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt State} methods declared
// in the companion file {\tt ESMCI\_State.h}.  These are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
//

// associated header file
#include "ESMCI_State.h"

//insert any higher level, 3rd party or system includes here
#include <string.h>         // strlen()

// LogErr headers
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

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
static const char *const version = "$Id: ESMCI_State.C,v 1.10 2008/08/31 03:09:10 theurich Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {

  void FTN(f_esmf_statecreate)(ESMCI::State* state, char* statename, int* rc,
				ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_stateaddarray)(ESMCI::State* state, ESMCI::Array** array, 
                                 int* rc);

  void FTN(f_esmf_stateprint)(ESMCI::State* state, int* rc);

  void FTN(f_esmf_stategetarray)(ESMCI::State* state, char* name, 
                                 ESMCI::Array** array, int* rc, 
                                 ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_statedestroy)(ESMCI::State* state, int* rc);

};

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the State routines
//
//

namespace ESMCI {
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::create()"
//BOP
// !IROUTINE:  ESMCI::State::create - Create a new State
//
// !INTERFACE:
      State *State::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::State object
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
   //Local variables
    int localrc;
    int nlen;
    char* fName = NULL;

    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    localrc = ESMF_RC_NOT_IMPL;

    // allocate the new State object
    State* state;
    try{
      state = new State;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::State.", rc);
      return ESMC_NULL_POINTER;
    }

    // convert file name to fortran string
    nlen = strlen(name);
    fName = new char[nlen];
    localrc = ESMC_CtoF90string(name, fName, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
      delete[] fName;
      return ESMC_NULL_POINTER;
    }

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_statecreate)(state, fName, &localrc, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
      delete[] fName;
      return ESMC_NULL_POINTER;
    }


    rc = &localrc;
    return state;

 } // end State create


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::addArray()"
//BOP
// !IROUTINE:  ESMCI::State::addArray - Add an array to this state
//
// !INTERFACE:
      int State::addArray(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
     Array *array){       // in - array being added
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
  
      
    // Invoque the fortran interface through the F90-C++ "glue" code
     FTN(f_esmf_stateaddarray)(this, &array, &localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
       return localrc;

     rc = localrc;

     return rc;

   } // end ESMC_StateAddArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getArray()"
//BOP
// !IROUTINE:  ESMCI::State::getArray - Get an array from this state
//
// !INTERFACE:
      int State::getArray(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      char  *name,         // in - array name
      Array **array){      // out - array being geted
//
// !DESCRIPTION:
//      Get an array from an existing state
//
//EOP
      //local variables
      int rc;
      int localrc;
      int nlen;
      char* fName;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;

    // convert file name to fortran string
    nlen = strlen(name);
    fName = new char[nlen];
    localrc = ESMC_CtoF90string(name, fName, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
      delete[] fName;
      return localrc;
    }

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_stategetarray)(this, fName, array, &localrc, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
      delete[] fName;
      return localrc;
    }


      printf("In ESMC_StateGetArray, after  calling the glue \n");

      rc = localrc;
      return rc;

   } // end ESMC_StateGetArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::print()"
//BOP
// !IROUTINE:  ESMCI::State::print - print the internal data for a state

// !INTERFACE:
      int State::print(){

// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
//   none

//  !DESCRIPTION
//    Prints information about the {\tt state} to {\tt stdout}.

    // Local data
    int rc, localrc;

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_stateprint)(this, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return localrc;

    rc = localrc;
    return rc;

} // end State::print

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::destroy()"
//BOP
// !IROUTINE:  ESMCI::State::destroy - free a State created with Create
//
// !INTERFACE:
      int State::destroy(
//
// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
  State *state){
  
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
    int localrc;
   
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_statedestroy)(state, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return localrc;
    
    delete state;

    rc = localrc;
    return rc;

 } // end State::destroy

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
} // namespace ESMCI
