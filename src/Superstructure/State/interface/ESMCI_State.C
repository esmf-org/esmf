//$1.10 2007/04/26 16:13:59 rosalind Exp $
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
static const char *const version = "$Id: ESMCI_State.C,v 1.16.2.1 2010/02/05 20:04:51 svasquez Exp $";
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

  void FTN(f_esmf_stateaddfield)(ESMCI::State* state, ESMCI::Field* field,
                                 int* rc);
  
  void FTN(f_esmf_stateprint)(ESMCI::State* state, int* rc);

  void FTN(f_esmf_stategetarray)(ESMCI::State* state, char* name, 
                                 ESMCI::Array** array, int* rc, 
                                 ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_stategetfield)(ESMCI::State* state, char* name, 
                                 ESMCI::Field* field, int* rc, 
                                 ESMCI_FortranStrLenArg nlen);
  
  void FTN(f_esmf_statedestroy)(ESMCI::State* state, int* rc);

  void FTN(f_esmf_stategetnumitems)(ESMCI::State* state, 
                                    int*          itemCount, 
                                    int*          rc);

  void FTN(f_esmf_stategetitemnames)(ESMCI::State*              state, 
                                     int*                       numItems, 
                                     char*                      itemNameList, 
                                     ESMCI::ESMC_StateItemType* itemTypeList, 
                                     int*                       rc,
                                     ESMCI_FortranStrLenArg     itemNameLen);
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

    delete[] fName;

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
#define ESMC_METHOD "ESMCI::State::addField()"
//BOP
// !IROUTINE:  ESMCI::State::addField - Add a Field to this state
//
// !INTERFACE:
      int State::addField(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
     Field *field){       // in - Field being added
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
  
      
    // Invoque the fortran interface through the F90-C++ "glue" code
     FTN(f_esmf_stateaddfield)(this, field, &localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
       return localrc;

     rc = localrc;

     return rc;

   } // end ESMC_StateAddField

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


    //  printf("In ESMC_StateGetArray, after  calling the glue \n");

    delete[] fName;
    rc = localrc;
    return rc;

   } // end ESMC_StateGetArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getField()"
//BOP
// !IROUTINE:  ESMCI::State::getField - Get a Field from this state
//
// !INTERFACE:
      int State::getField(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      char  *name,         // in - Field name
      Field **field){      // out - Field being geted
//
// !DESCRIPTION:
//      Get a Field from an existing state
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
    
    //TODO: this leaves a memory leak!!!
    Field *fieldMem = new Field;
    *field = fieldMem;  // point to this new allocation

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_stategetfield)(this, fName, fieldMem, &localrc, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
      delete[] fName;
      return localrc;
    }

    //  printf("In ESMC_StateGetArray, after  calling the glue \n");

    delete[] fName;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getNumItems()"
//BOP
// !IROUTINE:  ESMCI::State::getNumItems - Get the number of items contained
//             in this state
//
// !INTERFACE:
      int State::getNumItems(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      int*   numItems){             // out - number of items in the state
//
// !DESCRIPTION:
//      Get the number of items contained in an existing state
//
//EOP
      //local variables
      int rc;
      int localrc;

      //Initialize return code
      rc = ESMF_RC_NOT_IMPL;
      localrc = ESMF_RC_NOT_IMPL;

      *numItems = 0;

      FTN(f_esmf_stategetnumitems)(this, numItems, &localrc);

      rc = localrc;
      return rc;

   } // end ESMC_StateGetNumItems

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getItemNames()"
//BOP
// !IROUTINE:  ESMCI::State::getItemNames - Get the names of the items 
//             contained in this state
//
// !INTERFACE:
      vector<string> State::getItemNames(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ){  
//
// !DESCRIPTION:
//      Get the number of items contained in an existing state
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

      vector<string>  itemNames;

      //***
      // First, get the number of items in the list.  We need to do this
      // first because we have to allocate the space for the item names
      // before making the call to get the names.
      //***
      int	numItems = 0;
      getNumItems(&numItems);

      //***
      // Allocate the space for the list of names... I'm allocating the
      // maximum amount of space needed.  I'm also creating the array
      // for the item types list.
      //***
      char*	itemNameList = new char[numItems * ESMF_MAXSTR];
      memset(itemNameList, '\0', numItems * ESMF_MAXSTR);

      ESMC_StateItemType   *itemTypeList = new ESMC_StateItemType[numItems];
 
      //***
      // Make the fortran call to get the information from the state
      //***
      //printf("In ESMC_StateGetItemNames, before  calling the glue \n");
      FTN(f_esmf_stategetitemnames)(this, &numItems, 
                                    itemNameList, itemTypeList, 
                                    &localrc, ESMF_MAXSTR);
      //printf("In ESMC_StateGetItemNames, after  calling the glue \n");

      //***
      // Go through the list and add the item names to the vector to be
      // returned.
      //***
      for (int i = 0; i < numItems; ++i)
      {
         char *string_p = itemNameList + i*ESMF_MAXSTR;
         int lastchar;

         // Ignore trailing blanks
         for (lastchar = ESMF_MAXSTR; lastchar == 0; lastchar--)
           if (string_p[lastchar] != ' ') break;

         string thisName(string_p, lastchar);

         //***
         // Add the name to the vector of item names
         //***
         itemNames.push_back(thisName);
      }

      //***
      // Clean up the allocated space
      //***
      delete itemTypeList;
      delete itemNameList;

      rc = localrc;
      return itemNames;

   } // end ESMC_StateGetItemNames

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getNumItems()"
//BOP
// !IROUTINE:  ESMCI::State::getNumItems - Get the number of items of the 
//             specified type contained in this state
//
// !INTERFACE:
      int State::getNumItems(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      int*                numItems,   // out - number of items in the state
      ESMC_StateItemType  itemType){  // in - the item type
//
// !DESCRIPTION:
//      Get the number of items of the specified type contained in an 
//      existing state
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

      *numItems = 0;

      //***
      // First, get the number of items in the list.  We need to do this
      // first because we have to allocate the space for the item names
      // before making the call to get the names.
      //***
      int	maxItems = 0;
      getNumItems(&maxItems);

      //***
      // Create the array for the item types list... we need this array so
      // so that we can count only those items of a specified type.
      //***
      char*	itemNameList = new char[maxItems * ESMF_MAXSTR];
      memset(itemNameList, '\0', maxItems * ESMF_MAXSTR);

      ESMC_StateItemType   *itemTypeList = new ESMC_StateItemType[maxItems];
 
      //***
      // Make the fortran call to get the information from the state
      //***
      FTN(f_esmf_stategetitemnames)(this, &maxItems, 
                                    itemNameList, itemTypeList, 
                                    &localrc, ESMF_MAXSTR);

      //***
      // Go through the list and update the item count if the item type
      // matches the specified item type.
      //***
      for (int i = 0; i < maxItems; ++i)
      {
         //printf("Item Type[%d]: %d\n", i, itemTypeList[i]);
         if (itemTypeList[i] == itemType)
         {
            ++(*numItems);
         }
      }

      //***
      // Clean up the allocated space
      //***
      delete itemTypeList;
      delete itemNameList;

      rc = localrc;
      return rc;

   } // end ESMC_StateGetNumItems

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getItemNames()"
//BOP
// !IROUTINE:  ESMCI::State::getItemNames - Get the names of the items 
//             of the specified type contained in this state
//
// !INTERFACE:
      vector<string> State::getItemNames(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ESMC_StateItemType  itemType){     // in - the item type
//
// !DESCRIPTION:
//      Get the names of the items of the specified type contained in 
//      an existing state
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

      vector<string>  itemNames;

      //***
      // First, get the number of items in the list.  We need to do this
      // first because we have to allocate the space for the item names
      // before making the call to get the names.
      //***
      int	numItems = 0;
      getNumItems(&numItems);

      //***
      // Allocate the space for the list of names... I'm allocating the
      // maximum amount of space needed.  I'm also creating the array
      // for the item types list.
      //***
      char*	itemNameList = new char[numItems * ESMF_MAXSTR];
      memset(itemNameList, '\0', numItems * ESMF_MAXSTR);

      ESMC_StateItemType   *itemTypeList = new ESMC_StateItemType[numItems];
 
      //***
      // Make the fortran call to get the information from the state
      //***
      //printf("In ESMC_StateGetItemNames, before  calling the glue \n");
      FTN(f_esmf_stategetitemnames)(this, &numItems, 
                                    itemNameList, itemTypeList, 
                                    &localrc, ESMF_MAXSTR);
      //printf("In ESMC_StateGetItemNames, after  calling the glue \n");

      //***
      // Go through the list and add the item names to the vector to be
      // returned.
      //***
      for (int i = 0; i < numItems; ++i)
      {
         //printf("Item Type[%d]: %d\n", i, itemTypeList[i]);
         if (itemTypeList[i] == itemType)
         {
           char *string_p = itemNameList + i*ESMF_MAXSTR;
           int lastchar;

           // Ignore trailing blanks
           for (lastchar = ESMF_MAXSTR; lastchar == 0; lastchar--)
             if (string_p[lastchar] != ' ') break;

           string thisName(string_p, lastchar);

           //***
           // Add the name to the vector of item names
           //***
           itemNames.push_back(thisName);
         }
      }

      //***
      // Clean up the allocated space
      //***
      delete itemTypeList;
      delete itemNameList;

      rc = localrc;
      return itemNames;

   } // end ESMC_StateGetItemNames

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getNumArrays()"
//BOP
// !IROUTINE:  ESMCI::State::getNumArrays - Get the number of arrays contained
//             in this state
//
// !INTERFACE:
      int State::getNumArrays(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      int* numArrays){      // out - number of arrays in the state
//
// !DESCRIPTION:
//      Get the number of arrays contained in an existing state
//
//EOP

      return getNumItems(numArrays, ESMC_STATEITEM_ARRAY);

   } // end ESMC_StateGetNumArrays

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::State::getArrayNames()"
//BOP
// !IROUTINE:  ESMCI::State::getArrayNames - Get the names of the arrays 
//             contained in this state
//
// !INTERFACE:
      vector<string> State::getArrayNames(
//
// !RETURN VALUE:
//     return code rc.
//
// !ARGUMENTS:
      ){      // out - list of names of arrays in the state
//
// !DESCRIPTION:
//      Get the number of arrays contained in an existing state
//
//EOP

      return getItemNames(ESMC_STATEITEM_ARRAY);

   } // end ESMC_StateGetArrayNames

//-----------------------------------------------------------------------------
} // namespace ESMCI
