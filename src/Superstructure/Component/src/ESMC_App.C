// $Id: ESMC_App.C,v 1.1 2003/01/07 21:37:27 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC App method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ App methods declared
// in the companion file ESMC_App.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_App.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_App.C,v 1.1 2003/01/07 21:37:27 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the App routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_AppCreate - Create a new App
//
// !INTERFACE:
      ESMC_App *ESMC_AppCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_App
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new App from ... Allocates memory for a new App
//      object and uses the internal routine ESMC_AppContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_AppInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_App.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_AppCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_AppDestroy - free a App created with Create
//
// !INTERFACE:
      int ESMC_AppDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_App *app) {    // in - application object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a App object previously allocated
//      via an ESMC_AppCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_App.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppConstruct - fill in an already allocated App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated App object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_AppDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_AppCreate, which calls
//      ESMC_AppConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppDestruct - release resources associated w/a App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_AppConstruct, does any additional cleanup before the
//      original App object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_AppDestroy, which calls
//      ESMC_AppDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppInit - initializes a App object
//
// !INTERFACE:
      int ESMC_App::ESMC_AppInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) { 
//
// !DESCRIPTION:
//      ESMF routine which only initializes App values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_AppCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppGetConfig - get configuration info from a App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_AppConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the App object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppSetConfig - set configuration info for a App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_AppConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the App object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_AppSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppGet<Value> - get <Value> for a App
//
// !INTERFACE:
      //int ESMC_App::ESMC_AppGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of App member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_AppGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppSet<Value> - set <Value> for a App
//
// !INTERFACE:
      //int ESMC_App::ESMC_AppSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the App member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_AppSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppValidate - internal consistency check for a App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a App is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_AppValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AppPrint - print contents of a App
//
// !INTERFACE:
      int ESMC_App::ESMC_AppPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a App.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_AppPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_App - native C++ constructor
//
// !INTERFACE:
      ESMC_App::ESMC_App(
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

 } // end ESMC_App

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_App - native C++ destructor
//
// !INTERFACE:
      ESMC_App::~ESMC_App(void) {
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

 } // end ~ESMC_App
