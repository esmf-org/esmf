// $Id: ESMC_Route.C,v 1.1 2003/03/05 17:04:54 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Route method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Route methods declared
// in the companion file ESMC_Route.h
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Route.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
               "$Id: ESMC_Route.C,v 1.1 2003/03/05 17:04:54 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Route routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteCreate - Create a new Route
//
// !INTERFACE:
      ESMC_Route *ESMC_RouteCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Route
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Route from ... Allocates memory for a new Route
//      object and uses the internal routine ESMC_RouteContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_RouteInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Route.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_RouteCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteDestroy - free a Route created with Create
//
// !INTERFACE:
      int ESMC_RouteDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Route *route) {    // in - route object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Route object previously allocated
//      via an ESMC_RouteCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Route.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteConstruct - fill in an already allocated Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Route object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RouteDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RouteCreate, which calls
//      ESMC_RouteConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteDestruct - release resources associated w/a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RouteConstruct, does any additional cleanup before the
//      original Route object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RouteDestroy, which calls
//      ESMC_RouteDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteInit - initializes a Route object
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes Route values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_RouteCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteGetConfig - get configuration info from a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RouteConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Route object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteSetConfig - set configuration info for a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_RouteConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Route object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteGet<Value> - get <Value> for a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Route member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteSet<Value> - set <Value> for a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Route member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RouteSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteValidate - internal consistency check for a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Route is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_RouteValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrint - print contents of a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Route.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_RoutePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Route - native C++ constructor
//
// !INTERFACE:
      ESMC_Route::ESMC_Route(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
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

 } // end ESMC_Route

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Route - native C++ destructor
//
// !INTERFACE:
      ESMC_Route::~ESMC_Route(void) {
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

 } // end ~ESMC_Route
