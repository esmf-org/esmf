// $Id: ESMC_GComp.C,v 1.1 2003/01/29 00:01:11 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC GComp method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ GComp methods declared
// in the companion file ESMC\_GComp.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_GComp.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_GComp.C,v 1.1 2003/01/29 00:01:11 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the GComp routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_GCompCreate - Create a new GComp
//
// !INTERFACE:
      ESMC_GComp *ESMC_GCompCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_GComp
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new GComp from ... Allocates memory for a new GComp
//      object and uses the internal routine ESMC\_GCompContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC\_GCompInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_GComp.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_GCompCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_GCompDestroy - free a GComp created with Create
//
// !INTERFACE:
      int ESMC_GCompDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_GComp *gcomp) {    // in - gridded component object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a GComp object previously allocated
//      via an ESMC\_GCompCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_GComp.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompConstruct - fill in an already allocated GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated GComp object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_GCompDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_GCompCreate, which calls
//      ESMC\_GCompConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompDestruct - release resources associated w/a GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF\_GCompConstruct, does any additional cleanup before the
//      original GComp object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC\_GCompDestroy, which calls
//      ESMC\_GCompDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompInit - initializes a GComp object
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which only initializes GComp values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC\_GCompCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompGetConfig - get configuration info from a GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_GCompConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the GComp object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompSetConfig - set configuration info for a GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_GCompConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the GComp object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GCompSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompGet<Value> - get <Value> for a GComp
//
// !INTERFACE:
      //int ESMC_GComp::ESMC_GCompGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of GComp member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_GCompGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompSet<Value> - set <Value> for a GComp
//
// !INTERFACE:
      //int ESMC_GComp::ESMC_GCompSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the GComp member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_GCompSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompValidate - internal consistency check for a GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a GComp is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_GCompValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GCompPrint - print contents of a GComp
//
// !INTERFACE:
      int ESMC_GComp::ESMC_GCompPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a GComp.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_GCompPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GComp - native C++ constructor
//
// !INTERFACE:
      ESMC_GComp::ESMC_GComp(
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

 } // end ESMC_GComp

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_GComp - native C++ destructor
//
// !INTERFACE:
      ESMC_GComp::~ESMC_GComp(void) {
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

 } // end ~ESMC_GComp
