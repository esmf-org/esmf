// $Id: ESMC_Xform.C,v 1.1 2003/01/07 21:37:36 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Xform method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Xform methods declared
// in the companion file ESMC_Xform.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Xform.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Xform.C,v 1.1 2003/01/07 21:37:36 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Xform routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_XformCreate - Create a new Xform
//
// !INTERFACE:
      ESMC_Xform *ESMC_XformCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Xform
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Xform from ... Allocates memory for a new Xform
//      object and uses the internal routine ESMC_XformContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_XformInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Xform.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_XformCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_XformDestroy - free a Xform created with Create
//
// !INTERFACE:
      int ESMC_XformDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Xform *xform) {    // in - xform object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Xform object previously allocated
//      via an ESMC_XformCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Xform.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformConstruct - fill in an already allocated Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) { 
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Xform object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_XformDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_XformCreate, which calls
//      ESMC_XformConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformDestruct - release resources associated w/a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_XformConstruct, does any additional cleanup before the
//      original Xform object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_XformDestroy, which calls
//      ESMC_XformDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformInit - initializes a Xform object
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) { 
//
// !DESCRIPTION:
//      ESMF routine which only initializes Xform values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_XformCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformGetConfig - get configuration info from a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XformConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Xform object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformSetConfig - set configuration info for a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_XformConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Xform object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_XformSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformGet<Value> - get <Value> for a Xform
//
// !INTERFACE:
      //int ESMC_Xform::ESMC_XformGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Xform member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_XformGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformSet<Value> - set <Value> for a Xform
//
// !INTERFACE:
      //int ESMC_Xform::ESMC_XformSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Xform member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_XformSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformValidate - internal consistency check for a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Xform is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_XformValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformPrint - print contents of a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Xform.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_XformPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Xform - native C++ constructor
//
// !INTERFACE:
      ESMC_Xform::ESMC_Xform(
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

 } // end ESMC_Xform

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Xform - native C++ destructor
//
// !INTERFACE:
      ESMC_Xform::~ESMC_Xform(void) {
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

 } // end ~ESMC_Xform
