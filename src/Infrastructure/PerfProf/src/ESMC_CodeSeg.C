// $Id: ESMC_CodeSeg.C,v 1.1 2002/11/14 18:14:36 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC CodeSeg method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CodeSeg methods declared
// in the companion file ESMC_CodeSeg.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_CodeSeg.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_CodeSeg.C,v 1.1 2002/11/14 18:14:36 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the CodeSeg routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_CodeSegCreate - Create a new CodeSeg
//
// !INTERFACE:
      ESMC_CodeSeg *ESMC_CodeSegCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_CodeSeg
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new CodeSeg from ... Allocates memory for a new CodeSeg
//      object and uses the internal routine ESMC_CodeSegContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_CodeSegInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CodeSeg.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_CodeSegCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_CodeSegDestroy - free a CodeSeg created with Create
//
// !INTERFACE:
      int ESMC_CodeSegDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CodeSeg *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a CodeSeg object previously allocated
//      via an ESMC_CodeSegCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CodeSeg.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegConstruct - fill in an already allocated CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegConstruct(
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
//      allocated CodeSeg object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_CodeSegDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_CodeSegCreate, which calls
//      ESMC_CodeSegConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegDestruct - release resources associated w/a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CodeSegConstruct, does any additional cleanup before the
//      original CodeSeg object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CodeSegDestroy, which calls
//      ESMC_CodeSegDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegInit - initializes a CodeSeg object
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegInit(
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
//      ESMF routine which only initializes CodeSeg values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_CodeSegCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegGetConfig - get configuration info from a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CodeSegConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the CodeSeg object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegSetConfig - set configuration info for a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CodeSegConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the CodeSeg object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegGet<Value> - get <Value> for a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of CodeSeg member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegSet<Value> - set <Value> for a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the CodeSeg member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CodeSegSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegValidate - internal consistency check for a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a CodeSeg is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_CodeSegValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSegPrint - print contents of a CodeSeg
//
// !INTERFACE:
      int ESMC_CodeSeg::ESMC_CodeSegPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a CodeSeg.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_CodeSegPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CodeSeg - native C++ constructor
//
// !INTERFACE:
      ESMC_CodeSeg::ESMC_CodeSeg(
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

 } // end ESMC_CodeSeg

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_CodeSeg - native C++ destructor
//
// !INTERFACE:
      ESMC_CodeSeg::~ESMC_CodeSeg(void) {
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

 } // end ~ESMC_CodeSeg
