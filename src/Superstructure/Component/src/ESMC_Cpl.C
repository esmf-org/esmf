// $Id: ESMC_Cpl.C,v 1.1 2003/01/07 21:37:32 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Cpl method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Cpl methods declared
// in the companion file ESMC_Cpl.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Cpl.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Cpl.C,v 1.1 2003/01/07 21:37:32 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Cpl routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_CplCreate - Create a new Cpl
//
// !INTERFACE:
      ESMC_Cpl *ESMC_CplCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Cpl
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Cpl from ... Allocates memory for a new Cpl
//      object and uses the internal routine ESMC_CplContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_CplInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Cpl.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_CplCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_CplDestroy - free a Cpl created with Create
//
// !INTERFACE:
      int ESMC_CplDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Cpl *cpl) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Cpl object previously allocated
//      via an ESMC_CplCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Cpl.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplConstruct - fill in an already allocated Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Cpl object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_CplDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_CplCreate, which calls
//      ESMC_CplConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplDestruct - release resources associated w/a Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CplConstruct, does any additional cleanup before the
//      original Cpl object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CplDestroy, which calls
//      ESMC_CplDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplInit - initializes a Cpl object
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which only initializes Cpl values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_CplCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplGetConfig - get configuration info from a Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CplConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Cpl object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplSetConfig - set configuration info for a Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CplConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Cpl object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CplSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplGet<Value> - get <Value> for a Cpl
//
// !INTERFACE:
      //int ESMC_Cpl::ESMC_CplGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Cpl member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_CplGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplSet<Value> - set <Value> for a Cpl
//
// !INTERFACE:
      //int ESMC_Cpl::ESMC_CplSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Cpl member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_CplSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplValidate - internal consistency check for a Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Cpl is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_CplValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplPrint - print contents of a Cpl
//
// !INTERFACE:
      int ESMC_Cpl::ESMC_CplPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Cpl.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_CplPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Cpl - native C++ constructor
//
// !INTERFACE:
      ESMC_Cpl::ESMC_Cpl(
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

 } // end ESMC_Cpl

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Cpl - native C++ destructor
//
// !INTERFACE:
      ESMC_Cpl::~ESMC_Cpl(void) {
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

 } // end ~ESMC_Cpl
