// $Id: ESMC_Layout.C,v 1.1 2002/10/25 20:20:50 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Layout method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Layout methods declared
// in the companion file ESMC_Layout.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Layout.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Layout.C,v 1.1 2002/10/25 20:20:50 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Layout routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutCreate - Create a new Layout
//
// !INTERFACE:
      ESMC_Layout *ESMC_Layout::ESMC_LayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Layout
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Layout from ... Allocates memory for a new Layout
//      object and uses the internal routine ESMC_LayoutContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_LayoutInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_LayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutDestroy - free a Layout created with Create
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Layout object previously allocated
//      via an ESMC_LayoutCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutConstruct - fill in an already allocated Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutConstruct(
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
//      allocated Layout object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_LayoutDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_LayoutCreate, which calls
//      ESMC_LayoutConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutDestruct - release resources associated w/a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_LayoutConstruct, does any additional cleanup before the
//      original Layout object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_LayoutDestroy, which calls
//      ESMC_LayoutDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutInit - initializes a Layout object
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutInit(
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
//      ESMF routine which only initializes Layout values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_LayoutCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetConfig - get configuration info from a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_LayoutConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Layout object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutSetConfig - set configuration info for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_LayoutConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Layout object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGet<Value> - get <Value> for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Layout member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutSet<Value> - set <Value> for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Layout member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutValidate - internal consistency check for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Layout is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_LayoutValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutPrint - print contents of a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Layout.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_LayoutPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Layout - native C++ constructor
//
// !INTERFACE:
      ESMC_Layout::ESMC_Layout(
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

 } // end ESMC_Layout

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Layout - native C++ destructor
//
// !INTERFACE:
      ESMC_Layout::~ESMC_Layout(void) {
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

 } // end ~ESMC_Layout
