// $Id: ESMC_Grid.C,v 1.1 2002/11/01 17:54:41 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Grid method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Grid methods declared
// in the companion file ESMC_Grid.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Grid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Grid.C,v 1.1 2002/11/01 17:54:41 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Grid routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCreate - Create a new Grid
//
// !INTERFACE:
      ESMC_Grid *ESMC_Grid::ESMC_GridCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Grid
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Grid from ... Allocates memory for a new Grid
//      object and uses the internal routine ESMC_GridContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_GridInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_GridCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridDestroy - free a Grid created with Create
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Grid object previously allocated
//      via an ESMC_GridCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridConstruct - fill in an already allocated Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridConstruct(
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
//      allocated Grid object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_GridDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_GridCreate, which calls
//      ESMC_GridConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridDestruct - release resources associated w/a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_GridConstruct, does any additional cleanup before the
//      original Grid object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_GridDestroy, which calls
//      ESMC_GridDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridInit - initializes a Grid object
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridInit(
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
//      ESMF routine which only initializes Grid values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_GridCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridGetConfig - get configuration info from a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_GridConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Grid object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridSetConfig - set configuration info for a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_GridConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Grid object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridGet<Value> - get <Value> for a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Grid member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridSet<Value> - set <Value> for a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Grid member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_GridSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridValidate - internal consistency check for a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Grid is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_GridValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridPrint - print contents of a Grid
//
// !INTERFACE:
      int ESMC_Grid::ESMC_GridPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Grid.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_GridPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Grid - native C++ constructor
//
// !INTERFACE:
      ESMC_Grid::ESMC_Grid(
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

 } // end ESMC_Grid

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Grid - native C++ destructor
//
// !INTERFACE:
      ESMC_Grid::~ESMC_Grid(void) {
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

 } // end ~ESMC_Grid
