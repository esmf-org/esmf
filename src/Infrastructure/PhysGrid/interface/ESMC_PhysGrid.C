// $Id: ESMC_PhysGrid.C,v 1.1 2002/11/01 19:53:25 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC PhysGrid method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ PhysGrid methods declared
// in the companion file ESMC_PhysGrid.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_PhysGrid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_PhysGrid.C,v 1.1 2002/11/01 19:53:25 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the PhysGrid routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridCreate - Create a new PhysGrid
//
// !INTERFACE:
      ESMC_PhysGrid *ESMC_PhysGrid::ESMC_PhysGridCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_PhysGrid
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new PhysGrid from ... Allocates memory for a new PhysGrid
//      object and uses the internal routine ESMC_PhysGridContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_PhysGridInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_PhysGridCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridDestroy - free a PhysGrid created with Create
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a PhysGrid object previously allocated
//      via an ESMC_PhysGridCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridConstruct - fill in an already allocated PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridConstruct(
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
//      allocated PhysGrid object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_PhysGridDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_PhysGridCreate, which calls
//      ESMC_PhysGridConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridDestruct - release resources associated w/a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_PhysGridConstruct, does any additional cleanup before the
//      original PhysGrid object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_PhysGridDestroy, which calls
//      ESMC_PhysGridDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridInit - initializes a PhysGrid object
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridInit(
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
//      ESMF routine which only initializes PhysGrid values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_PhysGridCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridGetConfig - get configuration info from a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PhysGridConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the PhysGrid object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridSetConfig - set configuration info for a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_PhysGridConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the PhysGrid object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridGet<Value> - get <Value> for a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of PhysGrid member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridSet<Value> - set <Value> for a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the PhysGrid member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PhysGridSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridValidate - internal consistency check for a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a PhysGrid is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_PhysGridValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGridPrint - print contents of a PhysGrid
//
// !INTERFACE:
      int ESMC_PhysGrid::ESMC_PhysGridPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a PhysGrid.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_PhysGridPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PhysGrid - native C++ constructor
//
// !INTERFACE:
      ESMC_PhysGrid::ESMC_PhysGrid(
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

 } // end ESMC_PhysGrid

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_PhysGrid - native C++ destructor
//
// !INTERFACE:
      ESMC_PhysGrid::~ESMC_PhysGrid(void) {
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

 } // end ~ESMC_PhysGrid
