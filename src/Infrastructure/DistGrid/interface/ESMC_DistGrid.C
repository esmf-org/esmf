// $Id: ESMC_DistGrid.C,v 1.1 2002/11/01 18:29:37 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC DistGrid method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DistGrid methods declared
// in the companion file ESMC_DistGrid.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_DistGrid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_DistGrid.C,v 1.1 2002/11/01 18:29:37 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the DistGrid routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridCreate - Create a new DistGrid
//
// !INTERFACE:
      ESMC_DistGrid *ESMC_DistGrid::ESMC_DistGridCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_DistGrid
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new DistGrid from ... Allocates memory for a new DistGrid
//      object and uses the internal routine ESMC_DistGridContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_DistGridInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_DistGridCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridDestroy - free a DistGrid created with Create
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a DistGrid object previously allocated
//      via an ESMC_DistGridCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridConstruct - fill in an already allocated DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridConstruct(
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
//      allocated DistGrid object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_DistGridDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_DistGridCreate, which calls
//      ESMC_DistGridConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridDestruct - release resources associated w/a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_DistGridConstruct, does any additional cleanup before the
//      original DistGrid object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_DistGridDestroy, which calls
//      ESMC_DistGridDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridInit - initializes a DistGrid object
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridInit(
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
//      ESMF routine which only initializes DistGrid values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_DistGridCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridGetConfig - get configuration info from a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DistGridConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the DistGrid object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridSetConfig - set configuration info for a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_DistGridConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the DistGrid object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridGet<Value> - get <Value> for a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of DistGrid member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridSet<Value> - set <Value> for a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the DistGrid member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DistGridSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridValidate - internal consistency check for a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a DistGrid is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_DistGridValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGridPrint - print contents of a DistGrid
//
// !INTERFACE:
      int ESMC_DistGrid::ESMC_DistGridPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a DistGrid.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_DistGridPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DistGrid - native C++ constructor
//
// !INTERFACE:
      ESMC_DistGrid::ESMC_DistGrid(
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

 } // end ESMC_DistGrid

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_DistGrid - native C++ destructor
//
// !INTERFACE:
      ESMC_DistGrid::~ESMC_DistGrid(void) {
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

 } // end ~ESMC_DistGrid
