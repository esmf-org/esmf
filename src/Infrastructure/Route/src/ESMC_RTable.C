// $Id: ESMC_RTable.C,v 1.2 2003/03/11 03:01:03 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC RTable method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RTable methods declared
// in the companion file ESMC_RTable.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_RTable.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_RTable.C,v 1.2 2003/03/11 03:01:03 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the RTable routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableCreate - Create a new RTable
//
// !INTERFACE:
      ESMC_RTable *ESMC_RTableCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_RTable
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new RTable from ... Allocates memory for a new RTable
//      object and uses the internal routine ESMC_RTableConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_RTableInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_RTable.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_RTableCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableDestroy - free a RTable created with Create
//
// !INTERFACE:
      int ESMC_RTableDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RTable *rtable) {    // in - rtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a RTable object previously allocated
//      via an ESMC_RTableCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_RTable.h)
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableConstruct - fill in an already allocated RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableConstruct(
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
//      allocated RTable object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RTableDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RTableCreate, which calls
//      ESMC_RTableConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableDestruct - release resources associated w/a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RTableConstruct, does any additional cleanup before the
//      original RTable object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RTableDestroy, which calls
//      ESMC_RTableDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableInit - initializes a RTable object
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableInit(
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
//      ESMF routine which only initializes RTable values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_RTableCreate.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableGetConfig - get configuration info from a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RTableConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the RTable object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableSetConfig - set configuration info for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_RTableConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the RTable object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableGet<Value> - get <Value> for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of RTable member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableSet<Value> - set <Value> for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the RTable member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_RTableSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableValidate - internal consistency check for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a RTable is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_RTableValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTablePrint - print contents of a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTablePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a RTable.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_RTablePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTable - native C++ constructor
//
// !INTERFACE:
      ESMC_RTable::ESMC_RTable(
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

 } // end ESMC_RTable

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_RTable - native C++ destructor
//
// !INTERFACE:
      ESMC_RTable::~ESMC_RTable(void) {
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

 } // end ~ESMC_RTable
