// $Id: ESMC_CommTable.C,v 1.2 2003/03/11 03:01:03 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC CommTable method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CommTable methods declared
// in the companion file ESMC_CommTable.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_CommTable.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_CommTable.C,v 1.2 2003/03/11 03:01:03 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the CommTable routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableCreate - Create a new CommTable
//
// !INTERFACE:
      ESMC_CommTable *ESMC_CommTableCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_CommTable
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new CommTable from ... Allocates memory for a new CommTable
//      object and uses the internal routine ESMC_CommTableConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_CommTableInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CommTable.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_CommTableCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableDestroy - free a CommTable created with Create
//
// !INTERFACE:
      int ESMC_CommTableDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CommTable *commtable) {    // in - commtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a CommTable object previously allocated
//      via an ESMC_CommTableCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CommTable.h)
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableConstruct - fill in an already allocated CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableConstruct(
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
//      allocated CommTable object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_CommTableDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_CommTableCreate, which calls
//      ESMC_CommTableConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableDestruct - release resources associated w/a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CommTableConstruct, does any additional cleanup before the
//      original CommTable object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CommTableDestroy, which calls
//      ESMC_CommTableDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableInit - initializes a CommTable object
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableInit(
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
//      ESMF routine which only initializes CommTable values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_CommTableCreate.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableGetConfig - get configuration info from a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CommTableConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the CommTable object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableSetConfig - set configuration info for a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CommTableConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the CommTable object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableGet<Value> - get <Value> for a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of CommTable member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableSet<Value> - set <Value> for a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the CommTable member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_CommTableSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableValidate - internal consistency check for a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a CommTable is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_CommTableValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTablePrint - print contents of a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTablePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a CommTable.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_CommTablePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTable - native C++ constructor
//
// !INTERFACE:
      ESMC_CommTable::ESMC_CommTable(
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

 } // end ESMC_CommTable

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_CommTable - native C++ destructor
//
// !INTERFACE:
      ESMC_CommTable::~ESMC_CommTable(void) {
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

 } // end ~ESMC_CommTable
