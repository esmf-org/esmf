// $Id: ESMC_Comm.C,v 1.1 2002/10/25 19:21:33 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Comm method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Comm methods declared
// in the companion file ESMC_Comm.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Comm.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Comm.C,v 1.1 2002/10/25 19:21:33 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Comm routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommCreate - Create a new Comm
//
// !INTERFACE:
      ESMC_Comm *ESMC_Comm::ESMC_CommCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Comm
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Comm from ... Allocates memory for a new Comm
//      object and uses the internal routine ESMC_CommContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_CommInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_CommCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommDestroy - free a Comm created with Create
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Comm object previously allocated
//      via an ESMC_CommCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommConstruct - fill in an already allocated Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommConstruct(
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
//      allocated Comm object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_CommDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_CommCreate, which calls
//      ESMC_CommConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommDestruct - release resources associated w/a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CommConstruct, does any additional cleanup before the
//      original Comm object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CommDestroy, which calls
//      ESMC_CommDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommInit - initializes a Comm object
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommInit(
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
//      ESMF routine which only initializes Comm values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_CommCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommGetConfig - get configuration info from a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CommConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Comm object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommSetConfig - set configuration info for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CommConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Comm object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommGet<Value> - get <Value> for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Comm member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommSet<Value> - set <Value> for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Comm member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommValidate - internal consistency check for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Comm is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_CommValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommPrint - print contents of a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Comm.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_CommPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Comm - native C++ constructor
//
// !INTERFACE:
      ESMC_Comm::ESMC_Comm(
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

 } // end ESMC_Comm

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Comm - native C++ destructor
//
// !INTERFACE:
      ESMC_Comm::~ESMC_Comm(void) {
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

 } // end ~ESMC_Comm
