// $Id: ESMC_Machine.C,v 1.1 2002/10/25 19:21:33 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Machine method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Machine methods declared
// in the companion file ESMC_Machine.h
//
// The ESMF Machine class stores information about the architecture and
// operating environment of a platform in a generic way.  Only one 
// Machine is created per application.
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Machine.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Machine.C,v 1.1 2002/10/25 19:21:33 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Machine routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineCreate - Create a new Machine
//
// !INTERFACE:
      ESMC_Machine *ESMC_Machine::ESMC_MachineCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Machine
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Machine. Allocates memory for a new Machine
//      object and uses the internal routine ESMC_MachineContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_MachineInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//

 } // end ESMC_MachineCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineDestroy - free a Machine created with Create
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Machine object previously allocated
//      via an ESMC_MachineCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineConstruct - fill in an already allocated Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineConstruct(
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
//      allocated Machine object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_MachineDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_MachineCreate, which calls
//      ESMC_MachineConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineDestruct - release resources associated w/a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_MachineConstruct, does any additional cleanup before the
//      original Machine object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_MachineDestroy, which calls
//      ESMC_MachineDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineInit - initializes a Machine object
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineInit(
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
//      ESMF routine which only initializes Machine values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_MachineCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetConfig - get configuration info from a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_MachineConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Machine object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSetConfig - set configuration info for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_MachineConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Machine object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGet<Value> - get <Value> for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Machine member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSet<Value> - set <Value> for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Machine member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_MachineSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineValidate - internal consistency check for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Machine is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_MachineValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachinePrint - print contents of a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachinePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Machine.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_MachinePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Machine - native C++ constructor
//
// !INTERFACE:
      ESMC_Machine::ESMC_Machine(
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

 } // end ESMC_Machine

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Machine - native C++ destructor
//
// !INTERFACE:
      ESMC_Machine::~ESMC_Machine(void) {
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

 } // end ~ESMC_Machine
