// $Id: ESMC_Timer.C,v 1.2 2003/03/10 05:14:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Timer method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Timer methods declared
// in the companion file ESMC_Timer.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Timer.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Timer.C,v 1.2 2003/03/10 05:14:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Timer routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_TimerCreate - Create a new Timer
//
// !INTERFACE:
      ESMC_Timer *ESMC_TimerCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Timer
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Timer from ... Allocates memory for a new Timer
//      object and uses the internal routine ESMC_TimerConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_TimerInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Timer.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_TimerCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_TimerDestroy - free a Timer created with Create
//
// !INTERFACE:
      int ESMC_TimerDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Timer *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Timer object previously allocated
//      via an ESMC_TimerCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Timer.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerConstruct - fill in an already allocated Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerConstruct(
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
//      allocated Timer object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_TimerDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_TimerCreate, which calls
//      ESMC_TimerConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerDestruct - release resources associated w/a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_TimerConstruct, does any additional cleanup before the
//      original Timer object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_TimerDestroy, which calls
//      ESMC_TimerDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerInit - initializes a Timer object
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerInit(
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
//      ESMF routine which only initializes Timer values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_TimerCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetConfig - get configuration info from a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimerConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Timer object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerSetConfig - set configuration info for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_TimerConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Timer object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGet<Value> - get <Value> for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Timer member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerSet<Value> - set <Value> for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Timer member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_TimerSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerValidate - internal consistency check for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Timer is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_TimerValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerPrint - print contents of a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Timer.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_TimerPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Timer - native C++ constructor
//
// !INTERFACE:
      ESMC_Timer::ESMC_Timer(
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

 } // end ESMC_Timer

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Timer - native C++ destructor
//
// !INTERFACE:
      ESMC_Timer::~ESMC_Timer(void) {
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

 } // end ~ESMC_Timer
