// $Id: ESMC_Perf.C,v 1.2 2003/03/10 05:14:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Perf method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Perf methods declared
// in the companion file ESMC_Perf.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Perf.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Perf.C,v 1.2 2003/03/10 05:14:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Perf routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_PerfCreate - Create a new Perf
//
// !INTERFACE:
      ESMC_Perf *ESMC_PerfCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Perf
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Perf from ... Allocates memory for a new Perf
//      object and uses the internal routine ESMC_PerfConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_PerfInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Perf.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_PerfCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_PerfDestroy - free a Perf created with Create
//
// !INTERFACE:
      int ESMC_PerfDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Perf *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Perf object previously allocated
//      via an ESMC_PerfCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Perf.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfConstruct - fill in an already allocated Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfConstruct(
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
//      allocated Perf object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_PerfDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_PerfCreate, which calls
//      ESMC_PerfConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfDestruct - release resources associated w/a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_PerfConstruct, does any additional cleanup before the
//      original Perf object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_PerfDestroy, which calls
//      ESMC_PerfDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfInit - initializes a Perf object
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfInit(
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
//      ESMF routine which only initializes Perf values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_PerfCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfGetConfig - get configuration info from a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PerfConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Perf object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfSetConfig - set configuration info for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_PerfConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Perf object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfGet<Value> - get <Value> for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Perf member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfSet<Value> - set <Value> for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Perf member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PerfSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfValidate - internal consistency check for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Perf is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_PerfValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfPrint - print contents of a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Perf.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_PerfPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Perf - native C++ constructor
//
// !INTERFACE:
      ESMC_Perf::ESMC_Perf(
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

 } // end ESMC_Perf

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Perf - native C++ destructor
//
// !INTERFACE:
      ESMC_Perf::~ESMC_Perf(void) {
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

 } // end ~ESMC_Perf
