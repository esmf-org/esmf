// $Id: ESMC_Regrid.C,v 1.1 2002/11/01 19:59:00 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Regrid method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Regrid methods declared
// in the companion file ESMC_Regrid.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Regrid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Regrid.C,v 1.1 2002/11/01 19:59:00 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Regrid routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridCreate - Create a new Regrid
//
// !INTERFACE:
      ESMC_Regrid *ESMC_Regrid::ESMC_RegridCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Regrid
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Regrid from ... Allocates memory for a new Regrid
//      object and uses the internal routine ESMC_RegridContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_RegridInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_RegridCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridDestroy - free a Regrid created with Create
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Regrid object previously allocated
//      via an ESMC_RegridCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridConstruct - fill in an already allocated Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridConstruct(
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
//      allocated Regrid object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RegridDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RegridCreate, which calls
//      ESMC_RegridConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridDestruct - release resources associated w/a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RegridConstruct, does any additional cleanup before the
//      original Regrid object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RegridDestroy, which calls
//      ESMC_RegridDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridInit - initializes a Regrid object
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridInit(
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
//      ESMF routine which only initializes Regrid values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_RegridCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridGetConfig - get configuration info from a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RegridConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Regrid object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridSetConfig - set configuration info for a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_RegridConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Regrid object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridGet<Value> - get <Value> for a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Regrid member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridSet<Value> - set <Value> for a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Regrid member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_RegridSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridValidate - internal consistency check for a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Regrid is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_RegridValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridPrint - print contents of a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Regrid.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_RegridPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Regrid - native C++ constructor
//
// !INTERFACE:
      ESMC_Regrid::ESMC_Regrid(
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

 } // end ESMC_Regrid

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Regrid - native C++ destructor
//
// !INTERFACE:
      ESMC_Regrid::~ESMC_Regrid(void) {
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

 } // end ~ESMC_Regrid
