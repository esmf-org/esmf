// $Id: ESMC_class.C,v 1.20.2.3 2010/02/01 20:48:45 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC <Class> method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ <Class> methods declared
// in the companion file ESMC_<Class>.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_<Class>.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_class.C,v 1.20.2.3 2010/02/01 20:48:45 svasquez Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the <Class> routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Create - Create a new <Class>
//
// !INTERFACE:
      ESMC_<Class> *ESMC_<Class>Create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_<Class>
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new <Class> from ... Allocates memory for a new <Class>
//      object and uses the internal routine ESMC_<Class>Construct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_<Class>Init.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_<Class>.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_<Class>Create

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Destroy - free a <Class> created with Create
//
// !INTERFACE:
      int ESMC_<Class>Destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_<Class> *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a <Class> object previously allocated
//      via an ESMC_<Class>Create routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_<Class>.h)
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Destroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Construct - fill in an already allocated <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Construct(
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
//      allocated <Class> object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_<Class>Destruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_<Class>Create, which calls
//      ESMC_<Class>Construct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Construct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Destruct - release resources associated w/a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Destruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_<Class>Construct, does any additional cleanup before the
//      original <Class> object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_<Class>Destroy, which calls
//      ESMC_<Class>Destruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Destruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Init - initializes a <Class> object
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Init(
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
//      ESMF routine which only initializes <Class> values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_<Class>Create.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Init

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>GetConfig - get configuration info from a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>GetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_<Class>Config *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the <Class> object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>GetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>SetConfig - set configuration info for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>SetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_<Class>Config *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the <Class> object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>SetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Get<Value> - get <Value> for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Get<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of <Class> member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Get<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Set<Value> - set <Value> for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Set<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the <Class> member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_<Class>Set<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Validate - internal consistency check for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a <Class> is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_<Class>Validate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Print - print contents of a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a <Class>.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_<Class>Print

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class> - native C++ constructor
//
// !INTERFACE:
      ESMC_<Class>::ESMC_<Class>(
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

 } // end ESMC_<Class>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_<Class> - native C++ destructor
//
// !INTERFACE:
      ESMC_<Class>::~ESMC_<Class>(void) {
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

 } // end ~ESMC_<Class>
