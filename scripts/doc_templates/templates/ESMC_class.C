// $Id: ESMC_class.C,v 1.1 2002/10/07 16:28:55 eschwab Exp $
//
// ESMC <Class> method code (body) file
//
// < Something here from legal about the status of the code, like:
//  This code developed by NASA/NCAR/ESMC whatever, and is covered by
//  the terms of the GNU public license.  See license file for more details. >
//

//-------------------------------------------------------------------------
//
// !PURPOSE:
//
// The code in this file implements the C++ <Class> methods defined
// in the companion file ESMC_<Class>.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//
//-------------------------------------------------------------------------
//

#include <ESMC_<Class>.h>

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_class.C,v 1.1 2002/10/07 16:28:55 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the <Class> routines
//
//

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      ESMC_<Class> *ESMC_<Class>::ESMC_<Class>Create(
//
// !ARGUMENTS:
      int arg1,      // input arg1
      int arg2,      // input arg2
      char *arg3) {  // input arg3
//
// !DESCRIPTION:
//      Create a new <Class> from ... Allocates memory for a new <Class>
//      object and uses the internal routine ESMC_<Class>Contruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_<Class>Init.
//      Can be multiple overloaded methods with the same name, but
//      different argument lists.
//
// !REQUIREMENTS:  AAAn.n.n
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Create

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Destroy(void) {
//
// !DESCRIPTION:
//      ESMF routine which destroys a <Class> object previously allocated
//      via an ESMC_<Class>Create routine.  Define for deep classes only.
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Destroy

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Construct(
//
// !ARGUMENTS:
      int arg1,      // input arg1
      int arg2,      // input arg2
      char *arg3) {  // input arg3
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated <Class> object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_<Class>Destruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_<Class>Create, which calls
//      ESMC_<Class>Construct.  Define for deep classes only.
//
// !REQUIREMENTS:  AAAn.n.n
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Construct

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Destruct(void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_<Class>Construct, does any additional cleanup before the
//      original <Class> object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_<Class>Destroy, which calls
//      ESMC_<Class>Destruct.  Define for deep classes only.
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Destruct

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Init(
//
// !ARGUMENTS:
      int arg1,      // input arg1
      int arg2,      // input arg2
      char *arg3) {  // input arg3
//
// !DESCRIPTION:
//      ESMF routine which only initializes <Class> values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_<Class>Create.
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Init

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>SetConfig(
//
// !ARGUMENTS:
      ESMC_<Class>Config *config) {     // input resources
//
// !DESCRIPTION:
//    Configures the <Class> object with set of resources given.
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>SetConfig

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>GetConfig(
//
// !ARGUMENTS:
      ESMC_<Class>Config *config) {     // output resources
//
// !DESCRIPTION:
//    Returns the set of resources the <Class> object was configured with.
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>GetConfig

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Set<Value>(
//
// !ARGUMENTS:
      <value type> *value) {     // input value
//
// !DESCRIPTION:
//     Sets the <Class> member <Value> with the given value.
//     Can be multiple routines, one per value
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Set<Value>

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Get<Value>(
//
// !ARGUMENTS:
      <value type> *value) {     // output value
//
// !DESCRIPTION:
//     Returns the value of <Class> member <Value>.
//     Can be multiple routines, one per value
//
// !REQUIREMENTS:  developer's guide for classes
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Get<Value>

//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Validate(
//
// !ARGUMENTS:
      char *options) {
//
// !DESCRIPTION:
//      Validates that a <Class> is internally consistent.
//      Returns error code if problems are found.
//
// !REQUIREMENTS:  XXXn.n, YYYn.n
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Validate


//-------------------------------------------------------------------------
//BOP
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Print(
//
// !ARGUMENTS:
      char *options) {
//
// !DESCRIPTION:
//      Print information about a <Class>.  The options control the
//      type of information and level of detail.
//
// !REQUIREMENTS:  SSSn.n, GGGn.n
//EOP

//
//  code goes here
//

} // end ESMC_<Class>Print
