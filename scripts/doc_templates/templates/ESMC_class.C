// $Id: ESMC_class.C,v 1.2 2002/10/09 21:37:58 eschwab Exp $
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

// associated class definition file
#include <ESMC_<Class>.h>

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_class.C,v 1.2 2002/10/09 21:37:58 eschwab Exp $";
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
// !IROUTINE:  ESMC_<Class>Create - Create a new <Class>
//
// !INTERFACE:
      ESMC_<Class> *ESMC_<Class>::ESMC_<Class>Create(
//
// !ARGUMENTS:
      int arg1,      // in
      int arg2,      // in
      char *arg3     // in
      int rc) {      // out 
//
// !DESCRIPTION:
//      Create a new <Class> from ... Allocates memory for a new <Class>
//      object and uses the internal routine ESMC_<Class>Contruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_<Class>Init.
//      Can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

} // end ESMC_<Class>Create

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Destroy - free a <Class> created with Create
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Destroy(void) {
//
// !ARGUMENTS: none
//
// !DESCRIPTION:
//      ESMF routine which destroys a <Class> object previously allocated
//      via an ESMC_<Class>Create routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>Destroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Construct - fill in an already allocated <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Construct(
//
// !ARGUMENTS:
      int arg1,      // in
      int arg2,      // in
      char *arg3) {  // in
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
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

} // end ESMC_<Class>Construct

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Destruct - release resources associated w/a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Destruct(void) {
//
// !ARGUMENTS: none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_<Class>Construct, does any additional cleanup before the
//      original <Class> object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_<Class>Destroy, which calls
//      ESMC_<Class>Destruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>Destruct

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Init - initializes a <Class> object
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Init(
//
// !ARGUMENTS:
      int arg1,      // in
      int arg2,      // in
      char *arg3) {  // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes <Class> values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_<Class>Create.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>Init

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>SetConfig - set configuration info for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>SetConfig(
//
// !ARGUMENTS:
      const ESMC_<Class>Config *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the <Class> object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>SetConfig

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>GetConfig - get configuration info from a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>GetConfig(
//
// !ARGUMENTS:
      ESMC_<Class>Config *config) {     // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the <Class> object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>GetConfig

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Set<Value> - set <Value> for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Set<Value>(
//
// !ARGUMENTS:
      const <value type> *value) {     // in - value
//
// !DESCRIPTION:
//     Sets the <Class> member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>Set<Value>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Get<Value> - get <Value> for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Get<Value>(
//
// !ARGUMENTS:
      <value type> *value) {     // out - value
//
// !DESCRIPTION:
//     Returns the value of <Class> member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

} // end ESMC_<Class>Get<Value>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Validate - internal consistency check for a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Validate(
//
// !ARGUMENTS:
      const char *options) {    // in
//
// !DESCRIPTION:
//      Validates that a <Class> is internally consistent.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

} // end ESMC_<Class>Validate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Print - print contents of a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Print(
//
// !ARGUMENTS:
      const char *options) {     //  in
//
// !DESCRIPTION:
//      Print information about a <Class>.  The options control the
//      type of information and level of detail.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

} // end ESMC_<Class>Print
