// $Id: inter_ESMC_class.C,v 1.5.2.3 2010/02/01 20:48:49 svasquez Exp $
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
// The code in this file is a C++ wrapper for the F90 implemention 
// of the <Class> class.
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
 static const char *const version = "$Id: inter_ESMC_class.C,v 1.5.2.3 2010/02/01 20:48:49 svasquez Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the <Class> routine wrappers.
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
//      For a deep class, allocates memory for a new <Class> object and 
//      call F90 to create the actual object.  The C++ object contains only
//      a 'this' pointer to the F90 derived type.
//      (for shallow classes define and use ESMC_<Class>Init instead.)
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//
     ESMC_<Class> *<class>;

     FTN(f_esmf_<class>create)(<class>, &arg1, &arg2, &arg3, &rc);

     return <class>;

 } // end ESMC_<Class>Create

//-----------------------------------------------------------------------------

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
    int rc;

    FTN(f_esmf_<class>destroy)(<class>, &rc);

    return rc;

 } // end ESMC_<Class>Destroy


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
//      for deep classes define and use routines Create/Destroy instead.
//      Can be overloaded like ESMC_<Class>Create.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    int rc;

    FTN(f_esmf_<class>init)(this, &arg1, &arg2, &arg3, &rc);

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
    int rc;

    FTN(f_esmf_<class>getconfig)(this, &config, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_<class>setconfig)(this, config, &rc);

    return rc;

 } // end ESMC_<Class>SetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Get - get values from a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Get(
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
    int rc;

    FTN(f_esmf_<class>get)(this, &value, &rc);

    return rc;

 } // end ESMC_<Class>Get

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class>Set - set values in a <Class>
//
// !INTERFACE:
      int ESMC_<Class>::ESMC_<Class>Set(
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
    int rc;

    FTN(f_esmf_<class>set)(this, value, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_<class>validate)(this, options, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_<class>print)(this, options, &rc);

    return rc;

 } // end ESMC_<Class>Print

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_<Class> - native C++ constructor
//
// !INTERFACE:
      ESMC_<Class>::ESMC_<Class>(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      none
//
// !DESCRIPTION:
//      Default constructor.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here, if needed.
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
//       Native destructor.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here, if needed.
//

 } // end ~ESMC_<Class>
