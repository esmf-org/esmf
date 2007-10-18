// $Id: ESMC_Regrid.C,v 1.4.8.3 2007/10/18 02:43:08 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Regrid method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file is a C++ wrapper for the F90 implemention 
// of the Regrid class.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_Regrid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Regrid.C,v 1.4.8.3 2007/10/18 02:43:08 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Regrid routine wrappers.
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridCreate - Create a new Regrid
//
// !INTERFACE:
      ESMC_Regrid *ESMC_RegridCreate(
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
//      For a deep class, allocates memory for a new Regrid object and 
//      call F90 to create the actual object.  The C++ object contains only
//      a 'this' pointer to the F90 derived type.
//      (for shallow classes define and use ESMC_RegridInit instead.)
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//
     ESMC_Regrid *regrid;

     FTN(f_esmf_regridcreate)(regrid, &arg1, &arg2, &arg3, &rc);

     return regrid;

 } // end ESMC_RegridCreate

//-----------------------------------------------------------------------------

// !IROUTINE:  ESMC_RegridDestroy - free a Regrid created with Create
//
// !INTERFACE:
      int ESMC_RegridDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Regrid *regrid) {    // in - regrid object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Regrid object previously allocated
//      via an ESMC_RegridCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Regrid.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    int rc;

    FTN(f_esmf_regriddestroy)(regrid, &rc);

    return rc;

 } // end ESMC_RegridDestroy


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
//      for deep classes define and use routines Create/Destroy instead.
//      Can be overloaded like ESMC_RegridCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    int rc;

    FTN(f_esmf_regridinit)(this, &arg1, &arg2, &arg3, &rc);

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
    int rc;

    FTN(f_esmf_regridgetconfig)(this, &config, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_regridsetconfig)(this, config, &rc);

    return rc;

 } // end ESMC_RegridSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridGet - get values from a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridGet(
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
    int rc;

    FTN(f_esmf_regridget)(this, &value, &rc);

    return rc;

 } // end ESMC_RegridGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RegridSet - set values in a Regrid
//
// !INTERFACE:
      int ESMC_Regrid::ESMC_RegridSet(
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
    int rc;

    FTN(f_esmf_regridset)(this, value, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_regridvalidate)(this, options, &rc);

    return rc;

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
    int rc;

    FTN(f_esmf_regridprint)(this, options, &rc);

    return rc;

 } // end ESMC_RegridPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Regrid - native C++ constructor
//
// !INTERFACE:
      ESMC_Regrid::ESMC_Regrid(void) {
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
//       Native destructor.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here, if needed.
//

 } // end ~ESMC_Regrid
