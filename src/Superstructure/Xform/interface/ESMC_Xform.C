// $Id: ESMC_Xform.C,v 1.3.8.3 2007/10/18 02:44:08 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Xform method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Xform methods declared
// in the companion file ESMC\_Xform.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include "ESMC_Start.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: Transform object
//
// !DESCRIPTION:
//  Transform class which provides interfaces to the Fortran implementation
//    of Transforms.
//EOP
//-----------------------------------------------------------------------------

 // associated class definition file
 #include "ESMC_Xform.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
               "$Id: ESMC_Xform.C,v 1.3.8.3 2007/10/18 02:44:08 cdeluca Exp $";
//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Xform routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformInit - Initialize a new Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,              // in - xform name
      void (*funcp)()) {       // in - xform function to execute
//
// !DESCRIPTION:
//      Initialize a new Xform object.   This is a shallow class.
//
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//
    // TODO: this is remnants of the original template code which
    // was going to do the implementation in C++.  this maybe needs to be
    // turned into a call to the f_esmf_XXX interface code, although it
    // is a shallow class, so it doesn't need to call thru to F90?

    // this->name = name;
    // this->funcp = funcp;
    return ESMF_FAILURE;

 } // end ESMC_XformInit


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformGetConfig - get configuration info from a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XformConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Xform object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_XformGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformSetConfig - set configuration info for a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_XformConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Xform object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_XformSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformGet<Value> - get <Value> for a Xform
//
// !INTERFACE:
      //int ESMC_Xform::ESMC_XformGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Xform member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    //return ESMF_FAILURE;

 //} // end ESMC_XformGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformSet<Value> - set <Value> for a Xform
//
// !INTERFACE:
      //int ESMC_Xform::ESMC_XformSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Xform member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    //return ESMF_FAILURE;

 //} // end ESMC_XformSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformValidate - internal consistency check for a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Xform is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_XformValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XformPrint - print contents of a Xform
//
// !INTERFACE:
      int ESMC_Xform::ESMC_XformPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Xform.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_XformPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Xform - native C++ constructor
//
// !INTERFACE:
      ESMC_Xform::ESMC_Xform(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) { 
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

 } // end ESMC_Xform

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Xform - native C++ destructor
//
// !INTERFACE:
      ESMC_Xform::~ESMC_Xform(void) {
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

 } // end ~ESMC_Xform
