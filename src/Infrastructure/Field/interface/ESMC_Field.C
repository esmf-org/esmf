// $Id: ESMC_Field.C,v 1.1 2003/03/10 21:54:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Field method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Field methods declared
// in the companion file ESMC_Field.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC.h"

 // associated class definition file
#include "ESMC_Field.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Field.C,v 1.1 2003/03/10 21:54:21 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

// non-method functions
ESMC_Field *ESMC_FieldCreate(int *rc) {
    ESMC_Field *f = new ESMC_Field;
    FTN(f_esmf_fieldcreate)(f, rc);
    return f;
} 

int ESMC_FieldDestroy(ESMC_Field *f) {
    int rc;
    FTN(f_esmf_fielddestroy)(f, &rc);
    delete f;
    f = 0;
    return rc;
}


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldConstruct - fill in an already allocated Field
//
// !INTERFACE:
      int ESMC_Field::ESMC_FieldConstruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Field object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_FieldDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_FieldCreate, which calls
//      ESMC_FieldConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldDestruct - release resources associated w/a Field
//
// !INTERFACE:
      int ESMC_Field::ESMC_FieldDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_FieldConstruct, does any additional cleanup before the
//      original Field object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_FieldDestroy, which calls
//      ESMC_FieldDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldGetConfig - get configuration info from a Field
//
// !INTERFACE:
      //int ESMC_Field::ESMC_FieldGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //ESMC_FieldConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Field object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

// } // end ESMC_FieldGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldSetConfig - set configuration info for a Field
//
// !INTERFACE:
  //    int ESMC_Field::ESMC_FieldSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
   //   const ESMC_FieldConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Field object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

// } // end ESMC_FieldSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldGet<Value> - get <Value> for a Field
//
// !INTERFACE:
 //      int ESMC_Field::ESMC_FieldGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
   //    <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Field member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

  //} // end ESMC_FieldGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldSet<Value> - set <Value> for a Field
//
// !INTERFACE:
    //   int ESMC_Field::ESMC_FieldSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      // <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Field member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 // } // end ESMC_FieldSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldValidate - internal consistency check for a Field
//
// !INTERFACE:
      int ESMC_Field::ESMC_FieldValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Field is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FieldPrint - print contents of a Field
//
// !INTERFACE:
      int ESMC_Field::ESMC_FieldPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Field.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldPrint

