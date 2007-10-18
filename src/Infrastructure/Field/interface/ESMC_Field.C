// $Id: ESMC_Field.C,v 1.5.8.3 2007/10/18 02:42:39 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Field method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Field} methods declared
// in the companion file ESMC_Field.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"

 // associated class definition file
#include "ESMC_Field.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Field.C,v 1.5.8.3 2007/10/18 02:42:39 cdeluca Exp $";
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
//      allocated {\tt ESMC\_Field} object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_FieldDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_FieldCreate}, which calls
//      {\tt ESMC\_FieldConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

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
//      {\tt ESMF\_FieldConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_Field} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_FieldDestroy}, which calls
//      {\tt ESMC\_FieldDestruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldDestruct

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
//     Returns the value of {\tt ESMC\_Field} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

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
//     Sets the {\tt ESMC\_Field} member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

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
//      Validates that a {\tt ESMC\_Field} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
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
//      Print information about a {\tt ESMC\_Field}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_FieldPrint

