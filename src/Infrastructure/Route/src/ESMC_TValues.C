// $Id: ESMC_TValues.C,v 1.2 2003/09/25 16:28:06 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC RHandle method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RHandle methods declared
// in the companion file ESMC_RHandle.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include "ESMC.h"
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include "ESMC_TValues.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TValues.C,v 1.2 2003/09/25 16:28:06 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the TransformValues routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesCreate - Create a new TransformValues
//
// !INTERFACE:
      ESMC_TransformValues *ESMC_TransformValuesCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_TransformValues
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new TransformValues from ... Allocates memory for a new TransformValues
//      object and uses the internal routine ESMC_TransformValuesConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_TransformValuesInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_TransformValues.h)
//
//EOP

    ESMC_TransformValues *newrt = new ESMC_TransformValues();

    *rc = newrt->ESMC_TransformValuesConstruct();

    return newrt;

 } // end ESMC_TransformValuesCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesDestroy - free a TransformValues created with Create
//
// !INTERFACE:
      int ESMC_TransformValuesDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TransformValues *tv) {    // in - rtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a TransformValues object previously allocated
//      via an ESMC_TransformValuesCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_TransformValues.h)
//
//EOP

    tv->ESMC_TransformValuesDestruct();
    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesConstruct - fill in an already allocated TransformValues
//
// !INTERFACE:
      int ESMC_TransformValues::ESMC_TransformValuesConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) { 
// 
// !DESCRIPTION: 
//      ESMF routine which fills in the contents of an already
//      allocated TransformValues object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_TransformValuesDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_TransformValuesCreate, which calls
//      ESMC_TransformValuesConstruct.  Define for deep classes only.
//
//EOP

    numlist = 0;
    srcindex = NULL;
    dstindex = NULL;
    weights = NULL;

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesDestruct - release resources associated w/a TransformValues
//
// !INTERFACE:
      int ESMC_TransformValues::ESMC_TransformValuesDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_TransformValuesConstruct, does any additional cleanup before the
//      original TransformValues object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_TransformValuesDestroy, which calls
//      ESMC_TransformValuesDestruct.  Define for deep classes only.
//
//EOP

    // TODO:  do we own the things we are pointing to?  should we destroy
    // them here??

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesDestruct


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesGet - Get multiple values in one call.
//
// !INTERFACE:
    int ESMC_TransformValues::ESMC_TransformValuesGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *numlist,                  // out - number of domains in list
      ESMC_LocalArray **si,          // out - source index list
      ESMC_LocalArray **di,          // out - destination index list
      ESMC_LocalArray **w) const {   // out - weights

//
// !DESCRIPTION:
//    Query for multiple values in a single call.  (Inline calls should exist
//    to return individual items.)
//
//EOP

    if (numlist) *numlist = this->numlist;
    if (si) *si = srcindex;
    if (di) *di = dstindex;
    if (w) *w = weights;

    return ESMF_SUCCESS;

} // end ESMC_TransformValuesGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesSet - Set multiple values in one call.
//
// !INTERFACE:
    int ESMC_TransformValues::ESMC_TransformValuesSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int numlist,                  // in - number of domains in list
      ESMC_LocalArray *si,          // in - source index list
      ESMC_LocalArray *di,          // in - destination index list
      ESMC_LocalArray *w) {         // in - weights

//
// !DESCRIPTION:
//    Set multiple values in a single call.  (Inline calls should exist
//    to set individual items.)
//
//EOP
    int len;

    this->numlist = numlist;
    srcindex = si;
    dstindex = di;
    weights = w;

    return ESMF_SUCCESS;

} // end ESMC_TransformValuesSet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TransformValuesValidate - validate a handle
//
// !INTERFACE:
      int ESMC_TransformValues::ESMC_TransformValuesValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a TransformValues is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP

    return ESMF_FAILURE;

 } // end ESMC_TransformValuesValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValuesPrint - print contents of a TransformValues
//
// !INTERFACE:
      int ESMC_TransformValues::ESMC_TransformValuesPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a TransformValues.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
  
    printf("TransformValues: \n");
    printf(" <to be coded real soon now> \n");

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TransformValues - native C++ constructor
//
// !INTERFACE:
      ESMC_TransformValues::ESMC_TransformValues(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP


 } // end ESMC_TransformValues

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TransformValues - native C++ destructor
//
// !INTERFACE:
      ESMC_TransformValues::~ESMC_TransformValues(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP

   // default destructor ok

 } // end ~ESMC_TransformValues
