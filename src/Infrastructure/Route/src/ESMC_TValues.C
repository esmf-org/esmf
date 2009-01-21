// $Id: ESMC_TValues.C,v 1.19.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC RHandle method implementation (body) file
#define ESMF_FILENAME "ESMC_TValues"

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
 #include "ESMC_Start.h"
 #include <stdio.h>
 #include <stdlib.h>
 #include "ESMC_LocalArray.h"
 #include "ESMC_LogErr.h"

 // associated class definition file
 #include "ESMC_TValues.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TValues.C,v 1.19.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the TransformValues routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesCreate"
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

    *rc = newrt->ESMC_TransformValuesConstruct(0);

    return newrt;

 } // end ESMC_TransformValuesCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesCreate"
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
      int count,           // in - number of items in each array
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new TransformValues
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

    *rc = newrt->ESMC_TransformValuesConstruct(count);

    return newrt;

 } // end ESMC_TransformValuesCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesDestroy"
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

    // TODO:
    // A flag may needed to indicate to remove the content or the
    // container
    //
    // this will delete the content:
    // tv->ESMC_TransformValuesDestruct();
    //
    // this will delete the container:
    // delete tv;

    delete tv;

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesDestroy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesConstruct"
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
      int count) {        // in - number of items to preallocate space for
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
    int rc;
    int count2 = count * 2;

    numlist = 0;
    if (count > 0) {
        srcindex = ESMC_LocalArray::ESMC_LocalArrayCreate(1, ESMC_TYPEKIND_I4, 
                                         &count, NULL, ESMC_DATA_COPY, 
                                         NULL, &rc);

        dstindex = ESMC_LocalArray::ESMC_LocalArrayCreate(1, ESMC_TYPEKIND_I4, 
                                         &count2, NULL, ESMC_DATA_COPY,
                                         NULL, &rc);

        weights = ESMC_LocalArray::ESMC_LocalArrayCreate(1, ESMC_TYPEKIND_R8, 
                                        &count, NULL, ESMC_DATA_COPY,
                                        NULL, &rc);
    } else {
        srcindex = NULL;
        dstindex = NULL;
        weights = NULL;
    }

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesDestruct"
//BOP
// !IROUTINE:  ESMC_TransformValuesDestruct - release resources associated w/a TransformValues
//
// !INTERFACE:
      int ESMC_TransformValues::ESMC_TransformValuesDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
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
    if (srcindex) ESMC_LocalArray::ESMC_LocalArrayDestroy(srcindex);
    if (dstindex) ESMC_LocalArray::ESMC_LocalArrayDestroy(dstindex);
    if (weights)  ESMC_LocalArray::ESMC_LocalArrayDestroy(weights);

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesGet"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesGet"
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
      int *numlist,                 // out - number of domains in list
      struct c_F90ptr *si,          // out - data for source index list
      struct c_F90ptr *di,          // out - data for destination index list
      struct c_F90ptr *w) const {   // out - data for weights

//
// !DESCRIPTION:
//    Query for multiple values in a single call.  (Inline calls should exist
//    to return individual items.)
//
//EOP

    if (numlist) *numlist = this->numlist;
    if (si) srcindex->ESMC_LocalArrayGetF90Ptr(si);
    if (di) dstindex->ESMC_LocalArrayGetF90Ptr(di);
    if (w) weights->ESMC_LocalArrayGetF90Ptr(w);

    return ESMF_SUCCESS;

} // end ESMC_TransformValuesGet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesSet"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesValidate"
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
    int rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_TransformValuesValidate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValuesPrint"
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
    char msgbuf[ESMF_MAXSTR];
  
    sprintf(msgbuf,"TransformValues: \n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    sprintf(msgbuf,"  number of links: %d\n", numlist);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    sprintf(msgbuf,"  source index:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    srcindex->ESMC_LocalArrayPrint();
    sprintf(msgbuf,"  destination index:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    dstindex->ESMC_LocalArrayPrint();
    sprintf(msgbuf,"  weights:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    weights->ESMC_LocalArrayPrint();

    return ESMF_SUCCESS;

 } // end ESMC_TransformValuesPrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TransformValues()"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_TransformValues()"
//BOP
// !IROUTINE:  ~ESMC_TransformValues - native C++ destructor
//
// !INTERFACE:
      ESMC_TransformValues::~ESMC_TransformValues(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//    none
//
// !DESCRIPTION:
//
//EOP

   // default destructor ok

 } // end ~ESMC_TransformValues
