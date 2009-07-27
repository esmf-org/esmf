// $Id: ESMC_RHandle.C,v 1.25 2009/07/27 23:23:34 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC RHandle method implementation (body) file
#define ESMF_FILENAME "ESMC_RHandle.C"

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
// associated class definition file
#include "ESMC_RHandle.h"

// higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// ESMF headers
#include "ESMC_Start.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_ArrayBundle.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
       "$Id: ESMC_RHandle.C,v 1.25 2009/07/27 23:23:34 theurich Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the RouteHandle routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleCreate"
//BOP
// !IROUTINE:  ESMC_RouteHandleCreate - Create a new RouteHandle
//
// !INTERFACE:
      ESMC_RouteHandle *ESMC_RouteHandleCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_RouteHandle
//
// !ARGUMENTS:
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new RouteHandle
//      object and uses the internal routine ESMC_RouteHandleConstruct to
//      initialize it. 
//
//EOP

    ESMC_RouteHandle *newrt = new ESMC_RouteHandle();

    *rc = newrt->ESMC_RouteHandleConstruct();

    return newrt;

 } // end ESMC_RouteHandleCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleDestroy"
//BOP
// !IROUTINE:  ESMC_RouteHandleDestroy - free a RouteHandle created with Create
//
// !INTERFACE:
      int ESMC_RouteHandleDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RouteHandle *rhandle) {    // in - rtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a RouteHandle object previously allocated
//      via an ESMC_RouteHandleCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_RouteHandle.h)
//
//EOP

    rhandle->ESMC_RouteHandleDestruct();
    delete rhandle;                      // delete container
    return ESMF_SUCCESS;

 } // end ESMC_RouteHandleDestroy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleConstruct"
//BOP
// !IROUTINE:  ESMC_RouteHandleConstruct - fill in an already allocated RouteHandle
//
// !INTERFACE:
      int ESMC_RouteHandle::ESMC_RouteHandleConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) { 
// 
// !DESCRIPTION: 
//      ESMF routine which fills in the contents of an already
//      allocated RouteHandle object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RouteHandleDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RouteHandleCreate, which calls
//      ESMC_RouteHandleConstruct.  Define for deep classes only.
//
//EOP

    htype = ESMC_UNINITIALIZEDHANDLE;
    storage = NULL;

    return ESMF_SUCCESS;

 } // end ESMC_RouteHandleConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleDestruct"
//BOP
// !IROUTINE:  ESMC_RouteHandleDestruct - release resources associated w/a RouteHandle
//
// !INTERFACE:
      int ESMC_RouteHandle::ESMC_RouteHandleDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RouteHandleConstruct, does any additional cleanup before the
//      original RouteHandle object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RouteHandleDestroy, which calls
//      ESMC_RouteHandleDestruct.  Define for deep classes only.
//
//EOP

    int i;

    // call into the respective distributed data class to release route handle
    switch (htype){
      case ESMC_ARRAYXXE:
        ESMCI::Array::sparseMatMulRelease(this);
        break;
      case ESMC_ARRAYBUNDLEXXE:
        ESMCI::ArrayBundle::sparseMatMulRelease(this);
        break;
      default:
        break;
    }

    return ESMF_SUCCESS;

 } // end ESMC_RouteHandleDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleValidate"
//BOP
// !IROUTINE: ESMC_RouteHandleValidate - validate a handle
//
// !INTERFACE:
      int ESMC_RouteHandle::ESMC_RouteHandleValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a RouteHandle is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP

    return ESMF_FAILURE;

 } // end ESMC_RouteHandleValidate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandlePrint"
//BOP
// !IROUTINE:  ESMC_RouteHandlePrint - print contents of a RouteHandle
//
// !INTERFACE:
      int ESMC_RouteHandle::ESMC_RouteHandlePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a RouteHandle.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
    int i;

    return ESMF_SUCCESS;

 } // end ESMC_RouteHandlePrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandle()"
//BOP
// !IROUTINE:  ESMC_RouteHandle - native C++ constructor
//
// !INTERFACE:
      ESMC_RouteHandle::ESMC_RouteHandle(
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


 } // end ESMC_RouteHandle

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_RouteHandle()"
//BOP
// !IROUTINE:  ~ESMC_RouteHandle - native C++ destructor
//
// !INTERFACE:
      ESMC_RouteHandle::~ESMC_RouteHandle(
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

   // default destructor ok

 } // end ~ESMC_RouteHandle
 
 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
