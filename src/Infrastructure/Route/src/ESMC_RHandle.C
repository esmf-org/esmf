// $Id: ESMC_RHandle.C,v 1.6 2004/06/08 13:14:14 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
 // insert any higher level, 3rd party or system includes here
 #include "ESMC_Start.h"
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include "ESMC_RHandle.h"
 #include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
       "$Id: ESMC_RHandle.C,v 1.6 2004/06/08 13:14:14 nscollins Exp $";
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
    rhandle1 = NULL;
    rhandle2 = NULL;
    tvalues = NULL;
    label = NULL;

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

    //if (tvalues != NULL) delete [] tvalues;

    return ESMF_SUCCESS;

 } // end ESMC_RouteHandleDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleGet"
//BOP
// !IROUTINE:  ESMC_RouteHandleGet - Get multiple values in one call.
//
// !INTERFACE:
    int ESMC_RouteHandle::ESMC_RouteHandleGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_HandleType *h,            // out - handle type
      ESMC_Route **rh1,              // out - first route table
      ESMC_Route **rh2,              // out - optional second route table
      ESMC_TransformValues **td,     // out - weights, whatever
      char **l) const {              // out - additional name/label

//
// !DESCRIPTION:
//    Query for multiple values in a single call.  (Inline calls exist
//    to return individual items.)
//
//EOP

    if (h) *h = htype;
    if (rh1) *rh1 = rhandle1;
    if (rh2) *rh2 = rhandle2;
    if (td) *td = tvalues;
    if (l) *l = label;

    return ESMF_SUCCESS;

} // end ESMC_RouteHandleGet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleSet"
//BOP
// !IROUTINE:  ESMC_RouteHandleSet - Set multiple values in one call.
//
// !INTERFACE:
    int ESMC_RouteHandle::ESMC_RouteHandleSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_HandleType h,            // in - handle type
      ESMC_Route *rh1,              // in - first route table
      ESMC_Route *rh2,              // in - optional second route table
      ESMC_TransformValues *td,     // in - weights, whatever
      char *l) {                    // in - additional name/label

//
// !DESCRIPTION:
//    Query for multiple values in a single call.  (Inline calls exist
//    to return individual items.)
//
//EOP
    int len;

    if (h) htype = h;
    if (rh1) rhandle1 = rh1;
    if (rh2) rhandle2 = rh2;
    if (td) tvalues = td;
    if (l) {
        len = strlen(l) + 1; 
        if (label) delete [] label;
        label = new char[len];
        strcpy(label, l);
    }

    return ESMF_SUCCESS;

} // end ESMC_RouteHandleSet

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
    char msgbuf[ESMF_MAXSTR];
  
    sprintf(msgbuf, "RouteHandle: '%s'\n", label ? label : "(no name)");
    ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

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
