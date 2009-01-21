// $Id: ESMC_RHandle.C,v 1.17.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
#include "ESMC_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_ArrayBundle.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
       "$Id: ESMC_RHandle.C,v 1.17.2.4 2009/01/21 21:25:23 cdeluca Exp $";
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
    nroutes = 0;
    rmapping = ESMC_UNKNOWNHANDLEMAP;
    routes = NULL;
    rmap = NULL;
    ntvalues = 0;
    tvmapping = ESMC_UNKNOWNHANDLEMAP;
    tvalues = NULL;
    tvmap = NULL;
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

    int i;

    // call into the respective distributed data class to release route handle
    switch (htype){
      case ESMC_ARRAYXXE:
        ESMCI::Array::sparseMatMulRelease(this);
        if (routes != NULL) delete [] routes;
        break;
      case ESMC_ARRAYBUNDLEXXE:
        ESMCI::ArrayBundle::sparseMatMulRelease(this);
        if (routes != NULL) delete [] routes;
        break;
      default:
        for (i=0; i<nroutes; i++) {
          routes[i].ESMC_RouteDestruct();
        }
        if (routes != NULL) delete [] routes;
        break;
    }

    for (i=0; i<ntvalues; i++) {
       tvalues[i].ESMC_TransformValuesDestruct();
    }
    if (tvalues != NULL) delete [] tvalues;

    if (rmap != NULL) delete [] rmap;
    if (label != NULL) delete [] label;
    if (tvmap != NULL) delete [] tvmap;
    
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
      int *rt_count,                 // out - count of route tables
      ESMC_HandleMapping *rmaptype,  // out - type of route map
      int which_rt,                  // in - which route table to return
      ESMC_Route **rh,               // out - route table
      int *tv_count,                 // out - count of trans vals
      ESMC_HandleMapping *tvmaptype, // out - type of trans vals map
      int which_tv,                  // in - which transform values to return
      ESMC_TransformValues **td,     // out - regrid weight info
      char **l) const {              // out - additional name/label

//
// !DESCRIPTION:
//    Query for multiple values in a single call.  (Inline calls exist
//    to return individual items.)
//
//EOP

    if (h) *h = htype;
    if (rt_count) *rt_count = nroutes;
    if (rmaptype) *rmaptype = rmapping;
    if (rh) {
        if (which_rt >= nroutes) *rh = NULL;
        else *rh = &routes[which_rt];
    }
    if (tv_count) *tv_count = ntvalues;
    if (tvmaptype) *tvmaptype = tvmapping;
    if (td) {
        if (which_tv >= ntvalues) *td = NULL;
        else *td = &tvalues[which_tv];
    }
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
      int rt_count,                 // in - how many rtables to allocate
      ESMC_HandleMapping rmaptype,  // in - kind of route map
      int which_rt,                 // in - which route to set
      ESMC_Route *rh,               // in - route table list
      int tv_count,                 // in - how many tvs to allocate
      ESMC_HandleMapping tvmaptype, // in - kind of tv map
      int which_tv,                 // in - which tv to set
      ESMC_TransformValues *td,     // in - weights, whatever
      char *l) {                    // in - additional name/label

//
// !DESCRIPTION:
//    Set multiple values in a single call.  (Inline calls exist
//    to set individual items.)   For this version of the call, all
//    values must be set except for the route and td values themselves.
//
//EOP
    int i, len;

    htype = h;
    nroutes = rt_count;
    rmapping = rmaptype;
    if (rt_count > 0) {
        if (routes) delete [] routes;
        routes = new ESMC_Route[rt_count];
        if (rh) 
            for (i=0; i<rt_count; i++)
                routes[i] = rh[i];
    }
    ntvalues = tv_count;
    tvmapping = tvmaptype;
    if (tv_count > 0) {
	if (tvalues) delete [] tvalues;
        tvalues = new ESMC_TransformValues[tv_count]; 
        if (td)
            for (i=0; i<tv_count; i++)
                tvalues[i] = td[i];
    }
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
#define ESMC_METHOD "ESMC_RouteHandleSetRouteCount"
//BOP
// !IROUTINE:  ESMC_RouteHandleSetRouteCount - Set number of routes
//
// !INTERFACE:
    int ESMC_RouteHandle::ESMC_RouteHandleSetRouteCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rtcount) {                // in - how many rtables to allocate

//
// !DESCRIPTION:
//    Set number of routes to allocate.
//
//EOP
    int i;

    nroutes = rtcount;
    if (nroutes > 0) {
        if (routes) delete [] routes;
        routes = new ESMC_Route[nroutes];
    } else {
        if (routes) delete [] routes;
        routes = NULL;
    }

    return ESMF_SUCCESS;

} // end ESMC_RouteHandleSetRouteCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleSetTVCount"
//BOP
// !IROUTINE:  ESMC_RouteHandleSetTVCount - Set number of transform vals
//
// !INTERFACE:
    int ESMC_RouteHandle::ESMC_RouteHandleSetTVCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int tvcount) {                // in - how many TVs to allocate

//
// !DESCRIPTION:
//    Set number of transform value tables to allocate.
//
//EOP
    int i;

    ntvalues = tvcount;
    if (ntvalues > 0) {
	if (tvalues) delete [] tvalues;
        tvalues = new ESMC_TransformValues[ntvalues]; 
    } else {
	if (tvalues) delete [] tvalues;
        tvalues = NULL;
    }

    return ESMF_SUCCESS;

} // end ESMC_RouteHandleSetTVCount

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
    char msgbuf[ESMF_MAXSTR];
  
    sprintf(msgbuf, "RouteHandle: '%s'\n", label ? label : "(no name)");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    printf(" number of routes = %d\n", nroutes);
    for (i=0; i<nroutes; i++)
	routes[i].ESMC_RoutePrint(options);
    printf(" number of transform values = %d\n", ntvalues);
    for (i=0; i<ntvalues; i++)
        ; // tvalues[i].ESMC_TransformValuesPrint(options);
    // TODO: this is commented out because TVPrint does not seem to be
    // working.

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

 int ESMC_RouteHandle::ESMC_RouteHandleSetLabel(char *l) {
        int len = strlen(l) + 1;
        if (label != NULL) delete [] label;
        label = new char[len]; 
        strcpy(label, l);
        return ESMF_SUCCESS;
}

