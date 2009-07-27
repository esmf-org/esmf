// $Id: ESMC_RHandle_F.C,v 1.21 2009/07/27 23:23:34 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_RHandle.h"
#include "ESMCI_LocalArray.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt RHandle} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {


//------------------------------------------------------------------------------
//  RouteHandle interfaces

       void FTN(c_esmc_routehandlecreate)(ESMC_RouteHandle **ptr, int *status){
           *ptr = ESMC_RouteHandleCreate(status);
       }

       void FTN(c_esmc_routehandledestroy)(ESMC_RouteHandle **ptr, int *status){
           *status = ESMC_RouteHandleDestroy(*ptr);
       }

       // get just interesting numbers
       void FTN(c_esmc_routehandlegetinfo)(ESMC_RouteHandle **ptr, int *htype, 
                                 int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           
           *(ESMC_HandleType *)htype = (*ptr)->ESMC_RouteHandleGetType();
           
           *status = ESMF_SUCCESS;
       }

       void FTN(c_esmc_routehandlesettype)(ESMC_RouteHandle **ptr, 
                                           int *h, int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetType(*(ESMC_HandleType *)h);
       }

       void FTN(c_esmc_routehandlevalidate)(ESMC_RouteHandle **ptr, char *opts,
                                            int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           // TODO: opts needs null term
           *status = (*ptr)->ESMC_RouteHandleValidate(opts);
       }

       void FTN(c_esmc_routehandleprint)(ESMC_RouteHandle **ptr, char *opts, 
                                         int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           // TODO: opts needs null term
           *status = (*ptr)->ESMC_RouteHandlePrint(opts);
       }

};


