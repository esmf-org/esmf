// $Id: ESMC_RHandle_F.C,v 1.12 2004/04/23 21:58:24 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
#include "ESMC_LocalArray.h"
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
//  TransformValues interfaces

       void FTN(c_esmc_transformvaluescreate)(ESMC_TransformValues **ptr, 
                         int *count, int *status) {
           *ptr = ESMC_TransformValuesCreate(*count, status);
       }

       void FTN(c_esmc_transformvaluesdestroy)(ESMC_TransformValues **ptr, int *status) {
           *status = ESMC_TransformValuesDestroy(*ptr);
       }

       // the int needs to be an enum, the label needs to be added and handled
       void FTN(c_esmc_transformvaluesget)(ESMC_TransformValues **ptr, 
                                       int *numlist, 
                                       ESMC_LocalArray **src, 
                                       ESMC_LocalArray **dst, 
                                       ESMC_LocalArray **w, 
                                       int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              printf("transformvalueget: bad object\n");
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_TransformValuesGet(numlist, src, dst, w);
       }

       // the int needs to be an enum, the label needs to be added and handled
       void FTN(c_esmc_transformvaluesgetf90ptr)(ESMC_TransformValues **ptr, 
                                       int *numlist, 
                                       struct c_F90ptr *src, 
                                       struct c_F90ptr *dst, 
                                       struct c_F90ptr *w, 
                                       int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              printf("transformvaluegetf90ptr: bad object\n");
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_TransformValuesGet(numlist, src, dst, w);
       }

       void FTN(c_esmc_transformvaluesset)(ESMC_TransformValues **ptr, 
                                       int *numlist, 
                                       ESMC_LocalArray **src, 
                                       ESMC_LocalArray **dst, 
                                       ESMC_LocalArray **w, 
                                       int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              printf("transformvaluesset: bad object\n");
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_TransformValuesSet(*numlist, *src, *dst, *w);
       }

       void FTN(c_esmc_transformvaluesvalidate)(ESMC_TransformValues **ptr, 
                                                char *opts, int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              printf("transformvaluesvalidate: bad object\n");
              *status = ESMF_FAILURE;
              return;
           }
           // TODO: opts needs null term
           *status = (*ptr)->ESMC_TransformValuesValidate(opts);
       }

       void FTN(c_esmc_transformvaluesprint)(ESMC_TransformValues **ptr, 
                                             char *opts, int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              printf("transformvaluesprint: bad object\n");
              *status = ESMF_FAILURE;
              return;
           }
           // TODO: opts needs null term
           *status = (*ptr)->ESMC_TransformValuesPrint(opts);
       }

//------------------------------------------------------------------------------
//  RouteHandle interfaces

       void FTN(c_esmc_routehandlecreate)(ESMC_RouteHandle **ptr, int *status) {
           *ptr = ESMC_RouteHandleCreate(status);
       }

       void FTN(c_esmc_routehandledestroy)(ESMC_RouteHandle **ptr, int *status) {
           *status = ESMC_RouteHandleDestroy(*ptr);
       }

       // the int needs to be an enum, the label needs to be added and handled
       void FTN(c_esmc_routehandleget)(ESMC_RouteHandle **ptr, 
                                       ESMC_HandleType *htype, 
                                       ESMC_Route **r1, ESMC_Route **r2, 
                                       ESMC_TransformValues **tv,
                                       // char **label,   not null terminated
                                       int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleGet(htype, r1, r2, tv, (char **)(NULL));
       }

       void FTN(c_esmc_routehandleset)(ESMC_RouteHandle **ptr, 
                                       ESMC_HandleType *htype, 
                                       ESMC_Route **r1, ESMC_Route **r2, 
                                       ESMC_TransformValues **tv,
                                       // char **label,   not null terminated
                                       int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSet(*htype, *r1, *r2, *tv, (char*)(NULL));
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


