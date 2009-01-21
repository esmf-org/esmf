// $Id: ESMC_RHandle_F.C,v 1.16.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
       void FTN(c_esmc_routehandleget)(ESMC_RouteHandle **ptr, int *htype, 
                               int *rt_count,  ESMC_HandleMapping *rmaptype,
                               int *which_rt, ESMC_Route **r, 
                               int *tv_count, ESMC_HandleMapping *tvmaptype,
			       int *which_tv, ESMC_TransformValues **tv,
                               char *label, int *status, int labellen) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleGet((ESMC_HandleType *)htype, 
                                       rt_count, rmaptype, *which_rt, r, 
                                       tv_count, tvmaptype, *which_tv, tv, 
                                       (char **)(NULL));
       }

       // get a specific route
       void FTN(c_esmc_routehandlegetroute)(ESMC_RouteHandle **ptr, 
                                            int *which_rt, ESMC_Route **r,
                                            int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *r = (*ptr)->ESMC_RouteHandleGetRoute(*which_rt);
           *status = (*r != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
       }

       // get a specific tv
       void FTN(c_esmc_routehandlegettvalues)(ESMC_RouteHandle **ptr, 
                                              int *which_tv, 
                                              ESMC_TransformValues **tv, 
                                              int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *tv = (*ptr)->ESMC_RouteHandleGetTValues(*which_tv);
           *status = (*tv != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
       }

       // get just interesting numbers
       void FTN(c_esmc_routehandlegetinfo)(ESMC_RouteHandle **ptr, int *htype, 
                                 int *rt_count, ESMC_HandleMapping *rmaptype,
                                 int *tv_count, ESMC_HandleMapping *tvmaptype,
                                 int *status) {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleGet((ESMC_HandleType *)htype, 
                                                 rt_count, rmaptype, 0, NULL,
                                                 tv_count, tvmaptype, 0, NULL,
                                                 (char **)(NULL));
       }

       void FTN(c_esmc_routehandleset)(ESMC_RouteHandle **ptr, 
                                       int *htype, int *rt_count, 
                                       ESMC_HandleMapping *rmaptype, 
                                       int *which_rt, ESMC_Route **r, 
                                       int *tv_count, 
                                       ESMC_HandleMapping *tvmaptype, 
                                       int *which_tv,
                                       ESMC_TransformValues **tv,
                                       char *label,   // not null terminated
                                       int *status, int labellen) {

           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSet(*(ESMC_HandleType *)htype, 
                                                 *rt_count, *rmaptype,
                                                 *which_rt, *r, *tv_count,
                                                 *tvmaptype, *which_tv, *tv, 
                                                 (char*)(NULL));
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

       void FTN(c_esmc_routehandlesetroutecount)(ESMC_RouteHandle **ptr, 
                                                 int *rtcount, int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetRouteCount(*rtcount);
       }

       void FTN(c_esmc_routehandlesetrmaptype)(ESMC_RouteHandle **ptr, 
					           ESMC_HandleMapping *rmaptype,
                                                   int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetRMapType(*rmaptype);
       }

       void FTN(c_esmc_routehandlesetroute)(ESMC_RouteHandle **ptr, 
                                            int *which_rt, ESMC_Route **rh,
                                            int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetRoute(*which_rt, *rh);
       }

       void FTN(c_esmc_routehandlesettvcount)(ESMC_RouteHandle **ptr, 
	 				      int *tvcount, int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetTVCount(*tvcount);
       }

       void FTN(c_esmc_routehandlesettvmaptype)(ESMC_RouteHandle **ptr, 
                                                ESMC_HandleMapping *tvmaptype,
                                                int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetTVMapType(*tvmaptype);
       }

       void FTN(c_esmc_routehandlesettvalues)(ESMC_RouteHandle **ptr, 
                                              int *which_tv, 
                                              ESMC_TransformValues **tv,
                                              int *status)
       {
           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }
           *status = (*ptr)->ESMC_RouteHandleSetTValues(*which_tv, *tv);
       }

       void FTN(c_esmc_routehandlesetlabel)(ESMC_RouteHandle **ptr, 
                                            char *label, int *status, 
                                            int labellen)
       {
           char *tlabel;

           if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
           }

           tlabel = new char[labellen+1];
           strncpy(tlabel, label, labellen);
           tlabel[labellen] = '\0';

           *status = (*ptr)->ESMC_RouteHandleSetLabel(tlabel);
  
           delete [] tlabel;
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


