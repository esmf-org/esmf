// $Id: ESMC_RHandle_F.C,v 1.3 2003/08/29 21:11:19 jwolfe Exp $
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
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_RHandle.h"
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
           *status = (*ptr)->ESMC_RouteHandleGet(htype, r1, r2, tv, (char **)(NULL));
       }

       void FTN(c_esmc_routehandleset)(ESMC_RouteHandle **ptr, 
                                       ESMC_HandleType *htype, 
                                       ESMC_Route **r1, ESMC_Route **r2, 
                                       ESMC_TransformValues **tv,
                                       // char **label,   not null terminated
                                       int *status) {
           *status = (*ptr)->ESMC_RouteHandleSet(*htype, *r1, *r2, *tv, (char*)(NULL));
       }

       void FTN(c_esmc_routehandlevalidate)(ESMC_RouteHandle **ptr, char *opts,
                                            int *status) {
           *status = (*ptr)->ESMC_RouteHandleValidate(opts);
       }

       void FTN(c_esmc_routehandleprint)(ESMC_RouteHandle **ptr, char *opts, 
                                         int *status) {
           *status = (*ptr)->ESMC_RouteHandlePrint(opts);
       }

};


