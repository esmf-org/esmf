// $Id: ESMC_Route_F.C,v 1.1 2003/03/05 17:04:53 nscollins Exp $
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
#include "ESMC_Route.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Route} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_routecreate)(ESMC_Route **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_RouteCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_routedestroy)(ESMC_Route **ptr, int *status) {
           *status = ESMC_RouteDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_routeinit)(ESMC_Route **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_RouteInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_routegetconfig)(ESMC_Route **ptr, 
                                         ESMC_RouteConfig *config, int *status} {
           *status = (*ptr)->ESMC_RouteGetConfig(&config);
       }

       void FTN(c_esmc_routesetconfig)(ESMC_Route **ptr, 
                                         ESMC_RouteConfig *config, int *status} {
           *status = (*ptr)->ESMC_RouteSetConfig(config);
       }

       void FTN(c_esmc_routeget)(ESMC_Route **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_RouteGet(&value);
       }

       void FTN(c_esmc_routeset)(ESMC_Route **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_RouteSet(value);
       }

       void FTN(c_esmc_routevalidate)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RouteValidate(opts);
       }

       void FTN(c_esmc_routeprint)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RoutePrint(opts);
       }

};


