// $Id: ESMC_Route_F.C,v 1.2 2003/03/11 22:57:20 nscollins Exp $
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
#include "ESMC_DELayout.h"
#include "ESMC_Array.h"
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
       void FTN(c_esmc_routecreate)(ESMC_Route **ptr, ESMC_DELayout **layout, 
                                                   int *status) {
           *ptr = ESMC_RouteCreate(*layout, status);
       }

       void FTN(c_esmc_routedestroy)(ESMC_Route **ptr, int *status) {
           *status = ESMC_RouteDestroy(*ptr);
       }

       //void FTN(c_esmc_routeget)(ESMC_Route **ptr, 
       //                                  <value> *value, int *status) {
       //    *status = (*ptr)->ESMC_RouteGet(&value);
       //}

       void FTN(c_esmc_routesetsend)(ESMC_Route **ptr, int *dest_de, 
                             ESMC_Array **ap, ESMC_XPacket *xp, int *status) {
           void *base_addr;

           (*ap)->ESMC_ArrayGetBaseAddr(&base_addr);
           *status = (*ptr)->ESMC_RouteSetSend(*dest_de, base_addr, xp);
       }

       void FTN(c_esmc_routesetrecv)(ESMC_Route **ptr, int *src_de, 
                            ESMC_Array **ap, ESMC_XPacket *xp, int *status) {
           void *base_addr;

           (*ap)->ESMC_ArrayGetBaseAddr(&base_addr);
           *status = (*ptr)->ESMC_RouteSetRecv(*src_de, base_addr, xp);
       }

       void FTN(c_esmc_routevalidate)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RouteValidate(opts);
       }

       void FTN(c_esmc_routeprint)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RoutePrint(opts);
       }

};


