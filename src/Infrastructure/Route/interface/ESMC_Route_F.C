// $Id: ESMC_Route_F.C,v 1.7 2003/04/29 19:34:45 jwolfe Exp $
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
                                              ESMC_XPacket *xp, int *status) {

           *status = (*ptr)->ESMC_RouteSetSend(*dest_de, xp);
       }

       void FTN(c_esmc_routesetrecv)(ESMC_Route **ptr, int *src_de, 
                                           ESMC_XPacket *xp, int *status) {

           *status = (*ptr)->ESMC_RouteSetRecv(*src_de, xp);
       }

       void FTN(c_esmc_routerun)(ESMC_Route **ptr, ESMC_Array **src,
                                          ESMC_Array **dst, int *status) {
           void *src_base_addr = NULL;
           void *dst_base_addr = NULL;

	   if (((long int)*src != 0) && ((long int)*src != -1))
               (*src)->ESMC_ArrayGetBaseAddr(&src_base_addr);
	   if (((long int)*dst != 0) && ((long int)*dst != -1))
               (*dst)->ESMC_ArrayGetBaseAddr(&dst_base_addr);

           *status = (*ptr)->ESMC_RouteRun(src_base_addr, dst_base_addr);
       }

       void FTN(c_esmc_routeprecompute)(ESMC_Route **ptr, int *rank, 
                   int *my_DE_rcv, ESMC_AxisIndex *AI_rcv, int *AI_rcv_count,
                   ESMC_DELayout **layout_rcv,
                   int *my_DE_snd, ESMC_AxisIndex *AI_snd, int *AI_snd_count,
                   ESMC_DELayout **layout_snd, int *status) {

           *status = (*ptr)->ESMC_RoutePrecompute(*rank, 
                              *my_DE_rcv, AI_rcv, *AI_rcv_count, *layout_rcv,
                              *my_DE_snd, AI_snd, *AI_snd_count, *layout_snd);
       }

       void FTN(c_esmc_routeprecomputehalo)(ESMC_Route **ptr, int *rank, 
                   int *my_DE, ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                   int *AI_count, ESMC_DELayout **layout, int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeHalo(*rank, 
                              *my_DE, AI_exc, AI_tot, *AI_count, *layout);
       }

       void FTN(c_esmc_routegetcached)(int *rank, 
                int *my_DE_rcv, ESMC_AxisIndex *AI_rcv, int *AI_rcv_count,
                ESMC_DELayout **layout_rcv,
                int *my_DE_snd, ESMC_AxisIndex *AI_snd, int *AI_snd_count,
                ESMC_DELayout **layout_snd,
                ESMC_Logical *hascachedroute, ESMC_Route **route, int *status) {

           *status = ESMC_RouteGetCached(*rank, 
                              *my_DE_rcv, AI_rcv, *AI_rcv_count, *layout_rcv,
                              *my_DE_snd, AI_snd, *AI_snd_count, *layout_snd,
                              hascachedroute, route);
       }


       void FTN(c_esmc_routevalidate)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RouteValidate(opts);
       }

       void FTN(c_esmc_routeprint)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RoutePrint(opts);
       }

};


