// $Id: ESMC_Route_F.C,v 1.32 2004/12/07 22:31:28 nscollins Exp $
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
#include "ESMC_DELayout.h"
#include "ESMC_LocalArray.h"
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
       void FTN(c_esmc_routecreate)(ESMC_Route **ptr, 
                                    ESMC_VM **vm, int *status) {
           *ptr = ESMC_RouteCreate(*vm, status);
       }

       void FTN(c_esmc_routedestroy)(ESMC_Route **ptr, int *status) {
           *status = ESMC_RouteDestroy(*ptr);
       }

       //void FTN(c_esmc_routeget)(ESMC_Route **ptr, 
       //                                  <value> *value, int *status) {
       //    *status = (*ptr)->ESMC_RouteGet(&value);
       //}

       void FTN(c_esmc_routesetsend)(ESMC_Route **ptr, int *dest_pet, 
                                     ESMC_XPacket *xp, int *status) {

           *status = (*ptr)->ESMC_RouteSetSend(*dest_pet, xp);
       }

       void FTN(c_esmc_routesetrecv)(ESMC_Route **ptr, int *src_pet, 
                                     ESMC_XPacket *xp, int *status) {

           *status = (*ptr)->ESMC_RouteSetRecv(*src_pet, xp);
       }

       void FTN(c_esmc_routesetrecvitems)(ESMC_Route **ptr, int *nitems, 
                                                               int *status) {

           *status = (*ptr)->ESMC_RouteSetRecvItems(*nitems);
       }

       void FTN(c_esmc_routegetrecvitems)(ESMC_Route **ptr, int *nitems, 
                                                               int *status) {

           *nitems = (*ptr)->ESMC_RouteGetRecvItems();
           *status = ESMF_SUCCESS;
       }

       void FTN(c_esmc_routerunla)(ESMC_Route **ptr, ESMC_LocalArray **src,
                                   ESMC_LocalArray **dst, int *status) {
           void *src_base_addr = NULL;
           void *dst_base_addr = NULL;
           ESMC_DataKind dk;

	   if (((long int)*src != 0) && ((long int)*src != -1)) {
               (*src)->ESMC_LocalArrayGetBaseAddr(&src_base_addr);
               dk = (*src)->ESMC_LocalArrayGetKind();
           }
	   if (((long int)*dst != 0) && ((long int)*dst != -1)) {
               (*dst)->ESMC_LocalArrayGetBaseAddr(&dst_base_addr);
               dk = (*dst)->ESMC_LocalArrayGetKind();
           }

           *status = (*ptr)->ESMC_RouteRun(src_base_addr, dst_base_addr, dk);
       }

       void FTN(c_esmc_routerunna)(ESMC_Route **ptr, void *src,
                                   void *dst, ESMC_DataKind *dk, int *status) {

           *status = (*ptr)->ESMC_RouteRun(src, dst, *dk);
       }

       void FTN(c_esmc_routeprecomputeregrid)(ESMC_Route **ptr, int *rank, 
                     int *my_DE_rcv, 
                     ESMC_AxisIndex *AI_rcv_exc, ESMC_AxisIndex *AI_rcv_tot,
                     int *AI_rcv_count, int *global_start_rcv,
                     int *global_count_rcv, 
                     ESMC_DELayout **layout_rcv,
                     int *my_DE_snd, 
                     ESMC_AxisIndex *AI_snd_exc, ESMC_AxisIndex *AI_snd_tot, 
                     int *AI_snd_count, int *global_start_snd,
                     int *global_count_snd, 
                     ESMC_DELayout **layout_snd,
                     int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeRegrid(*rank, 
                             *my_DE_rcv, AI_rcv_exc, AI_rcv_tot, 
                             *AI_rcv_count, global_start_rcv, 
                             global_count_rcv, *layout_rcv,
                             *my_DE_snd, AI_snd_exc, AI_snd_tot, 
                             *AI_snd_count, global_start_snd,
                             global_count_snd, *layout_snd);
       }

       void FTN(c_esmc_routeprecomputeredist)(ESMC_Route **ptr, int *rank, 
                     int *dstMyDE, ESMC_AxisIndex *dstCompAI,
                     ESMC_AxisIndex *dstTotalAI, int *dstAICount,
                     int *dstGlobalStart, int *dstGlobalCount,
                     ESMC_DELayout **dstLayout,
                     int *srcMyDE, ESMC_AxisIndex *srcCompAI,
                     ESMC_AxisIndex *srcTotalAI, int *srcAICount,
                     int *srcGlobalStart, int *srcGlobalCount,
                     ESMC_DELayout **srcLayout, 
                     int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeRedist(*rank, 
                             *dstMyDE, dstCompAI, dstTotalAI, *dstAICount,
                             dstGlobalStart, dstGlobalCount, *dstLayout,
                             *srcMyDE, srcCompAI, srcTotalAI, *srcAICount,
                             srcGlobalStart, srcGlobalCount, *srcLayout);
       }

       void FTN(c_esmc_routeprecomputehalo)(ESMC_Route **ptr, int *rank, 
                  int *my_DE, ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                  int *AI_count, int *global_start, int *global_count,
                  ESMC_DELayout **layout, 
                  ESMC_Logical *periodic, int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeHalo(*rank, *my_DE, AI_exc,
                             AI_tot, *AI_count, global_start, global_count,
                             *layout, periodic);
       }

       void FTN(c_esmc_routeprecomputedomlist)(ESMC_Route **ptr, int *rank, 
                  ESMC_DELayout **srcDELayout, ESMC_DELayout **dstDELayout,
                  ESMC_DomainList *sendDomainList, 
                  ESMC_DomainList *recvDomainList, 
                  ESMC_Logical *hasSrcData, ESMC_Logical *hasDstData,
                  int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeDomList(*rank, 
                             *srcDELayout, *dstDELayout,
                             sendDomainList, recvDomainList,
                             hasSrcData, hasDstData);
       }

       void FTN(c_esmc_routegetcached)(int *rank, 
                int *my_DE_rcv, 
                ESMC_AxisIndex *AI_rcv_exc, ESMC_AxisIndex *AI_rcv_tot, 
                int *AI_rcv_count, 
                ESMC_DELayout **layout_rcv,
                int *my_DE_snd, 
                ESMC_AxisIndex *AI_snd_exc, ESMC_AxisIndex *AI_snd_tot, 
                int *AI_snd_count, 
                ESMC_DELayout **layout_snd,
                ESMC_Logical *periodic, 
                ESMC_Logical *hascachedroute, ESMC_Route **route, int *status) {

           *status = ESMC_RouteGetCached(*rank, 
                *my_DE_rcv, AI_rcv_exc, AI_rcv_tot, *AI_rcv_count, *layout_rcv,
                *my_DE_snd, AI_snd_exc, AI_snd_tot, *AI_snd_count, *layout_snd,
                periodic, hascachedroute, route);
       }


       void FTN(c_esmc_routevalidate)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RouteValidate(opts);
       }

       void FTN(c_esmc_routeprint)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RoutePrint(opts);
       }

};


