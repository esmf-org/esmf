// $Id: ESMC_Route_F.C,v 1.47.2.3 2009/01/21 21:25:23 cdeluca Exp $
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

#define ESMC_FILENAME "ESMC_Route_F.C"

#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"
#include "ESMCI_DELayout.h"
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
                                    ESMCI::VM **vm, int *status) {
           *ptr = ESMC_RouteCreate(*vm, status);
       }

       void FTN(c_esmc_routedestroy)(ESMC_Route **ptr, int *status) {
           *status = ESMC_RouteDestroy(*ptr);
       }

       //void FTN(c_esmc_routeget)(ESMC_Route **ptr, 
       //                                  <value> *value, int *status) {
       //    *status = (*ptr)->ESMC_RouteGet(&value);
       //}

       void FTN(c_esmc_routeset)(ESMC_Route **ptr, 
                                 ESMC_RouteOptions *options, int *status) {
           *status = (*ptr)->ESMC_RouteSetOptions(*options);
       }

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

#define ESMC_METHOD "c_ESMC_RouteRunLA"
       void FTN(c_esmc_routerunla)(ESMC_Route **ptr, ESMC_LocalArray **src,
                                   ESMC_LocalArray **dst, int *status) {
           void *src_base_addr = NULL;
           void *dst_base_addr = NULL;
           ESMC_TypeKind sdk, ddk;

           sdk = ESMF_NOKIND;
           ddk = ESMF_NOKIND;

	   if (((ESMC_I8)src != 0) && ((ESMC_I8)src != -1) &&
	       ((ESMC_I8)*src != 0) && ((ESMC_I8)*src != -1)) {
               (*src)->ESMC_LocalArrayGetBaseAddr(&src_base_addr);
               sdk = (*src)->ESMC_LocalArrayGetTypeKind();
               // allow destination to be optional; if not specified, use the
               // src as both src and dst.
               dst_base_addr = src_base_addr;
               ddk = sdk;
           }

	   if (((ESMC_I8)dst != 0) && ((ESMC_I8)dst != -1) &&
	       ((ESMC_I8)*dst != 0) && ((ESMC_I8)*dst != -1)) {
               (*dst)->ESMC_LocalArrayGetBaseAddr(&dst_base_addr);
               ddk = (*dst)->ESMC_LocalArrayGetTypeKind();
           }

           if (sdk != ddk) {
               ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SAMETYPE,
                     "; source & destination datatypes not the same", status);
               return;
           }
           *status = (*ptr)->ESMC_RouteRun(src_base_addr, dst_base_addr, sdk);
       }
#undef ESMC_METHOD

#define ESMC_METHOD "c_ESMC_RouteRunLAL"
       void FTN(c_esmc_routerunlal)(ESMC_Route **ptr, ESMC_LocalArray **src,
                                   ESMC_LocalArray **dst, 
                                   int *srcCount, int *dstCount, int *status) {

           void **src_base_addr = NULL;
           void **dst_base_addr = NULL;
           ESMC_TypeKind sdk, ddk;
           bool hasdst;
           int n;

           sdk = ESMF_NOKIND;
           ddk = ESMF_NOKIND;
           hasdst = true;

           if (*srcCount > 0) 
 	       src_base_addr = new void*[*srcCount];

           if (*dstCount > 0) 
 	       dst_base_addr = new void*[*dstCount];

           // TODO: how do arrays of derived types come across?  since
           // each contains only a pointer, is it simply an array of ptrs?
	   if (((ESMC_I8)src != 0) && ((ESMC_I8)src != -1)
	       && ((ESMC_I8)*src != 0) && ((ESMC_I8)*src != -1)
               && (*srcCount > 0)) {
               for (n=0; n<*srcCount; n++) {
                   // get the data start address for each array in the list   
                   (src[n])->ESMC_LocalArrayGetBaseAddr(src_base_addr+n);

                   // TODO: for now, only support list which have identical 
                   // data types.  in the future sdk and ddk should turn
                   // into arrays of data types, and if they are not all
                   // identical, then in routerun the outer loop must be
                   // by block, and no packing is allowed between blocks.
                   if (n==0)
                       sdk = (src[n])->ESMC_LocalArrayGetTypeKind();
                   else {
                       if (sdk != (src[n])->ESMC_LocalArrayGetTypeKind()) {
                           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SAMETYPE,
                           "; all source datatypes must be the same", status);
                           return;
                       }
                   }
               }
           }

           // This used to be a one-liner, but the optimizer on one of the
           // IRIX machines got confused.  Now check each step before going
           // on to dereference the pointers, etc. 
	   if (hasdst && ((ESMC_I8)dst == 0)) hasdst = false;
           if (hasdst && ((ESMC_I8)dst == -1)) hasdst = false;
	   if (hasdst && ((ESMC_I8)*dst == 0))  hasdst = false;
           if (hasdst && ((ESMC_I8)*dst == -1)) hasdst = false;
           if (hasdst && (*dstCount <= 0)) hasdst = false;
	   //if (((ESMC_I8)dst != 0) && ((ESMC_I8)dst != -1)
	   //    && ((ESMC_I8)*dst != 0) && ((ESMC_I8)*dst != -1)
           //    && (*dstCount > 0)) {
           if (hasdst) {
               for (n=0; n<*dstCount; n++) {
                   // get the data start address for each array in the list   
                   (dst[n])->ESMC_LocalArrayGetBaseAddr(dst_base_addr+n);
                   // TODO: ditto comment above about lists of ddks
                   if (n==0)
                       ddk = (dst[n])->ESMC_LocalArrayGetTypeKind();
                   else {
                       if (ddk != (dst[n])->ESMC_LocalArrayGetTypeKind()) {
                           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SAMETYPE,
                        "; all destination datatypes must be the same", status);
                           return;
                       }
                   }
               }

               if (sdk != ddk) {
                   ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SAMETYPE,
                     "; source & destination datatypes not the same", status);
                   return;
               }

               // TODO: compare srcCount and dstCount - if specified, they must 
               //  be the same.
           } 

           // if destination not specified, replicate source and pass that
           // down.  halo needs this, for example - and the problem is that
           // fortran does not allow the same variable to be specified
           // as more than 1 argument to a subroutine call.  since we cannot
           // do this at the fortran level, do it here.

           if (hasdst) {
               *status = (*ptr)->ESMC_RouteRun(src_base_addr, dst_base_addr, 
                                               sdk, *srcCount);

               delete [] dst_base_addr;

           } else {
               *status = (*ptr)->ESMC_RouteRun(src_base_addr, src_base_addr, 
                                               sdk, *srcCount);
           }

           delete [] src_base_addr;

           return;
       }
#undef ESMC_METHOD

       void FTN(c_esmc_routerunna)(ESMC_Route **ptr, void *src,
                                   void *dst, ESMC_TypeKind *dk, int *status) {
           *status = (*ptr)->ESMC_RouteRun(src, dst, *dk);
       }

       void FTN(c_esmc_routeprecomputeregrid)(ESMC_Route **ptr, int *rank, 
                     int *my_DE_rcv, 
                     ESMC_AxisIndex *AI_rcv_exc, ESMC_AxisIndex *AI_rcv_tot,
                     int *AI_rcv_count, int *global_start_rcv,
                     int *global_count_rcv, 
                     ESMCI::DELayout **layout_rcv,
                     int *my_DE_snd, 
                     ESMC_AxisIndex *AI_snd_exc, ESMC_AxisIndex *AI_snd_tot, 
                     int *AI_snd_count, int *global_start_snd,
                     int *global_count_snd, 
                     ESMCI::DELayout **layout_snd,
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
                     ESMC_Logical *hasSrcData,
                     ESMCI::DELayout **srcDELayout,
                     int *mySrcDE, int *srcDECount,
                     ESMC_AxisIndex *srcGlobalCompAIperDEperRank,
                     ESMC_AxisIndex *mySrcGlobalTotalAIperRank,
                     ESMC_Logical *hasDstData,
                     ESMCI::DELayout **dstDELayout,
                     int *myDstDE, int *dstDECount,
                     ESMC_AxisIndex *dstGlobalCompAIperDEperRank,
                     ESMC_AxisIndex *myDstGlobalTotalAIperRank,
                     int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeRedist(*rank, *hasSrcData,
                                   *srcDELayout, *mySrcDE, *srcDECount,
                                   srcGlobalCompAIperDEperRank,
                                   mySrcGlobalTotalAIperRank,
                                   *hasDstData, *dstDELayout, *myDstDE,
                                   *dstDECount, dstGlobalCompAIperDEperRank,
                                   myDstGlobalTotalAIperRank);
       }

       void FTN(c_esmc_routeprecomputeredistv)(ESMC_Route **ptr, int *rank, 
                     ESMC_Logical *hasDstData, 
                     int *dstMyDE, ESMC_Logical *dstVector, 
                     ESMC_AxisIndex *dstCompAI, ESMC_AxisIndex *dstTotalAI,
                     int *dstAICount, int *dstAICountPerDE,
                     int *dstGlobalStart, int *dstGSCount,
                     int *dstGlobalCount, ESMCI::DELayout **dstLayout,
                     ESMC_Logical *hasSrcData, 
                     int *srcMyDE, ESMC_Logical *srcVector,
                     ESMC_AxisIndex *srcCompAI, ESMC_AxisIndex *srcTotalAI,
                     int *srcAICount, int *srcAICountPerDE,
                     int *srcGlobalStart, int *srcGSCount,
                     int *srcGlobalCount, ESMCI::DELayout **srcLayout, 
                     int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeRedistV(*rank, *hasDstData,
                             *dstMyDE, *dstVector, dstCompAI, dstTotalAI,
                             *dstAICount, dstAICountPerDE,
                             dstGlobalStart, *dstGSCount,
                             dstGlobalCount, *dstLayout, *hasSrcData,
                             *srcMyDE, *srcVector, srcCompAI, srcTotalAI,
                             *srcAICount, srcAICountPerDE,
                             srcGlobalStart, *srcGSCount,
                             srcGlobalCount, *srcLayout);
       }

       void FTN(c_esmc_routeprecomputeredista2a)(ESMC_Route **ptr, int *rank,
                     ESMC_Logical *hasDstData,
                     int *dstMyDE,
                     ESMC_AxisIndex *dstCompAI,
                     int *dstAICount, int *dstAICountPerDE,
                     int *dstGlobalStart, int *dstGSCount,
                     int *dstGlobalCount, ESMCI::DELayout **dstLayout,
                     ESMC_Logical *hasSrcData,
                     int *srcMyDE,
                     ESMC_AxisIndex *srcCompAI,
                     int *srcAICount, int *srcAICountPerDE,
                     int *srcGlobalStart, int *srcGSCount,
                     int *srcGlobalCount, ESMCI::DELayout **srcLayout,
                     int *status) {

         *status = (*ptr)->ESMC_RoutePrecomputeRedistA2A(*rank,*hasDstData,
                             *dstMyDE, dstCompAI,
                             *dstAICount, dstAICountPerDE,
                             dstGlobalStart, *dstGSCount,
                             dstGlobalCount, *dstLayout, *hasSrcData,
                             *srcMyDE, srcCompAI,
                             *srcAICount, srcAICountPerDE,
                             srcGlobalStart, *srcGSCount,
                             srcGlobalCount, *srcLayout);
       }

       void FTN(c_esmc_routeprecomputehalo)(ESMC_Route **ptr, int *rank, 
                  int *my_DE, ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                  int *AI_count, int *global_start, int *global_count,
                  ESMCI::DELayout **layout, 
                  ESMC_Logical *periodic, int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeHalo(*rank, *my_DE, AI_exc,
                             AI_tot, *AI_count, global_start, global_count,
                             *layout, periodic);
       }

       void FTN(c_esmc_routeprecomputedomlist)(ESMC_Route **ptr, int *rank, 
                  ESMCI::DELayout **srcDELayout, ESMCI::DELayout **dstDELayout,
                  ESMC_DomainList *sendDomainList, 
                  ESMC_DomainList *recvDomainList, 
                  ESMC_Logical *hasSrcData, ESMC_Logical *hasDstData,
                  int *status) {

           *status = (*ptr)->ESMC_RoutePrecomputeDomList(*rank, 
                             *srcDELayout, *dstDELayout,
                             sendDomainList, recvDomainList,
                             hasSrcData, hasDstData);
       }


       void FTN(c_esmc_routevalidate)(ESMC_Route **ptr, 
                                      int *srcbufcount, int *srcbufsizes, 
                                      int *dstbufcount, int *dstbufsizes, 
                                      char *opts, int *status) {

           *status = (*ptr)->ESMC_RouteValidate(*srcbufcount, srcbufsizes, 
                                              *dstbufcount, dstbufsizes, opts);
       }

       void FTN(c_esmc_routeprint)(ESMC_Route **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RoutePrint(opts);
       }

};


