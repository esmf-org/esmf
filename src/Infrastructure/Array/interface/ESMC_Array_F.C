// $Id: ESMC_Array_F.C,v 1.54 2007/06/23 04:00:01 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Array_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <string.h>

#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMC_DistGrid.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_RHandle.h"

#include "ESMC_Array.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Array} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN(c_esmc_arraycreatelocalarray)(ESMCI::Array **ptr, 
    ESMC_LocalArray **larrayList, int *larrayCount, ESMCI::DistGrid **distgrid,
    ESMCI::InterfaceInt **dimmap, ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, 
    ESMCI::InterfaceInt **totalLWidthArg, ESMCI::InterfaceInt **totalUWidthArg,
    ESMC_IndexFlag *indexflag, int *staggerLoc, int *vectorDim, 
    ESMCI::InterfaceInt **lboundsArg, ESMCI::InterfaceInt **uboundsArg,
    char *name, int *len_name, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatealldecomp()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(larrayList, *larrayCount, *distgrid, *dimmap,
      *computationalLWidthArg, *computationalUWidthArg, *totalLWidthArg,
      *totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag),
      ESMC_NOT_PRESENT_FILTER(staggerLoc), ESMC_NOT_PRESENT_FILTER(vectorDim),
      *lboundsArg, *uboundsArg, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the Array object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid string", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }
  
  void FTN(c_esmc_arraycreateallocate)(ESMCI::Array **ptr, 
    ESMC_ArraySpec *arrayspec, ESMCI::DistGrid **distgrid,
    ESMCI::InterfaceInt **dimmap, ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, 
    ESMCI::InterfaceInt **totalLWidthArg, ESMCI::InterfaceInt **totalUWidthArg,
    ESMC_IndexFlag *indexflag, int *staggerLoc, int *vectorDim, 
    ESMCI::InterfaceInt **lboundsArg, ESMCI::InterfaceInt **uboundsArg,
    char *name, int *len_name, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatealldecomp()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(arrayspec, *distgrid, *dimmap,
      *computationalLWidthArg, *computationalUWidthArg, *totalLWidthArg,
      *totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag),
      ESMC_NOT_PRESENT_FILTER(staggerLoc), ESMC_NOT_PRESENT_FILTER(vectorDim),
      *lboundsArg, *uboundsArg, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the Array object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid string", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }
  
  void FTN(c_esmc_arraydestroy)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydestroy()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
  void FTN(c_esmc_arrayget)(ESMCI::Array **ptr, ESMC_TypeKind *typekind, 
    int *rank, ESMC_LocalArray **opt_localArrayList,
    int *len_localArrayList, ESMCI::DistGrid **distgrid,
    ESMCI::DELayout **delayout,
    ESMC_IndexFlag *indexflag, ESMCI::InterfaceInt **dimmap,
    ESMCI::InterfaceInt **inverseDimmap, ESMCI::InterfaceInt **exclusiveLBound,
    ESMCI::InterfaceInt **exclusiveUBound,
    ESMCI::InterfaceInt **computationalLBound,
    ESMCI::InterfaceInt **computationalUBound,
    ESMCI::InterfaceInt **totalLBound, ESMCI::InterfaceInt **totalUBound,
    ESMCI::InterfaceInt **computationalLWidth,
    ESMCI::InterfaceInt **computationalUWidth,
    ESMCI::InterfaceInt **totalLWidth, ESMCI::InterfaceInt **totalUWidth,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayget()"
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // fill simple return values    
    if (ESMC_NOT_PRESENT_FILTER(distgrid) != ESMC_NULL_POINTER)
      *distgrid = (*ptr)->getDistigrid();
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDelayout();
    if (ESMC_NOT_PRESENT_FILTER(typekind) != ESMC_NULL_POINTER)
      *typekind = (*ptr)->getTypekind();
    if (ESMC_NOT_PRESENT_FILTER(rank) != ESMC_NULL_POINTER)
      *rank = (*ptr)->getRank();
    if (ESMC_NOT_PRESENT_FILTER(indexflag) != ESMC_NULL_POINTER)
      *indexflag = (*ptr)->getIndexflag();
    // helper variables
    int dimCount = (*ptr)->getDistigrid()->getDimCount();
    int localDeCount = (*ptr)->getDelayout()->getLocalDeCount();
    // fill localArrayList
    if (*len_localArrayList != 0){
      // opt_localArrayList was provided
      if (*len_localArrayList < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- opt_localArrayList must provide localDeCount elements", rc);
        return;
      }
      // opt_localArrayList has correct number of elements
      for (int i=0; i<localDeCount; i++)
        opt_localArrayList[i] = ((*ptr)->getLocalarrayList())[i];
    }
    // fill dimmap
    if (*dimmap != NULL){
      // dimmap was provided -> do some error checking
      if ((*dimmap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- dimmap array must be of rank 1", rc);
        return;
      }
      if ((*dimmap)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of dimmap array must be of size 'dimCount'", rc);
        return;
      }
      // fill in dimmap
      memcpy((*dimmap)->array, (*ptr)->getDimmap(), sizeof(int) * dimCount);
    }
    // fill inverseDimmap
    if (*inverseDimmap != NULL){
      // inverseDimmap was provided -> do some error checking
      if ((*inverseDimmap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- inverseDimmap array must be of rank 1", rc);
        return;
      }
      if ((*inverseDimmap)->extent[0] < (*ptr)->getRank()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of inverseDimmap array must be of size 'rank'", rc);
        return;
      }
      // fill in inverseDimmap
      memcpy((*inverseDimmap)->array, (*ptr)->getInverseDimmap(),
        sizeof(int) * (*ptr)->getRank());
    }
    // fill exclusiveLBound
    if (*exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*exclusiveLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 2", rc);
        return;
      }
      if ((*exclusiveLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of exclusiveLBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*exclusiveLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of exclusiveLBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveLBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveLBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*exclusiveLBound)->array[i*(*exclusiveLBound)->extent[0]]),
          &(((*ptr)->getExclusiveLBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill exclusiveUBound
    if (*exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*exclusiveUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 2", rc);
        return;
      }
      if ((*exclusiveUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of exclusiveUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*exclusiveUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of exclusiveUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*exclusiveUBound)->array[i*(*exclusiveUBound)->extent[0]]),
          &(((*ptr)->getExclusiveUBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill computationalLBound
    if (*computationalLBound != NULL){
      // computationalLBound was provided -> do some error checking
      if ((*computationalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalLBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*computationalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalLBound must be of size 'localDeCount'", 
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalLBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalLBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalLBound)->
          array[i*(*computationalLBound)->extent[0]]),
          &(((*ptr)->getComputationalLBound())[i*dimCount]), 
          sizeof(int)*dimCount);
    }
    // fill computationalUBound
    if (*computationalUBound != NULL){
      // computationalUBound was provided -> do some error checking
      if ((*computationalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*computationalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalUBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalUBound)->
          array[i*(*computationalUBound)->extent[0]]),
          &(((*ptr)->getComputationalUBound())[i*dimCount]), 
          sizeof(int) * dimCount);
    }
    // fill totalLBound
    if (*totalLBound != NULL){
      // totalLBound was provided -> do some error checking
      if ((*totalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLBound array must be of rank 2", rc);
        return;
      }
      if ((*totalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*totalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalLBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalLBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalLBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalLBound)->array[i*(*totalLBound)->extent[0]]),
          &(((*ptr)->getTotalLBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill totalUBound
    if (*totalUBound != NULL){
      // totalUBound was provided -> do some error checking
      if ((*totalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUBound array must be of rank 2", rc);
        return;
      }
      if ((*totalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*totalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalUBound)->array[i*(*totalUBound)->extent[0]]),
          &(((*ptr)->getTotalUBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill computationalLWidth
    if (*computationalLWidth != NULL){
      // computationalLWidth was provided -> do some error checking
      if ((*computationalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalLWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalLWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*computationalLWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of computationalLWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*computationalLWidth)->array[i*(*computationalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*dimCount+j] -
            ((*ptr)->getComputationalLBound())[i*dimCount+j];
    }
    // fill computationalUWidth
    if (*computationalUWidth != NULL){
      // computationalUWidth was provided -> do some error checking
      if ((*computationalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalUWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalUWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*computationalUWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of computationalUWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*computationalUWidth)->array[i*(*computationalUWidth)->extent[0]+j] =
            ((*ptr)->getComputationalUBound())[i*dimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*dimCount+j];
    }
    // fill totalLWidth
    if (*totalLWidth != NULL){
      // totalLWidth was provided -> do some error checking
      if ((*totalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalLWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*totalLWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalLWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*totalLWidth)->array[i*(*totalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*dimCount+j] -
            ((*ptr)->getTotalLBound())[i*dimCount+j];
    }
    // fill totalUWidth
    if (*totalUWidth != NULL){
      // totalUWidth was provided -> do some error checking
      if ((*totalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalUWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*totalUWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalUWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*totalUWidth)->array[i*(*totalUWidth)->extent[0]+j] =
            ((*ptr)->getTotalUBound())[i*dimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*dimCount+j];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  
    
    
#if 0
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(distgrid) == ESMC_NULL_POINTER)
      opt_distgrid = NULL;
    else opt_distgrid = distgrid;
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER)
      opt_delayout = NULL;
    else opt_delayout = delayout;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->get(
      ESMC_NOT_PRESENT_FILTER(typekind), ESMC_NOT_PRESENT_FILTER(rank),
      opt_localArrayList, *len_localArrayList, opt_distgrid, opt_delayout,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *dimmap, *inverseDimmap, *exclusiveLBound, *exclusiveUBound,
      *computationalLBound, *computationalUBound, *totalLBound, *totalUBound,
      *computationalLWidth, *computationalUWidth, *totalLWidth, *totalUWidth),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
#endif
  }


  void FTN(c_esmc_arrayprint)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayprint()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  

  void FTN(c_esmc_arraysparsematmulstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_R8 *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rootPet,
    ESMC_RouteHandle **routehandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysparsematmulstore()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::sparseMatMulStore(
      *srcArray, *dstArray, factorList, *factorListCount, *factorIndexList,
      *rootPet, routehandle),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  

  void FTN(c_esmc_arraysparsematmul)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysparsematmul()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::sparseMatMul(
      *srcArray, *dstArray, routehandle),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
  void FTN(c_esmc_arrayscatter)(ESMCI::Array **array, void *farray,
    ESMC_TypeKind *typekind, int *rank, int *counts,
    int *patch, int *rootPet, ESMCI::VM **vm, int *rc){
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatter()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->scatter(
      farray, *typekind, *rank, counts, ESMC_NOT_PRESENT_FILTER(patch),
      *rootPet, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
  void FTN(c_esmc_arrayscatternotroot)(ESMCI::Array **array,
    int *patch, int *rootPet, ESMCI::VM **vm, int *rc){
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatternotroot()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->scatter(
      NULL, ESMF_NOKIND, 0, NULL, ESMC_NOT_PRESENT_FILTER(patch),
      *rootPet, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
  void FTN(c_esmc_arrayserialize)(ESMCI::Array **array, char *buf, int *length,
    int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayserialize()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->serialize(
      buf, length, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  

  void FTN(c_esmc_arraydeserialize)(ESMCI::Array **array, char *buf,
    int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydeserialize()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    *array = new ESMCI::Array;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->deserialize(
      buf, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  
  
#undef  ESMC_METHOD
}






//-------------------------------------------------------------------------
// The following glue code is for a first newArray prototype which I used
// to check out some communication ideas: DE-nonblocking paradigm!
//-------------------------------------------------------------------------



// the interface subroutine names MUST be in lower case
extern "C" {

  // - ESMF-public methods:

  void FTN(c_esmc_newarraycreate)(ESMC_newArray **ptr, ESMC_LocalArray **larray,
    int *haloWidth, int *len_haloWidth, int *deCount, int *rootPET, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarraycreate()"
    // Deal with optional arguments
    ESMC_LocalArray *opt_larray;
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    int *opt_haloWidth = NULL;
    if (*len_haloWidth) opt_haloWidth = haloWidth;
    int opt_deCount;
    if (ESMC_NOT_PRESENT_FILTER(deCount) == ESMC_NULL_POINTER) opt_deCount = 0;
    else opt_deCount = *deCount;
    *ptr = ESMC_newArrayCreate(opt_larray, opt_haloWidth, opt_deCount, *rootPET,
      &localrc);
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarraydestroy)(ESMC_newArray **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarraydestroy()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_newArrayDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_newarrayprint)(ESMC_newArray **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayprint()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayPrint(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_newarrayget)(ESMC_newArray **ptr, int *rank,
    ESMCI::DELayout **delayout, ESMC_LocalArray **localArrays, 
    int *len_localArrays, int *globalFullLBound, int *len_globalFullLBound,
    int *globalFullUBound, int *len_globalFullUBound,
    int *globalDataLBound, int *len_globalDataLBound,
    int *globalDataUBound, int *len_globalDataUBound,
    int *localDataLBound, int *len_localDataLBound,
    int *localDataUBound, int *len_localDataUBound,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayget()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayGet(
      ESMC_NOT_PRESENT_FILTER(rank), ESMC_NOT_PRESENT_FILTER(delayout),
      localArrays, *len_localArrays, globalFullLBound, len_globalFullLBound,
      globalFullUBound, len_globalFullUBound, 
      globalDataLBound, len_globalDataLBound,
      globalDataUBound, len_globalDataUBound,
      localDataLBound, len_localDataLBound, 
      localDataUBound, len_localDataUBound),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_newarrayscatterb)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, ESMCI::VM **vm, int *rc){
    // PET-based blocking scatter
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatterb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarrayscatternbroot)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, ESMC_newArrayCommHandle **commh,
    ESMCI::VM **vm, int *rc){
    // DE-based non-blocking scatter (root call)
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatternbroot()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET;
    opt_vm->get(&localPET, NULL, NULL, NULL, NULL);
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
#ifndef ESMF_NO_INITIALIZERS
    if (*commh != ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      " - a previously used commhandle has not been deleted",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
#endif
    // Allocate a new commhandle
    *commh = new ESMC_newArrayCommHandle;
    (*commh)->commhandleCount = 0;  // reset
    (*commh)->pthidCount = 0;       // reset
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, *commh, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarrayscatternb)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, int *de, ESMCI::VM **vm, int *rc){
    // DE-based non-blocking scatter
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatternb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, *de, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarrayreducescalarb)(ESMC_newArray **ptr, void *result,
    ESMC_TypeKind *dtk, ESMC_Operation *op, int *rootPET, ESMCI::VM **vm, 
    int *rc){
    // PET-based blocking scalar reduce
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarrayreducescalarnbroot)(ESMC_newArray **ptr, void *result,
    ESMC_TypeKind *dtk, ESMC_Operation *op, int *rootPET,
    ESMC_newArrayCommHandle **commh, ESMCI::VM **vm, int *rc){
    // DE-based non-blocking reduce (root call)
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarnbroot()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET;
    opt_vm->get(&localPET, NULL, NULL, NULL, NULL);
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
#ifndef ESMF_NO_INITIALIZERS
    if (*commh != ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      " - a previously used commhandle has not been deleted",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
#endif
    // Allocate a new commhandle
    *commh = new ESMC_newArrayCommHandle;
    (*commh)->commhandleCount = 0;  // reset
    (*commh)->pthidCount = 0;       // reset
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, *commh, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_newarrayreducescalarnb)(ESMC_newArray **ptr, void *result,
    ESMC_TypeKind *dtk, ESMC_Operation *op, int *rootPET, int *de,
    ESMCI::VM **vm, int *rc){
    // PET-based blocking scalar reduce
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarnb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, *de, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
// ---- Wait methods ---  
  
  void FTN(c_esmc_newarraywaitroot)(ESMC_newArray **ptr, int *rootPET,
    ESMC_newArrayCommHandle **commh, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatter()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET;
    opt_vm->get(&localPET, NULL, NULL, NULL, NULL);
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
    // Check if a valid commhandle was provided
    if (*commh == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      " - a valid commhandle must be provided",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayWait(
      *rootPET, *commh, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // delete the commhandle and set to NULL
    delete *commh;
    *commh = ESMC_NULL_POINTER;
  }
  
  void FTN(c_esmc_newarraywaitde)(ESMC_newArray **ptr, int *de, ESMCI::VM **vm, 
    int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatter()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_newArrayWait(
      *de, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

#undef  ESMC_METHOD
}
