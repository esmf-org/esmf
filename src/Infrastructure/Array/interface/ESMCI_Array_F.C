// $Id: ESMCI_Array_F.C,v 1.1.2.14 2009/01/21 21:25:19 cdeluca Exp $
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
#define ESMC_FILENAME "ESMCI_Array_F.C"
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
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"
#include "ESMC_RHandle.h"

#include "ESMCI_Array.h"

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
    ESMC_DataCopy *copyflag,
    ESMCI::InterfaceInt **distgridToArrayMap,
    ESMCI::InterfaceInt **computationalEdgeLWidthArg,
    ESMCI::InterfaceInt **computationalEdgeUWidthArg,
    ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, 
    ESMCI::InterfaceInt **totalLWidthArg, ESMCI::InterfaceInt **totalUWidthArg,
    ESMC_IndexFlag *indexflag, int *staggerLoc, int *vectorDim, 
    ESMCI::InterfaceInt **undistLBoundArg,
    ESMCI::InterfaceInt **undistUBoundArg,
    char *name, int *len_name, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatelocalarray()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(larrayList, *larrayCount, *distgrid,
      *copyflag, *distgridToArrayMap,
      *computationalEdgeLWidthArg, *computationalEdgeUWidthArg,
      *computationalLWidthArg, *computationalUWidthArg, *totalLWidthArg,
      *totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag),
      ESMC_NOT_PRESENT_FILTER(staggerLoc), ESMC_NOT_PRESENT_FILTER(vectorDim),
      *undistLBoundArg, *undistUBoundArg, &localrc);
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
    ESMCI::ArraySpec *arrayspec, ESMCI::DistGrid **distgrid,
    ESMCI::InterfaceInt **distgridToArrayMap,
    ESMCI::InterfaceInt **computationalEdgeLWidthArg,
    ESMCI::InterfaceInt **computationalEdgeUWidthArg,
    ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, 
    ESMCI::InterfaceInt **totalLWidthArg, ESMCI::InterfaceInt **totalUWidthArg,
    ESMC_IndexFlag *indexflag, int *staggerLoc, int *vectorDim, 
    ESMCI::InterfaceInt **undistLBoundArg,
    ESMCI::InterfaceInt **undistUBoundArg,
    char *name, int *len_name, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreateallocate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(arrayspec, *distgrid, *distgridToArrayMap,
      *computationalEdgeLWidthArg, *computationalEdgeUWidthArg,
      *computationalLWidthArg, *computationalUWidthArg, *totalLWidthArg,
      *totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag), NULL,
      ESMC_NOT_PRESENT_FILTER(staggerLoc), ESMC_NOT_PRESENT_FILTER(vectorDim),
      *undistLBoundArg, *undistUBoundArg, &localrc);
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
  
  void FTN(c_esmc_arraycreatecopy)(ESMCI::Array **ptr, 
    ESMCI::Array **arrayOut, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatecopy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *arrayOut = ESMCI::Array::create(*ptr, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  
  void FTN(c_esmc_arraydestroy)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arrayget)(ESMCI::Array **ptr, ESMC_TypeKind *typekind, 
    int *rank, ESMC_LocalArray **opt_localArrayList,
    int *len_localArrayList, ESMCI::DistGrid **distgrid,
    ESMCI::DELayout **delayout,
    ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **distgridToArrayMap,
    ESMCI::InterfaceInt **distgridToPackedArrayMap,
    ESMCI::InterfaceInt **arrayToDistGridMap,
    ESMCI::InterfaceInt **undistLBound,
    ESMCI::InterfaceInt **undistUBound,
    ESMCI::InterfaceInt **exclusiveLBound,
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
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values    
    if (ESMC_NOT_PRESENT_FILTER(distgrid) != ESMC_NULL_POINTER)
      *distgrid = (*ptr)->getDistGrid();
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(typekind) != ESMC_NULL_POINTER)
      *typekind = (*ptr)->getTypekind();
    if (ESMC_NOT_PRESENT_FILTER(rank) != ESMC_NULL_POINTER)
      *rank = (*ptr)->getRank();
    if (ESMC_NOT_PRESENT_FILTER(indexflag) != ESMC_NULL_POINTER)
      *indexflag = (*ptr)->getIndexflag();
    // helper variables
    int dimCount = (*ptr)->getDistGrid()->getDimCount();
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
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
    // fill distgridToArrayMap
    if (*distgridToArrayMap != NULL){
      // distgridToArrayMap was provided -> do some error checking
      if ((*distgridToArrayMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- distgridToArrayMap array must be of rank 1", rc);
        return;
      }
      if ((*distgridToArrayMap)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of distgridToArrayMap array must be of size "
          "'dimCount'", rc);
        return;
      }
      // fill in distgridToArrayMap
      memcpy((*distgridToArrayMap)->array, (*ptr)->getDistGridToArrayMap(),
        sizeof(int) * dimCount);
    }
    // fill distgridToPackedArrayMap
    if (*distgridToPackedArrayMap != NULL){
      // distgridToPackedArrayMap was provided -> do some error checking
      if ((*distgridToPackedArrayMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- distgridToPackedArrayMap array must be of rank 1", rc);
        return;
      }
      if ((*distgridToPackedArrayMap)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of distgridToPackedArrayMap array must be of size "
          "'dimCount'", rc);
        return;
      }
      // fill in distgridToPackedArrayMap
      memcpy((*distgridToPackedArrayMap)->array,
        (*ptr)->getDistGridToPackedArrayMap(), sizeof(int) * dimCount);
    }
    // fill arrayToDistGridMap
    if (*arrayToDistGridMap != NULL){
      // arrayToDistGridMap was provided -> do some error checking
      if ((*arrayToDistGridMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- arrayToDistGridMap array must be of rank 1", rc);
        return;
      }
      if ((*arrayToDistGridMap)->extent[0] < (*ptr)->getRank()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of arrayToDistGridMap array must be of size 'rank'",
          rc);
        return;
      }
      // fill in arrayToDistGridMap
      memcpy((*arrayToDistGridMap)->array, (*ptr)->getArrayToDistGridMap(),
        sizeof(int) * (*ptr)->getRank());
    }

    // fill undistLBound
    if (*undistLBound != NULL){
      //  undistLBound was provided -> do some error checking
      if ((*undistLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- undistLBound array must be of rank 1", rc);
        return;
      }
      if ((*undistLBound)->extent[0] < (*ptr)->getTensorCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- undistLBound array must at least be of size tensorCount", rc);
        return;
      }
      // fill in undistLBound
      if ((*ptr)->getTensorCount()) { // 0 - sized undistLBound are legit, but memcpy may not behave
        memcpy((*undistLBound)->array, (*ptr)->getUndistLBound(), sizeof(int) * (*ptr)->getTensorCount());
      }
    }

    // fill undistUBound
    if (*undistUBound != NULL){
      //  undistUBound was provided -> do some error checking
      if ((*undistUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- undistUBound array must be of rank 1", rc);
        return;
      }
      if ((*undistUBound)->extent[0] < (*ptr)->getTensorCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- undistUBound array must at least be of size tensorCount", rc);
        return;
      }
      // fill in undistUBound
      if ((*ptr)->getTensorCount()) { // 0 - sized undistUBound are legit, but memcpy may not behave
        memcpy((*undistUBound)->array, (*ptr)->getUndistUBound(), sizeof(int) * (*ptr)->getTensorCount());
      }
    }

    int redDimCount = (*ptr)->getRank() - (*ptr)->getTensorCount();
    // fill exclusiveLBound
    if (*exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*exclusiveLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 2", rc);
        return;
      }
      if ((*exclusiveLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of exclusiveLBound must be of size 'redDimCount'",
          rc);
        return;
      }
      if ((*exclusiveLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of exclusiveLBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveLBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*exclusiveLBound)->array[i*(*exclusiveLBound)->extent[0]]),
          &(((*ptr)->getExclusiveLBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill exclusiveUBound
    if (*exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*exclusiveUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 2", rc);
        return;
      }
      if ((*exclusiveUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of exclusiveUBound must be of size 'redDimCount'",
          rc);
        return;
      }
      if ((*exclusiveUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of exclusiveUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*exclusiveUBound)->array[i*(*exclusiveUBound)->extent[0]]),
          &(((*ptr)->getExclusiveUBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill computationalLBound
    if (*computationalLBound != NULL){
      // computationalLBound was provided -> do some error checking
      if ((*computationalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalLBound must be of size 'redDimCount'", rc);
        return;
      }
      if ((*computationalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalLBound must be of size 'localDeCount'", 
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalLBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalLBound)->
          array[i*(*computationalLBound)->extent[0]]),
          &(((*ptr)->getComputationalLBound())[i*redDimCount]), 
          sizeof(int)*redDimCount);
    }
    // fill computationalUBound
    if (*computationalUBound != NULL){
      // computationalUBound was provided -> do some error checking
      if ((*computationalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalUBound must be of size 'redDimCount'", rc);
        return;
      }
      if ((*computationalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalUBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalUBound)->
          array[i*(*computationalUBound)->extent[0]]),
          &(((*ptr)->getComputationalUBound())[i*redDimCount]), 
          sizeof(int) * redDimCount);
    }
    // fill totalLBound
    if (*totalLBound != NULL){
      // totalLBound was provided -> do some error checking
      if ((*totalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLBound array must be of rank 2", rc);
        return;
      }
      if ((*totalLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLBound must be of size 'redDimCount'", rc);
        return;
      }
      if ((*totalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalLBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalLBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalLBound)->array[i*(*totalLBound)->extent[0]]),
          &(((*ptr)->getTotalLBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill totalUBound
    if (*totalUBound != NULL){
      // totalUBound was provided -> do some error checking
      if ((*totalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUBound array must be of rank 2", rc);
        return;
      }
      if ((*totalUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUBound must be of size 'redDimCount'", rc);
        return;
      }
      if ((*totalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalUBound)->array[i*(*totalUBound)->extent[0]]),
          &(((*ptr)->getTotalUBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill computationalLWidth
    if (*computationalLWidth != NULL){
      // computationalLWidth was provided -> do some error checking
      if ((*computationalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalLWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalLWidth must be of size"
          " 'redDimCount'", rc);
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
        for (int j=0; j<redDimCount; j++)
          (*computationalLWidth)->array[i*(*computationalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*redDimCount+j] -
            ((*ptr)->getComputationalLBound())[i*redDimCount+j];
    }
    // fill computationalUWidth
    if (*computationalUWidth != NULL){
      // computationalUWidth was provided -> do some error checking
      if ((*computationalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalUWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalUWidth must be of size"
          " 'redDimCount'", rc);
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
        for (int j=0; j<redDimCount; j++)
          (*computationalUWidth)->array[i*(*computationalUWidth)->extent[0]+j] =
            ((*ptr)->getComputationalUBound())[i*redDimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*redDimCount+j];
    }
    // fill totalLWidth
    if (*totalLWidth != NULL){
      // totalLWidth was provided -> do some error checking
      if ((*totalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalLWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLWidth must be of size 'redDimCount'", rc);
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
        for (int j=0; j<redDimCount; j++)
          (*totalLWidth)->array[i*(*totalLWidth)->extent[0]+j] =
            ((*ptr)->getComputationalLBound())[i*redDimCount+j] -
            ((*ptr)->getTotalLBound())[i*redDimCount+j];
    }
    // fill totalUWidth
    if (*totalUWidth != NULL){
      // totalUWidth was provided -> do some error checking
      if ((*totalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalUWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUWidth must be of size 'redDimCount'", rc);
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
        for (int j=0; j<redDimCount; j++)
          (*totalUWidth)->array[i*(*totalUWidth)->extent[0]+j] =
            ((*ptr)->getTotalUBound())[i*redDimCount+j] -
            ((*ptr)->getComputationalUBound())[i*redDimCount+j];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_arrayprint)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arrayvalidate)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->validate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arrayrediststore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle, 
    ESMCI::InterfaceInt **srcToDstTransposeMap, ESMC_TypeKind *typekind,
    void *factor, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayrediststore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::redistStore(
      *srcArray, *dstArray, routehandle, *srcToDstTransposeMap, *typekind,
      factor),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arrayrediststorenf)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle, 
    ESMCI::InterfaceInt **srcToDstTransposeMap, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayrediststorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::redistStore(
      *srcArray, *dstArray, routehandle, *srcToDstTransposeMap),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arrayredist)(ESMCI::Array **srcArray, ESMCI::Array **dstArray,
    ESMC_RouteHandle **routehandle, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayredist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::redist(
      *srcArray, *dstArray, routehandle, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_arraysmmstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle, 
    ESMC_TypeKind *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmmstore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::sparseMatMulStore(
      *srcArray, *dstArray, routehandle, *typekind, factorList,
      *factorListCount, *factorIndexList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraysmmstorenf)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmmstorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::sparseMatMulStore(
      *srcArray, *dstArray, routehandle),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraysmm)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle,
    ESMC_RegionFlag *zeroflag, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Array::sparseMatMul(
      *srcArray, *dstArray, routehandle, *zeroflag, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_arraygather)(ESMCI::Array **array, void *farray,
    ESMC_TypeKind *typekind, int *rank, int *counts,
    int *patch, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraygather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->gather(
      farray, *typekind, *rank, counts, ESMC_NOT_PRESENT_FILTER(patch),
      *rootPet, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_arraygathernotroot)(ESMCI::Array **array,
    int *patch, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraygathernotroot()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->gather(
      NULL, ESMF_NOKIND, 0, NULL, ESMC_NOT_PRESENT_FILTER(patch),
      *rootPet, opt_vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_arrayscatter)(ESMCI::Array **array, void *farray,
    ESMC_TypeKind *typekind, int *rank, int *counts,
    int *patch, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatter()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatternotroot()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
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
  
  void FTN(c_esmc_arrayset)(ESMCI::Array **array, int *staggerLoc, 
    int *vectorDim, ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayset()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // set staggerLoc
    if (ESMC_NOT_PRESENT_FILTER(staggerLoc)!=ESMC_NULL_POINTER)
      (*array)->setStaggerLoc(*staggerLoc);
    // set vectorDim
    if (ESMC_NOT_PRESENT_FILTER(vectorDim)!=ESMC_NULL_POINTER)
      (*array)->setVectorDim(*vectorDim);
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->setComputationalLWidth(
      *computationalLWidthArg),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*array)->setComputationalUWidth(
      *computationalUWidthArg),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_arraysettensor)(ESMCI::Array **array,
    ESMCI::InterfaceInt **tensorIndexArg, int *staggerLoc, int *vectorDim,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysettensor()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // check that tensorIndexArgs was provided    
    if (*tensorIndexArg == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Must provide tensorIndexArg-", rc);
      return;
    }
    
    // check that tensorIndexArg is within limits -> construct tensorIndex
    if ((*tensorIndexArg)->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- tensorIndexArg array must be of rank 1", rc);
      return;
    }
    int tensorCount = (*array)->getTensorCount();
    if ((*tensorIndexArg)->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- first dim of tensorIndexArg array must be of size tensorCount", rc);
      return;
    }
    const int *undistLBound = (*array)->getUndistLBound();
    const int *undistUBound = (*array)->getUndistUBound();
    int tensorIndex = 0;
    for (int i=tensorCount-1; i>=0; i--){
      tensorIndex *= undistUBound[i] - undistLBound[i] + 1;
      if ((*tensorIndexArg)->array[i] < undistLBound[i] ||
        (*tensorIndexArg)->array[i] > undistUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
          "- tensorIndexArg entry is out of range", rc);
        return;
      }
      tensorIndex += (*tensorIndexArg)->array[i] - undistLBound[i];
    }
    
    // set staggerLoc
    if (ESMC_NOT_PRESENT_FILTER(staggerLoc)!=ESMC_NULL_POINTER)
      (*array)->setStaggerLoc(*staggerLoc, tensorIndex);
    // set vectorDim
    if (ESMC_NOT_PRESENT_FILTER(vectorDim)!=ESMC_NULL_POINTER)
      (*array)->setVectorDim(*vectorDim, tensorIndex);
  }
  
  void FTN(c_esmc_arrayserialize)(ESMCI::Array **array, char *buf, int *length,
    int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
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
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
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

#ifdef FIRSTNEWARRAYPROTOTYPE


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
    int localPET = opt_vm->getLocalPet();
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
    int localPET = opt_vm->getLocalPet();
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
    int localPET = opt_vm->getLocalPet();
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

#endif
