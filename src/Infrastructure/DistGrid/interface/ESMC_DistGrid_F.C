// $Id: ESMC_DistGrid_F.C,v 1.14 2007/06/12 21:29:42 dneckels Exp $
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
#define ESMC_FILENAME "ESMC_DistGrid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"

#include "ESMC_DistGrid.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt DistGrid} class functions.
//
//EOP
//-------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN(c_esmc_distgridcreaterd)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minIndex, ESMC_InterfaceInt **maxIndex,
    ESMC_InterfaceInt **regDecomp,
    ESMC_DecompFlag *decompflag, int *decompflagCount, 
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    ESMC_DELayout **delayout, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_DELayout *opt_delayout;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterd()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreatedb)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minIndex, ESMC_InterfaceInt **maxIndex,
    ESMC_InterfaceInt **deBlockList,
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    ESMC_DELayout **delayout, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_DELayout *opt_delayout;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedb()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minIndex, *maxIndex, *deBlockList,
      *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreaterdfa)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minIndex, ESMC_InterfaceInt **maxIndex,
    ESMC_InterfaceInt **regDecomp,
    ESMC_DecompFlag *decompflag, int *decompflagCount, 
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    int *fastAxis, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdfa()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreaterdp)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minIndex, ESMC_InterfaceInt **maxIndex,
    ESMC_InterfaceInt **regDecomp,
    ESMC_DecompFlag *decompflag, int *decompflagCount1, int *decompflagCount2, 
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    ESMC_DELayout **delayout, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_DELayout *opt_delayout;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdp()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount1, *decompflagCount2, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgriddestroy)(ESMC_DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_DistGridDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridget)(ESMC_DistGrid **ptr, ESMC_DELayout **delayout,
    int *patchCount, ESMC_InterfaceInt **patchList, int *dimCount,
    ESMC_InterfaceInt **dimExtent, ESMC_Logical *regDecompFlag, int *rc){
    ESMC_DELayout **opt_delayout;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER)
      opt_delayout = NULL;
    else opt_delayout = delayout;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridGet(
      opt_delayout, ESMC_NOT_PRESENT_FILTER(patchCount), *patchList,
      ESMC_NOT_PRESENT_FILTER(dimCount),
      *dimExtent,
      ESMC_NOT_PRESENT_FILTER(regDecompFlag)), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridgetpdepdim)(ESMC_DistGrid **ptr, int *de, int *dim,
    ESMC_InterfaceInt **indexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetpdepdim()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridGet(
      *de, *dim, *indexList), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridprint)(ESMC_DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridPrint(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_connection)(
    ESMC_InterfaceInt **connection, int *patchIndexA,
    int *patchIndexB, ESMC_InterfaceInt **positionVector,
    ESMC_InterfaceInt **orientationVector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMC_Connection(*connection, *patchIndexA,
      *patchIndexB, *positionVector, *orientationVector), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgrid_storeabidx)(
    ESMC_DistGrid **dptr,
    ESMC_InterfaceInt **abidx, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgrid_storeabidx()"

    if (rc!=NULL)
      *rc = ESMC_RC_NOT_IMPL;
    // :)
    ESMC_LogDefault.ESMC_LogMsgFoundError((*dptr)->ESMC_DistGrid::SetArbIdx(*abidx),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  
#undef  ESMC_METHOD
  
  
#undef  ESMC_METHOD


}

