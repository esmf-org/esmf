// $Id: ESMC_DistGrid_F.C,v 1.15 2007/06/20 01:29:20 theurich Exp $
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
        
  void FTN(c_esmc_distgridcreaterd)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount, 
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
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
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreatedb)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **deBlockList,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
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
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *deBlockList,
      *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreaterdfa)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount, 
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    int *fastAxis, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdfa()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridcreaterdp)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount1, int *decompflagCount2,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
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
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount1, *decompflagCount2, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgriddestroy)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::DistGrid::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridget)(ESMCI::DistGrid **ptr,
    ESMCI::DELayout **delayout, int *patchCount,
    ESMCI::InterfaceInt **patchList, int *dimCount,
    ESMCI::InterfaceInt **dimExtent, ESMC_Logical *regDecompFlag,
    int *rc){
    ESMCI::DELayout **opt_delayout;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER)
      opt_delayout = NULL;
    else opt_delayout = delayout;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->get(
      opt_delayout, ESMC_NOT_PRESENT_FILTER(patchCount), *patchList,
      ESMC_NOT_PRESENT_FILTER(dimCount),
      *dimExtent,
      ESMC_NOT_PRESENT_FILTER(regDecompFlag)), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridgetpdepdim)(ESMCI::DistGrid **ptr, int *de, int *dim,
    ESMCI::InterfaceInt **indexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetpdepdim()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->get(
      *de, *dim, *indexList), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridprint)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_connection)(
    ESMCI::InterfaceInt **connection, int *patchIndexA,
    int *patchIndexB, ESMCI::InterfaceInt **positionVector,
    ESMCI::InterfaceInt **orientationVector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMCI::DistGrid::connection(*connection, *patchIndexA,
      *patchIndexB, *positionVector, *orientationVector), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgrid_storeabidx)(
    ESMCI::DistGrid **dptr,
    ESMCI::InterfaceInt **abidx, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgrid_storeabidx()"
    
    if (rc!=NULL)
      *rc = ESMC_RC_NOT_IMPL;
      
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      (*dptr)->setArbIdx(*abidx),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD


}

