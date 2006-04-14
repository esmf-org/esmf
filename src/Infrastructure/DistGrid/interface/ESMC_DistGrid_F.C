// $Id: ESMC_DistGrid_F.C,v 1.6 2006/04/14 16:17:26 theurich Exp $
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
        
  void FTN(c_esmc_distgridcreateregdecomp)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minCorner, ESMC_InterfaceInt **maxCorner,
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
#define ESMC_METHOD "c_esmc_distgridcreateregdecomp()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minCorner, *maxCorner, *regDecomp,
      decompflag, *decompflagCount, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreatedeblocks)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minCorner, ESMC_InterfaceInt **maxCorner,
    ESMC_InterfaceInt **deBlockList,
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    ESMC_DELayout **delayout, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_DELayout *opt_delayout;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedeblocks()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minCorner, *maxCorner, *deBlockList,
      *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreateregdecompfa)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minCorner, ESMC_InterfaceInt **maxCorner,
    ESMC_InterfaceInt **regDecomp,
    ESMC_DecompFlag *decompflag, int *decompflagCount, 
    ESMC_InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMC_InterfaceInt **connectionList,
    ESMC_InterfaceInt **connectionTransformList,
    int *fastAxis, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreateregdecompfa()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minCorner, *maxCorner, *regDecomp,
      decompflag, *decompflagCount, *deLabelList,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreateregdecomppatch)(ESMC_DistGrid **ptr, 
    ESMC_InterfaceInt **minCorner, ESMC_InterfaceInt **maxCorner,
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
#define ESMC_METHOD "c_esmc_distgridcreateregdecomp()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DistGridCreate(*minCorner, *maxCorner, *regDecomp,
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
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_DistGridDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridget)(ESMC_DistGrid **ptr, ESMC_DELayout **delayout,
    ESMC_InterfaceInt **patchList, int *dimCount,
    ESMC_InterfaceInt **dimExtent, ESMC_Logical *regDecompFlag, int *rc){
    ESMC_DELayout **opt_delayout;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER)
      opt_delayout = NULL;
    else opt_delayout = delayout;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridGet(
      opt_delayout, *patchList,
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
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridGet(
      *de, *dim, *indexList), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridprint)(ESMC_DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DistGridPrint(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_connectionelementconstruct)(
    ESMC_InterfaceInt **connectionElement, int *patchIndexA,
    int *patchIndexB, ESMC_InterfaceInt **positionVector,
    ESMC_InterfaceInt **orientationVector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connectionelementconstruct()"
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMC_ConnectionElementConstruct(*connectionElement, *patchIndexA,
      *patchIndexB, *positionVector, *orientationVector), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  
#undef  ESMC_METHOD


}

