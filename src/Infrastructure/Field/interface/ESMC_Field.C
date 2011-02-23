// $Id: ESMC_Field.C,v 1.23 2011/02/23 05:06:40 w6ws Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Field.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt Field} class
// functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES
#include "ESMC_Field.h"

#include "ESMCI_Macros.h"
#include "ESMCI_Field.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr

using namespace ESMCI;

extern "C" {

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreate()"
  ESMC_Field ESMC_FieldCreate(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec,
    ESMC_InterfaceInt gridToFieldMap, ESMC_InterfaceInt ungriddedLBound,
    ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field = NULL;  // initialize

    // Invoque the C++ interface
    field = reinterpret_cast<void *>(ESMCI::Field::create(mesh, arrayspec,
      gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return field; // bail out

    if(rc) *rc = localrc;
    return field;
  }
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldDestroy()"
  int ESMC_FieldDestroy(ESMC_Field *field){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(*field);

    // Invoque the C++ interface
    localrc = ESMCI::Field::destroy(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;

    // invalidate pointer
    *field = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------
  

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldPrint()"
  int ESMC_FieldPrint(ESMC_Field field){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field);

    // Invoque the C++ interface
    localrc = fieldp->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------
  

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGetMesh()"
  ESMC_Mesh ESMC_FieldGetMesh(ESMC_Field field, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field);

    // Invoque the C++ interface
    ESMC_Mesh mesh = fieldp->getMesh(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return mesh;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return mesh;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGetArray()"
  ESMC_Array ESMC_FieldGetArray(ESMC_Field field, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field);

    // Invoque the C++ interface
    ESMC_Array array = fieldp->getArray(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return array;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return array;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGetPtr()"
void *ESMC_FieldGetPtr(ESMC_Field field, int localDe, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Array array = ESMC_FieldGetArray(field, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    rc)) return NULL;  // bail out

  void *ptr = ESMC_ArrayGetPtr(array, localDe, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    rc)) return NULL;  // bail out

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return ptr;
} 
//--------------------------------------------------------------------------

}
