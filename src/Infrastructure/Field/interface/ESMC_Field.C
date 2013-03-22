// $Id: ESMC_Field.C,v 1.41 2012/07/18 22:21:25 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
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
#include "ESMF_LogMacros.inc" // TODO: remove once this comes through ESMCI_LogErr.h
#include "ESMC_Field.h"

#include "ESMCI_Macros.h"
#include "ESMCI_Field.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"

using namespace ESMCI;

extern "C" {

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateGridArraySpec()"
  ESMC_Field ESMC_FieldCreateGridArraySpec(ESMC_Grid grid, 
    ESMC_ArraySpec arrayspec, enum ESMC_StaggerLoc staggerloc,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, ESMC_InterfaceInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoque the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&grid, arrayspec,
      staggerloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, 
      name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc)){
      field.ptr = NULL;  // invalidate
      return field; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return field;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateGridTypeKind()"
  ESMC_Field ESMC_FieldCreateGridTypeKind(ESMC_Grid grid, 
    enum ESMC_TypeKind_Flag typekind, enum ESMC_StaggerLoc staggerloc,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, ESMC_InterfaceInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoque the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&grid, typekind,
      staggerloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, 
      name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc)){
      field.ptr = NULL;  // invalidate
      return field; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return field;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateMeshArraySpec()"
  ESMC_Field ESMC_FieldCreateMeshArraySpec(ESMC_Mesh mesh, 
    ESMC_ArraySpec arrayspec,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, ESMC_InterfaceInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoque the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(mesh, arrayspec,
      gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc)){
      field.ptr = NULL;  // invalidate
      return field; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return field;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateMeshTypeKind()"
  ESMC_Field ESMC_FieldCreateMeshTypeKind(ESMC_Mesh mesh, enum ESMC_TypeKind_Flag typekind,
    enum ESMC_MeshLoc_Flag meshloc, ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, ESMC_InterfaceInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoque the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(mesh, typekind,
      meshloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc)){
      field.ptr = NULL;  // invalidate
      return field; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
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
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field->ptr);

    // Invoque the C++ interface
    localrc = ESMCI::Field::destroy(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;

    // invalidate pointer
    field->ptr = NULL;

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
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

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
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

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
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

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


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridGetArea()"
  int ESMC_FieldRegridGetArea(ESMC_Field field) {

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

    // Invoque the C++ interface
    localrc = ESMCI::Field::regridgetarea(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridStore()"
  int ESMC_FieldRegridStore(ESMC_Field srcField, ESMC_Field dstField,
                            ESMC_InterfaceInt *srcMaskValues, 
                            ESMC_InterfaceInt *dstMaskValues,
                            ESMC_RouteHandle *routehandle, 
                            enum ESMC_RegridMethod_Flag *regridmethod, 
                            enum ESMC_UnmappedAction_Flag *unmappedaction,
                            ESMC_Field *srcFracField,
                            ESMC_Field *dstFracField){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    ESMCI::Field *srcfracp = NULL;
    ESMCI::Field *dstfracp = NULL;
    // typecase into ESMCI type
    ESMCI::Field *fieldpsrc = reinterpret_cast<ESMCI::Field *>(srcField.ptr);
    ESMCI::Field *fieldpdst = reinterpret_cast<ESMCI::Field *>(dstField.ptr);
    if (srcFracField != NULL)
      srcfracp = reinterpret_cast<ESMCI::Field *>(srcFracField->ptr);
    if (dstFracField != NULL)
      dstfracp = reinterpret_cast<ESMCI::Field *>(dstFracField->ptr);
    ESMCI::RouteHandle *rhPtr;
    rhPtr=NULL;   

    // Invoque the C++ interface
    localrc = ESMCI::Field::regridstore(fieldpsrc, fieldpdst, 
      srcMaskValues, dstMaskValues, &rhPtr, regridmethod, unmappedaction,
      srcfracp, dstfracp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;  // bail out

    // return rhPtr in routehandle argument
    routehandle->ptr = NULL;
    routehandle->ptr = (void *)rhPtr;
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegrid()"
  int ESMC_FieldRegrid(ESMC_Field srcField, ESMC_Field dstField, 
                            ESMC_RouteHandle routehandle, 
                            enum ESMC_Region_Flag *zeroregion){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecase into ESMCI type
    ESMCI::Field *fieldpsrc = reinterpret_cast<ESMCI::Field *>(srcField.ptr);
    ESMCI::Field *fieldpdst = reinterpret_cast<ESMCI::Field *>(dstField.ptr);
    ESMCI::RouteHandle *routehandlep = 
      reinterpret_cast<ESMCI::RouteHandle *>(routehandle.ptr);

    // Invoque the C++ interface
    localrc = ESMCI::Field::regrid(fieldpsrc, fieldpdst, routehandlep, 
                                   zeroregion);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridRelease()"
  int ESMC_FieldRegridRelease(ESMC_RouteHandle *routehandle){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecase into ESMCI type
    ESMCI::RouteHandle *routehandlep = reinterpret_cast<ESMCI::RouteHandle *>(routehandle->ptr);

    // Invoque the C++ interface
    localrc = ESMCI::Field::regridrelease(routehandlep);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;  // bail out
    
    // mark invalid
    routehandle->ptr = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------

}
