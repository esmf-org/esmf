// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
#include "ESMCI_Grid.h"

#include <string>
#include <iostream>

using namespace ESMCI;

extern "C" {

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateGridArraySpec()"
  ESMC_Field ESMC_FieldCreateGridArraySpec(ESMC_Grid grid, 
    ESMC_ArraySpec arrayspec, enum ESMC_StaggerLoc staggerloc,
    ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&grid, arrayspec,
      staggerloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, 
      name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
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
    ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&grid, typekind,
      staggerloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, 
      name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
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
    ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(mesh, arrayspec,
      gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
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
    enum ESMC_MeshLoc_Flag meshloc, ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(mesh, typekind,
      meshloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
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

    // Invoke the C++ interface
    localrc = ESMCI::Field::destroy(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

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

    // Invoke the C++ interface
    localrc = fieldp->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

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

    // Invoke the C++ interface
    ESMC_Mesh mesh = fieldp->getMesh(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return mesh;

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

    // Invoke the C++ interface
    ESMC_Array array = fieldp->getArray(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return array;

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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL;  // bail out

  void *ptr = ESMC_ArrayGetPtr(array, localDe, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL;  // bail out

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return ptr;
} 
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGetBounds()"
int ESMC_FieldGetBounds(ESMC_Field field,
                          int *localDe,
                          int *exclusiveLBound,
                          int *exclusiveUBound,
                          int rank){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  // typecase into ESMCI type
  ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

  // create InterArrays to pass into C++
  ESMCI::InterArray<int> *exLB = new ESMCI::InterArray<int>(exclusiveLBound, rank);
  ESMCI::InterArray<int> *exUB = new ESMCI::InterArray<int>(exclusiveUBound, rank);

  // Invoke the C++ interface
  localrc = ESMCI::Field::getbounds(fieldp, localDe, exLB, exUB);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // copy from C++ to Python
  for (int i=0; i<exLB->extent[0]; i++)
    exclusiveLBound[i] = exLB->array[i];
  for (int i=0; i<exUB->extent[0]; i++)
    exclusiveUBound[i] = exUB->array[i];

  // clean up
  delete exLB;
  delete exUB;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
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

    // Invoke the C++ interface
    localrc = ESMCI::Field::regridgetarea(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------
  

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRead()"
  int ESMC_FieldRead(ESMC_Field field, const char *file,
      const char *variableName, int timeslice, ESMC_IOFmt_Flag iofmt){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // Set iofmt based on file name extension (if present)
    ESMC_IOFmt_Flag opt_iofmt;
    if (iofmt)
      opt_iofmt = iofmt;
    else {
      const char *file_ext_p = strrchr (file, '.');
      if (file_ext_p) {
        std::string file_ext = std::string(file_ext_p);
        if (file_ext == ".nc")
          opt_iofmt = ESMF_IOFMT_NETCDF;
        else if (file_ext == ".bin")
          opt_iofmt = ESMF_IOFMT_BIN;
        else
          opt_iofmt = ESMF_IOFMT_NETCDF;
      } else
        opt_iofmt = ESMF_IOFMT_NETCDF;
    }
 
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

    // Invoke the C++ interface
    localrc = fieldp->read(file, variableName, timeslice, opt_iofmt);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridStore()"
  int ESMC_FieldRegridStore(ESMC_Field srcField, ESMC_Field dstField,
                            ESMC_InterArrayInt *srcMaskValues, 
                            ESMC_InterArrayInt *dstMaskValues,
                            ESMC_RouteHandle *routehandle, 
                            enum ESMC_RegridMethod_Flag *regridmethod, 
                            enum ESMC_PoleMethod_Flag *polemethod,
                            int *regridPoleNPnts,
                            enum ESMC_LineType_Flag *lineType,
                            enum ESMC_NormType_Flag *normType,
                            enum ESMC_ExtrapMethod_Flag *extrapMethod,
                            int *extrapNumSrcPnts,
                            float *extrapDistExponent,
                            enum ESMC_UnmappedAction_Flag *unmappedaction,
                            ESMC_Logical *ignoreDegenerate,
                            double **factorList,
                            int **factorIndexList,
                            int *numFactors,
                            ESMC_Field *srcFracField,
                            ESMC_Field *dstFracField){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    ESMCI::Field *srcfracp = NULL;
    ESMCI::Field *dstfracp = NULL;
    // typecasetinto ESMCI type
    ESMCI::Field *fieldpsrc = reinterpret_cast<ESMCI::Field *>(srcField.ptr);
    ESMCI::Field *fieldpdst = reinterpret_cast<ESMCI::Field *>(dstField.ptr);
    if (srcFracField != NULL)
      srcfracp = reinterpret_cast<ESMCI::Field *>(srcFracField->ptr);
    if (dstFracField != NULL)
      dstfracp = reinterpret_cast<ESMCI::Field *>(dstFracField->ptr);
    ESMCI::RouteHandle *rhPtr;
    rhPtr=NULL;

    // HACK (bekozi): This workaround avoids some undefined behavior with
    //                C_ASSOCIATED in the Fortran regrid store layer. In the
    //                Fortran layer, if numFactors = -999 then we are returning
    //                factor lists.
    int local_numFactors;
    if (!numFactors) {
      local_numFactors = -1;
    } else {
      local_numFactors = *numFactors;
    }

    // C++ regrid store
    localrc = ESMCI::Field::regridstore(fieldpsrc,
                                        fieldpdst,
                                        srcMaskValues,
                                        dstMaskValues,
                                        &rhPtr,
                                        regridmethod,
                                        polemethod,
                                        regridPoleNPnts,
                                        lineType,
                                        normType,
                                        extrapMethod,
                                        extrapNumSrcPnts,
                                        extrapDistExponent,
                                        unmappedaction,
                                        ignoreDegenerate,
                                        factorList,
                                        factorIndexList,
                                        &local_numFactors,
                                        srcfracp,
                                        dstfracp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // HACK (bekozi): If the external factor count pointer was not passed in as
    //                NULL assign out local factor count to the pointer.
    //                Otherwise, the factor count value will not be returned
    //                appropriately.
    if (numFactors) {
      *numFactors = local_numFactors;
    }

    // Assign the route handle pointer. bekozi is not sure if the pointer needs
    // to be nullified before it is assigned.
    routehandle->ptr = NULL;
    routehandle->ptr = (void *)rhPtr;

    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridStoreFile()"
  int ESMC_FieldRegridStoreFile(ESMC_Field srcField, ESMC_Field dstField,
                            const char *filename,
                            ESMC_InterArrayInt *srcMaskValues,
                            ESMC_InterArrayInt *dstMaskValues,
                            ESMC_RouteHandle *routehandle,
                            enum ESMC_RegridMethod_Flag *regridmethod,
                            enum ESMC_PoleMethod_Flag *polemethod,
                            int *regridPoleNPnts,
                            enum ESMC_LineType_Flag *lineType,
                            enum ESMC_NormType_Flag *normType,
                            enum ESMC_UnmappedAction_Flag *unmappedaction,
                            enum ESMC_Logical *ignoreDegenerate,
                            enum ESMC_Logical *create_rh,
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

    // Invoke the C++ interface
    localrc = ESMCI::Field::regridstorefile(fieldpsrc, fieldpdst, filename,
      srcMaskValues, dstMaskValues, &rhPtr, regridmethod,
      polemethod, regridPoleNPnts, lineType, normType, unmappedaction, 
      ignoreDegenerate, create_rh, srcfracp, dstfracp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out

    // return rhPtr in routehandle argument
    if (create_rh == NULL) {
      routehandle->ptr = NULL;
      routehandle->ptr = (void *)rhPtr;
    }
    else if (*create_rh == ESMF_TRUE) {
      routehandle->ptr = NULL;
      routehandle->ptr = (void *)rhPtr;
    }

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

    // Invoke the C++ interface
    localrc = ESMCI::Field::regrid(fieldpsrc, fieldpdst, routehandlep, 
                                   zeroregion);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out

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
    
    // Invoke the C++ interface
    localrc = ESMCI::Field::regridrelease(routehandlep);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out
    
    // mark invalid
    routehandle->ptr = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------
  
//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldRegridReleaseFactors()"
  int ESMC_FieldRegridReleaseFactors(double **factorList, int **factorIndexList, int* numFactors){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // Invoke the C++ interface
    localrc = ESMCI::Field::regridreleasefactors(factorList, factorIndexList, numFactors);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------
  

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldSMMStore()"
  int ESMC_FieldSMMStore(ESMC_Field srcField, ESMC_Field dstField,
                         const char *filename, ESMC_RouteHandle *routehandle,
                         ESMC_Logical *ignoreUnmatchedIndices,
                         int *srcTermProcessing, int *pipeLineDepth){

    // Initialize return code. Assume routine not implemented
    int rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecast Fields into ESMCI type
    ESMCI::Field *fieldpsrc = reinterpret_cast<ESMCI::Field *>(srcField.ptr);
    ESMCI::Field *fieldpdst = reinterpret_cast<ESMCI::Field *>(dstField.ptr);

    // ensure routehandle object is present    
    if (routehandle==NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to routehandle argument", ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
    ESMCI::RouteHandle **routehandlep = (ESMCI::RouteHandle **) &(routehandle->ptr);

    //RLO: removed transposeRoutehandle until user request
    /*// deal with fact that transposeRoutehandle may be absent
    ESMCI::RouteHandle **troutehandlep = NULL;   // default: not present 
    if (transposeRoutehandle != NULL)
      troutehandlep = (ESMCI::RouteHandle **) &(transposeRoutehandle->ptr);*/

    // Invoke the C++ interface
    localrc = ESMCI::Field::smmstore(fieldpsrc, fieldpdst, filename, routehandlep,
        ignoreUnmatchedIndices, srcTermProcessing, pipeLineDepth);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldWrite()"
  int ESMC_FieldWrite(ESMC_Field field, const char *file,
      const char *variableName, int overwrite, ESMC_FileStatus_Flag status,
      int timeslice, ESMC_IOFmt_Flag iofmt){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // Set iofmt based on file name extension (if present)
    ESMC_IOFmt_Flag opt_iofmt;
    if (iofmt)
      opt_iofmt = iofmt;
    else {
      const char *file_ext_p = strrchr (file, '.');
      if (file_ext_p) {
        std::string file_ext = std::string(file_ext_p);
        if (file_ext == ".nc")
          opt_iofmt = ESMF_IOFMT_NETCDF;
        else if (file_ext == ".bin")
          opt_iofmt = ESMF_IOFMT_BIN;
        else
          opt_iofmt = ESMF_IOFMT_NETCDF;
      } else
        opt_iofmt = ESMF_IOFMT_NETCDF;
    }

    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field.ptr);

    // Invoke the C++ interface
    localrc = fieldp->write(file, variableName, overwrite, status, timeslice, opt_iofmt);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreateLocStreamArraySpec()"
  ESMC_Field ESMC_FieldCreateLocStreamArraySpec(ESMC_LocStream locstream, 
    ESMC_ArraySpec arrayspec,
    ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&locstream, arrayspec,
      gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
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
#define ESMC_METHOD "ESMC_FieldCreateLocStreamTypeKind()"
  ESMC_Field ESMC_FieldCreateLocStreamTypeKind(ESMC_LocStream locstream, 
    enum ESMC_TypeKind_Flag typekind,
    ESMC_InterArrayInt *gridToFieldMap, 
    ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
    const char *name, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field;

    // Invoke the C++ interface
    field.ptr = reinterpret_cast<void *>(ESMCI::Field::create(&locstream, typekind,
      gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      field.ptr = NULL;  // invalidate
      return field; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return field;
  }
//--------------------------------------------------------------------------

}
