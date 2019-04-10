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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#define masking

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldRegridCsrvUTest - Check ESMC_FieldRegrid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  // Test variables
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  
  // Field variables
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield, srcAreaField, dstAreaField, 
             srcFracField, dstFracField;

  // Mesh variables
  int pdim=2;
  int sdim=2;
  ESMC_Mesh srcmesh;
  int num_elem_s, num_node_s;
  ESMC_Mesh dstmesh;
  int num_elem_d, num_node_d;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //----------------------- MESH CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //              Source Mesh
  // 
  //
  //  2.0   7 ------- 8 -------- 9
  //        |         |          |
  //        |    3    |    4     |
  //        |         |          |
  //  1.0   4 ------- 5 -------- 6
  //        |         |          |
  //        |    1    |    2     |
  //        |         |          |
  //  0.0   1 ------- 2 -------- 3
  //
  //       0.0       1.0        2.0 
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0) 
  // 

  // set Mesh parameters
  num_elem_s = 4;
  num_node_s = 9;

  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, 1.0,0.0, 2.0,0.0,
               0.0,1.0, 1.0,1.0, 2.0,1.0,
               0.0,2.0, 1.0,2.0, 2.0,2.0};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int elemId_s [] ={1,2,3,4};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD};
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};
  int elemMask [] = {0,0,1,0};

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  srcmesh = ESMC_MeshCreate(pdim,sdim,&local_coordSys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(srcmesh, num_node_s, nodeId_s, nodeCoord_s, nodeOwner_s);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  rc = ESMC_MeshAddElements(srcmesh, num_elem_s, elemId_s, elemType_s, 
                            elemConn_s, elemMask, NULL, NULL);
#else
  rc = ESMC_MeshAddElements(srcmesh, num_elem_s, elemId_s, elemType_s, 
                            elemConn_s, NULL, NULL, NULL);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out_s;
  rc = ESMC_MeshGetLocalNodeCount(srcmesh, &num_node_out_s);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node_s==num_node_out_s, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_node_s = %d\nnum_node_out_s=%d\n", num_node_s, num_node_out_s);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_s;
  rc = ESMC_MeshGetLocalElementCount(srcmesh, &num_elem_out_s);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_s==num_elem_out_s, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_elem_s = %d\nnum_elem_out_s=%d\n", num_elem_s, num_elem_out_s);



  //              Destination Mesh
  // 
  //
  //  2.0   13 -------14 --------15--------16
  //        |         |          |         |
  //        |    7    |    8     |   9     |
  //        |         |          |         |
  //  1.5   9 ------- 10 --------11--------12
  //        |         |          |         |
  //        |    4    |    5     |   6     |
  //        |         |          |         |
  //  0.5   5 ------- 6 -------- 7-------- 8
  //        |         |          |         |
  //        |    1    |    2     |   3     |
  //        |         |          |         |
  //  0.0   1 ------- 2 -------- 3-------- 4
  //
  //       0.0       0.5        1.5       2.0
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0) 
  // 

  // set Mesh parameters
  num_elem_d = 9;
  num_node_d = 16;

  int nodeId_d [] ={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  double nodeCoord_d [] ={0.0,0.0, 0.5,0.0, 1.5,0.0, 2.0,0.0,
               0.0,0.5, 0.5,0.5, 1.5,0.5, 2.0,0.5,
               0.0,1.5, 0.5,1.5, 1.5,1.5, 2.0,1.5,
               0.0,2.0, 0.5,2.0, 1.5,2.0, 2.0,2.0};
  int nodeOwner_d [] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int elemId_d [] ={1,2,3,4,5,6,7,8,9};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_d [] = {ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD};
  int elemConn_d [] ={1,2,6,5,
              2,3,7,6,
              3,4,8,7,
              5,6,10,9,
              6,7,11,10,
              7,8,12,11,
              9,10,14,13,
              10,11,15,14,
              11,12,16,15};

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstmesh = ESMC_MeshCreate(pdim,sdim,&local_coordSys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(dstmesh, num_node_d, nodeId_d, nodeCoord_d, nodeOwner_d);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(dstmesh, num_elem_d, elemId_d, elemType_d, 
                            elemConn_d, NULL, NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out_d;
  rc = ESMC_MeshGetLocalNodeCount(dstmesh, &num_node_out_d);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node_d==num_node_out_d, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_node_d = %d\nnum_node_out_d=%d\n", num_node_d, num_node_out_d);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_d;
  rc = ESMC_MeshGetLocalElementCount(dstmesh, &num_elem_out_d);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_d==num_elem_out_d, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_elem_d = %d\nnum_elem_out_d=%d\n", num_elem_d, num_elem_out_d);

  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateMeshTypeKind(srcmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateMeshTypeKind(dstmesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * srcFieldPtr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  for(int i=0; i<num_elem_s; ++i) {
#ifdef masking
    if (elemMask[i] == 1) {
      srcFieldPtr[i] = 100000000.0;
      //printf("Mask applied at position %d\n", i);
    } else
#endif
      srcFieldPtr[i] = 20.0;
  }
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstFieldPtr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  for(int i=0; i<num_elem_d; ++i)
    dstFieldPtr[i] = 0.0;

  //----------------------------------------------------------------------------
  //NEX_UTest
  int *maskValues = (int *)malloc(sizeof(int));
  maskValues[0] = 1;
  strcpy(name, "Create an InterArray for maskValues in ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_InterArrayInt i_maskValues;
  rc = ESMC_InterArrayIntSet(&i_maskValues, maskValues, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  srcFracField = ESMC_FieldCreateMeshTypeKind(srcmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "dstFracField", &rc);
  dstFracField = ESMC_FieldCreateMeshTypeKind(dstmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "dstFracField", &rc);

  ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_CONSERVE;
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
  ESMC_NormType_Flag normtype = ESMC_NORMTYPE_FRACAREA;
#ifdef masking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, 
                             &i_maskValues, NULL,
                             &routehandle,
                             &regridmethod, NULL, NULL, NULL,
                             &normtype, NULL, NULL, NULL, &unmappedaction, NULL,
                             NULL, NULL, NULL, &srcFracField, &dstFracField);
#else
  rc = ESMC_FieldRegridStore(srcfield, dstfield, 
                             NULL, NULL,
                             &routehandle,
                             &regridmethod, NULL, NULL, NULL,
                             &normtype, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, &srcFracField, &dstFracField);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Release an ESMC_RouteHandle via ESMC_FieldRegridRelease()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegridGetArea() - source");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  srcAreaField = ESMC_FieldCreateMeshTypeKind(srcmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "srcAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(srcAreaField);

  //printf("Source Area Field pointer\n");
  double *srcAreaFieldPtr = (double *)ESMC_FieldGetPtr(srcAreaField, 0, &rc);
  bool pass = true;
  for(int i=0; i<num_elem_s; ++i) {
    //printf("%f\n",srcAreaFieldPtr[i]);
    if (srcAreaFieldPtr[i] <= 0.0) pass = false;
  }
  //printf("\n");

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegridGetArea() - destination");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  dstAreaField = ESMC_FieldCreateMeshTypeKind(dstmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "dstAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(dstAreaField);

  //printf("Destination Area Field pointer\n");
  double * dstAreaFieldPtr = (double *)ESMC_FieldGetPtr(dstAreaField, 0, &rc);
  pass = true;
  for(int i=0; i<num_elem_d; ++i) {
    //printf("%f\n",dstAreaFieldPtr[i]);
    if (dstAreaFieldPtr[i] <= 0.0) pass = false;
  }
  //printf("\n");

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Validate regridding");
  strcpy(failMsg, "Did not return correct analytic values");

  // get the fraction fields
  double * srcFracFieldPtr = (double *)ESMC_FieldGetPtr(srcFracField, 0, &rc);
  double * dstFracFieldPtr = (double *)ESMC_FieldGetPtr(dstFracField, 0, &rc);

  double srcmass = 0;
  for (int i=0; i<num_elem_s; ++i)
    srcmass += srcFieldPtr[i]*srcAreaFieldPtr[i]*srcFracFieldPtr[i];

  bool correct = true;
  double dstmass = 0;
  // check destination field against source field
  for(int i=0;i<num_elem_d;++i) {
    // compute the mass
    dstmass += dstFieldPtr[i]*dstAreaFieldPtr[i]*dstFracFieldPtr[i];
    // if error is too big report an error
#ifdef masking
    if (ESMC_dabs(dstFieldPtr[i]-(20.0)) > 100) {
#else
    if (ESMC_dabs(dstFieldPtr[i]-(20.0)) > .0001) {
#endif
      printf("dstfieldptr[%d] = %f\n and it should be = %f\n", i, dstFieldPtr[i], 20.0);
      correct=false;
    }
  }
  // check that the mass is conserved
  if (ESMC_dabs(srcmass - dstmass) > .0001) correct = false;
  //printf("srcmass = %f, dstmass = %f\n", srcmass, dstmass);
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(maskValues);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&srcfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&dstfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&srcmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&dstmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
