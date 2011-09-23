// $Id: ESMC_FieldRegridUTest.C,v 1.1 2011/09/23 20:53:11 rokuingh Exp $
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldRegridUTest - Check ESMC_FieldRegrid functionality
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
  ESMC_ArraySpec arrayspec;
  ESMC_RouteHandle routehandle;
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound;
  ESMC_InterfaceInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  ESMC_Field srcfield, dstfield;

  // Mesh variables
  int pdim=2;
  int sdim=3;
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

#if 0
  conn_size_s = num_elem_s*4;
  // Allocate the arrays to describe Mesh
  nodeId_s    = (int *) malloc (num_node_s * sizeof (int));
  nodeCoord_s = (double *) malloc (3*num_node_s * sizeof (double));
  nodeOwner_s = (int *) malloc (num_node_s * sizeof (int));

  elemId_s   = (int *) malloc (num_elem_s * sizeof (int));
  elemType_s = (int *) malloc (num_elem_s * sizeof (int));
  elemConn_s = (int *) malloc (conn_size_s * sizeof (int));

  // MAKE SURE EVERYTHING IS 1 BASED!!! (except coordinates)
  *nodeId_s=(1,2,3,4,5,6,7,8,9);
  *nodeCoord_s=(0.0,0.0, 1.0,0.0, 2.0,0.0,
               0.0,1.0, 1.0,1.0, 2.0,1.0,
               0.0,2.0, 1.0,2.0, 2.0,2.0);
  *nodeOwner_s=(0,0,0,0,0,0,0,0,0); // everything on proc 0
  *elemId_s=(1,2,3,4);
  *elemType_s=(9,9,9,; // ESMF_MESHELEMTYPE_QUAD=9  could not get ESMC version
  *elemConn_s=(1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8);
#endif
  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, 1.0,0.0, 2.0,0.0,
               0.0,1.0, 1.0,1.0, 2.0,1.0,
               0.0,2.0, 1.0,2.0, 2.0,2.0};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int elemId_s [] ={1,2,3,4};
  // ESMF_MESHELEMTYPE_QUAD=9  could not get ESMC version
  int elemType_s [] ={9,9,9,9};
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcmesh = ESMC_MeshCreate(pdim,sdim,&rc);
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
  rc = ESMC_MeshAddElements(srcmesh, num_elem_s, elemId_s, elemType_s, elemConn_s);
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
printf("num_node_s = %d\nnum_node_out_s=%d\n", num_node_s, num_node_out_s);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_s;
  rc = ESMC_MeshGetLocalElementCount(srcmesh, &num_elem_out_s);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_s==num_elem_out_s, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
printf("num_elem_s = %d\nnum_elem_out_s=%d\n", num_elem_s, num_elem_out_s);



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

#if 0
  conn_size_d = num_elem_d*4;
  // Allocate the arrays to describe Mesh
  nodeId_d    = (int *) malloc (num_node_d * sizeof (int));
  nodeCoord_d = (double *) malloc (3*num_node_d * sizeof (double));
  nodeOwner_d = (int *) malloc (num_node_d * sizeof (int));

  elemId_d   = (int *) malloc (num_elem_d * sizeof (int));
  elemType_d = (int *) malloc (num_elem_d * sizeof (int));
  elemConn_d = (int *) malloc (conn_size_d * sizeof (int));

  // MAKE SURE EVERYTHING IS 1 BASED!!! (except coordinates)
  *nodeId_d=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
  *nodeCoord_d=(0.0,0.0, 0.5,0.0, 1.5,0.0, 2.0,0.0,
               0.0,0.5, 0.5,0.5, 1.5,0.5, 2.0,0.5,
               0.0,1.5, 0.5,1.5, 1.5,1.5, 2.0,1.5,
               0.0,2.0, 0.5,2.0, 1.5,2.0, 2.0,2.0);
  *nodeOwner_d=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0); // everything on proc 0
  *elemId_d=(1,2,3,4,5,6,7,8,9);
  *elemType_d=9; // ESMF_MESHELEMTYPE_QUAD=9  could not get ESMC version
  *elemConn_d=(1,2,6,5,
              2,3,7,6,
              3,4,8,7,
              5,6,10,9,
              6,7,11,10,
              7,8,12,11,
              9,10,14,13,
              10,11,15,14,
              11,12,16,15);
#endif
  int nodeId_d [] ={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  double nodeCoord_d [] ={0.0,0.0, 0.5,0.0, 1.5,0.0, 2.0,0.0,
               0.0,0.5, 0.5,0.5, 1.5,0.5, 2.0,0.5,
               0.0,1.5, 0.5,1.5, 1.5,1.5, 2.0,1.5,
               0.0,2.0, 0.5,2.0, 1.5,2.0, 2.0,2.0};
  int nodeOwner_d [] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int elemId_d [] ={1,2,3,4,5,6,7,8,9};
  // ESMF_MESHELEMTYPE_QUAD=9  could not get ESMC version
  int elemType_d [] = {9,9,9,9,9,9,9,9,9};
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
  dstmesh = ESMC_MeshCreate(pdim,sdim,&rc);
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
  rc = ESMC_MeshAddElements(dstmesh, num_elem_d, elemId_d, elemType_d, elemConn_d);
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
printf("num_node_d = %d\nnum_node_out_d=%d\n", num_node_d, num_node_out_d);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_d;
  rc = ESMC_MeshGetLocalElementCount(dstmesh, &num_elem_out_d);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_d==num_elem_out_d, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
printf("num_elem_d = %d\nnum_elem_out_d=%d\n", num_elem_d, num_elem_out_d);


  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 3, ESMC_TYPEKIND_R8);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up gridToFieldMap");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gridToFieldMap = (int *)malloc(sizeof(int));
  gridToFieldMap[0] = 1;
  i_gridToFieldMap = ESMC_InterfaceIntCreate(gridToFieldMap, 1, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedLBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedLBound = (int *)malloc(2*sizeof(int));
  ungriddedLBound[0] = 1;
  ungriddedLBound[1] = 1;
  i_ungriddedLBound = ESMC_InterfaceIntCreate(ungriddedLBound, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedUBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedUBound = (int *)malloc(2*sizeof(int));
  ungriddedUBound[0] = 2;
  ungriddedUBound[1] = 3;
  i_ungriddedUBound = ESMC_InterfaceIntCreate(ungriddedUBound, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreate(srcmesh, arrayspec, i_gridToFieldMap, i_ungriddedLBound,
    i_ungriddedUBound, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreate(dstmesh, arrayspec, i_gridToFieldMap, i_ungriddedLBound,
    i_ungriddedUBound, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * srcfieldptr = ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#if INITDATA
{
  // define analytic field on source field
  double x,y;
  int i;
  for(i=0;i<num_node_s;++i) {
    x=nodeCoord_s[2*i];
    y=nodeCoord_s[2*i+1];
    static_cast<double *>(srcfieldptr)[i] = 20.0+x+y;
  }
}
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * dstfieldptr = ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#if INITDATA
// define analytic field on source field
{
  int i;
  for(i=0;i<num_node_d;++i)
    static_cast<double *>(dstfieldptr)[i] = 0.0;
}
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest
  int regridmethod = 0;
  int unmappedaction = 0;			
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridStore(&srcfield, &dstfield, &routehandle, 
                        regridmethod, unmappedaction);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(&srcfield, &dstfield, &routehandle);
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
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------

#if INITDATA
{
  double x,y;
  int i;
  bool correct = true;
  // 2. check destination field against source field
  for(i=0;i<num_node_d;++i) {
    x=nodeCoord_d[2*i];
    y=nodeCoord_d[2*i+1];
    // if error is too big report an error
    if ( abs( static_cast<double *>(dstfieldptr)[i]-(x+y+20.0) ) > 0.0001)
      correct=false;
  }
  
  if (correct == false) printf("\n\nERROR IN REGRID ACCURACY!\n\n");
}
#endif

#if 0
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Get an ESMC_Mesh object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh1 = ESMC_FieldGetMesh(field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Get an ESMC_Array object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_FieldGetArray(field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * ptr = ESMC_FieldGetPtr(field, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Print an ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldPrint(field);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

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

  free(gridToFieldMap);
  ESMC_InterfaceIntDestroy(&i_gridToFieldMap);
  free(ungriddedLBound);
  ESMC_InterfaceIntDestroy(&i_ungriddedLBound);
  free(ungriddedUBound);
  ESMC_InterfaceIntDestroy(&i_ungriddedUBound);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
