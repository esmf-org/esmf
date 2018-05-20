// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include <string>

// ESMF header
#include "ESMC.h"

 // ESMF Test header
#include "ESMC_Test.h"

using std::string;

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
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield;

  // Mesh variables
  int pdim=2;
  int sdim=2;
  ESMC_Mesh srcmesh;
  int num_elem_s, num_node_s;
  ESMC_Mesh dstmesh;
  int num_elem_d, num_node_d;

  // LocStream variables
  ESMC_LocStream dstlocstream;
  int ls_size=16;
  const char *keyNameX="ESMF:X";
  const char *keyNameY="ESMF:Y";
  const char *keyNameM="ESMF:Mask";
  ESMC_Array keyArray;
  double *farray;
  double *farray2;
  int *farray3;


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
  rc = ESMC_MeshAddElements(srcmesh, num_elem_s, elemId_s, elemType_s,
                            elemConn_s, NULL, NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //--------------------------------------------------------------------------- -

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out_s;
  rc = ESMC_MeshGetLocalNodeCount(srcmesh, &num_node_out_s);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node_s==num_node_out_s,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //  printf("num_node_s = %d\nnum_node_out_s=%d\n", num_node_s, num_node_out_s);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_s;
  rc = ESMC_MeshGetLocalElementCount(srcmesh, &num_elem_out_s);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_s==num_elem_out_s,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //  printf("num_elem_s = %d\nnum_elem_out_s=%d\n", num_elem_s, num_elem_out_s);



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
  //--------------------------------------------------------------------------- -

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
   //  printf("num_node_d = %d\nnum_node_out_d=%d\n", num_node_d, num_node_out_d);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out_d;
  rc = ESMC_MeshGetLocalElementCount(dstmesh, &num_elem_out_d);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem_d==num_elem_out_d,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //  printf("num_elem_d = %d\nnum_elem_out_d=%d\n", num_elem_d, num_elem_out_d);

  //--------------------------------------------------------------------------- -
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateMeshTypeKind(srcmesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_NODE, NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
   //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateMeshTypeKind(dstmesh,
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_NODE, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
   strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  {
    double x,y;
    int i;
    for(i=0;i<num_node_s;++i) {
      x=nodeCoord_s[2*i];
      y=nodeCoord_s[2*i+1];
      srcfieldptr[i] = 20.0+x+y;
    }
  }

  //----------------------------------------------------------------------------
   //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  {
    int i;
    for(i=0;i<num_node_d;++i)
      dstfieldptr[i] = 0.0;
  }

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
  string filename = "meshmeshweights.nc";
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = ESMC_FieldRegridStoreFile(srcfield, dstfield, filename.c_str(),
                             NULL, NULL,
                             &routehandle,
                             NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL,
                             NULL, NULL, NULL);
#else
  rc = ESMC_FieldRegridStore(srcfield, dstfield,
                             NULL, NULL,
                             &routehandle,
                             NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL,
                             NULL, NULL, NULL,
                             NULL, NULL);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //--------------------------------------------------------------------------- -

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_RouteHandlePrint()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_RouteHandlePrint(routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //--------------------------------------------------------------------------- -

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

   //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Validation of regrid operation()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double x,y,should_be;
  int i;
  bool correct = true;
  // 2. check destination field against source field
  for(i=0;i<num_node_d;++i) {
    x=nodeCoord_d[2*i];
    y=nodeCoord_d[2*i+1];
    // if error is too big report an error
    if (ESMC_dabs(dstfieldptr[i]-(x+y+20.0)) > 0.0001) {
      printf("dstfieldptr[%d] = %f\n and it should be = %f\n", i, dstfieldptr[i], x+y+20.0);
      correct=false;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  rc = ESMC_MeshDestroy(&dstmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamCreateLocal");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstlocstream = ESMC_LocStreamCreateLocal(ls_size, NULL, &local_coordSys, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamAddKeyAlloc");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_TypeKind_Flag local_typeKind = ESMC_TYPEKIND_R8;
  rc = ESMC_LocStreamAddKeyAlloc(dstlocstream,keyNameX,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray = (double *) ESMC_LocStreamGetKeyPtr(dstlocstream,keyNameX,0,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  farray[0]=0.0;
  farray[1]=0.5;
  farray[2]=1.5;
  farray[3]=2.0;
  farray[4]=0.0;
  farray[5]=0.5;
  farray[6]=1.5;
  farray[7]=2.0;
  farray[8]=0.0;
  farray[9]=0.5;
  farray[10]=1.5;
  farray[11]=2.0;
  farray[12]=0.0;
  farray[13]=0.5;
  farray[14]=1.5;
  farray[15]=2.0;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamAddKeyAlloc");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_LocStreamAddKeyAlloc(dstlocstream,keyNameY,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray2 = (double *) ESMC_LocStreamGetKeyPtr(dstlocstream,keyNameY,0,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  farray2[0]=0.0;
  farray2[1]=0.0;
  farray2[2]=0.0;
  farray2[3]=0.0;
  farray2[4]=0.5;
  farray2[5]=0.5;
  farray2[6]=0.5;
  farray2[7]=0.5;
  farray2[8]=1.5;
  farray2[9]=1.5;
  farray2[10]=1.5;
  farray2[11]=1.5;
  farray2[12]=2.0;
  farray2[13]=2.0;
  farray2[14]=2.0;
  farray2[15]=2.0;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamAddKeyAlloc");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  local_typeKind = ESMC_TYPEKIND_I4;
  rc = ESMC_LocStreamAddKeyAlloc(dstlocstream,keyNameM,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray3 = (int *) ESMC_LocStreamGetKeyPtr(dstlocstream,keyNameM,0,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  farray3[0]=0;
  farray3[1]=0;
  farray3[2]=0;
  farray3[3]=1;
  farray3[4]=0;
  farray3[5]=2;
  farray3[6]=0;
  farray3[7]=0;
  farray3[8]=0;
  farray3[9]=0;
  farray3[10]=0;
  farray3[11]=0;
  farray3[12]=0;
  farray3[13]=0;
  farray3[14]=0;
  farray3[15]=0;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object on LocStream");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateLocStreamTypeKind(dstlocstream, ESMC_TYPEKIND_R8,
                                               NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  {
    int i;
    for(i=0;i<ls_size;++i)
      dstfieldptr[i] = -9999.0;
  }

  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridStore(srcfield, dstfield,
                             NULL, &i_maskValues,
                             &routehandle,
                             NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL,
                             NULL, NULL, NULL,
                             NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Region_Flag zeroregion = ESMC_REGION_SELECT;
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, &zeroregion);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Execute ESMC_RouteHandlePrint()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_RouteHandlePrint(routehandle);
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

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Validation of regrid operation()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  correct = true;
  // 2. check destination field against source field
  for(i=0;i<ls_size;++i) {
    x=nodeCoord_d[2*i];
    y=nodeCoord_d[2*i+1];

    // if masked, should be unchanged
    if (farray3[i] == 1) {
      should_be = -9999.0;
    } else {
      should_be = x+y+20.0;
    }

    // if error is too big report an error
    if (ESMC_dabs(dstfieldptr[i]-should_be) > 0.0001) {
      printf("dstfieldptr[%d] = %f\n and it should be = %f\n", i, dstfieldptr[i], should_be);
      correct=false;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------



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
  strcpy(name, "Destroy ESMC_LocStream object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_LocStreamDestroy(&dstlocstream);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

  free(maskValues);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
