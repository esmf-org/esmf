// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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
#include <cmath>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"


using std::abs;

//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshUTest - Check ESMC_Mesh functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  bool correct;

  int num_elem, num_node;
  ESMC_Mesh mesh;
   int pdim=2;
  int sdim=2;

  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

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
  num_elem = 4;
  num_node = 9;

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
  int elemMask_s [] ={1,1,1,1};
  double elemArea_s [] ={1.0,2.0,3.0,4.0}; // Wrong area, but just to test
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};
  double elemCoord_s [] ={0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  mesh = ESMC_MeshCreate(pdim,sdim,&local_coordSys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, num_node, nodeId_s, nodeCoord_s, nodeOwner_s);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId_s, elemType_s, elemConn_s, 
                            elemMask_s, elemArea_s, elemCoord_s);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out;
  rc = ESMC_MeshGetLocalNodeCount(mesh, &num_node_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out;
  rc = ESMC_MeshGetLocalElementCount(mesh, &num_elem_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_elem = %d\nnum_elem_out=%d\n", num_elem, num_elem_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetOwnedNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_owned_out;
  rc = ESMC_MeshGetOwnedNodeCount(mesh, &num_node_owned_out);
  ESMC_Test((rc==ESMF_SUCCESS),
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_node = %d\nnum_node_owned_out=%d\n", num_node, num_node_owned_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetOwnedNodeCount_OutputCorrect");
  strcpy(failMsg, "Returned wrong owned node count");
  ESMC_Test((num_node==num_node_owned_out), 
	    name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetOwnedElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_owned_out;
  rc = ESMC_MeshGetOwnedElementCount(mesh, &num_elem_owned_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_owned_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //printf("num_elem = %d\nnum_elem_owned_out=%d\n", num_elem, num_elem_owned_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *coords;
  coords = (double *)malloc(num_node_owned_out*3*sizeof(double));
  int num_nodes, num_dims;
   ESMC_MeshGetCoord(mesh, coords, &num_nodes, &num_dims, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  /*printf("Found num_nodes=%d, num_dims=%d\n", num_nodes, num_dims);
  for (int i=0; i< num_nodes; i++) {
    printf("%.1lf %.1lf\n", coords[i*2], coords[i*2+1]);
  }*/
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshVerifyCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  correct=true;
  if (num_nodes != num_node)
    correct=false;
  for(int i=0; i<num_nodes*2; i++) {
    if (abs(coords[i] - nodeCoord_s[i]) > .001) {
      printf("expecting coordinate of %.8f and got %.8f\n",nodeCoord_s[i],coords[i]);
      correct=false;
    }
  }
  ESMC_Test(correct, 
	    name, failMsg, &result, __FILE__, __LINE__, 0);
  free(coords);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetElemCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshGetOwnedElementCount(mesh, &num_elem_owned_out);
  double *elem_coords;
  elem_coords = (double *)malloc(num_elem_owned_out*3*sizeof(double));
  int num_elems;
  ESMC_MeshGetElemCoord(mesh, elem_coords, &num_elems, &num_dims, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  /*printf("Found num_elems=%d, num_dims=%d\n", num_elems, num_dims);
  int ind = 0;
  for (int i=0; i<num_elems; i++) {
    for (int j=0; j<num_dims; j++) {
      printf("%.1f ", elem_coords[ind++]);
    }
    printf("\n");
    }*/

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshVerifyElemCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  correct=true;
  if (num_elems != num_elem)
    correct=false;
  for(int i=0; i<num_elems*2; i++) {
    if (abs(elem_coords[i] - elemCoord_s[i]) > .001) {
      printf("expecting coordinate of %.8f and got %.8f\n",elemCoord_s[i],elem_coords[i]);
      correct=false;
    }
  }
  ESMC_Test(correct, 
	    name, failMsg, &result, __FILE__, __LINE__, 0);
  free(elem_coords);

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Write out the internal mesh data
  strcpy(name, "MeshWrite");
   strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshWrite(mesh, "MeshOutput");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // TODO: This call fails if called before nodes and elements have been added
  // Free internal mesh memory
  strcpy(name, "MeshFreeMemory");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshFreeMemory(mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Destroy mesh object
  strcpy(name, "MeshDestroy");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create mesh object from SCRIP file
  strcpy(name, "MeshCreateFromFile_SCRIP");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  mesh = ESMC_MeshCreateFromFile("data/ne4np4-pentagons.nc", ESMC_FILEFORMAT_SCRIP,
				 NULL, NULL, "", NULL, "", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  rc = ESMC_MeshDestroy(&mesh);
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create mesh object from ESMFMESH file
  strcpy(name, "MeshCreateFromFile_ESMFMESH");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  mesh = ESMC_MeshCreateFromFile("data/ne4np4-esmf.nc", ESMC_FILEFORMAT_ESMFMESH,
				 NULL, NULL, "", NULL, "", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
   rc = ESMC_MeshDestroy(&mesh);
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

