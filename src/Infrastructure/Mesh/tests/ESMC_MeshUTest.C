// $Id$
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

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
  // ESMF_MESHELEMTYPE_QUAD=9  could not get ESMC version
  int elemType_s [] ={9,9,9,9};
  double elemArea_s [] ={1.0,2.0,3.0,4.0}; // Wrong area, but just to test
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh = ESMC_MeshCreate(pdim,sdim,&rc);
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
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId_s, elemType_s, elemConn_s, NULL, elemArea_s);
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
  printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out;
  rc = ESMC_MeshGetLocalElementCount(mesh, &num_elem_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_elem = %d\nnum_elem_out=%d\n", num_elem, num_elem_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetOwnedNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_owned_out;
  rc = ESMC_MeshGetOwnedNodeCount(mesh, &num_node_owned_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_owned_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_node = %d\nnum_node_owned_out=%d\n", num_node, num_node_owned_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetOwnedElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_owned_out;
  rc = ESMC_MeshGetOwnedElementCount(mesh, &num_elem_owned_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_owned_out,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_elem = %d\nnum_elem_owned_out=%d\n", num_elem, num_elem_owned_out);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "MeshGetCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *coords;
  int num_nodes, num_dims;
  coords = ESMC_MeshGetCoord(mesh, &num_nodes, &num_dims, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

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
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

