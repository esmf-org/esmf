// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
  int num_elem, num_node, conn_size;
  int num_elem_out, num_node_out;
  ESMC_Mesh mesh;
  int pdim=2;
  int sdim=3;

  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;

  int *nodeDistG;
  int *elemDistG;
  int localPet, petCount;
  int numOwnedNodes, numOwnedNodes2;
  int numOwnedElems, numOwnedElems2;

  ESMC_VM vm;


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Create a mesh
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  mesh = ESMC_MeshCreate(pdim,sdim,&local_coordSys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Read input files' header data
  strcpy(name, "MeshVTKHeader");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshVTKHeader("data/testmesh", &num_elem, &num_node, &conn_size);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#if 0
  //----------------------------------------------------------------------------
  //NEX_disable_UTest_Multi_Proc_Only
  // TODO: This call fails if called before nodes and elements have been added
  // Free internal mesh memory
  strcpy(name, "MeshFreeMemory");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshFreeMemory(mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Read input files' header data
  strcpy(name, "MeshVTKHeader");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshVTKHeader("data/testmesh", &num_elem, &num_node, &conn_size);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Allocate the arrays to describe Mesh
  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (3*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));

  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (conn_size * sizeof (int));

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Read input files
  strcpy(name, "MeshVTKBody");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshVTKBody("data/testmesh", nodeId, nodeCoord, nodeOwner,
                        elemId, elemType, elemConn);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // VTKBody returns zero based elemConn, so make them 1 based
  for (int i = 0; i < conn_size; i++){
    elemConn[i] = elemConn[i]+1;
  }

  // We no longer use VTK 2D element types, so translate
  for (int i = 0; i < num_elem; i++){
    if (elemType[i]==5) elemType[i]=ESMC_MESHELEMTYPE_TRI;
    else if (elemType[i]==9) elemType[i]=ESMC_MESHELEMTYPE_QUAD;
  }

  // Calculate the number of owned nodes
  numOwnedNodes=0;
  for (int i = 0; i < num_node; i++){
    if (nodeOwner[i]==localPet) numOwnedNodes++;
  }
  // Number of owned elements is the same as the number of local elements
  numOwnedElems=num_elem;


  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalNodeCount before MeshAddNodes");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local nodes
  rc = ESMC_MeshGetLocalNodeCount(mesh, &num_node_out);

  // Note != below to negate success
  ESMC_Test((rc!=ESMF_SUCCESS),
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Add node information to the mesh
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local nodes
  rc = ESMC_MeshGetLocalNodeCount(mesh, &num_node_out);

  correct=true;
  if (num_node_out != num_node) {
   correct = false;
   printf("%d OUTPUT - num_node_out = %d, and num_node = %d\n",
               localPet, num_node_out, num_node);
  }

  ESMC_Test((rc==ESMF_SUCCESS) && correct==true,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalElementCount before MeshAddElements");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local elements
  rc = ESMC_MeshGetLocalElementCount(mesh, &num_elem_out);

  // Note != below to negate success
  ESMC_Test((rc!=ESMF_SUCCESS),
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Add element information to the mesh
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId, elemType, elemConn, NULL, NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local elements
  rc = ESMC_MeshGetLocalElementCount(mesh, &num_elem_out);

  correct=true;
  if (num_elem_out != num_elem) {
    correct = false;
        printf("OUTPUT - num_elem_out = %d, and num_elem = %d\n",
               num_elem_out, num_elem);
  }

  ESMC_Test((rc==ESMF_SUCCESS) && correct==true,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetOwnedElementCount");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local elements
  rc = ESMC_MeshGetOwnedElementCount(mesh, &numOwnedElems2);

  correct=true;
  if (numOwnedElems2 != numOwnedElems) {
    correct = false;
        printf("OUTPUT - num owned elems = %d, and actual num owned elems = %d\n",
               numOwnedElems2, numOwnedElems);
  }

  ESMC_Test((rc==ESMF_SUCCESS) && correct==true,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetOwnedNodeCount");
  strcpy(failMsg, "Incorrect result");
  rc = ESMF_SUCCESS;

  // Get the number of local elements
  rc = ESMC_MeshGetOwnedNodeCount(mesh, &numOwnedNodes2);

  correct=true;
  if (numOwnedNodes2 != numOwnedNodes) {
    correct = false;
        printf("%d OUTPUT - num owned nodes = %d, and actual num owned nodes = %d\n",
               localPet, numOwnedNodes2, numOwnedNodes);
  }

  ESMC_Test((rc==ESMF_SUCCESS) && correct==true,
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_disabled_UTest_Multi_Proc_Only
  // Create DisGrids for the nodes and elements of the mesh
//strcpy(name, "MeshCreateDistGrid");
//strcpy(failMsg, "Did not return ESMF_SUCCESS");
//rc = ESMC_MeshCreateDistGrids(mesh, nodeDistG, elemDistG, &num_node,
//&num_elem);
//ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Write out the internal mesh data
  strcpy(name, "MeshWrite");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshWrite(mesh, "MeshOutput");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // TODO: This call fails if called before nodes and elements have been added
  // Free internal mesh memory
  strcpy(name, "MeshFreeMemory");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshFreeMemory(mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Destroy mesh object
  strcpy(name, "MeshDestroy");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Free arrays used to create Mesh
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);

  free(elemId);
  free(elemType);
  free(elemConn);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

