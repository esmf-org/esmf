// $Id: ESMC_MeshUTest.C,v 1.11.2.1 2010/02/05 19:59:44 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
  int num_elem, num_node, conn_size;
  int num_elements, num_nodes;
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

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Create a mesh
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh = ESMC_MeshCreate(&pdim,&sdim,&rc);
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

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Add node information to the mesh
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, &num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Add element information to the mesh
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(mesh, &num_elem, elemId, elemType, elemConn);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  // Get the number of local nodes
  strcpy(name, "MeshGetNumNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshGetNumNodes(mesh, &num_nodes);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // Get the number of local elements
  strcpy(name, "MeshGetNumElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshGetNumElements(mesh, &num_elements);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
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
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

