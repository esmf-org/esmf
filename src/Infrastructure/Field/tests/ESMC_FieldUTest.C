// $Id: ESMC_FieldUTest.C,v 1.2 2009/09/22 15:33:22 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
#include "ESMC_ArraySpec.h"
#include "ESMC_Mesh.h"
#include "ESMC_Field.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldUTest - Check ESMC_Field functionality
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
  
  ESMC_ArraySpec arrayspec;
  int *minIndexValues, *maxIndexValues;
  ESMC_InterfaceInt minIndex, maxIndex;
  ESMC_Field field;

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
  //N-E-X_UTest
  // Create a mesh
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh = ESMC_MeshCreate(&pdim,&sdim,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //N-E-X_UTest
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
  //N-E-X_UTest
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
  //N-E-X_UTest
  // Add node information to the mesh
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(&mesh, &num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //N-E-X_UTest
  // Add element information to the mesh
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(&mesh, &num_elem, elemId, elemType, elemConn);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 2, ESMC_TYPEKIND_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  strcpy(name, "Set up minIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  minIndexValues = (int *)malloc(2*sizeof(int));
  minIndexValues[0] = minIndexValues[1] = 1;
  minIndex = ESMC_InterfaceIntCreate(minIndexValues, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  strcpy(name, "Set up maxIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  maxIndexValues = (int *)malloc(2*sizeof(int));
  maxIndexValues[0] = 5;
  maxIndexValues[1] = 10;
  maxIndex = ESMC_InterfaceIntCreate(maxIndexValues, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  field = ESMC_FieldCreate(&mesh, &arrayspec, 0, 0, 0, "field1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
//  //----------------------------------------------------------------------------
//  //N-E-X_UTest
//  strcpy(name, "Print ESMC_Field object");
//  strcpy(failMsg, "Did not return ESMF_SUCCESS");
//  rc = ESMC_FieldPrint(field);
//  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
//  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //N-E-X_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
