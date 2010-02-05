// $Id: ESMC_FieldUTest.C,v 1.12.2.1 2010/02/05 19:56:10 svasquez Exp $
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
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound;
  ESMC_InterfaceInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  ESMC_Field field;

  int num_elem, num_node, conn_size;
  ESMC_Mesh mesh, mesh1;
  int pdim=2;
  int sdim=3;

  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a mesh
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh = ESMC_MeshCreate(&pdim,&sdim,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
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
  //NEX_UTest
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
  //NEX_UTest
  // Add node information to the mesh
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, &num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add element information to the mesh
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(mesh, &num_elem, elemId, elemType, elemConn);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 3, ESMC_TYPEKIND_I4);
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
  field = ESMC_FieldCreate(mesh, arrayspec, i_gridToFieldMap, i_ungriddedLBound,
    i_ungriddedUBound, "field1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get an ESMC_Mesh object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh1 = ESMC_FieldGetMesh(field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print an ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldPrint(field);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&field);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);

  free(elemId);
  free(elemType);
  free(elemConn);

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
