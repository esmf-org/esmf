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
  enum ESMC_StaggerLoc staggerloc=ESMC_STAGGERLOC_CENTER;
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound, *maxIndex;
  ESMC_InterfaceInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  ESMC_InterfaceInt i_maxIndex;
  ESMC_Field field, field2;

  int num_elem, num_node, conn_size;
  ESMC_Grid grid;
  ESMC_Mesh mesh, mesh1, mesh2;
  ESMC_Array array, array2;
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

  ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a mesh
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  mesh = ESMC_MeshCreate(pdim,sdim,&local_coordSys,&rc);
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

  // We no longer use VTK 2D element types, so translate
  for (int i = 0; i < num_elem; i++){
    if (elemType[i]==5) elemType[i]=ESMC_MESHELEMTYPE_TRI;
    else if (elemType[i]==9) elemType[i]=ESMC_MESHELEMTYPE_QUAD;
  }

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add node information to the mesh
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add element information to the mesh
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId, elemType, elemConn, NULL, 
                            NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 1, ESMC_TYPEKIND_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up gridToFieldMap");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gridToFieldMap = (int *)malloc(sizeof(int));
  gridToFieldMap[0] = 1;
  rc = ESMC_InterfaceIntSet(&i_gridToFieldMap, gridToFieldMap, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedLBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedLBound = (int *)malloc(2*sizeof(int));
  ungriddedLBound[0] = 1;
  ungriddedLBound[1] = 1;
  rc = ESMC_InterfaceIntSet(&i_ungriddedLBound, ungriddedLBound, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedUBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedUBound = (int *)malloc(2*sizeof(int));
  ungriddedUBound[0] = 2;
  ungriddedUBound[1] = 3;
  rc = ESMC_InterfaceIntSet(&i_ungriddedUBound, ungriddedUBound, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  field = ESMC_FieldCreateMeshArraySpec(mesh, arrayspec,
    &i_gridToFieldMap, NULL, NULL, "field1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  field2 = ESMC_FieldCreateMeshTypeKind(mesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_ELEMENT, 
    NULL, NULL, NULL, "field2", &rc);
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
  strcpy(name, "Get an ESMC_Array object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_FieldGetArray(field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * ptr = ESMC_FieldGetPtr(field, 0, &rc);
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
  strcpy(name, "Get an ESMC_Mesh object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh2 = ESMC_FieldGetMesh(field2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get an ESMC_Array object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array2 = ESMC_FieldGetArray(field2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * ptr2 = ESMC_FieldGetPtr(field2, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print an ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldPrint(field2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&field2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object from grid and arrayspec");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  maxIndex = (int *)malloc(2*sizeof(int));
  maxIndex[0] = 4;
  maxIndex[1] = 4;
  rc = ESMC_InterfaceIntSet(&i_maxIndex, maxIndex, 2);
  rc = ESMC_ArraySpecSet(&arrayspec, 2, ESMC_TYPEKIND_R8);
  grid=ESMC_GridCreateNoPeriDim(&i_maxIndex, 0, 0, NULL, &rc);
  field=ESMC_FieldCreateGridArraySpec(grid, arrayspec, staggerloc, 0, 0, 0, "must have a name", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object from grid and arrayspec with null optional arguments");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  field=ESMC_FieldCreateGridArraySpec(grid, arrayspec, staggerloc, 0, 0, 0, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);

  free(elemId);
  free(elemType);
  free(elemConn);

  free(gridToFieldMap);
  free(ungriddedLBound);
  free(ungriddedUBound);
  free(maxIndex);
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
