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

  ESMC_ArraySpec arrayspec, arrayspec2;
  enum ESMC_StaggerLoc staggerloc=ESMC_STAGGERLOC_CENTER;
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound, *maxIndex;
  ESMC_InterArrayInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  ESMC_InterArrayInt i_maxIndex;
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

  ESMC_LocStream locstream;
  int ls_size=16;
  const char *keyNameX="ESMF:X";
  const char *keyNameY="ESMF:Y";
  const char *keyNameM="ESMF:Mask";
  ESMC_Array keyArray;
  double *farray;
  double *farray2;
  int *farray3;
  ESMC_Field fieldls1, fieldls2;
  double * fieldls1ptr, *fieldls2ptr;

  bool correct = false;

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
  rc = ESMC_InterArrayIntSet(&i_gridToFieldMap, gridToFieldMap, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedLBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedLBound = (int *)malloc(2*sizeof(int));
  ungriddedLBound[0] = 1;
  ungriddedLBound[1] = 1;
  rc = ESMC_InterArrayIntSet(&i_ungriddedLBound, ungriddedLBound, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedUBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedUBound = (int *)malloc(2*sizeof(int));
  ungriddedUBound[0] = 2;
  ungriddedUBound[1] = 3;
  rc = ESMC_InterArrayIntSet(&i_ungriddedUBound, ungriddedUBound, 2);
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
  rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, 2);
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


  //---------------------------- LOCSTREAM -------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamCreateLocal");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  locstream = ESMC_LocStreamCreateLocal(ls_size, NULL, &local_coordSys, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamAddKeyAlloc");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_TypeKind_Flag local_typeKind = ESMC_TYPEKIND_R8;
  rc = ESMC_LocStreamAddKeyAlloc(locstream,keyNameX,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray = (double *) ESMC_LocStreamGetKeyPtr(locstream,keyNameX,0,&rc);
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
  rc = ESMC_LocStreamAddKeyAlloc(locstream,keyNameY,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray2 = (double *) ESMC_LocStreamGetKeyPtr(locstream,keyNameY,0,&rc);
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
  rc = ESMC_LocStreamAddKeyAlloc(locstream,keyNameM,&local_typeKind);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray3 = (int *) ESMC_LocStreamGetKeyPtr(locstream,keyNameM,0,&rc);
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
  strcpy(name, "Create ESMC_Field object on LocStream using TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  fieldls1 = ESMC_FieldCreateLocStreamTypeKind(locstream, ESMC_TYPEKIND_R8,
                                               NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  fieldls1ptr = (double *)ESMC_FieldGetPtr(fieldls1, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  {
    int i;
    for(i=0;i<ls_size;++i)
      fieldls1ptr[i] = -9999.0;
  }

#if 0
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Create ESMC_Field object on LocStream using ArraySpec");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec2, 1, ESMC_TYPEKIND_R8);
  fieldls2 = ESMC_FieldCreateLocStreamArraySpec(locstream, arrayspec2,
    NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object 2");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  fieldls2ptr = (double *) ESMC_FieldGetPtr(fieldls2, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  {
    int i;
    for(i=0;i<ls_size;++i) {
      fieldls2ptr[i] = -9999.0;
      if (fieldls2ptr[i] != fieldls1ptr[i]) correct = false;
    }
  }

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Validate C pointers to data from ESMC_Field objects created on LocStream");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Test((correct == true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

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
