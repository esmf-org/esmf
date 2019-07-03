// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
// !PROGRAM: ESMC_GridUTest - Check ESMC_Grid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

ESMC_Grid create_grid_2D(int *maxIndex, double *minCoord, double *maxCoord, int *rc);

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int localPet, petCount;
  int localrc, rc;
  bool correct;
  ESMC_StaggerLoc stagger = ESMC_STAGGERLOC_CORNER;

  ESMC_VM vm;
  bool pass;
  int maxIndex[2];
  double minCoord[2];
  double maxCoord[2];

#define NUM_SIDE_A_GRIDS 1
  ESMC_Grid side_a_grids[NUM_SIDE_A_GRIDS];

#define NUM_SIDE_B_GRIDS 2
  ESMC_Grid side_b_grids[NUM_SIDE_B_GRIDS];

  ESMC_XGrid xgrid;
  int factorListCount=0;
  double *factorList;
  int *factorIndexList;


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, 
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);
  if (rc != ESMF_SUCCESS) return 0;


#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  //EX_UTest
  //  Setup Side A Grid - 0 
  //----------------------------------------------------------------------------
  strcpy(name, "Create Side A Grid");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Set Grid info
  maxIndex[0]=10;   maxIndex[1]=10;
  minCoord[0]=5.0;  minCoord[1]=5.0;
  maxCoord[0]=40.0; maxCoord[1]=40.0;

  // Create Grid
  side_a_grids[0]=create_grid_2D(maxIndex, minCoord, maxCoord, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Debug
  //rc=ESMC_GridWrite(side_a_grids[0], stagger, "side_a_grid0");  

  ESMC_Test((rc==ESMF_SUCCESS)&&correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  //  Setup Side B Grid - 0 
  //----------------------------------------------------------------------------
  strcpy(name, "Create Side B Grid - 0");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Set Grid info
  maxIndex[0]=20;   maxIndex[1]=20;
  minCoord[0]=5.0;  minCoord[1]=5.0;
  maxCoord[0]=25.0; maxCoord[1]=25.0;

  // Create Grid
  side_b_grids[0]=create_grid_2D(maxIndex, minCoord, maxCoord, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Debug
  //rc=ESMC_GridWrite(side_b_grids[0], stagger, "side_b_grid0x");

  ESMC_Test((rc==ESMF_SUCCESS)&&correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------



  //----------------------------------------------------------------------------
  //EX_UTest
  //  Setup Side B Grid - 1 
  //----------------------------------------------------------------------------
  strcpy(name, "Create Side B Grid - 1");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Set Grid info
  maxIndex[0]=10;   maxIndex[1]=10;
  minCoord[0]=25.0;  minCoord[1]=25.0;
  maxCoord[0]=45.0; maxCoord[1]=45.0;

  // Create Grid
  side_b_grids[1]=create_grid_2D(maxIndex, minCoord, maxCoord, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Debug
  //rc=ESMC_GridWrite(side_b_grids[1], stagger, "side_b_grid1x");
  //printf("gridwrite rc=%d SUCCESS=%d\n",rc,ESMF_SUCCESS);

  ESMC_Test((rc==ESMF_SUCCESS)&&correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  //  Create XGrid
  //----------------------------------------------------------------------------
  strcpy(name, "Create XGrid");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Set priority lists
  int sideAGridPriority[NUM_SIDE_A_GRIDS]={1};
  ESMC_InterArrayInt i_sideAGridPriority;
  localrc = ESMC_InterArrayIntSet(&i_sideAGridPriority, 
                                  sideAGridPriority, NUM_SIDE_A_GRIDS);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;


  int sideBGridPriority[NUM_SIDE_B_GRIDS]={1,2};
  ESMC_InterArrayInt i_sideBGridPriority;
  localrc = ESMC_InterArrayIntSet(&i_sideBGridPriority, 
                                  sideBGridPriority, NUM_SIDE_B_GRIDS);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Set mask values, these aren't used yet, but we can at least check 
  // that they make it into the F90 API correctly.
  int maskValsA[]={1,2};
  ESMC_InterArrayInt i_maskValsA;
  localrc = ESMC_InterArrayIntSet(&i_maskValsA, 
                              maskValsA, 2);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  int maskValsB[]={4,5};
  ESMC_InterArrayInt i_maskValsB;
  localrc = ESMC_InterArrayIntSet(&i_maskValsB, 
                              maskValsB, 2);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Create XGrid
  xgrid=ESMC_XGridCreate(NUM_SIDE_A_GRIDS, side_a_grids, 
                         0,  NULL, // int sideAMeshCount,  ESMC_Mesh *sideAMesh, 
                         NUM_SIDE_B_GRIDS, side_b_grids, 
                         0,  NULL, // int sideBMeshCount,  ESMC_Mesh *sideBMesh, 
                         &i_sideAGridPriority, 
                         NULL,  // ESMC_InterArrayInt *sideAMeshPriority, 
                         &i_sideBGridPriority, 
                         NULL,  // ESMC_InterArrayInt *sideBMeshPriority, 
                         &i_maskValsA, 
                         &i_maskValsB, 
                         1,     // int storeOverlay, 
                         &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  ESMC_Test((rc==ESMF_SUCCESS)&&correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetSideAGridCount
  strcpy(name, "Get Side A Grid Count");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  int sideAGridCount = ESMC_XGridGetSideAGridCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Check
  correct=true;
  if (sideAGridCount != NUM_SIDE_A_GRIDS) correct=false;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetSideAMeshCount
  strcpy(name, "Get Side A Mesh Count");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  int sideAMeshCount = ESMC_XGridGetSideAMeshCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Check
  correct=true;
  if (sideAMeshCount != 0) correct=false;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetSideBGridCount
  strcpy(name, "Get Side B Grid Count");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  int sideBGridCount = ESMC_XGridGetSideBGridCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;  

  // Check
  correct=true;
  if (sideBGridCount != NUM_SIDE_B_GRIDS) correct=false;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetSideBMeshCount
  strcpy(name, "Get Side B Mesh Count");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  int sideBMeshCount = ESMC_XGridGetSideBMeshCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Check
  correct=true;
  if (sideBMeshCount != 0) correct=false;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetDimCount
  strcpy(name, "Get Dim Count");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  int dimCount = ESMC_XGridGetDimCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Check
  correct=true;
  if (dimCount != 2) correct=false;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetDimCount
  strcpy(name, "Get Mesh");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get information
  ESMC_Mesh mesh = ESMC_XGridGetMesh(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Write to check
  // localrc=ESMC_MeshWrite(mesh,"tstMesh");
  //if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetArea
  strcpy(name, "Get area");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get Element Count
  int elementCount = ESMC_XGridGetElementCount(xgrid, &localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;


  // Allocate area array
  ESMC_R8 *elementArea=(ESMC_R8 *)malloc(elementCount*sizeof(ESMC_R8));

  // Fill with bad value
  for (int i=0; i< elementCount; i++) {
    elementArea[i]=-1.0;
  }

  // Get area
  ESMC_XGridGetElementArea(xgrid, elementArea, &localrc);

  // Check
  correct=true;
  for (int i=0; i< elementCount; i++) {
    if (elementArea[i] < 0.0) correct=false;
  }

  // Deallocate area array
  free(elementArea);

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid GetArea
  strcpy(name, "Get Centroid");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Allocate area array
  // DEPENDS ON elementCount and dimCount from before
  ESMC_R8 *elementCentroid=(ESMC_R8 *)malloc(elementCount*dimCount*sizeof(ESMC_R8));

  // Fill with bad value
  for (int p=0,i=0; i<elementCount; i++) {
    for (int j=0; j<dimCount; j++) {
      elementCentroid[p]=-1.0;
      p++;
    }
  }

  // Get area
  ESMC_XGridGetElementCentroid(xgrid, elementCentroid, &localrc);

  // Check
  // The Centroid should be within range of min/max grid coords. 
  correct=true;
  for (int p=0,i=0; i<elementCount; i++) {
    for (int j=0; j<dimCount; j++) {
      if ((elementCentroid[p] < 5.0) || (elementCentroid[p] > 45.0)) correct=false;
      p++;
    }
  }

  // Deallocate area array
  free(elementCentroid);

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid A2X
  strcpy(name, "Get A2X Sparse Matrix");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get sparse matrix
  ESMC_XGridGetSparseMatA2X(xgrid,
                            0,                // sideAIndex,
                            &factorListCount, // factorListCount,
                            &factorList,      // factorList,
                            &factorIndexList, // factorIndexList,
                            &localrc);

#if 0
  printf("A2X factorListCount=%d\n",factorListCount);
  for (int i=0; i<factorListCount; i++) {
    printf("%d s=%d d=%d w=%f  \n",i,factorIndexList[2*i],factorIndexList[2*i+1],factorList[i]);
  }
#endif

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid Get X2A
  strcpy(name, "Get X2A Sparse Matrix");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get sparse matrix
  ESMC_XGridGetSparseMatX2A(xgrid,
                            0,                // sideAIndex,
                            &factorListCount, // factorListCount,
                            &factorList,      // factorList,
                            &factorIndexList, // factorIndexList,
                            &localrc);

#if 0
  printf("X2A factorListCount=%d\n",factorListCount);
  for (int i=0; i<factorListCount; i++) {
    printf("%d s=%d d=%d w=%f  \n",i,factorIndexList[2*i],factorIndexList[2*i+1],factorList[i]);
  }
#endif

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid Get B2X
  strcpy(name, "Get B2X Sparse Matrix");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get sparse matrix
  ESMC_XGridGetSparseMatB2X(xgrid,
                            0,                // sideAIndex,
                            &factorListCount, // factorListCount,
                            &factorList,      // factorList,
                            &factorIndexList, // factorIndexList,
                            &localrc);

#if 0
  printf("B2X factorListCount=%d\n",factorListCount);
  for (int i=0; i<factorListCount; i++) {
    printf("%d s=%d d=%d w=%f  \n",i,factorIndexList[2*i],factorIndexList[2*i+1],factorList[i]);
  }
#endif

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Test XGrid Get X2B
  strcpy(name, "Get X2B Sparse Matrix");
  strcpy(failMsg, "Test either had incorrect result or ESMF method(s) did not succeed.");

  // Init error stuff
  correct=true;
  rc=ESMF_SUCCESS;

  // Get sparse matrix
  ESMC_XGridGetSparseMatX2B(xgrid,
                            0,                // sideAIndex,
                            &factorListCount, // factorListCount,
                            &factorList,      // factorList,
                            &factorIndexList, // factorIndexList,
                            &localrc);

#if 0
  printf("X2B factorListCount=%d\n",factorListCount);
  for (int i=0; i<factorListCount; i++) {
    printf("%d s=%d d=%d w=%f  \n",i,factorIndexList[2*i],factorIndexList[2*i+1],factorList[i]);
  }
#endif

  ESMC_Test((rc==ESMF_SUCCESS) && correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy XGrid
  strcpy(name, "Destroy XGrid");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_XGridDestroy(&xgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  Destroy Grids
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy Grids
  strcpy(name, "Destroy Grid A - 0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&(side_a_grids[0]));
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy Grids
  strcpy(name, "Destroy Grid B - 0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&(side_b_grids[0]));
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy Grids
  strcpy(name, "Destroy Grid B - 1");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&(side_b_grids[1]));
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

ESMC_Grid create_grid_2D(int *maxIndex, double *minCoord, double *maxCoord, int *rc) {

  int p;
  int elbnd[2],eubnd[2];
  ESMC_Grid outGrid;
  int localrc;

  // Set index space
  ESMC_InterArrayInt i_maxIndex;
  localrc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, 2);
  if (localrc != ESMF_SUCCESS) {
    *rc=ESMF_FAILURE;
    return outGrid;
  }

  // Set coordinate information 
  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  ESMC_IndexFlag     indexflag=ESMC_INDEX_GLOBAL;

  // Create Grid
  outGrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, &indexflag, &localrc);
  if (localrc != ESMF_SUCCESS) {
    *rc=ESMF_FAILURE;
    return outGrid;
  }

  // Add Coords
  localrc = ESMC_GridAddCoord(outGrid, ESMC_STAGGERLOC_CORNER);
  if (localrc != ESMF_SUCCESS) {
    *rc=ESMF_FAILURE;
    return outGrid;
  }

  // Get pointers to coord arrays
  double *xCoord = (double *)ESMC_GridGetCoord(outGrid, 1,
                                               ESMC_STAGGERLOC_CORNER,
                                               NULL,
                                               elbnd, 
                                               eubnd, &localrc);
  if (localrc != ESMF_SUCCESS) {
    *rc=ESMF_FAILURE;
    return outGrid;
  }

  double *yCoord = (double *)ESMC_GridGetCoord(outGrid, 2,
                                               ESMC_STAGGERLOC_CORNER,
                                               NULL,
                                               elbnd, 
                                               eubnd, &localrc);
  if (localrc != ESMF_SUCCESS) {
    *rc=ESMF_FAILURE;
    return outGrid;
  }

  // Fill Coords
  p = 0;
  for (int i1=elbnd[1]; i1<=eubnd[1]; ++i1) {
    for (int i0=elbnd[0]; i0<=eubnd[0]; ++i0) {
      xCoord[p]=minCoord[0]+(maxCoord[0]-minCoord[0])*((double)(i0-1))/((double)(maxIndex[0]));
      yCoord[p]=minCoord[1]+(maxCoord[1]-minCoord[1])*((double)(i1-1))/((double)(maxIndex[1]));
      p++;
    }
  }

  // Set as successful
  *rc=ESMF_SUCCESS;

  // Return Grid
  return outGrid;
} 
