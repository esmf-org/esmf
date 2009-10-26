// $Id: user_CComponent.C,v 1.12 2009/10/26 23:15:26 theurich Exp $
//
// Example/test code which shows User Component calls.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

// standard C headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ESMF header -- provides access to the entire public ESMF C API
#include "ESMC.h"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void myInitInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  // Array variables
  ESMC_Array retrievedArray;
  double *ptr;
  int i, j, ij;
  
  // Mesh variables
  ESMC_Mesh mesh;
  int pdim=2;
  int sdim=3;
  int num_elem, num_node, conn_size;
  int *nodeId, *nodeOwner;
  double *nodeCoord;
  int *elemId, *elemType, *elemConn;

  // Field variables
  ESMC_Field field;
  ESMC_ArraySpec arrayspec;
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound;
  ESMC_InterfaceInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
    
  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myInitInC()\n");

  // get Array from import State
  *rc=ESMC_StateGetArray(importState, "array1", &retrievedArray);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // obtain access to Array data
  ptr = (double *)ESMC_ArrayGetPtr(retrievedArray, 0, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // test accesibility of Array data via print
  printf("local ptr[0] = %g\n", ptr[0]);
  printf("local ptr[1] = %g\n", ptr[1]);

  // modify Array data
  for (j=0; j<2; j++){
    for (i=0; i<5; i++){
      // interpret as 2D array with contiguous, column-major storage
      ij= j*5 +i;
      ptr[ij] = j;
    }
  }
  
  // print a modified value
  printf("local ptr[0] = %g\n", ptr[0]);
  
  // Create a Mesh from VTK file
  mesh = ESMC_MeshCreate(&pdim, &sdim, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // Read input files' header data
  *rc = ESMC_MeshVTKHeader("data/testmesh", &num_elem, &num_node, &conn_size);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // Allocate the arrays to describe Mesh
  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (3*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));

  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (conn_size * sizeof (int));

  // Read input files
  *rc = ESMC_MeshVTKBody("data/testmesh", nodeId, nodeCoord, nodeOwner, elemId,
    elemType, elemConn);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // VTKBody returns zero based elemConn, so make them 1 based
  for (int i = 0; i < conn_size; i++){
    elemConn[i] = elemConn[i]+1;
  }
  
  // Add node information to the mesh
  *rc = ESMC_MeshAddNodes(mesh, &num_node, nodeId, nodeCoord, nodeOwner);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // Add element information to the mesh
  *rc = ESMC_MeshAddElements(mesh, &num_elem, elemId, elemType, elemConn);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // garbage collection of temporary variables used to create Mesh object
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  
  // Create a Field from Mesh
  
  // Setup arrayspec
  *rc = ESMC_ArraySpecSet(&arrayspec, 3, ESMC_TYPEKIND_I4);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // Setup gridToFieldMap    
  gridToFieldMap = (int *)malloc(sizeof(int));
  gridToFieldMap[0] = 1;
  i_gridToFieldMap = ESMC_InterfaceIntCreate(gridToFieldMap, 1, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // Setup ungriddedLBound    
  ungriddedLBound = (int *)malloc(2*sizeof(int));
  ungriddedLBound[0] = 1;
  ungriddedLBound[1] = 1;
  i_ungriddedLBound = ESMC_InterfaceIntCreate(ungriddedLBound, 2, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // Setup ungriddedUBound    
  ungriddedUBound = (int *)malloc(2*sizeof(int));
  ungriddedUBound[0] = 2;
  ungriddedUBound[1] = 3;
  i_ungriddedUBound = ESMC_InterfaceIntCreate(ungriddedUBound, 2, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // Finally create Field  
  field = ESMC_FieldCreate(mesh, arrayspec, i_gridToFieldMap, i_ungriddedLBound,
    i_ungriddedUBound, "Field from C", rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // Add Field to the export State
  *rc = ESMC_StateAddField(exportState, field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // garbage collection of temporary variables used to create Field object
  *rc = ESMC_InterfaceIntDestroy(&i_gridToFieldMap);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  free(gridToFieldMap);
  *rc = ESMC_InterfaceIntDestroy(&i_ungriddedLBound);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  free(ungriddedLBound);
  *rc = ESMC_InterfaceIntDestroy(&i_ungriddedUBound);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  free(ungriddedUBound);
}


void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  ESMC_Array retrievedArray;
  double *ptr;
  int i, j, ij;
    
  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myRunInC()\n");

  // get Array from import State
  *rc=ESMC_StateGetArray(importState, "array1", &retrievedArray);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // obtain access to Array data
  ptr = (double *)ESMC_ArrayGetPtr(retrievedArray, 0, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // verify data values
  for (j=0; j<2; j++){
    for (i=0; i<5; i++){
      ij= j*5 +i;
      if ( fabs(ptr[ij]-float((j+1)*10+i+1)) > 1.e-8 ){
        printf("Array has wrong values at i=%d, j=%d, ij=%d\n", i, j, ij);
        *rc = ESMF_FAILURE; // indicate failure in return code
        return; // bail out
      }
    }
  }
}


void myFinalInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  ESMC_Field field;
  ESMC_Mesh mesh;
  
  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myFinalInC()\n");

  *rc = ESMC_StatePrint(importState);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  *rc = ESMC_ClockPrint(*clock);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // get Field from export State
  *rc = ESMC_StateGetField(exportState, "Field from C", &field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // print the Field object
  *rc = ESMC_FieldPrint(field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  // get the Mesh object from the Field
  mesh = ESMC_FieldGetMesh(field, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // destroy Field object
  *rc = ESMC_FieldDestroy(&field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // destroy Mesh object
  *rc = ESMC_MeshDestroy(&mesh);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

extern "C" {
  // The SetServices entry point must ensure to have external C linkage,
  // so it can be called from Fortran.
  
  void FTN(my_setservicesinc)(ESMC_GridComp gcomp, int *rc){
    // set entry points for standard Component methods Init(), Run(), Finalize()
    
    // initialize return code
    *rc = ESMF_SUCCESS;
    
    printf("In mySetServicesInC()\n");
    
    *rc = ESMC_GridCompPrint(gcomp, "");
    if (*rc!=ESMF_SUCCESS) return;  // bail out

    *rc = ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, myInitInC, 1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
    *rc = ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, myRunInC, 1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
    *rc = ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, myFinalInC, 1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
  }
} //extern "C"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
