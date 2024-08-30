// $Id$
//
//-------------------------------------------------------------------------
//ESMF_SYSTEM_TEST        String used by test script to count system tests.
//=========================================================================

//-------------------------------------------------------------------------
// //DESCRIPTION:
// System test CFieldRegrid
// This system test is a basic check of the C-API when used for regridding.
// It creates two Grids, two Field on the Grids, and then does regridding
// between them. After the regridding, it does a simple check that the
// destination values resulting from the regridding seem reasonable.
// This test also checks the ESMC_Initialize() and ESMC_Finalize() calls.
//-------------------------------------------------------------------------
//\begin{verbatim}

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

int main(void) {
#define ESMC_METHOD "CFieldRegridSTest"  
#define MAXSTRLEN 80
  char name[MAXSTRLEN];
  char failMsg[MAXSTRLEN];
  char msg[MAXSTRLEN];
  int result=0;
  int localrc, finalrc;
  int localPet, petCount;
  ESMC_VM vm;
  enum ESMC_IndexFlag indexFlag=ESMC_INDEX_GLOBAL;

  //-------------------------------------------------------------------------
  //-------------------------------------------------------------------------
  strcpy(name, "System Test ESMF_CFieldRegrid");
  strcpy(failMsg, "System Test failure");

  // Initialize
  localrc = ESMC_Initialize(NULL, ESMC_InitArgLogFilename("CFieldRegridSTest.Log"),
                       ESMC_ArgLast);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  localrc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  // Output that we started on this PET
  printf("%s started on localPet=%d\n",name,localPet);

  // Create source Grid
  int srcMaxIndex[2]={20,36};
  ESMC_InterArrayInt srcMaxIndexIA;
  localrc=ESMC_InterArrayIntSet(&srcMaxIndexIA, srcMaxIndex, 2);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  
  ESMC_Grid srcGrid = ESMC_GridCreate1PeriDim(&srcMaxIndexIA, NULL, NULL, NULL, NULL, NULL,
                                              &indexFlag, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Add Center stagger
  localrc=ESMC_GridAddCoord(srcGrid, ESMC_STAGGERLOC_CENTER);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Create source Field
  ESMC_Field srcField=ESMC_FieldCreateGridTypeKind(srcGrid, ESMC_TYPEKIND_R8, ESMC_STAGGERLOC_CENTER,
                                                   NULL, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  
  
  // Get source coordinates
  int srcExLBnd[2], srcExUBnd[2];
  double *srcLon=(double *)ESMC_GridGetCoord(srcGrid, 1, ESMC_STAGGERLOC_CENTER, NULL,
                                          srcExLBnd, srcExUBnd, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  double *srcLat=(double *)ESMC_GridGetCoord(srcGrid, 2, ESMC_STAGGERLOC_CENTER, NULL,
                                          srcExLBnd, srcExUBnd, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Get Field data
  double *srcData=(double *)ESMC_FieldGetPtr(srcField, 0, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Set source coords and data
  double dSrcLon=360.0/((double)srcMaxIndex[0]); // Delta increment for each lon index
  double dSrcLat=180.0/((double)srcMaxIndex[1]); // Delta increment for each lat index
  for (int p=0,i1=srcExLBnd[1]; i1<=srcExUBnd[1]; i1++) {
    for (int i0=srcExLBnd[0]; i0<=srcExUBnd[0]; i0++) {
      srcLon[p]=dSrcLon*(double)i0;       
      srcLat[p]=dSrcLat*(double)i1-90.0-0.5*dSrcLat; // Center lat by moving half a cell down
      srcData[p]=srcLat[p]; // Something simple since this more a test of the C interface than of regridding
      p++;
    }
  }

  // Debug output
  // localrc = ESMC_GridWrite(srcGrid, ESMC_STAGGERLOC_CENTER, "srcGrid");
  //if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Create destination Grid
  int dstMaxIndex[2]={10,18};
  ESMC_InterArrayInt dstMaxIndexIA;
  localrc=ESMC_InterArrayIntSet(&dstMaxIndexIA, dstMaxIndex, 2);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  
  ESMC_Grid dstGrid = ESMC_GridCreate1PeriDim(&dstMaxIndexIA, NULL, NULL, NULL, NULL, NULL,
                                              &indexFlag, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);


  // Add center stagger
  localrc=ESMC_GridAddCoord(dstGrid, ESMC_STAGGERLOC_CENTER);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Create destination Field
  ESMC_Field dstField=ESMC_FieldCreateGridTypeKind(dstGrid, ESMC_TYPEKIND_R8, ESMC_STAGGERLOC_CENTER,
                                                   NULL, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Get destination coords
  int dstExLBnd[2], dstExUBnd[2];
  double *dstLon=(double *)ESMC_GridGetCoord(dstGrid, 1, ESMC_STAGGERLOC_CENTER, NULL,
                                          dstExLBnd, dstExUBnd, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  double *dstLat=(double *)ESMC_GridGetCoord(dstGrid, 2, ESMC_STAGGERLOC_CENTER, NULL,
                                          dstExLBnd, dstExUBnd, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Get Field data
  double *dstData=(double *)ESMC_FieldGetPtr(dstField, 0, &localrc);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Set destination Grid coords and init destination Field to 0.0
  double dDstLon=360.0/((double)dstMaxIndex[0]); // Delta increment for each lon index
  double dDstLat=180.0/((double)dstMaxIndex[1]); // Delta increment for each lat index
  for (int p=0,i1=dstExLBnd[1]; i1<=dstExUBnd[1]; i1++) {
    for (int i0=dstExLBnd[0]; i0<=dstExUBnd[0]; i0++) {
      dstLon[p]=dDstLon*(double)i0;       
      dstLat[p]=dDstLat*(double)i1-90.0-0.5*dDstLat; // Center lat by moving half a cell down
      dstData[p]=0.0;
      p++;
    }
  }


  // Debug output
  //localrc = ESMC_GridWrite(dstGrid, ESMC_STAGGERLOC_CENTER, "dstGrid");
  //if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);


  // Calculate regridding weights and routeHandle
  ESMC_RouteHandle routehandle;
  localrc = ESMC_FieldRegridStore(srcField, dstField, NULL, NULL, &routehandle, 
                                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Apply routehandle
  localrc = ESMC_FieldRegrid(srcField, dstField, routehandle, NULL);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  
  // Deallocate routehandle
  localrc = ESMC_FieldRegridRelease(&routehandle);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  // Check output
  bool correct=true;
  for (int p=0,i1=dstExLBnd[1]; i1<=dstExUBnd[1]; i1++) {
    for (int i0=dstExLBnd[0]; i0<=dstExUBnd[0]; i0++) {

      // Check that destination values are close to what's expected (latitude)
      if (abs(dstLat[p]-dstData[p]) > 1.0E-12) correct=false;
      
      // Move to next memory location
      p++;
    }
  }
  
  // Deallocate source Field
  localrc = ESMC_FieldDestroy(&srcField);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  // Deallocate source Grid
  localrc = ESMC_GridDestroy(&srcGrid);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  
  // Deallocate destination Field
  localrc = ESMC_FieldDestroy(&dstField);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  // Deallocate destination Grid
  localrc = ESMC_GridDestroy(&dstGrid);
  if (localrc != ESMF_SUCCESS) ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  
  // Output that this PET finished test
  printf("%s finished on localPet=%d\n",name,localPet);

  // Write standard system test output.
  // (On Fortran side this is written by ESMF_STest, but that doesn't exist in C.) 
  snprintf(msg,MAXSTRLEN,"NUMBER_OF_PROCESSORS %d\n",petCount);
  ESMC_LogWrite(msg, ESMC_LOGMSG_INFO);

  // Check that test results were correct
  ESMC_Test(correct, name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // NOTE: 
  // There's kind of a chicken and egg problem here where when you run finalize
  // it gets rid of the log output, but then you can't report if finalize
  // succeeded. However, at least it gets run here and outputs an error to
  // standard out so we can see a failure with manual testing.
  // Also, I'm testing both interfaces here because we don't have
  // another test of them. Once we do, get rid of the if and just test one. 
  if (localPet%2 == 0) {
    localrc = ESMC_Finalize();
    if (localrc != ESMF_SUCCESS) {
      printf("ERROR! ESMF_Finalize() did not return success!\n");
    }
  } else {
    localrc = ESMC_FinalizeWithFlag(ESMC_END_NORMAL);
    if (localrc != ESMF_SUCCESS) {
      printf("ERROR! ESMF_FinalizeWithFlag() did not return success!\n");
    }
  }
}

