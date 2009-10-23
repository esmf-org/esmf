// $Id: user_CComponent.C,v 1.10 2009/10/23 17:44:41 theurich Exp $
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

  ESMC_Array retrievedArray;
  double *ptr;
  int i, j, ij;
    
  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myInitInC()\n");

  // get Array from export State
  *rc=ESMC_StateGetArray(exportState, "array1", &retrievedArray);
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
}


void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  ESMC_Array retrievedArray;
  double *ptr;
  int i, j, ij;
    
  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myRunInC()\n");

  // get Array from export State
  *rc=ESMC_StateGetArray(exportState, "array1", &retrievedArray);
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

  // initialize return code
  *rc = ESMF_SUCCESS;
    
  printf("In myFinalInC()\n");

  *rc = ESMC_StatePrint(importState);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  
  *rc = ESMC_ClockPrint(*clock);
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
