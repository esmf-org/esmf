// $Id: user_CComponent.C,v 1.8 2009/03/17 05:21:36 theurich Exp $
//==============================================================================


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ESMF header
#include "ESMC.h"

void myInitInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  int localrc;
  ESMC_Array retrievedArray;

  printf("I am in myInitInC()\n");

  localrc=ESMC_StateGetArray(exportState, "array1", &retrievedArray);
  
  double *ptr = (double *)ESMC_ArrayGetPtr(retrievedArray, 0, &localrc);
  
  printf("local ptr[0] = %g\n", ptr[0]);
  printf("local ptr[1] = %g\n", ptr[1]);


   for (int j=0; j<2; j++){
     for (int i=0; i<5; i++){
       int ij= j*5 +i;
       ptr[ij] = j;
     }
   }
    
  printf("local ptr[0] = %g\n", ptr[0]);

  printf("Reset farray values to farray(i,j)=j \n");
  printf("Leaving myInitInC()\n");

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  // local data
  ESMC_Array retrievedArray;
  int localrc;
  printf("I am in myRunInC()\n");


    localrc=ESMC_StateGetArray(exportState, "array1", &retrievedArray);
    double *ptr = (double *)ESMC_ArrayGetPtr(retrievedArray, 0, &localrc);

  // verify data values
     for (int j=0; j<2; j++){
       for (int i=0; i<5; i++){
         int ij= j*5 +i;
         if ( fabs(ptr[ij]-float((j+1)*10+i+1)) > 1.e-8 ){
           printf("ptr has wrong values at i=%d,j=%d,ij=%d\n", i,j,ij);
           *rc = ESMC_Finalize();
         }
       }
     }
     printf("Data values in exp state correct in myRunInC\n");

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myFinalInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myFinalizeInC()\n");

  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

extern "C" {
void FTN(my_registrationinc)(ESMC_GridComp gcomp, int *rc){
  // register Init(), Run(), Finalize()
  printf("I am in myRegistrationInC()\n");
  ESMC_GridCompPrint(gcomp, "");

  ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, myInitInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, myRunInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, myFinalInC, 0);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

} //extern "C"
