// $Id: user_CComponent.C,v 1.4 2008/09/03 21:48:27 theurich Exp $
//==============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ESMF header
#include "ESMC.h"
void myInitInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  int localrc;
  ESMC_Array retrievedArray;

  // do something here
  printf("I am in myInitInC()\n");

  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);

  localrc=ESMC_StateGetArray(importState, "array1", &retrievedArray);
  localrc=ESMC_ArrayPrint(retrievedArray);
  
  double *ptr = (double *)ESMC_ArrayGetPtr(retrievedArray, 0, &localrc);
  
  printf("local ptr[0] = %g\n", ptr[0]);

  printf("Leaving myInitInC()\n");

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myRunInC()\n");

  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);

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

  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetInit, myInitInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetRun, myRunInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetFinal, myFinalInC, 0);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

} //extern "C"
