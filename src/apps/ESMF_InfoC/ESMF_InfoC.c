/* $Id: ESMF_InfoC.c,v 1.1 2010/09/22 23:46:55 theurich Exp $ */

#include <stdio.h>
#include "ESMC.h"

int main(int argc, char *argv[]){

  int rc, localPet;
  int argIndex;
  ESMC_VM vm;

  ESMC_Initialize(NULL, ESMC_ArgLast);
  
  vm = ESMC_VMGetGlobal(&rc);
  
  rc = ESMC_VMGet(vm, &localPet, NULL, NULL, NULL, NULL, NULL);
  
  if (localPet == 0){
    /* check for standard command line arguments */
    argIndex = ESMC_UtilGetArgIndex(argc, argv, "--help", &rc);
    if (argIndex >= 0){
      /* standard --help argument was specified */
      printf("ESMF_InfoC: Print information about the ESMF installation.\n");
      printf("Options:\n");
      printf("  --help        Display this information\n");
    }else{
      /* regular execution */
      printf("ESMF_InfoC\n");
      printf("  ESMF_MAJOR_VERSION:  %d\n", ESMF_MAJOR_VERSION);
      printf("  ESMF_MINOR_VERSION:  %d\n", ESMF_MINOR_VERSION);
      printf("  ESMF_REVISION:       %d\n", ESMF_REVISION);
      printf("  ESMF_PATCHLEVEL:     %d\n", ESMF_PATCHLEVEL);
      printf("  ESMF_VERSION_STRING: %s\n", ESMF_VERSION_STRING);
    }
  }

  ESMC_Finalize();
  
  return 0;
}
