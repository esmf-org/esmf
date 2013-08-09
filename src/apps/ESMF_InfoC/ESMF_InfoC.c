/* $Id$ */

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
      printf("\n");
      printf("  ESMF_VERSION_STRING:       %s\n", ESMF_VERSION_STRING);
      printf("\n");
      printf("  ESMF_VERSION_MAJOR:        %d\n", ESMF_VERSION_MAJOR);
      printf("  ESMF_VERSION_MINOR:        %d\n", ESMF_VERSION_MINOR);
      printf("  ESMF_VERSION_REVISION:     %d\n", ESMF_VERSION_REVISION);
      printf("  ESMF_VERSION_PATCHLEVEL:   %d\n", ESMF_VERSION_PATCHLEVEL);
      printf("  ESMF_VERSION_PUBLIC:       %c\n", ESMF_VERSION_PUBLIC);
      printf("  ESMF_VERSION_BETASNAPSHOT: %c\n", ESMF_VERSION_BETASNAPSHOT);
    }
  }

  ESMC_Finalize();
  
  return 0;
}
