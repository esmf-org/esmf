/* $Id: ESMC_Info.c,v 1.2 2010/07/09 20:16:13 theurich Exp $ */

#include <stdio.h>
#include "ESMC.h"

int main(void){

  ESMC_Initialize(NULL, ESMC_ArgLast);
  
  printf("ESMC_Info\n");
  
  printf("  ESMF_MAJOR_VERSION:  %d\n", ESMF_MAJOR_VERSION);
  printf("  ESMF_MINOR_VERSION:  %d\n", ESMF_MINOR_VERSION);
  printf("  ESMF_REVISION:       %d\n", ESMF_REVISION);
  printf("  ESMF_PATCHLEVEL:     %d\n", ESMF_PATCHLEVEL);
  printf("  ESMF_VERSION_STRING: %s\n", ESMF_VERSION_STRING);

  ESMC_Finalize();
  
  return 0;
}
