/* Simple ESMC application */

#include <stdio.h>
#include "ESMC.h"

int main(void){

  ESMC_Initialize(NULL, ESMC_InitArgDefaultConfigFilename("dummy.config"),
    ESMC_ArgLast);
  
  printf("hi from program esmc_application\n");
  printf("======================= finished ================================\n");

  ESMC_Finalize();
  
  return 0;
}
