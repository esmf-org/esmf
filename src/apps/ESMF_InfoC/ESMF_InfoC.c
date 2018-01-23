// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdio.h>
#include "ESMC.h"
// #include "ESMC_Util.h"

int main(int argc, char *argv[]){

  int rc, localPet;
  int argIndex;
  int argFlag;
  ESMC_VM vm;

  ESMC_Initialize(NULL, ESMC_InitArgLogKindFlag(ESMC_LOGKIND_NONE), ESMC_ArgLast);
  
  vm = ESMC_VMGetGlobal(&rc);
  
  rc = ESMC_VMGet(vm, &localPet, NULL, NULL, NULL, NULL, NULL);
  
  if (localPet == 0){
    argFlag = 0;
    int vFlag = 0;
    int versionFlag = 0;

    /* check for standard command line arguments */
    argIndex = ESMC_UtilGetArgIndex(argc, argv, "--help", &rc);
    if (argIndex >= 0){
      argFlag=1;
      /* standard --help argument was specified */
      printf("ESMF_InfoC: Print information about the ESMF installation.\n");
      printf("Options:\n");
      printf("  --help        Display this information and exit.\n");
      printf("  --version     Display ESMF version and license information "
        "and exit.\n");
      printf("  -V            Display ESMF version string and exit.\n");
      printf("\n");
    }
    argIndex = ESMC_UtilGetArgIndex(argc, argv, "--version", &rc);
    if (argIndex >= 0){
      argFlag=1;
      /* standard --help argument was specified */
      versionFlag = 1;
    }
    argIndex = ESMC_UtilGetArgIndex(argc, argv, "-V", &rc);
    if (argIndex >= 0){
      argFlag=1;
      /* standard --help argument was specified */
      vFlag = 1;
    }
    if (argFlag) {
      ESMC_UtilVersionPrint (vFlag, versionFlag, &rc);
    } else {
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
