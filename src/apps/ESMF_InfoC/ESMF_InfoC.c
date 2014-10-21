// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdio.h>
#include "ESMC.h"

int main(int argc, char *argv[]){

  int rc, localPet;
  int argIndex;
  int argFlag;
  ESMC_VM vm;

  ESMC_Initialize(NULL, ESMC_ArgLast);
  
  vm = ESMC_VMGetGlobal(&rc);
  
  rc = ESMC_VMGet(vm, &localPet, NULL, NULL, NULL, NULL, NULL);
  
  if (localPet == 0){
    argFlag = 0;
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
      printf("  ESMF_VERSION_STRING:       %s\n", ESMF_VERSION_STRING);
      printf("  ESMF_VERSION_MAJOR:        %d\n", ESMF_VERSION_MAJOR);
      printf("  ESMF_VERSION_MINOR:        %d\n", ESMF_VERSION_MINOR);
      printf("  ESMF_VERSION_REVISION:     %d\n", ESMF_VERSION_REVISION);
      printf("  ESMF_VERSION_PATCHLEVEL:   %d\n", ESMF_VERSION_PATCHLEVEL);
      printf("  ESMF_VERSION_PUBLIC:       %c\n", ESMF_VERSION_PUBLIC);
      printf("  ESMF_VERSION_BETASNAPSHOT: %c\n", ESMF_VERSION_BETASNAPSHOT);
      printf("\n");
      printf("Earth System Modeling Framework\n");
      printf("\n");
      printf("Copyright (c) 2002-2014 University Corporation for Atmospheric Research,\n");
      printf("Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory,\n");
      printf("University of Michigan, National Centers for Environmental Prediction,\n");
      printf("Los Alamos National Laboratory, Argonne National Laboratory,\n");
      printf("NASA Goddard Space Flight Center.  All rights reserved.\n");
      printf("\n");
      printf("Permission is hereby granted, free of charge, to any person obtaining a copy\n");
      printf("of this software and associated documentation files (the \"Software\"), to\n");
      printf("deal with the Software without restriction, including without limitation the\n");
      printf("rights to use, copy, modify, merge, publish, distribute, sublicense, and/or\n");
      printf("sell copies of the Software, and to permit persons to whom the Software is\n");
      printf("furnished to do so, subject to the following conditions:\n");
      printf("   1. Redistributions of source code must retain the above copyright notice,\n");
      printf("      this list of conditions and the following disclaimers.\n");
      printf("   2. Redistributions in binary form must reproduce the above copyright\n");
      printf("      notice, this list of conditions and the following disclaimers in the\n");
      printf("      documentation and/or other materials provided with the distribution.\n");
      printf("   3. Neither the names of the organizations developing this software, nor\n");
      printf("      its contributors may be used to endorse or promote products derived\n");
      printf("      from this Software without specific prior written permission.\n");
      printf("\n");
      printf("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n");
      printf("IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n");
      printf("FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE\n");
      printf("CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n");
      printf("LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\n");
      printf("FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS\n");
      printf("WITH THE SOFTWARE.\n");
      printf("\n");
    }
    argIndex = ESMC_UtilGetArgIndex(argc, argv, "-V", &rc);
    if (argIndex >= 0){
      argFlag=1;
      /* standard --help argument was specified */
      printf("  ESMF_VERSION_STRING:       %s\n", ESMF_VERSION_STRING);
      printf("\n");
    }
    if (!argFlag){
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
