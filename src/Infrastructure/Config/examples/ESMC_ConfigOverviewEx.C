//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//

//-------------------------------------------------------------------------
//ESMF_EX______AMPLE        String used by test script to count examples.
//==============================================================================
//
// //DESCRIPTION:
// Example/test code which performs simple Configuration File routines.
//
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_LogErr.h"

#undef ESMC_METHOD
#define ESMC_METHOD "main()"
int main ( int argc, char **argv )
{
      // Local variables
      int i, j;
      ESMC_VM *vm;

//BOE
//\subsubsection{Common Code Arguments}

// Common Arguments used in the following code fragments:
//EOE

//BOC
      char fname [ESMF_MAXSTR] = "myResourceFile.rc";    // file name
      char fn1[10], fn2[10], fn3[10];
      int rc;             // error return code (0 is OK)
      ESMC_I4 n = 0;
      ESMC_R4 r = 0.0;
      ESMC_R4 table[7][3];

      ESMC_Config *cf;
//EOC

      int finalrc;
      finalrc = ESMF_SUCCESS;
  
      rc = ESMC_Initialize(NULL, ESMC_ArgLast);
      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_Initialize' failed\n");
      }
  
      vm = ESMC_VMGetGlobal(&rc);
      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_VMGetGlobal' failed\n");
      }

//-------------------------------------------------------------------------
//   // Example 1:
//   //

//BOE
//\subsubsection{Creation of a Config}

// The first step is to create the {\tt ESMC\_Config} and load the
// ASCII resource (rc) file into memory\footnote{See next section
// for a complete description of parameters for each routine/function}:
//EOE

//BOC  
      cf = ESMC_ConfigCreate(&rc);
//EOC

      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigCreate' failed\n");
      }

//BOC
      rc = ESMC_ConfigLoadFile(cf, fname, ESMC_ArgLast);
//EOC

      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigLoadFile' failed\n");
      }

//BOE
//\subsubsection{Retrieval of constants}

// The next step is to select the label (record) of interest, say
//EOE

//BOC
      rc = ESMC_ConfigFindLabel(cf, "constants:");
//EOC

      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigFindLabel' failed\n");
      }

//BOE
// Two constants, r and n, can be retrieved with the following code
// fragment:
//EOE

//BOC
      rc = ESMC_ConfigGetAttribute(cf, &r, ESMC_TYPEKIND_R4,
                      ESMC_ArgLast);  // results in r = 3.1415
      rc = ESMC_ConfigGetAttribute(cf, &n, ESMC_TYPEKIND_I4,
                      ESMC_ArgLast);  // results in n = 25
//EOC
      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigGetAttribute' failed\n");
      }

      float rtest = 3.1415;
      int ntest = 25;
      if (r != rtest || n != ntest) {
        finalrc = ESMF_FAILURE;
        printf("bad results from 'ESMC_ConfigGetAttribute'\n");
        printf("results expected: r = %f,  n = %i\n",rtest,ntest);
        printf("results obtained: r = %f,  n = %i\n",r,n);
      }

//BOE
//\subsubsection{Retrieval of file names}

// File names can be retrieved with the following code fragment:
//EOE

//BOC
      rc = ESMC_ConfigFindLabel(cf, "my_file_names:");
//EOC

      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigFindLabel' failed\n");
      }

//BOC
      rc = ESMC_ConfigGetAttribute(cf, fn1, ESMC_TYPEKIND_CHARACTER,
                      ESMC_ArgLast);  // results in fn1 = 'jan87.dat'
      rc = ESMC_ConfigGetAttribute(cf, fn2, ESMC_TYPEKIND_CHARACTER,
                      ESMC_ArgLast);  // results in fn2 = 'jan88.dat'
      rc = ESMC_ConfigGetAttribute(cf, fn3, ESMC_TYPEKIND_CHARACTER,
                      ESMC_ArgLast);  // results in fn3 = 'jan89.dat'
//EOC
      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigGetAttribute' failed\n");
      }

      if (strncmp("jan87.dat",fn1,9) != 0 ||
          strncmp("jan88.dat",fn2,9) != 0 ||
          strncmp("jan89.dat",fn3,9) != 0   ) {
        finalrc = ESMF_FAILURE;
        printf("bad results from ESMC_ConfigGetAttribute\n");
        printf("results obtained: fn1 = %s,  fn2 = %s,  fn3 = %s\n",fn1,fn2,fn3);
        printf("results expected: fn1 = %s,  fn2 = %s,  fn3 = %s\n",
               "jan87.dat","jan88.dat","jan89.dat");
      }

//BOE
//\subsubsection{Retrieval of tables}

// To access tabular data, the user first must use
// {\tt ESMC\_ConfigFindLabel()} to locate the beginning of the table, e.g.,
//EOE

//BOC
      rc = ESMC_ConfigFindLabel(cf, "my_table_name::");
//EOC

      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigFindLabel' failed\n");
      }

//BOE
// Subsequently, {\tt call ESMC\_ConfigNextLine()} can be used to gain
// access to each row of the table. Here is a code fragment to read the above
// table (7 rows, 3 columns):
//EOE

//BOC
      for (i = 0; i < 7; i++) {
        rc = ESMC_ConfigNextLine(cf, ESMC_ArgLast);
        for (j = 0; j < 3; j++) {
          rc = ESMC_ConfigGetAttribute(cf, (&table[i][j]), ESMC_TYPEKIND_R4,
                          ESMC_ArgLast);
        }
      }
//EOC

//BOE
//\subsubsection{Destruction of a Config}

// The work with the configuration file {\tt cf} is finalized by call to
// {\tt ESMC\_ConfigDestroy()}:
//EOE

//BOC
      rc = ESMC_ConfigDestroy(cf);
//EOC
      if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
        printf("'ESMC_ConfigDestroy' failed\n");
      }

      if (finalrc == ESMF_SUCCESS)
        printf("PASS: ESMC_ConfigOverviewEx.C\n");
      else
        printf("FAIL: ESMC_ConfigOverviewEx.C\n");

      rc = ESMC_Finalize();

}
