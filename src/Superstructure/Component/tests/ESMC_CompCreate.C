// $Id: ESMC_CompCreate.C,v 1.5 2004/04/20 19:30:08 nscollins Exp $
//
// Test code which creates a new Component.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// Tests, cursory and exahustive, for Component Create code.
//
//
//\begin{verbatim}

#include <stdlib.h>
#include <stdio.h>
#include "ESMC.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_Init.h"


int main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_newDELayout *delayout;
    ESMC_GridComp *comp1;
        
//-------------------------------------------------------------------------
//   Test 1:
//   
//   Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    rc = ESMC_Initialize(argc, argv);
 
    printf("Component Test 1:\n");

    // TODO: query framework for default layout here

    cname = "Atmosphere";
    delayout = NULL;
    comp1 = ESMC_GridCompCreate(cname, delayout, ESMF_ATM, NULL, "grid.rc", &rc);

    printf("Grid Comp Create returned, name = '%s'\n", cname);

    rc = comp1->ESMC_GridCompPrint("");
    printf("Comp Print returned\n");

    rc = ESMC_GridCompDestroy(comp1);
    printf("Comp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMC_Finalize();

    exit (0);
}
//\end{verbatim}
    
