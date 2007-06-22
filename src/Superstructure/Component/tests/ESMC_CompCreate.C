// $Id: ESMC_CompCreate.C,v 1.11 2007/06/22 23:21:47 cdeluca Exp $
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
#include "ESMCI.h"


int main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_DELayout *delayout;
    ESMC_InternGridComp *comp1;
    ESMC_Clock *clock = NULL;
        
//-------------------------------------------------------------------------
//   Test 1:
//   
//   Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    rc = ESMCI_Initialize(argc, argv);
 
    printf("Component Test 1:\n");

    // TODO: query framework for default layout here

    cname = "Atmosphere";
    comp1 = ESMC_InternGridCompCreate(cname, ESMF_ATM, NULL, "interngrid.rc", 
                                clock, &rc);

    printf("InternGrid Comp Create returned, name = '%s'\n", cname);

    rc = comp1->ESMC_InternGridCompPrint("");
    printf("Comp Print returned\n");

    rc = ESMC_InternGridCompDestroy(comp1);
    printf("Comp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMCI_Finalize();

    exit (0);
}
//\end{verbatim}
    
