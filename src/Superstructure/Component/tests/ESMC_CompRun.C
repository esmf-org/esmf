// $Id: ESMC_CompRun.C,v 1.16 2007/08/23 17:16:03 cdeluca Exp $
//
// Test code which creates a new Component in C++.  The called
// component is still in F90.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// //DESCRIPTION:
// Tests, cursory and exahustive, for Component Create code.
//
//
//\begin{verbatim}

#include <stdlib.h>
#include <stdio.h>
#include "ESMCI.h"

extern "C" void FTN(externaluser_setservices)(ESMC_GridComp *, int *rc);

main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_DELayout *delayout;
    ESMC_GridComp *comp1;
    ESMC_State *instate, *outstate;
    ESMC_Clock *clock = NULL;
        
//-------------------------------------------------------------------------
//   // Test 1:
//   //
//   //  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    rc = ESMCI_Initialize(argc, argv);
 
    printf("Component Test 1:\n");

    // TODO: query framework for default layout here

    cname = "Atmosphere";
    comp1 = ESMC_GridCompCreate(cname, ESMF_ATM, NULL, "grid.rc", clock, &rc);

    printf("GridComp Create returned, name = '%s'\n", cname);


    rc = comp1->ESMC_GridCompPrint("");
    printf("GridComp Print returned\n");

    // register other entry points
    rc = comp1->ESMC_GridCompSetServices(FTN(externaluser_setservices));

    // in a real application, these need to be created first.
    instate = NULL;
    outstate = NULL;
    clock = NULL;
    rc = comp1->ESMC_GridCompInitialize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("GridComp Initialize returned\n");

    rc = comp1->ESMC_GridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("GridComp Run returned\n");

    rc = comp1->ESMC_GridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("GridComp Run returned\n");

    rc = comp1->ESMC_GridCompFinalize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("GridComp Finalize returned\n");

    rc = comp1->ESMC_GridCompPrint("");
    printf("GridComp Print returned\n");

    rc = ESMC_GridCompDestroy(comp1);
    printf("GridComp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMCI_Finalize();

    exit (0);
}
//\end{verbatim}
