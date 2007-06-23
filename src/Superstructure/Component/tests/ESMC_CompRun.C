// $Id: ESMC_CompRun.C,v 1.13 2007/06/23 04:01:00 cdeluca Exp $
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

extern "C" void FTN(externaluser_setservices)(ESMC_IGridComp *, int *rc);

main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_DELayout *delayout;
    ESMC_IGridComp *comp1;
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
    comp1 = ESMC_IGridCompCreate(cname, ESMF_ATM, NULL, "igrid.rc", clock, &rc);

    printf("IGrid Comp Create returned, name = '%s'\n", cname);


    rc = comp1->ESMC_IGridCompPrint("");
    printf("IGrid Comp Print returned\n");

    // register other entry points
    rc = comp1->ESMC_IGridCompSetServices(FTN(externaluser_setservices));

    // in a real application, these need to be created first.
    instate = NULL;
    outstate = NULL;
    clock = NULL;
    rc = comp1->ESMC_IGridCompInitialize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("IGrid Comp Initialize returned\n");

    rc = comp1->ESMC_IGridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("IGrid Comp Run returned\n");

    rc = comp1->ESMC_IGridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("IGrid Comp Run returned\n");

    rc = comp1->ESMC_IGridCompFinalize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("IGrid Comp Finalize returned\n");

    rc = comp1->ESMC_IGridCompPrint("");
    printf("Comp Print returned\n");

    rc = ESMC_IGridCompDestroy(comp1);
    printf("Comp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMCI_Finalize();

    exit (0);
}
//\end{verbatim}
