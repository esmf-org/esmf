// $Id: ESMC_CompRun.C,v 1.12 2007/06/22 23:21:48 cdeluca Exp $
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

extern "C" void FTN(externaluser_setservices)(ESMC_InternGridComp *, int *rc);

main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_DELayout *delayout;
    ESMC_InternGridComp *comp1;
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
    comp1 = ESMC_InternGridCompCreate(cname, ESMF_ATM, NULL, "interngrid.rc", clock, &rc);

    printf("InternGrid Comp Create returned, name = '%s'\n", cname);


    rc = comp1->ESMC_InternGridCompPrint("");
    printf("InternGrid Comp Print returned\n");

    // register other entry points
    rc = comp1->ESMC_InternGridCompSetServices(FTN(externaluser_setservices));

    // in a real application, these need to be created first.
    instate = NULL;
    outstate = NULL;
    clock = NULL;
    rc = comp1->ESMC_InternGridCompInitialize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("InternGrid Comp Initialize returned\n");

    rc = comp1->ESMC_InternGridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("InternGrid Comp Run returned\n");

    rc = comp1->ESMC_InternGridCompRun(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("InternGrid Comp Run returned\n");

    rc = comp1->ESMC_InternGridCompFinalize(instate, outstate, clock, 0, ESMF_BLOCKING);
    printf("InternGrid Comp Finalize returned\n");

    rc = comp1->ESMC_InternGridCompPrint("");
    printf("Comp Print returned\n");

    rc = ESMC_InternGridCompDestroy(comp1);
    printf("Comp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMCI_Finalize();

    exit (0);
}
//\end{verbatim}
