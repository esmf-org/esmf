// $Id: ESMC_CompRun.C,v 1.5 2004/04/13 17:30:47 nscollins Exp $
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
#include "ESMC.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_State.h"
#include "ESMC_Clock.h"

extern "C" void FTN(externaluser_setservices)(ESMC_GridComp *, int *rc);

main(int argc, char **argv)
{
    
//  Local variables
    int rc;
    char *cname;
    ESMC_newDELayout *delayout;
    ESMC_GridComp *comp1;
    ESMC_State *instate, *outstate;
    ESMC_Clock *clock;
        
//-------------------------------------------------------------------------
//   // Test 1:
//   //
//   //  Quick Test - Create, Init, Run, Finalize, Destroy a Component.

    rc = ESMC_Initialize(argc, argv);
 
    printf("Component Test 1:\n");

    // TODO: query framework for default layout here

    cname = "Atmosphere";
    delayout = NULL;
    comp1 = ESMC_GridCompCreate(cname, delayout, ESMF_ATM, NULL, "grid.rc", &rc);

    printf("Grid Comp Create returned, name = '%s'\n", cname);


    rc = comp1->ESMC_GridCompPrint("");
    printf("Grid Comp Print returned\n");

    // register other entry points
    rc = comp1->ESMC_GridCompSetServices(FTN(externaluser_setservices));

    rc = comp1->ESMC_GridCompInitialize(instate, outstate, clock, 0);
    printf("Grid Comp Initialize returned\n");

    rc = comp1->ESMC_GridCompRun(instate, outstate, clock, 0);
    printf("Grid Comp Run returned\n");

    rc = comp1->ESMC_GridCompRun(instate, outstate, clock, 0);
    printf("Grid Comp Run returned\n");

    rc = comp1->ESMC_GridCompFinalize(instate, outstate, clock, 0);
    printf("Grid Comp Finalize returned\n");

    rc = comp1->ESMC_GridCompPrint("");
    printf("Comp Print returned\n");

    rc = ESMC_GridCompDestroy(comp1);
    printf("Comp Run returned\n");

    printf("Component Test 1 finished\n");

    rc = ESMC_Finalize();

    exit (0);
}
//\end{verbatim}
