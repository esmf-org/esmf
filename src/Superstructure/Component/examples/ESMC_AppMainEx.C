// $Id: ESMC_AppMainEx.C,v 1.26.2.1 2007/12/14 20:28:54 svasquez Exp $
//
// Example code which creates a main Application program.
// This is the cap component which creates other components below it.

//-------------------------------------------------------------------------
// ESMF_EXAMPLE        String used by test script to count examples.
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// Example code which creates a main Application program.
//
//
//\begin{verbatim}

//   TODO: finish this example
//   // Example program showing various interfaces to Components.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include <stdio.h>
#include "ESMCI.h"

void ATM_SetServices(ESMC_GridComp *gc, int *rc);
    
main(int argc, char **argv) {
//   // Local variables
     int x, y, rc, finalrc, mycell;
     char compname[32];
     ESMCI::VM *vm;
     ESMC_GridComp *gcomp1, *gcomp2, *gcomp3, *comp4;
     ESMC_CplComp *ccomp1;
     ESMC_Grid *grid = NULL;
     ESMC_Clock *clock = NULL;
        
//-------------------------------------------------------------------------
//   // Setup:
     finalrc = ESMF_SUCCESS;

     rc = ESMCI_Initialize();
     if (rc != ESMF_SUCCESS) finalrc = rc;

//-------------------------------------------------------------------------
//   // Example 1:
//   //

     vm = ESMCI::VM::getGlobal(&rc);
     gcomp1 = ESMC_GridCompCreate("Atmosphere", ESMF_ATM, grid, 
                                  "setup.rc", clock, &rc);

     rc = gcomp1->ESMC_GridCompSetServices(ATM_SetServices); 
     if (rc != ESMF_SUCCESS) finalrc = rc;
     printf("GridComp example 1 complete\n");

//-------------------------------------------------------------------------
//   // Example 2:
//   //

//   //rc = ESMC_CompInit(comp1, ...);
     //  internally calls ATM_Init() 

     //printf("Comp example 2 returned\n");

//-------------------------------------------------------------------------
//   // Example 3:
//   //

     // pass in time: 
     //    as clock, as timestep count, as time interval, as stoptime
     //rc = ESMC_CompRun(comp1, ...);
     // if (rc != ESMF_SUCCESS) finalrc = rc;
     //  internally calls ATM_Run()

     //printf("Comp example 3 returned\n");

//-------------------------------------------------------------------------
//   // Example 4:
//   //

//   //rc = ESMC_CompFinal(comp1, ...);
     // if (rc != ESMF_SUCCESS) finalrc = rc;
     //  internally calls ATM_Final()

     //printf("Comp example 4 returned\n");

//-------------------------------------------------------------------------
//   // Example 5:
//   //

     //rc = ESMC_CompDestroy(comp1);
     // if (rc != ESMF_SUCCESS) finalrc = rc;

     //printf("Comp example 5 returned\n");

     rc = ESMCI_Finalize();
     if (rc != ESMF_SUCCESS) finalrc = rc;

     if (finalrc == ESMF_SUCCESS) 
        printf("PASS: ESMC_AppMainEx.C\n");
     else
        printf("FAIL: ESMC_AppMainEx.C\n");
    

}

void ATM_SetServices(ESMC_GridComp *gc, int *rc) {
    // this is where it should register Init, Run, Finalize
   *rc = ESMF_SUCCESS;
}

// the actual arguments to these routines are yet to be decided.
void ATM_Init(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to set up internal data for component
}
    
// the actual arguments to these routines are yet to be decided.
void ATM_Run(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // computational code runs model timesteps here
}

// the actual arguments to these routines are yet to be decided.
void ATM_Final(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to flush output, close files, release memory and shut down
}
    
//\end{verbatim}
    
