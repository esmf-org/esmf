// $Id: ESMC_AppMainEx.C,v 1.9 2004/04/15 19:09:06 nscollins Exp $
//
// Example code which creates a main Application program.
// This is the cap component which creates other components below it.

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
#include "ESMC.h"
#include "ESMC_newDELayout.h"
#include "ESMC_Clock.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"
#include "ESMC_State.h"

void ATM_SetServices(ESMC_GridComp *gc, int *rc);
    
main(int argc, char **argv) {
//   // Local variables
     int x, y, rc, mycell;
     char compname[32];
     ESMC_newDELayout *layout;
     ESMC_VM vm;
     ESMC_GridComp *gcomp1, *gcomp2, *gcomp3, *comp4;
     ESMC_CplComp *ccomp1;
     ESMC_Grid *grid = NULL;
        
//-------------------------------------------------------------------------
//   // Setup:
     rc = ESMC_Initialize();

//-------------------------------------------------------------------------
//   // Example 1:
//   //

     //vm = ?? ;
     layout = ESMC_newDELayoutCreate(vm, NULL, 0, NULL, 0, NULL, &rc);
     gcomp1 = ESMC_GridCompCreate("Atmosphere", layout, ESMF_ATM, grid, 
                                  "setup.rc", &rc);

     rc = gcomp1->ESMC_GridCompSetServices(ATM_SetServices); 
     printf("Gridded Comp example 1 complete\n");

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
     //  internally calls ATM_Run()

     //printf("Comp example 3 returned\n");

//-------------------------------------------------------------------------
//   // Example 4:
//   //

//   //rc = ESMC_CompFinal(comp1, ...);
     //  internally calls ATM_Final()

     //printf("Comp example 4 returned\n");

//-------------------------------------------------------------------------
//   // Example 5:
//   //

     //rc = ESMC_CompDestroy(comp1);
     //rc = ESMC_newDELayoutDestroy(layout);

     //printf("Comp example 5 returned\n");

     rc = ESMC_Finalize();

}

void ATM_SetServices(ESMC_GridComp *gc, int *rc) {
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
    
