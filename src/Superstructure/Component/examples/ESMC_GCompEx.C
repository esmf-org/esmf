// $Id: ESMC_GCompEx.C,v 1.7 2004/04/23 16:13:47 nscollins Exp $
//
// Example/test code which creates a new comp.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// See the following code fragments for general examples of working
//  with Components.  For more specific examples, see the Application,
//  Gridded, or Coupling examples.
// Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various interfaces to Components.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include "ESMC.h"
#include "ESMC_Clock.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"
#include "ESMC_Init.h"
#include "ESMC_State.h"

#include <stdio.h>
    
main(int argc, char **argv) {
//   // Local variables
     int x, y, rc, mycell;
     char compname[32];
     ESMC_newDELayout *layout;
     ESMC_Comp *comp1, *comp2, *comp3, *comp4;
        
//-------------------------------------------------------------------------
//   // Setup:
     // create clock, layout here.

//-------------------------------------------------------------------------
//   // Example 1:
//   //

     //comp1 = ESMC_CompCreate("Atmosphere", layout, ESMF_GRIDCOMP,
     //                         ESMF_ATM, "/usr/local', &rc);

     //rc = ESMC_CompRegMethod(comp1, "initialize", ATM_Init);
     //rc = ESMC_CompRegMethod(comp1, "run", ATM_Run);
     //rc = ESMC_CompRegMethod(comp1, "finalize", ATM_Final);
     //printf("Comp example 1 returned\n");

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

}

// the actual arguments to these routines are yet to be decided.
int ATM_Init(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to set up internal data for component
    return ESMF_SUCCESS;
}
    
// the actual arguments to these routines are yet to be decided.
int ATM_Run(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // computational code runs model timesteps here
    return ESMF_SUCCESS;
}

// the actual arguments to these routines are yet to be decided.
int ATM_Final(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to flush output, close files, release memory and shut down
    return ESMF_SUCCESS;
}
    
//\end{verbatim}
    
