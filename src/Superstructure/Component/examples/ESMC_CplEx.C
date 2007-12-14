// $Id: ESMC_CplEx.C,v 1.22.2.1 2007/12/14 20:28:54 svasquez Exp $
//
// Example/test code which creates a new comp.

//-------------------------------------------------------------------------
// ESMF_EXAMPLE        String used by test script to count examples.
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
//  See the following code fragments for general examples of working
//  with Components.  For more specific examples, see the Application,
//  Gridded, or Coupling examples.
//  Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various interfaces to Components.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include <stdio.h>
#include "ESMCI.h"
    
main(int argc, char **argv) {
//   // Local variables
     int x, y, rc, finalrc, mycell;
     char compname[32];
     ESMCI::DELayout *layout;
     ESMC_Comp *comp1, *comp2, *comp3, *comp4;
        
//-------------------------------------------------------------------------
//   // Setup:
     finalrc = ESMF_SUCCESS;
     rc = ESMCI_Initialize();
     if (rc != ESMF_SUCCESS) finalrc = rc;

     // create clock, layout here.

//-------------------------------------------------------------------------
//   // Example 1:
//   //

     //comp1 = ESMC_CompCreate("AtmToOcean", layout, ESMF_CPLCOMP,
     //                         ESMF_ATM, "/usr/local', &rc);

     //rc = ESMC_CompRegMethod(comp1, "initialize", CPL_Init);
     //rc = ESMC_CompRegMethod(comp1, "run", CPL_Run);
     //rc = ESMC_CompRegMethod(comp1, "finalize", CPL_Final);
     //printf("Comp example 1 returned\n");

//-------------------------------------------------------------------------
//   // Example 2:
//   //

//   //rc = ESMC_CompInit(comp1, ...);
     //  internally calls CPL_Init() 

     //printf("Comp example 2 returned\n");

//-------------------------------------------------------------------------
//   // Example 3:
//   //

     // pass in time: 
     //    as clock, as timestep count, as time interval, as stoptime
     //rc = ESMC_CompRun(comp1, ...);
     //  internally calls CPL_Run()

     //printf("Comp example 3 returned\n");

//-------------------------------------------------------------------------
//   // Example 4:
//   //

//   //rc = ESMC_CompFinal(comp1, ...);
     //  internally calls CPL_Final()

     //printf("Comp example 4 returned\n");

//-------------------------------------------------------------------------
//   // Example 5:
//   //

     //rc = ESMC_CompDestroy(comp1);
     //rc = ESMC_DELayoutDestroy(layout);

     //printf("Comp example 5 returned\n");
     rc = ESMCI_Finalize();
     if (rc != ESMF_SUCCESS) finalrc = rc;

     if (finalrc == ESMF_SUCCESS)
        printf("PASS: ESMC_CplEx.C\n");
     else
        printf("FAIL: ESMC_CplEx.C\n");
}

// the actual arguments to these routines are yet to be decided.
int CPL_Init(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to set up internal data for coupling
    return ESMF_SUCCESS;
}
    
// the actual arguments to these routines are yet to be decided.
int CPL_Run(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // coupling manages data exchange here
    return ESMF_SUCCESS;
}

// the actual arguments to these routines are yet to be decided.
int CPL_Final(ESMC_State *importS, ESMC_State *exportS, ESMC_Clock *clock) {
     // code to flush output, close files, release memory and shut down
    return ESMF_SUCCESS;
}
    
//\end{verbatim}
    
