// $Id: ESMC_AppCompEx.C,v 1.4 2003/03/10 03:23:10 cdeluca Exp $
//
// Example code which creates an Application which can also
// be embedded as a Component in another Application.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// Example code which creates an Application which can also
// be embedded as a Component in another Application.
// Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various interfaces to Components.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include "ESMC.h"
#include "ESMC_DELayout.h"
#include "ESMC_Comp.h"

#include <stdio.h>
    
//-------------------------------------------------------------------------
//   // Example 1:
//   //

int ATM_Init(ESMF_Comp *topcomp, int rc) {
//   // Local variables
     int x, y, rc, mycell;
     char compname[32]
     ESMC_DELayout *layout;
     ESMC_Comp *comp1, *comp2, *comp3, *comp4;
        
//-------------------------------------------------------------------------
//   // Setup:

     //comp1 = ESMC_CompCreate("Atmosphere", layout, ESMF_GRIDCOMP,
     //                         ESMF_ATM, "/usr/local', &rc);

     //rc = ESMC_CompRegMethod(comp1, "initialize", ATM_Init);
     //rc = ESMC_CompRegMethod(comp1, "run", ATM_Run);
     //rc = ESMC_CompRegMethod(comp1, "finalize", ATM_Final);
     //printf("Comp example 1 returned\n");

}

//-------------------------------------------------------------------------
//   // Example 2:
//   //

int ATM_Run(ESMC_Comp *topcomp, ESMC_Clock *clock) {

     // pass in time: 
     //    as clock, as timestep count, as time interval, as stoptime
     //rc = ESMC_CompRun(comp1, ...);
     //  internally calls ATM_Run()

     //printf("Comp example 3 returned\n");

}

//-------------------------------------------------------------------------
//   // Example 3:
//   //

int ATM_Final(ESMF_Comp *topcomp, int rc) {

//   //rc = ESMC_CompFinal(comp1, ...);
     //  internally calls ATM_Final()

     //printf("Comp example 4 returned\n");
}


    
//\end{verbatim}
    
