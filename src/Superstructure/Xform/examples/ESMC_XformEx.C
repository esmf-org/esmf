// $Id: ESMC_XformEx.C,v 1.4 2004/04/23 22:15:48 nscollins Exp $
//
// Example/test code which creates a new Transforms.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// See the following code fragments for general examples of working
//  with Transforms.
// Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various interfaces to Transforms.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include "ESMC.h"


#include <stdio.h>
    
main(int argc, char **argv) {
//   // Local variables
     int rc;
     ESMC_State *state1;
     ESMC_Xform xformlist[2];

     extern void A2OCPLxform(void);
     extern void O2ACPLxform(void);
        

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//   // Example 1:
//   //
//   //  Initializing a Transform.

    printf("Xform Example 1n");

    // The third arguments here are names of subroutines.
    rc = xformlist[0].ESMC_XformInit("AtmToOcn", A2OCPLxform);
    rc = xformlist[1].ESMC_XformInit("OcnToAtm", O2ACPLxform);

    printf("Xform Example 1 finished\n");


//-------------------------------------------------------------------------
//   // Example 2:      
//   //
//   //  Calling a Transform from within a concurrently running Component

    printf("Xform Example 2: Using a Xform from within a Component\n");

    rc = state1->ESMC_StateTransform(xformlist);

    // When this returns, the transform code has been executed.

    printf("Xform Example 2 finished\n");


}
    
//\end{verbatim}
    
