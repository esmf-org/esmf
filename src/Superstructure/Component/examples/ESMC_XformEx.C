// $Id: ESMC_XformEx.C,v 1.2 2003/02/04 20:19:26 nscollins Exp $
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
#include "ESMC_State.h"
#include "ESMC_Xform.h"

#include <stdio.h>
    
main(int argc, char **argv) {
//   // Local variables
     int rc;
     ESMC_State *state1;
     ESMC_Xform xformlist[2];
        

//-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Initializing a Transform.

    printf("Xform Example 1n");

    ! The third arguments here are names of subroutines.
    rc = ESMF_XformInit(xformlist[0], "AtmToOcn", A2OCPLxform)
    rc = ESMF_XformInit(xformlist[1], "OcnToAtm", O2ACPLxform)

    printf("Xform Example 1 finished\n");


!-------------------------------------------------------------------------
!   ! Example 2:      
!   !
!   !  Calling a Transform from within a concurrently running Component

    printf("Xform Example 2: Using a Xform from within a Component\n");

    rc = ESMF_StateTransform(state1, "AtmToOcn", xformlist);

    ! When this returns, the transform code has been executed.

    printf("Xform Example 2 finished\n");


}
    
//\end{verbatim}
    
