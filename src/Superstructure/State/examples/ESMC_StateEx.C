// $Id: ESMC_StateEx.C,v 1.11.2.2 2008/05/06 04:31:39 cdeluca Exp $
//
// Example/test code which creates a new State.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// See the following code fragments for general examples of working
//  with States.
// Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various interfaces to States.
//   // See the more specific example programs for details of how
//   // the interfaces are used under various conditions.

#include <stdio.h>
#include "ESMCI.h"


    
main(int argc, char **argv) {
//   // Local variables
     int x, y, rc;
     char *statename, *compname;
     ESMC_State *state1, *state2, *state3, *state4;
        
//-------------------------------------------------------------------------
//   // Example 1:
//   //
//   // Creation of a state, might also be from a query of a component.
//   //

    printf("State Example 1: Import State\n");

//   // This will probably be called from inside the Component Init code
    compname = "Atmosphere";
    state1 = ESMF_StateCreate(compname, ESMF_STATE_IMPORT, &rc);
    printf("State Create returned, name = %s\n", compname);

    // Data would be added here and the State reused inside the run
    //  routine of a sequential application.

    // User code will probably get a handle to the state by querying
    // the component, maybe something like:
    // rc = ESMF_CompGetState(comp, ESMF_COMP_IMPORTSTATE, &state1);


    printf("State Example 1 finished\n");



//-------------------------------------------------------------------------
//   // Example 2:
//   //
//   //  Create, Add Data, Query, then Destroy a State.

    printf("State Example 2: Export State\n");

    compname = "Ocean";
    state2 = ESMF_StateCreate(compname, ESMF_STATE_EXPORT, rc)
    printf("State Create returned, name = %s\n", compname);

    bundlename = "Temperature";
    bundle1 = ESMF_FieldBundleCreate(bundlename, rc=rc);
    printf("FieldBundle Create returned, rc = %d", rc);

    rc = ESMF_StateAdd(state2, bundle1);
    printf("StateAdd returned, rc = %d", rc);

    // Loop here, updating FieldBundle contents each time step

    rc = ESMF_StateDestroy(state2);
    printf("State Destroy returned, rc = %d", rc);

    rc = ESMF_FieldBundleDestroy(bundle1);
    printf("FieldBundle Destroy returned, rc = %d", rc);

    printf("State Example 2 finished");


//-------------------------------------------------------------------------
//   // Example 3:
//   //
//   //  Create, Add Placeholder, Query, then Destroy a State.
//   //  This example applies to a Gridded Component which potentially
//   //  could create a large number of export items but it is unlikely
//   //  that any other component would require all of them.  This allows
//   //  the consuming component to mark those needed, and the producer
//   //  only has to fill the data items requested.

    printf("State Example 3: Export State with Placeholder"

    // The producing Component creates the menu of data items available.
    compname = "Ocean";
    state3 = ESMF_StateCreate(compname, ESMF_STATE_EXPORT, rc);
    printf("State Create returned", rc, " name = ", trim(compname)

    dataname = "Downward wind";
    rc = ESMF_StateAdd(state3, dataname);
    printf("StateAdd returned, rc = %d, name = %s\n", rc, dataname);

    dataname = "Humidity";
    rc = ESMF_StateAdd(state3, dataname)
    printf("StateAdd returned, rc = %d, name = %s\n", rc, dataname);

    // See next example for how this is used.

    printf("State Example 3 finished\n");


//-------------------------------------------------------------------------
//   // Example 4:
//   //
//   //  Mark and Query Needed flags, and add FieldBundle data

    printf("State Example 4: Get/Set Needed flags in Export State\n");

    // Given state3 from the previous example, the Coupler or Application
    // is given an opportunity to mark which data items are needed.

    dataname = "Downward wind";
    rc = ESMF_StateSetNeeded(state3, dataname, ESMF_NEEDED);
    printf("StateSetNeeded returned, rc = %d\n", rc);


//-------------------------------------------------------------------------
//   // Example 5:
//   //
//   //  Query Needed flags, and add FieldBundle data

    printf("State Example 5: Get/Set Needed flags in Export State, cont.\n");

    // Given state3 from the previous examples, the producing Component
    // can check the state to see what data items are required.

    dataname = "Downward wind";
    if (ESMF_StateIsNeeded(state3, dataname, rc)) {

        printf("Data marked as needed, name = %s\n", dataname);

        bundlename = dataname;
        bundle2 = ESMF_FieldBundleCreate(bundlename, rc=rc);
        printf("FieldBundle Create returned, rc = %d, name = %s\n", rc, bundlename);

        rc = ESMF_StateAdd(state3, bundle2);
        printf("StateAdd returned, rc = %d\n", rc);
    } else 
        printf("Data marked as not needed, name = %s\n", statename);

    rc = ESMF_StateDestroy(state3);
    printf("State Destroy returned, rc = %d\n", rc);

    printf("State Example 5 finished\n");

//-------------------------------------------------------------------------
//   // Similar flags exist for "Ready" and for "Valid" to mark each data
//   //  item as ready or having been validated, to help synchronize data
//   //  exchange between Components and Couplers.
//-------------------------------------------------------------------------



}

    
//\end{verbatim}
    
