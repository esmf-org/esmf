// $Id: ESMC_RouteEx.C,v 1.3 2004/04/20 19:37:32 nscollins Exp $
//
// Example/test code which creates a new field.

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

//BOP
//
// !DESCRIPTION:
// See the following code fragments for examples of how to create new Fields.  
// Also see the Programming Model section of this document.
//
//
//\begin{verbatim}

//   // Example program showing various ways to create a Field object
#include "ESMC.h"
#include "ESMC_Field.h"
#include "ESMC_Comp.h"
#include "ESMC_Init.h"

#include <stdio.h>
    
main(int argc, char **argv) {
//   // Local variables
    int x, y, rc, mycell;
//   ESMC_Grid *grid;
//   ESMC_ArraySpec arrayspec;
//   ESMC_Array *arraya, *arrayb;
//   ESMC_DataMap *datamap;
//   ESMC_Relloc relativelocation;
//   char fieldname[20]
//   ESMC_IOSpec iospec;
    ESMC_Field *field1, *field2, *field3, *field4;
        
//-------------------------------------------------------------------------
//   // Example 1:
//   //
//   //  The user has already created a Grid and has Field data
//   //  stored in an Array object.  This version of create simply
//   //  associates the data with the Grid.  The data is referenced
//   //  by default.  The DataMap is created with defaults.
 
    rc = ESMC_Initialize();

    //grid = ESMF_GridCreate(name="atmgrid", rc=rc);

//   // Not fully implemented.
//   // arraya = ESMF_ArrayCreate()  
//   // arrayb = ESMF_ArrayCreate()  

    //field1 = ESMF_FieldCreate(grid, arraya, 
    //      relloc=ESMF_CELL_CENTER, name="pressure", rc=rc);

    //printf("Field example 1 returned\n");

//-------------------------------------------------------------------------
//   // Example 2:
//   //
//   //  The user creates an ArraySpec that describes the data and the
//   //  Field create call allocates the appropriate memory for it. 

//   //   arrayspec = ESMF_ArraySpecCreate();

//    field2 = ESMF_FieldCreate(grid, arrayspec, ESMF_CELL_CENTER, &
      //                        name="rh", rc=rc);

    //printf("Field example 2 returned\n");

//-------------------------------------------------------------------------
//   // Example 3:
//   //
//   //  The user wishes to associate different data with the Field
//   //  created in example 1.  The detach data call returns the 
//   //  pointer to the old data array; the attach call passes in the 
//   //  pointer to the new array.

    // ESMC_FieldDetachData(field1, array=arraya, rc=rc);

    // ESMC_FieldAttachData(field1, array=arrayb, rc=rc);

    //printf("Field example 3 returned\n");

//-------------------------------------------------------------------------
//   // Example 4:
//   //
//   //  The user creates an empty Field, and adds the Grid and 
//   //  data in later calls.

//     field4 = ESMF_FieldCreateNoData("precip", rc=rc);
     //field4 = ESMC_FieldCreate(&rc);
//
//    // At some later time, associate a Grid with this Field
     //call ESMC_FieldSetGrid(field3, grid, rc);

//    // ...and associate a data Array.
//    call ESMC_FieldAttachArray(field3, arraya, rc=rc);

     printf("Field example 4 returned\n");

//-------------------------------------------------------------------------
//   // Example 5:
//   //
//   // Query a Field for number of local grid cells.

     //call ESMF_FieldGetLocalGridInfo(field3, ncell=mycell, rc=rc);

     //printf("Field example 5 returned\n");

     //ESMC_FieldDestroy(field1);
     //ESMC_FieldDestroy(field2);
     //ESMC_FieldDestroy(field3);
     //ESMC_FieldDestroy(field4);
     //ESMC_FieldDestroy(field5);

     rc = ESMC_Finalize();
}

    
//\end{verbatim}
    
