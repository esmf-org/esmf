// $Id: ESMC_Route.C,v 1.7 2003/03/11 20:28:31 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Route method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Route methods declared
// in the companion file ESMC_Route.h
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include <ESMC_Route.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
               "$Id: ESMC_Route.C,v 1.7 2003/03/11 20:28:31 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Route routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteCreate - Create a new Route
//
// !INTERFACE:
      ESMC_Route *ESMC_RouteCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Route
//
// !ARGUMENTS:
      ESMC_DELayout *layout,
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Route; allocates memory for a new Route
//      object and uses the internal routine ESMC_RouteConstruct to
//      initialize it.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Route.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n


    ESMC_Route *newr = new ESMC_Route;

    *rc = newr->ESMC_RouteConstruct(layout);

    if (*rc == ESMF_FAILURE)
        return NULL;
    else
        return newr;

 } // end ESMC_RouteCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteDestroy - free a Route created with Create
//
// !INTERFACE:
      int ESMC_RouteDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Route *route) {    // in - route object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Route object previously allocated
//      via an ESMC_RouteCreate routine.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Route.h)
//
//EOP
// !REQUIREMENTS:  

    // call destruct routine

    return ESMF_FAILURE;

 } // end ESMC_RouteDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteConstruct - fill in an already allocated Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout) {          // in
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Route object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RouteDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RouteCreate, which calls
//      ESMC_RouteConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    static int rseqnum = 1;
    int mype, rc;
    int pecount;         // total number of PEs in src + dst layouts

    //pecount = layout->ESMC_DELayoutGetPECount();
    //mype = layout->ESMC_DELayoutGetPECount();
    //TODO: what are these funcs?
    pecount = 4;
    mype = 1;

    
    routeid = rseqnum++;
    sendRT = ESMC_RTableCreate(mype, pecount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvRT = ESMC_RTableCreate(mype, pecount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    ct = ESMC_CommTableCreate(mype, pecount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    return ESMF_SUCCESS;

 } // end ESMC_RouteConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteDestruct - release resources associated w/a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RouteConstruct, does any additional cleanup before the
//      original Route object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RouteDestroy, which calls
//      ESMC_RouteDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    // free both route tables and comm table by calling Destroy on them.

    return ESMF_FAILURE;

 } // end ESMC_RouteDestruct


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteGet - get <Value> for a Route
//
// !INTERFACE:
      //int ESMC_Route::ESMC_RouteGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Route member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

    //return ESMF_FAILURE;

 //} // end ESMC_RouteGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteSetSend - set send work request in route table
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteSetSend(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int dest_pe,         // in - destination pe id
      void *base_addr,     // in - local source address
      ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet, base address, and destination pe to the
//     route table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = sendRT->ESMC_RTableSetEntry(dest_pe, base_addr, xp);

    return rc;

 } // end ESMC_RouteSetSend

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteSetRecv - set recv work request in route table
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteSetRecv(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int source_pe,       // in - sending pe id
      void *base_addr,     // in - local source address
      ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet, base address, and destination pe to the
//     route table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = recvRT->ESMC_RTableSetEntry(source_pe, base_addr, xp);

    return rc;

 } // end ESMC_RouteSetRecv

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteValidate - internal consistency check for a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Route is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_RouteValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrint - print contents of a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Route.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:

    int rc;

    printf("Route print:\n");
    printf(" id = %d\n", routeid);
    printf("  send table\n");
    rc = sendRT->ESMC_RTablePrint(options);
    printf("  recv table\n");
    rc = recvRT->ESMC_RTablePrint(options);

    return ESMF_SUCCESS;

 } // end ESMC_RoutePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Route - native C++ constructor
//
// !INTERFACE:
      ESMC_Route::ESMC_Route(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:

  // default constructor is fine.

 } // end ESMC_Route

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Route - native C++ destructor
//
// !INTERFACE:
      ESMC_Route::~ESMC_Route(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS: 

  // default destructor is fine.

 } // end ~ESMC_Route
