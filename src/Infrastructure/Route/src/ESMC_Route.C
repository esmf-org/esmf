// $Id: ESMC_Route.C,v 1.26 2003/03/22 00:51:25 nscollins Exp $
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
               "$Id: ESMC_Route.C,v 1.26 2003/03/22 00:51:25 nscollins Exp $";
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
    int myde, rc;
    int decount;         // total number of DE/PEs in src + dst layouts

    this->layout = layout;
    rc = layout->ESMC_DELayoutGetNumDEs(&decount);
    rc = layout->ESMC_DELayoutGetDEid(&myde);
    
    routeid = rseqnum++;
    sendRT = ESMC_RTableCreate(myde, decount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvRT = ESMC_RTableCreate(myde, decount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    ct = ESMC_CommTableCreate(myde, decount, &rc);
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
// !IROUTINE:  ESMC_RouteGetCached - Retrieve a precomputed Route
//
// !INTERFACE:
      int ESMC_RouteGetCached(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in both Fields
      int my_DE_rcv,               // in  - DE identifier in the DELayout of
                                   //       the receiving Field
      ESMC_AxisIndex *AI_rcv,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      ESMC_DELayout *layout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - DE identifier in the DELayout of
                                   //       the sending Field
      ESMC_AxisIndex *AI_snd,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      ESMC_DELayout *layout_snd,   // in  - pointer to the snd DELayout 
      int *hascachedroute,         // out - 0=false, 1=true
      ESMC_Route **route) {        // out - if true, cached route

//
// !DESCRIPTION:
//     Returns the value of Route member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

    *hascachedroute = 0;
    *route = NULL;

    return ESMF_SUCCESS;

 } // end ESMC_RouteGet

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
      int dest_de,         // in - destination de id
      ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet and destination de to the
//     route table, and marks this de as needed in the comm table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = sendRT->ESMC_RTableSetEntry(dest_de, xp);
    if (rc != ESMF_SUCCESS)
        return rc;

    rc = ct->ESMC_CommTableSetPartner(dest_de);
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
      int source_de,       // in - sending de id
      ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet and source DE to the route table,
//     and marks this de as needed in the comm table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = recvRT->ESMC_RTableSetEntry(source_de, xp);
    if (rc != ESMF_SUCCESS)
        return rc;

    rc = ct->ESMC_CommTableSetPartner(source_de);

    return rc;

 } // end ESMC_RouteSetRecv

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteRun - Execute the comm routine described by this obj
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteRun(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *srcaddr,       // in, local send buffer base address
      void *dstaddr) {     // in, local receive buffer base address
//
// !DESCRIPTION:
//     Calls the communications routines to send/recv the information
//     set up in this table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc = ESMF_FAILURE;
    int i, j, k, l;
    int ccount, xscount, xrcount;
    int mydeid, theirdeid;
    int needed;
    ESMC_XPacket *sendxp, *recvxp;
    int srank, rrank, mrank;
    int srcbytes, rcvbytes;
    int sleft, rleft;
    int sright, rright;
    int sstrides[ESMF_MAXDIM], rstrides[ESMF_MAXDIM];
    int snums[ESMF_MAXDIM], rnums[ESMF_MAXDIM];
    char *srcmem, *rcvmem;
    int srccount, rcvcount;

    rc = layout->ESMC_DELayoutGetDEid(&mydeid);
    rc = ct->ESMC_CommTableGetCount(&ccount);

    // for each destination in the comm table
    for (i=0; i<ccount; i++) {

        // find out who the next id is 
        rc = ct->ESMC_CommTableGetPartner(i, &theirdeid, &needed);
        if (!needed) {
            printf("RouteRun: comm partner %d not needed, looping\n", theirdeid);
	    continue;
        } else {
           printf("RouteRun: comm partner %d needed %d\n", theirdeid, needed);
        }

        // look up the corresponding send/recv xpackets in the rtables
        rc = sendRT->ESMC_RTableGetEntry(theirdeid, &xscount, &sendxp);
        if (xscount > 1) fprintf(stderr, "cannot handle multiple xps yet\n");
        if (xscount > 0) {
            rc = sendxp->ESMC_XPacketGet(&srank, &sleft, &sright, sstrides, snums);
            printf("RouteRun: sendxp\n");
            sendxp->ESMC_XPacketPrint();
        } else {
            sendxp = NULL;
            srank = 0;
            sleft=0; sright=0;
            for(j=0; j<ESMF_MAXDIM; j++) {
                snums[j]=0;
                sstrides[j]=0;
            }
            printf("nothing to send\n");
        }

        rc = recvRT->ESMC_RTableGetEntry(theirdeid, &xrcount, &recvxp);
        if (xrcount > 1) fprintf(stderr, "cannot handle multiple xps yet\n");
        if (xrcount > 0) {
            rc = recvxp->ESMC_XPacketGet(&rrank, &rleft, &rright, rstrides, rnums);
            printf("RouteRun: recvxp\n");
            recvxp->ESMC_XPacketPrint();
        } else {
            recvxp = NULL;
            rrank = 0;
            rleft=0; rright=0;
            for(j=0; j<ESMF_MAXDIM; j++) {
                rnums[j]=0;
                rstrides[j]=0;
            }
            printf("nothing to recv\n");
        }
        
       
        // ready to call the comm routines - possibly multiple times, one for
        //  each disjoint memory piece?
     
        // TODO: this section isn't finished by a long shot.  we need to
        //  call mpi with at least a source base addr and bytecount, 
        // receive base addr & bytecount, receiving proc id - not sure 
        // what else.  and can this be a V version of the call - which
        // does arrays of addresses & counts?  because the chunks may be
        // strided, which either means multiple single calls or a call
        // which does multiples at one time.
        // 
  
        // if sendxp == NULL, nothing to send
        // if recvxp == NULL, nothing to recv

        // how is this loop to be structured?  we've got to set up both
        // a send and receive each time.  ranks must match?
        mrank = MAX(srank, rrank);
        printf("srank=%d, rrank=%d, mrank=%d\n", srank, rrank, mrank);
        printf(" starting srcaddr=0x%08lx, dstaddr=0x%08lx\n", 
                             (long int)srcaddr, (long int)dstaddr);
        for (j=0, srcbytes = sleft, rcvbytes = rleft; j<mrank-1; j++) {
            printf("j=%d, snums[j]=%d, rnums[j]=%d\n", j, snums[j], rnums[j]);
            for (l=0; l<snums[j] || l<rnums[j]; l++, 
                            srcbytes += sstrides[j], rcvbytes += rstrides[j]) {
         
                 srcmem = (char *)srcaddr+srcbytes; 
                 rcvmem = (char *)dstaddr+rcvbytes; 
                 srccount = sendxp ? sright-sleft+1 : 0;
                 rcvcount = recvxp ? rright-rleft+1 : 0;

                 printf("ready to send %d bytes from 0x%08x on DE %d to DE %d\n",
                            srccount, (long int)srcmem, mydeid, theirdeid);
                 printf(" and to receive %d bytes into 0x%08x on DE %d from DE %d\n", 
                            rcvcount, (long int)rcvmem, mydeid, theirdeid);

                 printf(" (l=%d, srcbytes=%d, rcvbytes=%d, ", 
                                 l, srcbytes, rcvbytes);
                 printf("sleft=%d, sright=%d, rleft=%d, rright=%d)\n", 
                                   sleft, sright, rleft, rright);
                 printf(" (j=%d, sstrides[j]=%d, rstrides[j]=%d)\n", 
                                    j, sstrides[j], rstrides[j]);

             
                // TODO: here's where we're ready to make the call.
                //  theirdeid  is the other processor de number
                //  srcmem and rcvmem are the mem addresses 
                //  srccount and rcvcount are the byte counts.  they are 0
                //    if there is nothing to send or receive, respectively.
                //  (i don't know the arg order, but here are all the things
                //   i'm guessing it needs.  it probably doesn't need mydeid,
                //   but if it does, that's the variable name.)
                //rc = layout->ESMC_DELayoutSendRecv(mydeid, theirdeid, 
                //                     srcmem, srccount, rcvmem, rcvcount);
            }
        }

    }

    return rc;

 } // end ESMC_RouteRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrecompute - initialize a a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecompute(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in both Fields
      int my_DE_rcv,               // in  - DE identifier in the DELayout of
                                   //       the receiving Field
      ESMC_AxisIndex *AI_rcv,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      ESMC_DELayout *layout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - DE identifier in the DELayout of
                                   //       the sending Field
      ESMC_AxisIndex *AI_snd,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      ESMC_DELayout *layout_snd) { // in  - pointer to the snd DELayout 
//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI[ESMF_MAXDIM], their_AI[ESMF_MAXDIM];
    ESMC_XPacket *my_XP = new ESMC_XPacket;
    ESMC_XPacket *their_XP = new ESMC_XPacket;
    ESMC_XPacket *intersect_XP = NULL;
    int i, j, k;
    int their_de, their_de_parent, their_decount;

    // Calculate the sending table.  If this DE is not part of the sending
    // layout (my_DE_snd = -1 ?  TODO), skip this part
    if (my_DE_snd != -1) {
 
      // get "my" AI out of the AI_snd array
      // TODO:  this is NOT going to work for data dims which are not
      //  equal the grid dims, e.g. a 2d grid with 4d data.
      for (k=0; k<rank; k++) {
        my_AI[k] = AI_snd[my_DE_snd + k*AI_snd_count];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      my_XP->ESMC_XPacketFromAxisIndex(my_AI, rank);

      // loop over DE's from receiving layout to calculate send table
      layout_rcv->ESMC_DELayoutGetNumDEs(&their_decount);
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the rcv layout
          // layout_rcv->ESMC_DELayoutGetParentID(their_de, their_de_parent);
          their_de_parent = their_de;     // temporarily

          // get "their" AI out of the AI_rcv array
          for (k=0; k<rank; k++) {
            their_AI[k] = AI_rcv[their_de + k*AI_rcv_count];
          }
 
          // calculate "their" XPacket in the sense of the global data
          their_XP->ESMC_XPacketFromAxisIndex(their_AI, rank);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(my_XP, their_XP);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local data space
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI, rank);

          // load the intersecting XPacket into the sending RTable
          sendRT->ESMC_RTableSetEntry(their_de_parent, intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);
        }
    }


    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout (my_DE_rcv = -1 ?  TODO), skip this part
    if (my_DE_rcv != -1) {
 
      // get "my" AI out of the AI_rcv array
      for (k=0; k<rank; k++) {
        my_AI[k] = AI_rcv[my_DE_rcv + k*AI_rcv_count];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      my_XP->ESMC_XPacketFromAxisIndex(my_AI, rank);

      // loop over DE's from sending layout to calculate receive table
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the snd layout
          // layout_snd->ESMC_DELayoutGetParentID(their_de, their_de_parent);
          their_de_parent = their_de;     // temporarily

          // get "their" AI out of the AI_snd array
          for (k=0; k<rank; k++) {
            their_AI[k] = AI_snd[their_de + k*AI_snd_count];
          }
 
          // calculate "their" XPacket in the sense of the global data
          their_XP->ESMC_XPacketFromAxisIndex(their_AI, rank);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(my_XP, their_XP);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI, rank);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(their_de_parent, intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);
        }
    }

    printf("end of RoutePrecompute:\n");
    this->ESMC_RoutePrint("");
    return ESMF_SUCCESS;

 } // end ESMC_RoutePrecompute


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
    printf(" Routeid = %d\n", routeid);
    printf(" DELayout:\n");
    rc = layout->ESMC_DELayoutPrint(); // options);  // doesn't take opts yet
    printf(" Send table:\n");
    rc = sendRT->ESMC_RTablePrint(options);
    printf(" Recv table:\n");
    rc = recvRT->ESMC_RTablePrint(options);
    printf(" Comm table:\n");
    rc = ct->ESMC_CommTablePrint(options);

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
