// $Id: ESMC_Route.C,v 1.56 2003/08/07 16:20:14 nscollins Exp $
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
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_Route.h>
 #include <ESMC_Comm.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
               "$Id: ESMC_Route.C,v 1.56 2003/08/07 16:20:14 nscollins Exp $";
//-----------------------------------------------------------------------------


static ESMC_RouteCacheTable routetable = { 
   0, 0, NULL };
static int maxroutes = 10;

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
    int rc;

    // call destruct routine
    rc = route->ESMC_RouteDestruct();

    delete route;
    return rc;

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
    rc = layout->ESMC_DELayoutGetDEID(&myde);
    
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
    int rc;

    // free both route tables and comm table by calling Destroy on them.
    rc = sendRT->ESMC_RTableDestruct();
    if (rc == ESMF_FAILURE)
       return rc;
    delete sendRT;

    rc = recvRT->ESMC_RTableDestruct();
    if (rc == ESMF_FAILURE)
       return rc;
    delete recvRT;

    rc = ct->ESMC_CommTableDestruct();
    if (rc == ESMF_FAILURE)
       return rc;
    delete ct;

    return ESMF_SUCCESS;

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
      ESMC_AxisIndex *AI_rcv_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - exclusive region
      ESMC_AxisIndex *AI_rcv_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - total region
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      ESMC_DELayout *layout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - DE identifier in the DELayout of
                                   //       the sending Field
      ESMC_AxisIndex *AI_snd_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - exclusive region
      ESMC_AxisIndex *AI_snd_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - total region
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      ESMC_DELayout *layout_snd,   // in  - pointer to the snd DELayout 
      ESMC_Logical *periodic,      // in - if halo'ing, per/axis flag
      ESMC_Logical *hascachedroute,  // out - ESMF_TF_TRUE, ESMF_TF_FALSE
      ESMC_Route **route) {        // out - if true, cached route

//
// !DESCRIPTION:
//     Returns the value of Route member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  
    int i, j;
    ESMC_Logical tf;
    ESMC_RouteCacheEntry *ep;

    for (i=0; i<routetable.nroutes; i++) {

        ep = routetable.rcep[i];
    
        // see if we find a match.  
        if (ep->entrystatus != 1) continue;    // make this a type, and flag
        if (rank != ep->rank) continue;
        if (layout_snd != ep->snd_layout) continue;
        if (layout_rcv != ep->rcv_layout) continue;
        if (my_DE_snd != ep->snd_DE) continue; 
        if (my_DE_rcv != ep->rcv_DE) continue; 
        if (AI_snd_count != ep->snd_AI_count) continue; 
        if (AI_rcv_count != ep->rcv_AI_count) continue; 
        for (j=0; j<rank; j++) {
            tf = ESMC_AxisIndexEqual(AI_snd_exc, ep->snd_AI_exc); 
            if (tf == ESMF_TF_FALSE) continue;
            tf = ESMC_AxisIndexEqual(AI_snd_tot, ep->snd_AI_tot); 
            if (tf == ESMF_TF_FALSE) continue;
            tf = ESMC_AxisIndexEqual(AI_rcv_exc, ep->rcv_AI_exc); 
            if (tf == ESMF_TF_FALSE) continue;
            tf = ESMC_AxisIndexEqual(AI_rcv_tot, ep->rcv_AI_tot); 
            if (tf == ESMF_TF_FALSE) continue;
            if (periodic[j] != ep->periodic[j]) continue;
        }

        *hascachedroute = ESMF_TF_TRUE;
        *route = routetable.rcep[i]->theroute;

        return ESMF_SUCCESS;
    }
 
    *hascachedroute = ESMF_TF_FALSE;
    *route = NULL;

    return ESMF_SUCCESS;

 } // end ESMC_RouteGetCached

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
    int i, j, k, l, m;
    int ixs, ixr;
    int ccount, xscount, xrcount, xmcount;
    int mydeid, theirdeid;
    int needed;
    ESMC_XPacket *sendxp, *recvxp;
    int srank, rrank, mrank;
    int srcbytes, rcvbytes;
    int soffset, roffset;
    int scontig_length, rcontig_length;
    int sstride[ESMF_MAXDIM], rstride[ESMF_MAXDIM];
    int srep_count[ESMF_MAXDIM], rrep_count[ESMF_MAXDIM];
    void *srcmem, *rcvmem;
    int srccount, rcvcount;

    rc = layout->ESMC_DELayoutGetDEID(&mydeid);
    rc = ct->ESMC_CommTableGetCount(&ccount);

    //printf("Ready to run Route on DE %d, commtable count = %d:\n",
    //           mydeid, ccount);
    //ESMC_RoutePrint("");

    // for each destination in the comm table
    for (i=0; i<ccount; i++) {

        // find out who the next id is 
        rc = ct->ESMC_CommTableGetPartner(i, &theirdeid, &needed);
        if (!needed) {
        //    printf("RouteRun: comm partner %d not needed, looping\n", theirdeid);
	    continue;
        //} else {
        //   printf("RouteRun: comm partner %d needed %d\n", theirdeid, needed);
        }

        // find total number of xpackets
	rc = recvRT->ESMC_RTableGetCount(theirdeid, &xrcount);
	rc = sendRT->ESMC_RTableGetCount(theirdeid, &xscount);

        xmcount = MAX(xrcount, xscount);
        for (m=0, ixs=0, ixr=0; m < xmcount; m++, ixs++, ixr++){

            // look up the corresponding send/recv xpackets in the rtables

            //if (xscount > 1) printf("WARNING! cannot handle multiple xps yet %d\n",xscount);
            if (ixs < xscount) {
                rc = sendRT->ESMC_RTableGetEntry(theirdeid, ixs, &sendxp);
                rc = sendxp->ESMC_XPacketGet(&srank, &soffset, &scontig_length, sstride, srep_count);
                // printf("RouteRun: sendxp\n");
                // sendxp->ESMC_XPacketPrint();
            } else {
                sendxp = NULL;
                srank = 0;
                soffset=0; scontig_length=0;
                for(j=0; j<ESMF_MAXDIM; j++) {
                    srep_count[j]=0;
		    sstride[j]=0;
                }
                // printf("nothing to send\n");
            }

            //if (xrcount > 1) printf("WARNING! cannot handle multiple xps yet %d\n",xrcount);
            if (ixr < xrcount) {
                rc = recvRT->ESMC_RTableGetEntry(theirdeid, ixr, &recvxp);
                rc = recvxp->ESMC_XPacketGet(&rrank, &roffset, &rcontig_length, rstride, rrep_count);
                // printf("RouteRun: recvxp\n");
                // recvxp->ESMC_XPacketPrint();
            } else {
                recvxp = NULL;
                rrank = 0;
		roffset=0; rcontig_length=0;
                for(j=0; j<ESMF_MAXDIM; j++) {
                    rrep_count[j]=0;
                    rstride[j]=0;
                }
                // printf("nothing to recv\n");
            }
        
       
            // ready to call the comm routines - multiple times, one for
            //  each disjoint memory piece.
     
            // if sendxp == NULL, nothing to send
            // if recvxp == NULL, nothing to recv

           // TODO: for now, ranks must match.
           mrank = MAX(srank, rrank);
           //printf("srank=%d, rrank=%d, mrank=%d\n", srank, rrank, mrank);
           //printf(" starting srcaddr=0x%08lx, dstaddr=0x%08lx\n", 
           //                     (long int)srcaddr, (long int)dstaddr);
           for (j=0, srcbytes = soffset, rcvbytes = roffset; j<mrank-1; j++) {
               //printf("j=%d, srep_count[j]=%d, rrep_count[j]=%d\n", j, srep_count[j], rrep_count[j]);
               for (l=0; l<srep_count[j] || l<rrep_count[j]; l++, 
                               srcbytes += sstride[j], rcvbytes += rstride[j]) {
         
                     // TODO!!!  we need to standardize on either byte counts
                     // and void * from here down to the MPI level, or we need
                     // to compute # of items and item type and pass it down.
                     // this "sizeof(int)" is WRONG and just a hack to test the
                     // code for now.
         
                    srcmem = (void *)((char *)srcaddr+(srcbytes*sizeof(int))); 
                    rcvmem = (void *)((char *)dstaddr+(rcvbytes*sizeof(int))); 
              // jw   srccount = sendxp ? scontig_length-soffset+1 : 0;
              // jw   rcvcount = recvxp ? rcontig_length-roffset+1 : 0;
                    srccount = sendxp ? scontig_length : 0;
                    rcvcount = recvxp ? rcontig_length : 0;

                    // Debug:
                    if ((srccount == 0) && (rcvcount == 0)) 
                        printf("WARNING!! both send/recv counts = 0, myDE %d, theirDE %d\n", 
                                           mydeid, theirdeid); 
                    //printf("ready to send %d bytes from 0x%08x on DE %d to DE %d\n",
                    //           srccount, (long int)srcmem, mydeid, theirdeid);
                    //printf(" and to receive %d bytes into 0x%08x on DE %d from DE %d\n", 
                    //           rcvcount, (long int)rcvmem, mydeid, theirdeid);

                    //printf(" (l=%d, srcbytes=%d, rcvbytes=%d, ", 
                    //                l, srcbytes, rcvbytes);
                    //printf("soffset=%d, scontig_length=%d, roffset=%d, rcontig_length=%d)\n", 
                    //                  soffset, scontig_length, roffset, rcontig_length);
                    //printf(" (j=%d, sstride[j]=%d, rstride[j]=%d)\n", 
                    //                   j, sstride[j], rstride[j]);

             
                    //  theirdeid  is the other processor de number
                    //  srcmem and rcvmem are the mem addresses 
                    //  srccount and rcvcount are the byte counts.  they are 0
                    //    if there is nothing to send or receive, respectively.

                    rc = layout->ESMC_DELayoutSendRecv(srcmem, rcvmem,
                                                       srccount, rcvcount, 
                                                       theirdeid, theirdeid
#if 1 
// NEW_SEND_RECV_INTERFACE
                                                   , ESMF_KIND_R4);
#else 
// old interface
                                                       );
#endif
                }
            }
	}

    }

    // printf("End of Route run on DE %d\n", mydeid);

    return rc;

 } // end ESMC_RouteRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeHalo - initialize a a Route for a Halo
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeHalo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in Field
      int my_DE,                   // in  - DE identifier in the DELayout
      ESMC_AxisIndex *AI_exc,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
      ESMC_AxisIndex *AI_tot,      // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
      int AI_count,                // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      int *global_start,           // in  - array of global start information
                                   //       in each dimension and for all
                                   //       DE's in the DELayout
      int *global_count,           // in  - array of global stride information
                                   //       in each dimension
      ESMC_DELayout *layout,       // in  - pointer to the DELayout 
      ESMC_Logical *periodic) {    // in  - array of flags, one per dim
//
// !DESCRIPTION:
//      Initializes a Route for a Halo with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI[ESMF_MAXDIM], my_AI_tot[ESMF_MAXDIM];
    ESMC_AxisIndex their_AI[ESMF_MAXDIM];
    ESMC_XPacket intersect_XP;
    ESMC_XPacket *my_XP = NULL;
    ESMC_XPacket *their_XP = NULL;
    int my_global_start[ESMF_MAXDIM], their_global_start[ESMF_MAXDIM];
    int memory_stride[ESMF_MAXDIM];
    int my_XPcount, their_XPcount;
    ESMC_RouteCacheEntry *ep;
    int rc;   // TODO: really use this
    int i, j, k;
    int their_de, decount;

    // Calculate the sending table.
 
    //  set memory strides to include halo widths  TODO: pass in AIs as global
      for (k=0; k<rank; k++) {
        memory_stride[k] = global_count[k]
                + AI_exc[my_DE + k*AI_count].min - AI_tot[my_DE + k*AI_count].min
                + AI_tot[my_DE + k*AI_count].max - AI_exc[my_DE + k*AI_count].max;
        if (k >= 1) {
          memory_stride[k] = memory_stride[k] * memory_stride[k-1];
        }
      }

    // get "my" AI out of the AI_exc array
    // TODO:  this is NOT going to work for data dims which are not
    //  equal the grid dims, e.g. a 2d grid with 4d data.
    for (k=0; k<rank; k++) {
      my_AI[k] = AI_exc[my_DE + k*AI_count];
      my_AI_tot[k] = AI_tot[my_DE + k*AI_count];
      my_global_start[k] = global_start[my_DE + k*AI_count];
    }

    // calculate "my" (local DE's) XPacket in the sense of the global data
    // nsc - here is where we will need to do this again for periodic
    // boundary support if periodic is true along any axis and if this
    // xpacket is along a corresponding minimum-side boundary.
    rc = ESMC_XPacketFromAxisIndex(my_AI, rank, my_global_start, global_count,
                                   memory_stride, periodic, &my_XP, &my_XPcount);

    // printf("my_DE = %d, my_XPcount = %d\n", my_DE, my_XPcount);

    // loop over DE's from receiving layout to calculate send table
    layout->ESMC_DELayoutGetNumDEs(&decount);
    for (k=0; k<decount; k++) {
      their_de = k;

      // get "their" AI out of the AI_tot array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_tot[their_de + j*AI_count];
        their_global_start[j] = global_start[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, their_global_start,
                                     global_count, memory_stride, periodic,
                                     &their_XP, &their_XPcount);

      
      // printf("their_de = %d, their_XPcount = %d\n", their_de, their_XPcount);
      // calculate the intersection
      for (i=0; i<their_XPcount; i++) {

        // reuse the same XP for the entire loop.
        intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[i]);

        // if there's no intersection, no need to add an entry here
        if (intersect_XP.ESMC_XPacketEmpty()) {
          // printf("empty intersection between %d and %d, continuing\n",
          //             my_DE, their_de);
          continue;
        }

        // translate from global to local data space
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                               rank, my_global_start,
                                               memory_stride);

        // load the intersecting XPacket into the sending RTable
        sendRT->ESMC_RTableSetEntry(their_de, &intersect_XP);
        ct->ESMC_CommTableSetPartner(their_de);
      }
    }

    // Calculate the receiving table.
 
    // get "my" AI out of the AI_tot array
    for (k=0; k<rank; k++) {
      my_AI[k] = AI_tot[my_DE + k*AI_count];
      my_global_start[k] = global_start[my_DE + k*AI_count];
    }

    // calculate "my" (local DE's) XPacket in the sense of the global data
    // nsc - here is where we will need to do this again for periodic
    // boundary support if periodic is true along any axis and if this
    // xpacket is along a corresponding minimum-side boundary.
    rc = ESMC_XPacketFromAxisIndex(my_AI, rank, my_global_start, global_count,
                                   memory_stride, periodic, &my_XP, &my_XPcount);

    // loop over DE's from layout to calculate receive table
    for (k=0; k<decount; k++) {
      their_de = k;

      // get "their" AI out of the AI_exc array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_exc[their_de + j*AI_count];
        their_global_start[j] = global_start[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, their_global_start,
                                     global_count, memory_stride, periodic, 
                                     &their_XP, &their_XPcount);

      // calculate the intersection
      for (i=0; i<their_XPcount; i++) {

        intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[i]);

        // if there's no intersection, no need to add an entry here
        if (intersect_XP.ESMC_XPacketEmpty()) 
          continue;
         
        // translate from global to local
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot,
                                               rank, my_global_start,
                                               memory_stride);

        // load the intersecting XPacket into the receiving RTable
        recvRT->ESMC_RTableSetEntry(their_de, &intersect_XP);
        ct->ESMC_CommTableSetPartner(their_de);
      }
    }
 
    // free unneeded XPs here
    delete my_XP;  
    my_XP = NULL;
    delete their_XP; 
    their_XP = NULL;

    // add to cache table here.
    if (routetable.rcep == NULL) {
       routetable.rcep = (ESMC_RouteCacheEntry **) 
                       malloc(sizeof(ESMC_RouteCacheEntry *) * maxroutes);
       routetable.nalloc = maxroutes;

    }
    // if we decide to grow the table slowly...
    //if (routetable.nroutes <= routetable.nalloc) {
    //    realloc(routetable.rcep, (routetable.nalloc + 4) *
    //                                    sizeof(ESMC_RouteCacheEntry *));
    //    routetable.nalloc += 4;
    //}

    if (routetable.nroutes < maxroutes) {

        ep = new ESMC_RouteCacheEntry;
        routetable.rcep[routetable.nroutes] = ep;
        routetable.nroutes++;
        
        // store this in the cache
        ep->routeid = routeid;
        ep->theroute = this; 
        ep->entrystatus = 1;
        ep->rank = rank; 
        ep->snd_layout = layout;
        ep->snd_DE = my_DE; 
        ep->snd_AI_count = AI_count; 
        for (i=0; i<rank; i++) {
          ep->snd_AI_exc[i] = AI_exc[i]; 
          ep->snd_AI_tot[i] = AI_tot[i];
        }
        ep->rcv_layout = layout;
        ep->rcv_DE = my_DE; 
        ep->rcv_AI_count = AI_count; 
        for (i=0; i<rank; i++) {
          ep->rcv_AI_exc[i] = AI_exc[i]; 
          ep->rcv_AI_tot[i] = AI_tot[i];
          ep->periodic[i] = periodic[i];
        }
    } else {
       printf("Warning: this route not Cached - Cache table full\n");
    }
    
    return ESMF_SUCCESS;

 } // end ESMC_RoutePrecomputeHalo


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeRedist - initialize a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeRedist(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in both Fields
      int my_DE_rcv,               // in  - DE identifier in the DELayout of
                                   //       the receiving Field
      ESMC_AxisIndex *AI_rcv_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - exclusive region only
      ESMC_AxisIndex *AI_rcv_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - total region
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      int *global_start_rcv,       // in  - array of global starting indices
                                   //       for all DE's in the DELayout and in
                                   //       each direction for the receiving
                                   //       Field
      int *global_count_rcv,       // in  - array of global strides for each
                                   //       direction for the receiving Field
      ESMC_DELayout *layout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - DE identifier in the DELayout of
                                   //       the sending Field
      ESMC_AxisIndex *AI_snd_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - exclusive region only
      ESMC_AxisIndex *AI_snd_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - total region
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      int *global_start_snd,       // in  - array of global starting indices
                                   //       for all DE's in the DELayout and in
                                   //       each direction for the sending
                                   //       Field
      int *global_count_snd,       // in  - array of global strides for each
                                   //       direction for the sending Field
      ESMC_DELayout *layout_snd) { // in  - pointer to the snd DELayout 
//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI_exc[ESMF_MAXDIM], their_AI_exc[ESMF_MAXDIM];
    ESMC_AxisIndex my_AI_tot[ESMF_MAXDIM], their_AI_tot[ESMF_MAXDIM];
    ESMC_XPacket *my_XP = NULL;
    ESMC_XPacket *their_XP = NULL;
    ESMC_XPacket *intersect_XP = NULL;
    ESMC_RouteCacheEntry *ep;
    ESMC_Logical periodic[ESMF_MAXGRIDDIM];
    int my_XPcount, their_XPcount;
    int memory_stride_snd[ESMF_MAXDIM], memory_stride_rcv[ESMF_MAXDIM];
    int i, j, k, rc;
    int their_de, their_de_parent, their_decount;

    // Initialize
    periodic[0] = ESMF_TF_FALSE;
    periodic[1] = ESMF_TF_FALSE;

    // Calculate the sending table.  If this DE is not part of the sending
    // layout (my_DE_snd = -1 ?  TODO), skip this part
    if (my_DE_snd != -1) {
 
      // get "my" AI out of the AI_snd array
      // TODO:  this is NOT going to work for data dims which are not
      //  equal the grid dims, e.g. a 2d grid with 4d data.
      for (k=0; k<rank; k++) {
        my_AI_exc[k] = AI_snd_exc[my_DE_snd + k*AI_snd_count];
        my_AI_tot[k] = AI_snd_tot[my_DE_snd + k*AI_snd_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_start_snd,
                                     global_count_snd, memory_stride_snd, 
                                     periodic, &my_XP, &my_XPcount);

      // loop over DE's from receiving layout to calculate send table
      layout_rcv->ESMC_DELayoutGetNumDEs(&their_decount);
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the rcv layout
          layout_rcv->ESMC_DELayoutGetParentDEID(their_de, layout, 
                                                 &their_de_parent);
          //their_de_parent = their_de;     // temporarily
          if (their_de_parent != their_de) 
	     cout << "their_de = " << their_de << ", parent_de = " 
                  << their_de_parent << endl;

          // get "their" AI out of the AI_rcv array
          for (k=0; k<rank; k++) {
            their_AI_exc[k] = AI_rcv_exc[their_de + k*AI_rcv_count];
            their_AI_tot[k] = AI_rcv_tot[their_de + k*AI_rcv_count];
            their_AI_exc[k].max = their_AI_tot[k].max;
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_start_rcv,
                                         global_count_rcv, memory_stride_rcv, 
                                         periodic, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local data space
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI_tot, 
                                                  rank, global_start_snd,
                                                  memory_stride_snd);

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
        my_AI_exc[k] = AI_rcv_exc[my_DE_rcv + k*AI_rcv_count];
        my_AI_tot[k] = AI_rcv_tot[my_DE_rcv + k*AI_rcv_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_start_rcv,
                                     global_count_rcv, memory_stride_rcv, 
                                     periodic, &my_XP, &my_XPcount);

      // loop over DE's from sending layout to calculate receive table
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the snd layout
          layout_snd->ESMC_DELayoutGetParentDEID(their_de, layout, 
                                                 &their_de_parent);
          //their_de_parent = their_de;     // temporarily
          if (their_de_parent != their_de) 
	     cout << "their_de = " << their_de << ", parent_de = " 
                  << their_de_parent << endl;

          // get "their" AI out of the AI_snd array
          for (k=0; k<rank; k++) {
            their_AI_exc[k] = AI_snd_exc[their_de + k*AI_snd_count];
            their_AI_tot[k] = AI_snd_tot[their_de + k*AI_snd_count];
            their_AI_exc[k].max = their_AI_tot[k].max;
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_start_snd,
                                         global_count_snd, memory_stride_snd, 
                                         periodic, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI_tot, 
                                                  rank, global_start_rcv,
                                                  memory_stride_rcv);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(their_de_parent, intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);
        }
    }

    // free unneeded XPs here
    delete my_XP;  
    my_XP = NULL;
    delete their_XP; 
    their_XP = NULL;
    delete intersect_XP; // contents have been copied in RTable
    intersect_XP = NULL;

    // add to cache table here.
    if (routetable.rcep == NULL) {
       routetable.rcep = (ESMC_RouteCacheEntry **) 
                       malloc(sizeof(ESMC_RouteCacheEntry *) * maxroutes);
       routetable.nalloc = maxroutes;

    }
    // if we decide to grow the table slowly...
    //if (routetable.nroutes <= routetable.nalloc) {
    //    realloc(routetable.rcep, (routetable.nalloc + 4) *
    //                                    sizeof(ESMC_RouteCacheEntry *));
    //    routetable.nalloc += 4;
    //}

    if (routetable.nroutes < maxroutes) {

        ep = new ESMC_RouteCacheEntry;
        routetable.rcep[routetable.nroutes] = ep;
        routetable.nroutes++;
        
        // store this in the cache
        ep->routeid = routeid;
        ep->theroute = this; 
        ep->entrystatus = 1;
        ep->rank = rank; 
        ep->snd_layout = layout_snd;
        ep->snd_DE = my_DE_snd; 
        ep->snd_AI_count = AI_snd_count; 
        for (i=0; i<rank; i++) {
          ep->snd_AI_exc[i] = AI_snd_exc[i]; 
          ep->snd_AI_tot[i] = AI_snd_tot[i];
        }
        ep->rcv_layout = layout_rcv;
        ep->rcv_DE = my_DE_rcv; 
        ep->rcv_AI_count = AI_rcv_count; 
        for (i=0; i<rank; i++) {
          ep->rcv_AI_exc[i] = AI_rcv_exc[i]; 
          ep->rcv_AI_tot[i] = AI_rcv_tot[i];
          ep->periodic[i] = ESMF_TF_FALSE;
        }
    } else {
       printf("Warning: this route not Cached - Cache table full\n");
    }
        
    //printf("end of RoutePrecomputeRedist:\n");
    //this->ESMC_RoutePrint("");
    return ESMF_SUCCESS;

 } // end ESMC_RoutePrecomputeRedist


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeRegrid - initialize a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeRegrid(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in both Fields
      int my_DE_rcv,               // in  - DE identifier in the DELayout of
                                   //       the receiving Field
      ESMC_AxisIndex *AI_rcv_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - exclusive region only
      ESMC_AxisIndex *AI_rcv_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - total region
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      int *global_start_rcv,       // in  - array of global starting indices
                                   //       for all DE's in the DELayout and in
                                   //       each direction for the receiving
                                   //       Field
      int *global_count_rcv,       // in  - array of global strides for each
                                   //       direction for the receiving Field
      ESMC_DELayout *layout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - DE identifier in the DELayout of
                                   //       the sending Field
      ESMC_AxisIndex *AI_snd_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - exclusive region only
      ESMC_AxisIndex *AI_snd_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - total region
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      int *global_start_snd,       // in  - array of global starting indices
                                   //       for all DE's in the DELayout and in
                                   //       each direction for the sending
                                   //       Field
      int *global_count_snd,       // in  - array of global strides for each
                                   //       direction for the sending Field
      ESMC_DELayout *layout_snd) { // in  - pointer to the snd DELayout 
//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI_exc[ESMF_MAXDIM], their_AI_exc[ESMF_MAXDIM];
    ESMC_AxisIndex my_AI_tot[ESMF_MAXDIM], their_AI_tot[ESMF_MAXDIM];
    ESMC_XPacket *my_XP = NULL;
    ESMC_XPacket *their_XP = NULL;
    ESMC_XPacket *intersect_XP = NULL;
    ESMC_RouteCacheEntry *ep;
    ESMC_Logical periodic[ESMF_MAXGRIDDIM];
    int my_XPcount, their_XPcount;
    int memory_stride_snd[ESMF_MAXDIM], memory_stride_rcv[ESMF_MAXDIM];
    int i, j, k, rc;
    int their_de, their_de_parent, their_decount;

    // Initialize
    periodic[0] = ESMF_TF_FALSE;
    periodic[1] = ESMF_TF_FALSE;

    // Calculate the sending table.  If this DE is not part of the sending
    // layout (my_DE_snd = -1 ?  TODO), skip this part
    if (my_DE_snd != -1) {
 
      // get "my" AI out of the AI_snd array
      // TODO:  this is NOT going to work for data dims which are not
      //  equal the grid dims, e.g. a 2d grid with 4d data.
      for (k=0; k<rank; k++) {
        my_AI_exc[k] = AI_snd_exc[my_DE_snd + k*AI_snd_count];
        my_AI_tot[k] = AI_snd_tot[my_DE_snd + k*AI_snd_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_start_snd,
                                     global_count_snd, memory_stride_snd, 
                                     periodic, &my_XP, &my_XPcount);

      // loop over DE's from receiving layout to calculate send table
      layout_rcv->ESMC_DELayoutGetNumDEs(&their_decount);
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the rcv layout
          layout_rcv->ESMC_DELayoutGetParentDEID(their_de, layout, 
                                                 &their_de_parent);
          //their_de_parent = their_de;     // temporarily
          if (their_de_parent != their_de) 
	     cout << "their_de = " << their_de << ", parent_de = " 
                  << their_de_parent << endl;

          // get "their" AI out of the AI_rcv array
          for (k=0; k<rank; k++) {
            their_AI_exc[k] = AI_rcv_exc[their_de + k*AI_rcv_count];
            their_AI_tot[k] = AI_rcv_tot[their_de + k*AI_rcv_count];
            their_AI_exc[k].max = their_AI_tot[k].max;
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_start_rcv,
                                         global_count_rcv, memory_stride_rcv, 
                                         periodic, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local data space
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI_tot, 
                                                  rank, global_start_snd,
                                                  memory_stride_snd);

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
        my_AI_exc[k] = AI_rcv_exc[my_DE_rcv + k*AI_rcv_count];
        my_AI_tot[k] = AI_rcv_tot[my_DE_rcv + k*AI_rcv_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_start_rcv,
                                     global_count_rcv, memory_stride_rcv, 
                                     periodic, &my_XP, &my_XPcount);

      // loop over DE's from sending layout to calculate receive table
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the snd layout
          layout_snd->ESMC_DELayoutGetParentDEID(their_de, layout, 
                                                 &their_de_parent);
          //their_de_parent = their_de;     // temporarily
          if (their_de_parent != their_de) 
	     cout << "their_de = " << their_de << ", parent_de = " 
                  << their_de_parent << endl;

          // get "their" AI out of the AI_snd array
          for (k=0; k<rank; k++) {
            their_AI_exc[k] = AI_snd_exc[their_de + k*AI_snd_count];
            their_AI_tot[k] = AI_snd_tot[their_de + k*AI_snd_count];
            their_AI_exc[k].max = their_AI_tot[k].max;
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_start_snd,
                                         global_count_snd, memory_stride_snd, 
                                         periodic, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP = new ESMC_XPacket;
          intersect_XP->ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP->ESMC_XPacketEmpty()) {
              delete intersect_XP;
              intersect_XP = NULL;
              continue;
          }

          // translate from global to local
          intersect_XP->ESMC_XPacketGlobalToLocal(intersect_XP, my_AI_tot, 
                                                  rank, global_start_rcv,
                                                  memory_stride_rcv);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(their_de_parent, intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);
        }
    }

    // free unneeded XPs here
    delete my_XP;  
    my_XP = NULL;
    delete their_XP; 
    their_XP = NULL;
    delete intersect_XP; // contents have been copied in RTable
    intersect_XP = NULL;

    // add to cache table here.
    if (routetable.rcep == NULL) {
       routetable.rcep = (ESMC_RouteCacheEntry **) 
                       malloc(sizeof(ESMC_RouteCacheEntry *) * maxroutes);
       routetable.nalloc = maxroutes;

    }
    // if we decide to grow the table slowly...
    //if (routetable.nroutes <= routetable.nalloc) {
    //    realloc(routetable.rcep, (routetable.nalloc + 4) *
    //                                    sizeof(ESMC_RouteCacheEntry *));
    //    routetable.nalloc += 4;
    //}

    if (routetable.nroutes < maxroutes) {

        ep = new ESMC_RouteCacheEntry;
        routetable.rcep[routetable.nroutes] = ep;
        routetable.nroutes++;
        
        // store this in the cache
        ep->routeid = routeid;
        ep->theroute = this; 
        ep->entrystatus = 1;
        ep->rank = rank; 
        ep->snd_layout = layout_snd;
        ep->snd_DE = my_DE_snd; 
        ep->snd_AI_count = AI_snd_count; 
        for (i=0; i<rank; i++) {
          ep->snd_AI_exc[i] = AI_snd_exc[i]; 
          ep->snd_AI_tot[i] = AI_snd_tot[i];
        }
        ep->rcv_layout = layout_rcv;
        ep->rcv_DE = my_DE_rcv; 
        ep->rcv_AI_count = AI_rcv_count; 
        for (i=0; i<rank; i++) {
          ep->rcv_AI_exc[i] = AI_rcv_exc[i]; 
          ep->rcv_AI_tot[i] = AI_rcv_tot[i];
          ep->periodic[i] = ESMF_TF_FALSE;
        }
    } else {
       printf("Warning: this route not Cached - Cache table full\n");
    }
        
    //printf("end of RoutePrecomputeRegrid:\n");
    //this->ESMC_RoutePrint("");
    return ESMF_SUCCESS;

 } // end ESMC_RoutePrecomputeRegrid


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
