// $Id: ESMC_Route.C,v 1.87 2004/04/13 22:59:55 jwolfe Exp $
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
               "$Id: ESMC_Route.C,v 1.87 2004/04/13 22:59:55 jwolfe Exp $";
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
      ESMC_newDELayout *delayout,
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

    *rc = newr->ESMC_RouteConstruct(delayout);

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
      ESMC_newDELayout *delayout
      ) {          // in
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

    this->delayout = delayout;
    delayout->ESMC_newDELayoutGet(&decount, NULL, NULL, NULL, NULL, &myde,
      NULL, NULL, NULL, NULL);
        
    routeid = rseqnum++;
    sendRT = ESMC_RTableCreate(myde, decount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvRT = ESMC_RTableCreate(myde, decount, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvitems = 0;

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
// !IROUTINE:  ESMC_RouteAddCache - If space, add a route to the cache table
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteAddCache(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data in both Fields
      int my_DE_rcv,               // in  - my DE identifier in the DELayout 
                                   //       where I'm the receiving Field
      ESMC_AxisIndex *AI_rcv_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - exclusive region
      ESMC_AxisIndex *AI_rcv_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field - total region
      int AI_rcv_count,            // in  - number of sets of AI's in the rcv
                                   //       array (should be the same as the 
                                   //       number of DE's in the rcv layout)
      ESMC_newDELayout *delayout_rcv,   // in  - pointer to the rcv DELayout
      int my_DE_snd,               // in  - my DE identifier in the DELayout 
                                   //       where I'm the sending Field
      ESMC_AxisIndex *AI_snd_exc,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - exclusive region
      ESMC_AxisIndex *AI_snd_tot,  // in  - array of axis indices for all DE's
                                   //       in the DELayout for the sending
                                   //       Field - total region
      int AI_snd_count,            // in  - number of sets of AI's in the snd
                                   //       array (should be the same as the
                                   //       number of DE's in the snd layout)
      ESMC_newDELayout *delayout_snd,   // in  - pointer to the snd DELayout 
      ESMC_Logical *periodic) {    // in - if halo'ing, per/axis flag

//
// !DESCRIPTION:
//     If space, add this route to the cache table.
//
//EOP
// !REQUIREMENTS:  
    int i, bytes;
    ESMC_RouteCacheEntry *ep;

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

    // TODO: this should turn into a loop up to maxroutes looking for
    // an empty slot.
    if (routetable.nroutes < maxroutes) {

        ep = new ESMC_RouteCacheEntry;
        routetable.rcep[routetable.nroutes] = ep;
        routetable.nroutes++;
 
        //DEBUG:
        //printf("Cache: adding entry %d:\n", routetable.nroutes-1);
        
        // store this in the cache
        ep->routeid = routeid;
        ep->theroute = this; 
        ep->entrystatus = 1;
        ep->rank = rank; 
        ep->snd_delayout = delayout_snd;
        ep->snd_DE = my_DE_snd; 
        ep->snd_AI_count = AI_snd_count; 
        if (AI_snd_count > 0) {
            ep->snd_AI_exc = new ESMC_AxisIndex[rank * AI_snd_count];
            ep->snd_AI_tot = new ESMC_AxisIndex[rank * AI_snd_count];
            bytes = sizeof(ESMC_AxisIndex) * rank * AI_snd_count;
            memcpy(ep->snd_AI_exc, AI_snd_exc, bytes);
            memcpy(ep->snd_AI_tot, AI_snd_tot, bytes);
        }
        ep->rcv_delayout = delayout_rcv;
        ep->rcv_DE = my_DE_rcv; 
        ep->rcv_AI_count = AI_rcv_count; 
        if (AI_rcv_count > 0) {
            ep->rcv_AI_exc = new ESMC_AxisIndex[rank * AI_rcv_count];
            ep->rcv_AI_tot = new ESMC_AxisIndex[rank * AI_rcv_count];
            bytes = sizeof(ESMC_AxisIndex) * rank * AI_rcv_count;
            memcpy(ep->rcv_AI_exc, AI_rcv_exc, bytes);
            memcpy(ep->rcv_AI_tot, AI_rcv_tot, bytes);
        }
        for (i=0; i<rank; i++) 
            ep->periodic[i] = periodic ? periodic[i] : ESMF_FALSE;

        //DEBUG:
        //printf("Info: this route added to Cache, entry %d\n", 
        //                                       routetable.nroutes-1);
    } else {
        printf("Warning: this route not Cached - Cache table full\n");
    }
    
    return ESMF_SUCCESS;

 } // end ESMC_RouteAddCache

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteDropCache - Drop a route from the cache table.
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteDropCache(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//     Delete a route from the cache table.
//
//EOP

    int i, j, entries;
    ESMC_Logical tf;
    ESMC_RouteCacheEntry *ep;

    //DEBUG:
    //printf("Info: searching cache table\n");

    for (i=0; i<routetable.nroutes; i++) {

        ep = routetable.rcep[i];
    
        // see if we find a match.  one should be enough but
        // compare both just to be sure.
        if (ep->routeid != routeid) continue;
        if (ep->theroute != this) continue;

        ep->entrystatus = 0;   // TODO: make this an enum
        if (ep->snd_AI_exc) delete [] ep->snd_AI_exc;
        if (ep->snd_AI_tot) delete [] ep->snd_AI_tot;
        if (ep->rcv_AI_exc) delete [] ep->rcv_AI_exc;
        if (ep->rcv_AI_tot) delete [] ep->rcv_AI_tot;

        //DEBUG:
        //printf("Info: this route deleted from Cache, entry %d\n", i);
        return ESMF_SUCCESS;
    } 

    //DEBUG:
    //printf("Warning: this route not found in cache\n");
    return ESMF_FAILURE;

 } // end ESMC_RouteDropCache

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
      ESMC_newDELayout *delayout_rcv,   // in  - pointer to the rcv DELayout
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
      ESMC_newDELayout *delayout_snd,   // in  - pointer to the snd DELayout 
      ESMC_Logical *periodic,      // in - if halo'ing, per/axis flag
      ESMC_Logical *hascachedroute,  // out - ESMF_TRUE, ESMF_FALSE
      ESMC_Route **route) {        // out - if true, cached route

//
// !DESCRIPTION:
//     Returns the value of Route member <Value>.
//     Can be multiple routines, one per value
//
//EOP

    int i, j, entries;
    ESMC_Logical tf;
    ESMC_RouteCacheEntry *ep;

    //DEBUG:
    //printf("Info: searching cache table\n");

    for (i=0; i<routetable.nroutes; i++) {

        ep = routetable.rcep[i];
    
        // see if we find a match.  try the simplest comparisons first.
        if (ep->entrystatus != 1) continue;    // make this a type, and flag
        if (rank != ep->rank) continue;
        if (delayout_snd != ep->snd_delayout) continue;
        if (delayout_rcv != ep->rcv_delayout) continue;
        if (my_DE_snd != ep->snd_DE) continue; 
        if (my_DE_rcv != ep->rcv_DE) continue; 
        if (AI_snd_count != ep->snd_AI_count) continue; 
        if (AI_rcv_count != ep->rcv_AI_count) continue; 
        for (j=0; j<rank; j++)
            if (periodic[j] != ep->periodic[j]) goto next;
        if (ep->snd_AI_count > 0) {
            entries = ep->snd_AI_count * ep->rank;            
            for (j=0; j<entries; j++) {
                tf = ESMC_AxisIndexEqual(AI_snd_exc+j, ep->snd_AI_exc+j); 
                if (tf == ESMF_FALSE) goto next;
                tf = ESMC_AxisIndexEqual(AI_snd_tot+j, ep->snd_AI_tot+j); 
                if (tf == ESMF_FALSE) goto next;
            }
        }
        if (ep->rcv_AI_count > 0) {
            entries = ep->rcv_AI_count * ep->rank;            
            for (j=0; j<entries; j++) {
                tf = ESMC_AxisIndexEqual(AI_rcv_exc+j, ep->rcv_AI_exc+j); 
                if (tf == ESMF_FALSE) goto next;
                tf = ESMC_AxisIndexEqual(AI_rcv_tot+j, ep->rcv_AI_tot+j); 
                if (tf == ESMF_FALSE) goto next;
            }
        }

        *hascachedroute = ESMF_TRUE;
        *route = routetable.rcep[i]->theroute;

        //DEBUG: 
        //printf("Cache search success: precomputed route found, entry %d\n", i);
        return ESMF_SUCCESS;

  next:  
        ; // jump here from inner loop if no match (empty statement here
          // becase some compilers now fuss if there are no executable lines
          // between a label and the closing brace.)
    }
 
    *hascachedroute = ESMF_FALSE;
    *route = NULL;

    //DEBUG: 
    //printf("Cache search failure: precomputed route not found in %d entries\n",
    //                                                      routetable.nroutes);
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
// !IROUTINE:  ESMC_RouteGetRecvItems - get size of receive buffer in N items
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteGetRecvItems(
//
// !RETURN VALUE:
//    int number of items
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//     Normally for a route the receive buffer size is known in advance.
//     But in some cases it may be useful to store the required receive size
//     along with the route, in units of items (not bytecounts).  The caller
//     can query for the size, allocate a receive buffer, and then call 
//     RouteRun.
//
//EOP
// !REQUIREMENTS:  
    
    return recvitems;

 } // end ESMC_RouteGetRecvItems

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RouteSetRecvItems - set size of receive buffer in N items
//
// !INTERFACE:
      int ESMC_Route::ESMC_RouteSetRecvItems(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nitems) {      // in - number of items
//
// !DESCRIPTION:
//     Normally for a route the receive buffer size is known in advance.
//     But in some cases it may be useful to store the required receive size
//     along with the route, in units of items (not bytecounts).  The caller
//     can query for the size, allocate a receive buffer, and then call 
//     RouteRun.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    this->recvitems = nitems;

    return ESMF_SUCCESS;

 } // end ESMC_RouteSetRecvItems

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
      void *dstaddr,       // in, local receive buffer base address
      ESMC_DataKind dk) {  // in, data kind for both src & dest
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
    int srcitems, rcvitems;
    int soffset, roffset;
    int scontig_length, rcontig_length;
    int sstride[ESMF_MAXDIM], rstride[ESMF_MAXDIM];
    int srep_count[ESMF_MAXDIM], rrep_count[ESMF_MAXDIM];
    void *srcmem, *rcvmem;
    int srccount, rcvcount;
    int srctcount, rcvtcount;
    char *srcbufstart, *rcvbufstart;
    char *srcbuf, *rcvbuf;
    char *srcptr, *rcvptr;
    int nbytes;

    rc = delayout->ESMC_newDELayoutGet(NULL, NULL, NULL, NULL, NULL, &mydeid,
                                       NULL, NULL, NULL, NULL);
    rc = ct->ESMC_CommTableGetCount(&ccount);
    
    printf("ESMC_RouteRun: %p, %p\n", srcaddr, dstaddr);

    //printf("Ready to run Route on DE %d, commtable count = %d:\n",
    //           mydeid, ccount);
    //ESMC_RoutePrint("");

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
        
        // find total number of xpackets
	rc = recvRT->ESMC_RTableGetCount(theirdeid, &xrcount);
	rc = sendRT->ESMC_RTableGetCount(theirdeid, &xscount);

        xmcount = MAX(xrcount, xscount);
        for (m=0, ixs=0, ixr=0; m < xmcount; m++, ixs++, ixr++){

            // look up the corresponding send/recv xpackets in the rtables

            if (xscount > 1) printf("WARNING! cannot handle multiple xps yet %d\n",xscount);
            if (ixs < xscount) {
                rc = sendRT->ESMC_RTableGetEntry(theirdeid, ixs, &sendxp);
                rc = sendxp->ESMC_XPacketGet(&srank, &soffset, &scontig_length, sstride, srep_count);
                printf("RouteRun: sendxp\n");
                sendxp->ESMC_XPacketPrint();
            } else {
                sendxp = NULL;
                srank = 0;
                soffset=0; scontig_length=0;
                for(j=0; j<ESMF_MAXDIM; j++) {
                    srep_count[j]=0;
		    sstride[j]=0;
                }
                printf("nothing to send\n");
            }
            printf("soffset: %d\n", soffset);

            if (xrcount > 1) printf("WARNING! cannot handle multiple xps yet %d\n",xrcount);
            if (ixr < xrcount) {
                rc = recvRT->ESMC_RTableGetEntry(theirdeid, ixr, &recvxp);
                rc = recvxp->ESMC_XPacketGet(&rrank, &roffset, &rcontig_length, rstride, rrep_count);
                 printf("RouteRun: recvxp\n");
                 recvxp->ESMC_XPacketPrint();
            } else {
                recvxp = NULL;
                rrank = 0;
		roffset=0; rcontig_length=0;
                for(j=0; j<ESMF_MAXDIM; j++) {
                    rrep_count[j]=0;
                    rstride[j]=0;
                }
                printf("nothing to recv\n");
            }
            printf("roffset: %d\n", roffset);
        
       
            // ready to call the comm routines - multiple times, one for
            //  each disjoint memory piece.
     
            // if sendxp == NULL, nothing to send
            // if recvxp == NULL, nothing to recv
            // (but one way communication is certainly possible)

           // TODO: for now, ranks must match.
           mrank = MAX(srank, rrank);
           //printf("srank=%d, rrank=%d, mrank=%d\n", srank, rrank, mrank);
           //printf(" starting srcaddr=0x%08lx, dstaddr=0x%08lx\n", 
           //                     (long int)srcaddr, (long int)dstaddr);

	   // Count the total number of points to send/recv.  set count to 0
           // if this PE has nothing to send or receive (which is a possible
           // case which must be handled).
	   srctcount = sendxp ? scontig_length : 0;
	   rcvtcount = recvxp ? rcontig_length : 0;
           for (j=0; j<mrank-1; j++) {
               if (sendxp) srctcount *= srep_count[j];
               if (recvxp) rcvtcount *= rrep_count[j];
           }

           nbytes = ESMC_DataKindSize(dk);

	   // allocate temporary buffers
           if (srctcount > 0)
	       srcbufstart = (char *)(malloc(srctcount*nbytes));
           else
               srcbufstart = NULL;
           if (rcvtcount > 0)
	       rcvbufstart = (char *)(malloc(rcvtcount*nbytes));
           else
               rcvbufstart = NULL;
	   srcbuf = srcbufstart;
	   rcvbuf = rcvbufstart;
           
           printf("srcbufstart=%p, rcvbufstart%p\n", srcbufstart, rcvbufstart);

           // copy in to the send buffer
	   if(srctcount > 0) {
             switch (srank) {
               case 2:
                 srcitems = soffset;
                 for (l=0; l<srep_count[0] ; l++, srcitems += sstride[0]) {

                      srcptr = (char *)srcaddr+(srcitems*nbytes); 
		      memcpy(srcbuf,srcptr,scontig_length*nbytes);
		      srcbuf += scontig_length*nbytes;
                 }
                 break;
               case 3:
                 for (k=0; k<srep_count[1]; k++, soffset += sstride[1]) {
                   srcitems = soffset;
                   for (l=0; l<srep_count[0]; l++, srcitems += sstride[0]) {

                        srcptr = (char *)srcaddr+(srcitems*nbytes); 
		        memcpy(srcbuf,srcptr,scontig_length*nbytes);
		        srcbuf += scontig_length*nbytes;
                   }
                 }
                 break;
               default:
                 printf("no code to handle rank %d yet\n", srank);
                 return ESMF_FAILURE;
             }
           }

           if(mydeid == theirdeid)
	      rcvbuf = srcbufstart;
	   else
              delayout->ESMC_newDELayoutCopyCopy((void **)srcbufstart, NULL,
                (void **)rcvbufstart, NULL, srctcount*nbytes, rcvtcount*nbytes, 
                 mydeid, theirdeid, ESMF_TRUE);

           // copy out of the recv buffer
	   if(rcvtcount > 0) {
             switch (rrank) {
               case 2:
                 rcvitems = roffset;
                 for (l=0; l<rrep_count[0] ; l++, rcvitems += rstride[0]) {

                      rcvptr = (char *)dstaddr+(rcvitems*nbytes); 
                      memcpy(rcvptr,rcvbuf,rcontig_length*nbytes);
                      rcvbuf += rcontig_length*nbytes;
                 }
                 break;
               case 3:
                 for (k=0; k<rrep_count[1]; k++, roffset += rstride[1]) {
                   rcvitems = roffset;
                   for (l=0; l<rrep_count[0]; l++, rcvitems += rstride[0]) {

                        rcvptr = (char *)dstaddr+(rcvitems*nbytes); 
                        memcpy(rcvptr,rcvbuf,rcontig_length*nbytes);
                        rcvbuf += rcontig_length*nbytes;
                   }
                 }
                 break;
               default:
                 printf("no code to handle rank %d yet\n", rrank);
                 return ESMF_FAILURE;
             }
           }

           if (srcbufstart)
               free(srcbufstart);
           if (rcvbufstart)
               free(rcvbufstart);

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
      ESMC_newDELayout *delayout,       // in  - pointer to the DELayout 
      ESMC_Logical *periodic) {    // in  - array of flags, one per dim
//
// !DESCRIPTION:
//      Initializes a Route for a Halo with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI_exc[ESMF_MAXDIM], my_AI_tot[ESMF_MAXDIM];
    ESMC_AxisIndex their_AI[ESMF_MAXDIM];
    ESMC_XPacket intersect_XP;
    ESMC_XPacket *my_XP = NULL;
    ESMC_XPacket *their_XP = NULL;
    ESMC_DE *their_deid;
    ESMC_Logical boundary[ESMF_MAXGRIDDIM][2];
    int nde[ESMF_MAXGRIDDIM], their_DE_pos[ESMF_MAXGRIDDIM];
    int my_global_start[ESMF_MAXDIM];
    int my_XPcount, their_XPcount;
    int rc; 
    int i, j, k, start;
    int their_de, decount, dummy;

    // Calculate the sending table.  If this DE is not part of the sending
    // TODO: this assumes a 2D layout?  (certainly < 3D)
        delayout->ESMC_newDELayoutGet(&decount, NULL, NULL, NULL, NULL, NULL,
                                      NULL, NULL, nde, ESMF_MAXGRIDDIM);

    // Calculate the sending table.
 
    // get "my" AI out of the AI_exc array
    // TODO:  this is NOT going to work for data dims which are not
    //  equal the grid dims, e.g. a 2d grid with 4d data.
    for (k=0; k<rank; k++) {
      my_AI_exc[k] = AI_exc[my_DE + k*AI_count];
      my_AI_tot[k] = AI_tot[my_DE + k*AI_count];
      my_global_start[k] = global_start[my_DE + k*AI_count];
    }

    // calculate "my" (local DE's) XPacket in the sense of the global data
    // we only compute actual DE XPackets here; the periodic case is handled
    // when computing "their" XPs.  the "boundary" arg below is NULL - that
    // is where periodic information would be passed.
    rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_count, NULL, &my_XP,
                                   &my_XPcount);

    // loop over DE's from receiving layout to calculate send table
    // already obtained "decount" during last call
    for (k=0; k<decount; k++) {
      their_de = k;
      delayout->ESMC_newDELayoutGetDE(their_de, their_DE_pos, ESMF_MAXGRIDDIM,
                                      NULL, NULL, NULL, NULL, NULL);
      // get "their" AI out of the AI_tot array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_tot[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      for (j=0; j<rank; j++) {
        boundary[j][0] = ESMF_FALSE;
        boundary[j][1] = ESMF_FALSE;
        if (periodic[j]==ESMF_TRUE) {
          if (their_DE_pos[j] == 0) 
            boundary[j][0] = ESMF_TRUE;
          if (their_DE_pos[j] == nde[j]-1) 
            boundary[j][1] = ESMF_TRUE;
        }
      }
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, global_count, boundary,
                                     &their_XP, &their_XPcount);
      
      // calculate the intersection
      start = 0;
      if (my_DE == their_de) start=1;
      for (i=start; i<their_XPcount; i++) {

        // reuse the same XP for the entire loop.
        intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[i]);

        // if there's no intersection, no need to add an entry here
        if (intersect_XP.ESMC_XPacketEmpty()) {
          continue;
        }

        // translate from global to local data space
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                               rank, my_global_start);

        // load the intersecting XPacket into the sending RTable
        sendRT->ESMC_RTableSetEntry(their_de, &intersect_XP);
        ct->ESMC_CommTableSetPartner(their_de);
      }
  
      // free XPs allocated by XPacketFromAxisIndex() routine above
      delete [] their_XP;
    }

    // free the old exclusive my_XP before computing the total my_XP
    delete [] my_XP;

    // Calculate the receiving table.
 
    rc = ESMC_XPacketFromAxisIndex(my_AI_tot, rank, global_count, NULL, &my_XP,
                                   &my_XPcount);

    // loop over DE's from layout to calculate receive table
    for (k=0; k<decount; k++) {
      their_de = k;
      delayout->ESMC_newDELayoutGetDE(their_de, their_DE_pos, ESMF_MAXGRIDDIM,
                                      NULL, NULL, NULL, NULL, NULL);
      // get "their" AI out of the AI_exc array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_exc[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      for (j=0; j<rank; j++) {
        boundary[j][0] = ESMF_FALSE;
        boundary[j][1] = ESMF_FALSE;
        if (periodic[j] == ESMF_TRUE) {
          if (their_DE_pos[j] == 0) 
            boundary[j][0] = ESMF_TRUE;
          if (their_DE_pos[j] == nde[j]-1) 
            boundary[j][1] = ESMF_TRUE;
        }
      }
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, global_count, boundary, 
                                     &their_XP, &their_XPcount);

      // calculate the intersection
      start = 0;
      if (my_DE == their_de) start=1;
      for (i=start; i<their_XPcount; i++) {

        intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[i]);

        // if there's no intersection, no need to add an entry here
        if (intersect_XP.ESMC_XPacketEmpty()) 
          continue;
         
        // translate from global to local
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot,
                                               rank, my_global_start);

        // load the intersecting XPacket into the receiving RTable
        recvRT->ESMC_RTableSetEntry(their_de, &intersect_XP);
        ct->ESMC_CommTableSetPartner(their_de);
      }

      // free their_XP allocated in XPacketFromAxisIndex above.
      delete [] their_XP;
    }
    //ESMC_RoutePrint("");
 
    // and delete the total my_XP
    delete [] my_XP;  


    // add this route to the cache table
        ESMC_RouteAddCache(rank, my_DE, AI_exc, AI_tot, AI_count, delayout, 
                           my_DE, AI_exc, AI_tot, AI_count, delayout, 
                           periodic);

    return rc;

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
      int rank,                   // in  - rank of data in both Fields
      int dstMyDE,                // in  - DE identifier in the DELayout of
                                  //       the destination Field
      ESMC_AxisIndex *dstCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the destination
                                  //       Field covering the computational
                                  //       domain
      ESMC_AxisIndex *dstTotalAI, // in  - array of axis indices for all DE's
                                  //       in the DELayout for the destination
                                  //       Field covering the total domain
      int dstAICount,             // in  - number of sets of AI's in the dst
                                  //       array (should be the same as the 
                                  //       number of DE's in the dst layout)
      int *dstGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the destination
                                  //       Field
      int *dstGlobalCount,        // in  - array of global strides for each
                                  //       direction for the receiving Field
      ESMC_newDELayout *dstdeLayout,   // in  - pointer to the rcv DELayout
      int srcMyDE,                // in  - DE identifier in the DELayout of
                                  //       the source Field
      ESMC_AxisIndex *srcCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the source
                                  //       Field covering the computational
                                  //       domain
      ESMC_AxisIndex *srcTotalAI, // in  - array of axis indices for all DE's
                                  //       in the DELayout for the source
                                  //       Field covering the total domain
      int srcAICount,             // in  - number of sets of AI's in the src
                                  //       array (should be the same as the
                                  //       number of DE's in the src layout)
      int *srcGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the source
                                  //       Field
      int *srcGlobalCount,        // in  - array of global strides for each
                                  //       direction for the source Field
      ESMC_newDELayout *srcdeLayout) { // in  - pointer to the src DELayout 
//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex myAI[ESMF_MAXDIM], myTotalAI[ESMF_MAXDIM],
                                        theirAI[ESMF_MAXDIM];
    ESMC_XPacket *myXP = NULL;
    ESMC_XPacket *theirXP = NULL;
    ESMC_XPacket intersectXP;
    int myXPCount, theirXPCount;
    int myGlobalStart[ESMF_MAXDIM];
    int i, k, rc;
    int didsomething;
    int theirDE, theirDEParent, theirDECount;

    // set this here, because if neither send or recv are > 0 then we
    // do nothing here.
    rc = ESMF_SUCCESS;
    didsomething = 0;

    // Calculate the sending table.  If this DE is not part of the sending
    // layout skip this loop.
    if (srcMyDE != -1) {
 
       didsomething++;
 
      // get "my" AI out of the srcAI array
      // TODO:  this is NOT going to work for data dims which are not
      //  equal the grid dims, e.g. a 2d grid with 4d data.
      for (k=0; k<rank; k++) {
        myAI[k]          =      srcCompAI[srcMyDE + k*srcAICount];
        myTotalAI[k]     =     srcTotalAI[srcMyDE + k*srcAICount];
        myGlobalStart[k] = srcGlobalStart[srcMyDE + k*srcAICount];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(myAI, rank, srcGlobalCount,
                                     NULL, &myXP, &myXPCount);

      // loop over DE's from receiving layout to calculate send table
      dstdeLayout->ESMC_newDELayoutGet(&theirDECount, NULL, NULL, NULL, NULL,
                                       NULL, NULL, NULL, NULL, NULL);
      for (i=0; i<theirDECount; i++) {
          theirDE = i;

          // get the parent DE identifier for this DE in the rcv layout
          dstdeLayout->ESMC_newDELayoutGetDEMatch(theirDE, *delayout, NULL,
                                                  &theirDEParent, 1);
          printf("Match1: %d, %d\n", theirDE, theirDEParent);
          //theirDEParent = theirDE;     // temporarily
          if (theirDEParent != theirDE) 
	     cout << "theirDE = " << theirDE << ", parentDE = " 
                  << theirDEParent << endl;

          // get "their" AI out of the dstAI array
          for (k=0; k<rank; k++) {
            theirAI[k]     =  dstCompAI[theirDE + k*dstAICount];
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(theirAI, rank, dstGlobalCount,
                                         NULL, &theirXP, &theirXPCount);

          // calculate the intersection
          intersectXP.ESMC_XPacketIntersect(&myXP[0], &theirXP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersectXP.ESMC_XPacketEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] theirXP;
              continue;
          }

          // translate from global to local data space
          intersectXP.ESMC_XPacketGlobalToLocal(&intersectXP, myTotalAI, 
                                                rank, myGlobalStart);

          // load the intersecting XPacket into the sending RTable
          sendRT->ESMC_RTableSetEntry(theirDEParent, &intersectXP);
          ct->ESMC_CommTableSetPartner(theirDEParent);
 
          // free XPs allocated by XPacketFromAxisIndex() routine above
          delete [] theirXP;
        }

        // free the src myXP before computing the rcv myXP
        delete [] myXP;
    }

    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout skip this loop completely.
    if (dstMyDE != -1) {
 
       didsomething++;
 
      // get "my" AI out of the dstAI array
      for (k=0; k<rank; k++) {
        myAI[k]          =      dstCompAI[dstMyDE + k*dstAICount];
        myTotalAI[k]     =     dstTotalAI[dstMyDE + k*dstAICount];
        myGlobalStart[k] = dstGlobalStart[dstMyDE + k*dstAICount];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(myAI, rank, dstGlobalCount,
                                     NULL, &myXP, &myXPCount);

      // loop over DE's from sending layout to calculate receive table
      for (i=0; i<theirDECount; i++) {
          theirDE = i;

          // get the parent DE identifier for this DE in the src layout
          dstdeLayout->ESMC_newDELayoutGetDEMatch(theirDE, *delayout, NULL,
                                                  &theirDEParent, 1);
          printf("Match2: %d, %d\n", theirDE, theirDEParent);
          //theirDEParent = theirDE;     // temporarily
          if (theirDEParent != theirDE) 
	     cout << "theirDE = " << theirDE << ", parentDE = " 
                  << theirDEParent << endl;

          // get "their" AI out of the srcAI array
          for (k=0; k<rank; k++) {
            theirAI[k]     =  srcCompAI[theirDE + k*srcAICount];
          }
 
          // calculate "their" XPacket in the sense of the global data
          rc = ESMC_XPacketFromAxisIndex(theirAI, rank, srcGlobalCount,
                                         NULL, &theirXP, &theirXPCount);

          // calculate the intersection
          intersectXP.ESMC_XPacketIntersect(&myXP[0], &theirXP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersectXP.ESMC_XPacketEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] theirXP;
              continue;
          }

          // translate from global to local
          intersectXP.ESMC_XPacketGlobalToLocal(&intersectXP, myTotalAI, 
                                                rank, myGlobalStart);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(theirDEParent, &intersectXP);
          ct->ESMC_CommTableSetPartner(theirDEParent);
        }

        // free the dst myXP
        delete [] myXP;
    }

    // add this route to the cache table
    if (didsomething)
        ESMC_RouteAddCache(rank, 
                  dstMyDE, dstCompAI, dstTotalAI, dstAICount, dstdeLayout,
                  srcMyDE, srcCompAI, srcTotalAI, srcAICount, srcdeLayout,
                           NULL);

    //printf("end of RoutePrecomputeRedist:\n");
    //this->ESMC_RoutePrint("");

    return rc;

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
      ESMC_newDELayout *delayout_rcv,   // in  - pointer to the rcv DELayout
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
      ESMC_newDELayout *delayout_snd) { // in  - pointer to the snd DELayout 
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
    ESMC_XPacket intersect_XP;
    int my_global_start[ESMF_MAXDIM];
    int my_XPcount, their_XPcount;
    int i, k, rc;
    int didsomething;
    int their_de, their_de_parent, their_decount;

    // set this here, because if neither send or recv are > 0 then we
    // do nothing here.
    rc = ESMF_SUCCESS;
    didsomething = 0;

    // Calculate the sending table.  If this DE is not part of the sending

    // Calculate the sending table.  If this DE is not part of the sending
    // layout skip this code.
    if (my_DE_snd != -1) {
 
       didsomething++;
 
      // get "my" AI out of the AI_snd array
      // TODO:  this is NOT going to work for data dims which are not
      //  equal the grid dims, e.g. a 2d grid with 4d data.
      for (k=0; k<rank; k++) {
        my_AI_exc[k] = AI_snd_exc[my_DE_snd + k*AI_snd_count];
        my_AI_tot[k] = AI_snd_tot[my_DE_snd + k*AI_snd_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
        my_global_start[k] = global_start_snd[my_DE_snd + k*AI_snd_count];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_count_snd,
                                     NULL, &my_XP, &my_XPcount);

      // loop over DE's from receiving layout to calculate send table
      delayout_rcv->ESMC_newDELayoutGet(&their_decount, NULL, NULL, NULL, NULL,
                                        NULL, NULL, NULL, NULL, NULL);
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the rcv layout
          delayout_rcv->ESMC_newDELayoutGetDEMatch(their_de, *delayout, NULL,
                                                   &their_de_parent, 1);
          printf("Match1: %d, %d\n", their_de, their_de_parent);
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
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_count_rcv,
                                         NULL, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP.ESMC_XPacketEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] their_XP;
              continue;
          }

          // translate from global to local data space
          intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                                 rank, my_global_start);

          // load the intersecting XPacket into the sending RTable
          sendRT->ESMC_RTableSetEntry(their_de_parent, &intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);
        }

        // free the src my_XP before computing the rcv my_XP
        delete [] my_XP;
    }


    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout skip this code
    if (my_DE_rcv != -1) {
 
       didsomething++;
 
      // get "my" AI out of the AI_rcv array
      for (k=0; k<rank; k++) {
        my_AI_exc[k] = AI_rcv_exc[my_DE_rcv + k*AI_rcv_count];
        my_AI_tot[k] = AI_rcv_tot[my_DE_rcv + k*AI_rcv_count];
        my_AI_exc[k].max = my_AI_tot[k].max;
        my_global_start[k] = global_start_rcv[my_DE_rcv + k*AI_rcv_count];
      }

      // calculate "my" (local DE's) XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(my_AI_exc, rank, global_count_rcv,
                                     NULL, &my_XP, &my_XPcount);

      // loop over DE's from sending layout to calculate receive table
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent DE identifier for this DE in the snd layout
          delayout_snd->ESMC_newDELayoutGetDEMatch(their_de, *delayout, NULL,
                                                   &their_de_parent, 1);
          printf("Match2: %d, %d\n", their_de, their_de_parent);
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
          rc = ESMC_XPacketFromAxisIndex(their_AI_exc, rank, global_count_snd,
                                         NULL, &their_XP, &their_XPcount);

          // calculate the intersection
          intersect_XP.ESMC_XPacketIntersect(&my_XP[0], &their_XP[0]);

          // if there's no intersection, no need to add an entry here
          if (intersect_XP.ESMC_XPacketEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] their_XP;
              continue;
          }

          // translate from global to local
          intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                                 rank, my_global_start);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(their_de_parent, &intersect_XP);
          ct->ESMC_CommTableSetPartner(their_de_parent);

          // free XPs allocated by XPacketFromAxisIndex() routine above
          delete [] their_XP;
        }

        // free the rcv my_XP
        delete [] my_XP;
    }

    // add this route to the cache table
    if (didsomething)
        ESMC_RouteAddCache(rank, 
                 my_DE_rcv, AI_rcv_exc, AI_rcv_tot, AI_rcv_count, delayout_rcv,
                 my_DE_snd, AI_snd_exc, AI_snd_tot, AI_snd_count, delayout_snd,
                 NULL);
        
    //printf("end of RoutePrecomputeRegrid:\n");
    //this->ESMC_RoutePrint("");

    return rc;

 } // end ESMC_RoutePrecomputeRegrid


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeDomList - initialize a Route from a DomainList
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeDomList(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                    // in  - rank of data
      int my_DE,                   // in  - DE identifier in the DELayout
      ESMC_DomainList *sendDomainList,
                                   // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
      ESMC_DomainList *recvDomainList) {
                                   // in  - array of axis indices for all DE's
                                   //       in the DELayout for the receiving
                                   //       Field
//
// !DESCRIPTION:
//      Initializes a Route from a DomainList with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex my_AI[ESMF_MAXDIM], their_AI[ESMF_MAXDIM];
    ESMC_XPacket *my_XP = NULL;
    ESMC_XPacket *their_XP = NULL;
    int my_XPcount, their_XPcount;
    int global_count[ESMF_MAXDIM];
    int rc;  
    int j, k, theirOffset, offset, count;
    int their_de;

    for (j=0; j<rank; j++)
      global_count[j] = 0;

    // Calculate the sending table.
    // loop over DE's from DomainList to send to
    for (k=0; k<sendDomainList->num_domains; k++) {
      their_de = sendDomainList->ESMC_DomainListGetDE(k);
      //their_de = sendDomainList->domains[k].DE;
      // get "my" AI
      for (j=0; j<rank; j++) 
        my_AI[j] = sendDomainList->ESMC_DomainListGetAI(k, j);
        //my_AI[j] = sendDomainList->domains[k].ai_list[j];
      
 
      // calculate "my" XPacket from the AxisIndices -- in this case the
      // AIs are in local space and so is the XPacket
      rc = ESMC_XPacketFromAxisIndex(my_AI, rank, global_count, NULL,
                                     &my_XP, &my_XPcount);
      
      // load the XPacket into the sending RTable
      sendRT->ESMC_RTableSetEntry(their_de, my_XP);
      ct->ESMC_CommTableSetPartner(their_de);

      // free each XP before allocating another in XPacketFromAxisIndex
      delete [] my_XP;
    }

    // Calculate the receiving table.
    // loop over DE's from DomainList to receive from
    offset = 0;
    for (k=0; k<recvDomainList->num_domains; k++) {
      their_de = recvDomainList->ESMC_DomainListGetDE(k);
      //their_de = recvDomainList->domains[k].DE;
      // get "their" AI
      count = 1;
      for (j=0; j<rank; j++) {
        their_AI[j] = recvDomainList->ESMC_DomainListGetAI(k, j);
        //their_AI[j] = recvDomainList->domains[k].ai_list[j];
        count = count * (their_AI[j].max - their_AI[j].min + 1);
      }
 
      // calculate "their" XPacket from the AxisIndices -- in this case the
      // AIs are in local space and so is the XPacket
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, global_count, NULL,
                                     &their_XP, &their_XPcount);
      
      // modify the XPacket so it gets stored immediately after the last one
      theirOffset = their_XP->ESMC_XPacketGetOffset();
      theirOffset += offset;
      their_XP->ESMC_XPacketSetOffset(theirOffset);
      offset += count;

      // load the XPacket into the sending RTable
      recvRT->ESMC_RTableSetEntry(their_de, their_XP);
      ct->ESMC_CommTableSetPartner(their_de);

      // free each XP before allocating another in XPacketFromAxisIndex
      delete [] their_XP;
    }
 
    //ESMC_RoutePrint("");
 
    // add this route to the cache table
    ESMC_RouteAddCache(rank, my_DE, NULL, NULL, 0, delayout,
                             my_DE, NULL, NULL, 0, delayout, NULL);

    return rc;

 } // end ESMC_RoutePrecomputeDomainList


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
//    printf(" DELayout:\n");
//    rc = layout->ESMC_DELayoutPrint(); // options);  // doesn't take opts yet
    printf(" Send table:\n");
    rc = sendRT->ESMC_RTablePrint(options);
    printf(" Recv table:\n");
    rc = recvRT->ESMC_RTablePrint(options);
    printf(" Recv items: %d\n", recvitems);
    printf(" Comm table:\n");
    rc = ct->ESMC_CommTablePrint(options);

    return rc;

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
