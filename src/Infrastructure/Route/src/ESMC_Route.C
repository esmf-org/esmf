//$Id: ESMC_Route.C,v 1.165.2.3 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Route method implementation (body) file
#define ESMF_FILENAME "ESMC_Route.C"

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
 #include <ESMC_Start.h>
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include <ESMC_Route.h>
 #include <ESMC_LogErr.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
               "$Id: ESMC_Route.C,v 1.165.2.3 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------
class permuteLocal {
public:
   int index, offset;
};

class permuteGlobal {
public:
   int index, de, offset;
};

int compare1(const void *item1, const void *item2) {
  permuteLocal *a, *b;

  a = (permuteLocal*)item1;
  b = (permuteLocal*)item2;

  return(a->offset - b->offset);
}

int compare2(const void *item1, const void *item2) {
  permuteGlobal *a, *b;

  a = (permuteGlobal*)item1;
  b = (permuteGlobal*)item2;

  return(a->offset - b->offset);
}

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Route routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteCreate"
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
      ESMCI::VM *vm,
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

    *rc = newr->ESMC_RouteConstruct(vm);

    if (*rc == ESMF_FAILURE)
        return NULL;
    else
        return newr;

 } // end ESMC_RouteCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteDestroy"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteConstruct"
//BOP
// !IROUTINE:  ESMC_RouteConstruct - fill in an already allocated Route
//
// !INTERFACE:
    int ESMC_Route::ESMC_RouteConstruct(
//
// !RETURN VALUE:
//  int error return code 
//
// !ARGUMENTS:
    ESMCI::VM *vm) {          // in
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
    int rc;
    int npets;          // total number of PETs in this VM
    int mypet;          // pet id of this pet

    this->vm = vm;
    npets = vm->getNpets();
    mypet = vm->getMypet();
    
    // communications strategy: sync/async, pack by pet, xp, none, etc
    options = ESMC_ROUTE_OPTION_DEFAULT;
        
    routeid = rseqnum++;
    sendRT = ESMC_RTableCreate(mypet, npets, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvRT = ESMC_RTableCreate(mypet, npets, &rc);
    if (rc == ESMF_FAILURE)
       return rc;

    recvitems = 0;

    ct = ESMC_CommTableCreate(mypet, npets, &rc);
    if (rc == ESMF_FAILURE)
       return rc;
    
    // uncomment for profiling
    // timer1 = timer2 = timer3 = 0.0;
    
    return ESMF_SUCCESS;

 } // end ESMC_RouteConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteDestruct"
//BOP
// !IROUTINE:  ESMC_RouteDestruct - release resources associated w/a Route
//
// !INTERFACE:
    int ESMC_Route::ESMC_RouteDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    void) {
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteSetSend"
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
    int dest_pet,        // in - destination pet id
    ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet and destination pet to the
//     route table, and marks this pet as needed in the comm table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = sendRT->ESMC_RTableSetEntry(dest_pet, xp);
    if (rc != ESMF_SUCCESS)
        return rc;

    rc = ct->ESMC_CommTableSetPartner(dest_pet);
    return rc;

 } // end ESMC_RouteSetSend

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteSetRecv"
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
    int source_pet,       // in - sending pet id
      ESMC_XPacket *xp) {  // in - exchange packet
//
// !DESCRIPTION:
//     Adds an exchange packet and source pet to the route table,
//     and marks this pet as needed in the comm table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc;

    rc = recvRT->ESMC_RTableSetEntry(source_pet, xp);
    if (rc != ESMF_SUCCESS)
        return rc;

    rc = ct->ESMC_CommTableSetPartner(source_pet);

    return rc;

 } // end ESMC_RouteSetRecv

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteSetOptions"
//BOP
// !IROUTINE:  ESMC_RouteSetOptions - set Route option
//
// !INTERFACE:
    int ESMC_Route::ESMC_RouteSetOptions(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    ESMC_RouteOptions opt       // in - option
    ){
//
// !DESCRIPTION:
//     Set option, adding defaults if necessary!
//
//EOP
// !REQUIREMENTS:  
    
    // check for sync/async
    if (!(opt&ESMC_ROUTE_OPTION_ASYNC || opt&ESMC_ROUTE_OPTION_SYNC)){
      // set SYNC by default
      opt = (ESMC_RouteOptions)(opt|ESMC_ROUTE_OPTION_SYNC);
    }
    
    // check for pack option
    if (!(opt&ESMC_ROUTE_OPTION_PACK_PET || 
          opt&ESMC_ROUTE_OPTION_PACK_XP ||
          opt&ESMC_ROUTE_OPTION_PACK_NOPACK)){
      // set NOPACK by default
      opt = (ESMC_RouteOptions)(opt|ESMC_ROUTE_OPTION_PACK_PET);
    }

    options = opt;

    return ESMF_SUCCESS;

 } // end ESMC_RouteSetOptions

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteGetSumMaxXPsPerPET"
//BOP
// !IROUTINE:  ESMC_RouteGetSumMaxXPsPerPET - return a specialized sum
//
// !INTERFACE:
    int ESMC_Route::ESMC_RouteGetSumMaxXPsPerPET(
//
// !RETURN VALUE:
//    standard return code
//
// !ARGUMENTS:
    int *count) {   // the specialized sum as described below.
//
// !DESCRIPTION:
//     When executing a Route the current communication strategy uses a
//     SendRecv() call which can both send and receive a buffer in a single
//     operation.   Therefore when computing how many total operations are
//     going to be performed (which is needed, for example, when computing how
//     many handles need to be allocated for asychronous communications)
//     the total value is not simply the sum of all xpackets, but the sum of
//     the max number of xpackets destined for each remote PET. 
//  
//     This routine is used by the code which packs all regions from a single
//     XP into one communication buffer, so it must sum the max count of XPs
//     on a PET-by-PET basis over the entire table.
//
//EOP
// !REQUIREMENTS:  
    
    int i, rc, localcount;
    int theirPET, needed, commCount;
    int sendXPCount, recvXPCount;

    rc = ct->ESMC_CommTableGetCount(&commCount);

    localcount = 0;
    for (i=0; i<commCount; i++) {
        rc = ct->ESMC_CommTableGetPartner(i, &theirPET, &needed);
        if (!needed) continue;

        // find number of xpackets to be communicated with this PET
        rc = recvRT->ESMC_RTableGetCount(theirPET, &recvXPCount);
        rc = sendRT->ESMC_RTableGetCount(theirPET, &sendXPCount);

        // loop over the XPs
        localcount += MAX(recvXPCount, sendXPCount);
    }            // loop over PETs

    *count = localcount;

    return ESMF_SUCCESS;

 } // end ESMC_RouteGetSumMaxXPsPerPET

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteGetSumMaxRegionsPerXP"
//BOP
// !IROUTINE:  ESMC_RouteGetSumMaxRegionsPerXP - return a specialized sum
//
// !INTERFACE:
    int ESMC_Route::ESMC_RouteGetSumMaxRegionsPerXP(
//
// !RETURN VALUE:
//    standard return code
//
// !ARGUMENTS:
    int *count) {   // the specialized sum as described below.
//
// !DESCRIPTION:
//     When executing a Route the current communication strategy uses a
//     SendRecv() call which can both send and receive a buffer in a single
//     operation.   Therefore when computing how many total operations are
//     going to be performed (which is needed, for example, when computing how
//     many handles need to be allocated for asychronous communications)
//     the total value is not simply the sum of all xpackets, but the sum of
//     the max number of xpackets destined for each remote PET.
//
//     This routine is used by the code which does no packing, so each
//     discontiguous region in each XP must be counted individually, so here
//     we sum the max regions from each XP first on an XP-by-XP basis, and
//     then on a PET-by-PET basis over the entire route table.
//
//EOP
// !REQUIREMENTS:  
    
    int i, rc, localcount;
    int m, k, ixs, ixr;
    int theirPET, needed, commCount;
    int sendXPCount, recvXPCount, maxXPCount;
    int sendRank, recvRank;
    int sendRepCount[ESMF_MAXDIM], recvRepCount[ESMF_MAXDIM];
    int sendContigRank, recvContigRank;
    int sendReps, recvReps;   
    ESMC_XPacket *sendXP, *recvXP;

    rc = ct->ESMC_CommTableGetCount(&commCount);

    localcount = 0;
    for (i=0; i<commCount; i++) {

        rc = ct->ESMC_CommTableGetPartner(i, &theirPET, &needed);
        if (!needed) continue;
  
        // find number of xpackets to be communicated with this PET
        rc = recvRT->ESMC_RTableGetCount(theirPET, &recvXPCount);
        rc = sendRT->ESMC_RTableGetCount(theirPET, &sendXPCount);
        maxXPCount = MAX(recvXPCount, sendXPCount);
  
        // loop over the XPs
        for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){
  
            // loop until all the send and receive XPs have been tallied.
            // if the counts are not even, this means we have to take into
            // account that there might not be any more send or recv XPs
            // and do the right thing even so.
    
            // sends:
            if (ixs < sendXPCount) {
                rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
                rc = sendXP->ESMC_XPacketGet(&sendRank, NULL, NULL, NULL, 
                                             sendRepCount, NULL);
                sendContigRank = sendXP->ESMC_XPacketGetContigRank();
                sendReps = 1; 
                for (k=sendContigRank-1; k<sendRank-1; k++) {
                    sendReps *= sendRepCount[k];
                }
            } else {
                sendReps = 0; 
            }
    
    
            // recvs:
            if (ixr < recvXPCount) {
                rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
                rc = recvXP->ESMC_XPacketGet(&recvRank, NULL, NULL, NULL,
                                             recvRepCount, NULL);
                recvContigRank = recvXP->ESMC_XPacketGetContigRank();
                recvReps = 1; 
                for (k=recvContigRank-1; k<recvRank-1; k++) {
                    recvReps *= recvRepCount[k];
                }
            } else {
                recvReps = 0; 
            }
    
            localcount += MAX(sendReps, recvReps);
        }   // maxXPCount loop
    }  // loop over PETs

    *count = localcount;

    return ESMF_SUCCESS;

 } // end ESMC_RouteGetSumMaxRegionsPerXP

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteGetRecvItems"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteSetRecvItems"
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteRun"
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
    void *sendAddr,         // in, single local send buffer base address
    void *recvAddr,         // in, single local receive buffer base address
    ESMC_TypeKind dk) {     // in, data kind for both src & dest
//
// !DESCRIPTION:
//     Calls the communications routines to send/recv the information
//     set up in this table.
//
//EOP
// !REQUIREMENTS:  

     void *sendAddrList[1];
     void *recvAddrList[1];
   
     sendAddrList[0] = sendAddr;
     recvAddrList[0] = recvAddr;

     return ESMC_RouteRun(sendAddrList, recvAddrList, dk, 1);
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteRun"
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
      void **sendAddr,       // in, list of local send buffer base addresses
      void **recvAddr,       // in, list of local receive buffer base addresses
      ESMC_TypeKind dk,      // in, data kind for both src & dest
      int numAddrs) {        // in, count of src and dst addresses (must be = )
//
// !DESCRIPTION:
//     Calls the communications routines to send/recv the information
//     set up in this table.
//
//EOP
// !REQUIREMENTS:  
    
    int rc = ESMC_RC_NOT_IMPL;
    int i, j, k, l, m, n;
    int ixs, ixr;
    int commCount;
    int myPET, theirPET;
    int needed;
    bool sendContig, recvContig, madeSendBuf, madeRecvBuf;
    bool *madeSendBufList, *madeRecvBufList;
    ESMC_XPacket *sendXP, *recvXP;
    ESMC_XPacket **sendXPList, **recvXPList;
    int sendRank, recvRank, maxRank;
    int sendItemOffset, recvItemOffset;
    int sendItemPtr, recvItemPtr;
    int sendItems, recvItems;
    int sendOffset, recvOffset;
    int sendReps, recvReps, maxReps;
    int sendContigLength, recvContigLength;
    int sendIndex[ESMF_MAXDIM], recvIndex[ESMF_MAXDIM];
    int sendStride[ESMF_MAXDIM], recvStride[ESMF_MAXDIM];
    int sendRepCount[ESMF_MAXDIM], recvRepCount[ESMF_MAXDIM];
    int sendContigRank, recvContigRank; 
    int sendBufferSize, recvBufferSize;
    int sendBufferIndex, recvBufferIndex;
    char msgbuf[ESMF_MAXSTR];
    int VMType;
    int nbytes, maxReqCount;
    int sendXPCount, recvXPCount, maxXPCount;
#define STACKLIMIT 64
    ESMCI::VMK::commhandle *stackHandle[STACKLIMIT];
    char *stackSendBuffer[STACKLIMIT];
    char *stackRecvBuffer[STACKLIMIT];
    ESMC_RouteOptions useOptions;

    char *sendBuffer, *recvBuffer;
    char **sendBufferList, **recvBufferList;
    ESMCI::VMK::commhandle **handle;
    
    // uncomment for profiling
    // ESMC_R8 time, starttime;

    // debug
    // ESMC_RoutePrint();

    VMType = 0;   // TODO: unused so far, here for future use
    nbytes = ESMC_TypeKindSize(dk);
    useOptions = options;

// make sure at least one sync/async option set, and one
// pack option set (if not, set sync/pack pet). 

    if (((useOptions & ESMC_ROUTE_OPTION_SYNC) == 0) &&
        ((useOptions & ESMC_ROUTE_OPTION_ASYNC) == 0))
     useOptions = (ESMC_RouteOptions)(useOptions | ESMC_ROUTE_OPTION_SYNC);

    if (((useOptions & ESMC_ROUTE_OPTION_PACK_PET) == 0) &&
        ((useOptions & ESMC_ROUTE_OPTION_PACK_XP) == 0) &&
        ((useOptions & ESMC_ROUTE_OPTION_PACK_NOPACK) == 0))
     useOptions = (ESMC_RouteOptions)(useOptions | ESMC_ROUTE_OPTION_PACK_PET);

// -----------------------------------------------------------
// TODO:
// New branch needed here for the list of src and dst blocks
// -----------------------------------------------------------

   // TODO: currently we assume that number of source addresses *must*
   // equal dst addrs, because the route moves data from src 1 to dst 1,
   // src 2 to dst 2, etc.   

// -----------------------------------------------------------
// Branch here in a big way based on SYNC/ASYNC communications
// -----------------------------------------------------------

    if (useOptions & ESMC_ROUTE_OPTION_SYNC) {
      maxReqCount = 1;

      myPET = vm->getMypet();
      rc = ct->ESMC_CommTableGetCount(&commCount);
    
      // loop over each destination in the comm table
      for (i=0; i<commCount; i++) {

        // Find out theirPET id for this entry in the comm table.  If it's not
        // needed then no communication is necessary -- go to the next entry.
        rc = ct->ESMC_CommTableGetPartner(i, &theirPET, &needed);
        if (!needed) continue;

        // find total number of xpackets
	rc = recvRT->ESMC_RTableGetCount(theirPET, &recvXPCount);
	rc = sendRT->ESMC_RTableGetCount(theirPET, &sendXPCount);
        maxXPCount = MAX(recvXPCount, sendXPCount);

// -----------------------------------------------------------
// Branch again, this time based on packing option
// First up is packing by PET
// -----------------------------------------------------------

        // reset options only for packing by PET and if the maximum XPCount is 1
        // TODO: overload the options operator so you can go from integer
        // back and forth to RouteOptions.
        if ((useOptions & ESMC_ROUTE_OPTION_PACK_PET) && (maxXPCount == 1)) {
          useOptions = (ESMC_RouteOptions)(useOptions^ESMC_ROUTE_OPTION_PACK_PET);
          useOptions = (ESMC_RouteOptions)(useOptions|ESMC_ROUTE_OPTION_PACK_XP);
        }

        // First up is packing on a PET-level, where all the data that must be
        // exchanged between PETs is loaded up into single buffers for exchange
        if (useOptions & ESMC_ROUTE_OPTION_PACK_PET) {

          // get corresponding send/recv xpackets from the rtable
          if (sendXPCount > 0)
            sendXPList = new ESMC_XPacket*[sendXPCount];
          else
            sendXPList = NULL;
          if (recvXPCount > 0)
            recvXPList = new ESMC_XPacket*[recvXPCount];
          else
            recvXPList = NULL;

          // fill the array of pointers so they point to the XPs for this PET
          for (ixs=0; ixs<sendXPCount; ixs++){
            rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXPList[ixs]);
          }
          for (ixr=0; ixr<recvXPCount; ixr++){
            rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXPList[ixr]);
          }

          // create the buffer to hold the sending data, and pack it 
          rc = ESMC_XPacketMakeBuffer(sendXPCount, sendXPList, nbytes, numAddrs,
                                      &sendBuffer, &sendBufferSize);
          // uncomment for profiling
          // wtime(&starttime);
          rc = ESMC_XPacketPackBuffer(sendXPCount, sendXPList, dk, nbytes,
            numAddrs, sendAddr, sendBuffer);
          // uncomment for profiling
          // wtime(&time);
          // timer1 += time-starttime;

          // if my PET is both the sender and receiver, there is no need
          // to allocate a separate buffer; just point both at the single
          // send buffer.
          if (myPET == theirPET) {
            recvBuffer = sendBuffer;
          } else {
            // time to exchange data
            // create the buffer to hold the receiving data
            rc = ESMC_XPacketMakeBuffer(recvXPCount, recvXPList, nbytes,
              numAddrs, &recvBuffer, &recvBufferSize);

            // uncomment for profiling
            // wtime(&starttime);
            vm->sendrecv(sendBuffer, sendBufferSize, theirPET,
                             recvBuffer, recvBufferSize, theirPET);
            // uncomment for profiling
            // wtime(&time);
            // timer2 += time-starttime;
          }

          // whether myPET = theirPET or not, we still have to unpack the 
          // receive buffer to move the data into the final location.
          // uncomment for profiling
          // wtime(&starttime);
          rc = ESMC_XPacketUnpackBuffer(recvXPCount, recvXPList, dk, nbytes, 
                                        numAddrs, recvBuffer, recvAddr);
          // uncomment for profiling
          // wtime(&time);
          // timer3 += time-starttime;
          // delete the lists of pointers, plus the local packing buffers
          // allocated during this loop.
          delete [] sendXPList;
          delete [] recvXPList;
          delete [] sendBuffer;
          if (myPET != theirPET) 
              delete [] recvBuffer;

	}        // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is packing by XP
// -----------------------------------------------------------

        // Next is packing on an XP-level, where all the data that must be
        // exchanged between PETs loops over all the XPs that must be exchanged,
        // communicating them one at a time.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_XP) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            madeSendBuf = madeRecvBuf = false;

            // load up the corresponding send/recv xpackets from the rtables
            if (ixs < sendXPCount) {
              rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
              rc = sendXP->ESMC_XPacketGet(&sendRank, &sendOffset,
                                           &sendContigLength, sendStride,
                                           sendRepCount, &sendBufferIndex);

              // test contiguity of xpacket data 
              // if sending data is contiguous, set the pointer into the data;
              // otherwise create a buffer and pack it if necessary
              if ((numAddrs == 1) && sendXP->ESMC_XPacketIsContig()) {
                  sendContig = true;
                  sendBuffer = (char *)sendAddr[0]+(sendOffset*nbytes); 
                  sendBufferSize = sendContigLength * nbytes;
                  for (k=0; k<sendRank-1; k++)
                      sendBufferSize *= sendRepCount[k];
              } else {
                  sendContig = false;
                  rc = ESMC_XPacketMakeBuffer(1, &sendXP, nbytes, numAddrs,
                                              &sendBuffer, &sendBufferSize);
                  rc = ESMC_XPacketPackBuffer(1, &sendXP, dk, nbytes, numAddrs,
                                              sendAddr, sendBuffer);
                  madeSendBuf = true;
              }

            } else {  // nothing to more send, but data will be received
              sendBuffer = NULL;
              sendBufferSize = 0;
            }

            if (ixr < recvXPCount) {
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              rc = recvXP->ESMC_XPacketGet(&recvRank, &recvOffset, 
                                           &recvContigLength, recvStride,
                                           recvRepCount, &recvBufferIndex);

              // test contiguity of xpacket data  
              // the receive buffer is a bit more complicated - it depends both
              // on whether the receive buffer is contig and also if the sender
              // and receiver are the same PET.
              if ((numAddrs == 1) && recvXP->ESMC_XPacketIsContig()) {
                  recvContig = true;
                  recvBuffer = (char *)recvAddr[0]+(recvOffset*nbytes); 
                  recvBufferSize = recvContigLength * nbytes;
                  for (k=0; k<recvRank-1; k++)
                      recvBufferSize *= recvRepCount[k];
              } else {
                  recvContig = false;
                  // make a separate receive buffer if necessary (if the data
                  // isn't contig and can't be moved directly to where it will
                  // finally need to be.
                  if (myPET != theirPET) {
                      rc = ESMC_XPacketMakeBuffer(1, &recvXP, nbytes, numAddrs,
                                                  &recvBuffer, &recvBufferSize);
                      madeRecvBuf = true;
                  } else
                      recvBufferSize = sendBufferSize;
              }

            } else {   // nothing more to receive, but data will be sent
              recvBuffer = NULL;
              recvBufferSize = 0;
            }

            // the case where we are moving data around in the same PET 
            if (myPET == theirPET) {
              // if the receive buffer is contig, you can move the data directly
              // to where it belongs.  this could be done with memcpy,
              // unpack, or sendrecv.  we're using the latter for now.
              if (recvContig) {
                vm->sendrecv(sendBuffer, sendBufferSize, theirPET,
                                 recvBuffer, recvBufferSize, theirPET);
              } else {
                // otherwise, the send buffer is a copy of what needs to be
                // unpacked; no data movement is needed before unpacking.
                recvBuffer = sendBuffer;
              }
            } else {
              // myPET != theirPET 
              // now move the data 
              vm->sendrecv(sendBuffer, sendBufferSize, theirPET,
                               recvBuffer, recvBufferSize, theirPET);
            }
           

            // now if the receive buffer is not contig, we still need to
            //  unpack the receive buffer
            if ((recvBufferSize > 0) && (!recvContig)) {
              rc = ESMC_XPacketUnpackBuffer(1, &recvXP, dk, nbytes, numAddrs,
                                            recvBuffer, recvAddr);
            }
 
            // free buffers if allocated
            if (madeSendBuf)
              delete [] sendBuffer;
            if (madeRecvBuf)
              delete [] recvBuffer;

          }      // XP loop
	}        // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is MPI_type_vector packing
// -----------------------------------------------------------

        if (useOptions & ESMC_ROUTE_OPTION_PACK_VECTOR) {
           // if you wanted to send multiple XPs with one communication call 
           // using the MPI-Vector equivalent, you would need code here which
           // could take an array of VMTypes instead of a single one.

           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                 "Route option PACK_VECTOR not supported yet", &rc);

        }         // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is no packing
// -----------------------------------------------------------

        // Next is no packing, which means that each contiguous chunk of data
        // described by an XP is sent as a separate communication.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_NOPACK) {

         // loop over the addresses, doing the send/recv loops for each one
         for (n=0; n<numAddrs; n++) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            // load up the corresponding send/recv xpackets from the rtables
            if (ixs < sendXPCount) {
              rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
              rc = sendXP->ESMC_XPacketGet(&sendRank, &sendOffset,
                                           &sendContigLength, sendStride,
                                           sendRepCount, &sendBufferIndex);
              sendContigRank = sendXP->ESMC_XPacketGetContigRank();
            } else {
              sendXP = NULL;
              sendRank = ESMF_MAXDIM;
              ESMC_XPacketGetEmpty(&sendRank, &sendOffset,
                                   &sendContigLength, sendStride,
                                   sendRepCount, &sendBufferIndex);
              sendContigRank = 1;
            }

            if (ixr < recvXPCount) {
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              rc = recvXP->ESMC_XPacketGet(&recvRank, &recvOffset, 
                                           &recvContigLength, recvStride,
                                           recvRepCount, &recvBufferIndex);
              recvContigRank = recvXP->ESMC_XPacketGetContigRank();
            } else {
              recvXP = NULL;
              recvRank = ESMF_MAXDIM;
              ESMC_XPacketGetEmpty(&recvRank, &recvOffset,
                                   &recvContigLength, recvStride,
                                   recvRepCount, &recvBufferIndex);
              recvContigRank = 1;
            }

#if 1
            // default version: all loops collapsed into a single master loop
            // simpler code, but perhaps harder for the optimizer to handle.

            // TODO: the XPs now have a contigRank value which says up to which
            // rank the slabs are contig; so if we want to minimize the number
            // of communication calls (at the cost of more complicated code), 
            // we can multiple the contig length times the rep_count[n] up to
            // n < contigRank-1, and then start the loop which computes the
            // total number of reps (send/recv calls) at contigRank-1 instead 
            // of at 0.
            
            sendReps = 1; 
            for (k=sendContigRank-1; k<sendRank-1; k++)
                sendReps *= sendRepCount[k];
            
            recvReps = 1; 
            for (k=recvContigRank-1; k<recvRank-1; k++)
                recvReps *= recvRepCount[k];

            maxReps = MAX(sendReps, recvReps);
            
            sendItemPtr = sendOffset; 
            sendBufferSize = sendContigLength*nbytes;
            for (k=0; k<sendContigRank-1; k++) 
                sendBufferSize *= sendRepCount[k];

            recvItemPtr = recvOffset; 
            recvBufferSize = recvContigLength*nbytes;
            for (k=0; k<recvContigRank-1; k++) 
                recvBufferSize *= recvRepCount[k];

            for (k=0; k<sendRank-1; k++) 
                sendIndex[k] = 0;
            for (k=0; k<recvRank-1; k++) 
                recvIndex[k] = 0;

            for (k=0; k<maxReps; k++) {
                if (k < sendReps) {
                  // set up sendbuf 
                  sendBuffer = (char *)sendAddr[n] + (sendItemPtr*nbytes);
                } else if (k == sendReps) {
                  sendBuffer = NULL;
                  sendBufferSize = 0;
                } 

                if (k < recvReps) {
                  // set up recvbuf 
                  recvBuffer = (char *)recvAddr[n] + (recvItemPtr*nbytes);
                } else if (k == recvReps) {
                  recvBuffer = NULL;
                  recvBufferSize = 0;
                } 


                // time to exchange data
                // if myPET == theirPET, sendrecv should use memcpy instead of
                // calling real communication routines.
                vm->sendrecv(sendBuffer, sendBufferSize, theirPET,
                                 recvBuffer, recvBufferSize, theirPET);


                if (k < sendReps) {
                    sendIndex[0]++;
                    sendItemPtr += sendStride[0];
                    for (j=0; (j<sendRank-2) && (sendIndex[j]>=sendRepCount[j]);
                         j++) {
                        sendIndex[j] = 0;
                        sendIndex[j+1]++;
                        sendItemPtr -= (sendRepCount[j]*sendStride[j]);
                        sendItemPtr += sendStride[j+1];
                    }
                }
                if (k < recvReps) {
                    recvIndex[0]++;
                    recvItemPtr += recvStride[0];
                    for (j=0; (j<recvRank-2) && (recvIndex[j]>=recvRepCount[j]);
                         j++) {
                        recvIndex[j] = 0;
                        recvIndex[j+1]++;
                        recvItemPtr -= (recvRepCount[j]*recvStride[j]);
                        recvItemPtr += recvStride[j+1];
                    }
                }

            }      // end of k loop
#else
            // experimental version: the inner loop is pulled out so the
            // compiler has an opportunity to optimize it.   this one does
            // not include the code which knows whether some of the ranks
            // are contig.

            sendReps = 1; 
            for (k=1; k<sendRank-1; k++) {
                sendReps *= sendRepCount[k];
                sendIndex[k] = 0;
            }
            recvReps = 1; 
            for (k=1; k<recvRank-1; k++) {   // difference: start with 1
                recvReps *= recvRepCount[k];
                recvIndex[k] = 0;
            }
            maxReps = MAX(sendReps, recvReps);

            sendItemPtr = sendOffset; 
            recvItemPtr = recvOffset; 
            sendBufferSize = sendContigLength*nbytes;
            recvBufferSize = recvContigLength*nbytes;

            for (k=0; k<maxReps; k++) {
                // pull out the inner loop and make it explicit here.
                for (l=0; l<sendRepCount[0] || l<recvRepCount[0]; l++) {
     
                     // if we have either data left to send and nothing to
                     // receive, or visa versa set the no-op by setting
                     // the buffer sizes to 0.  otherwise, set the pointer
                     // and size to the start of this transfer.
                     if (l>=sendRepCount[0]) {
                       sendBuffer = NULL;
                       sendBufferSize = 0;
                     } else {
                       sendBuffer = (char *)sendAddr[n]+(sendItemPtr*nbytes);
                       sendBufferSize = sendContigLength*nbytes;
                       sendItemPtr += sendStride[0];
                     }
                     if (l>=recvRepCount[0]) {
                       recvBuffer = NULL;
                       recvBufferSize = 0;
                     } else {
                       recvBuffer = (char *)recvAddr[n]+(recvItemPtr*nbytes);
                       recvBufferSize = recvContigLength*nbytes;
                       recvItemPtr += recvStride[0];
                     }
     
                     // time to exchange data
                     // if myPET == theirPET, sendrecv should use memcpy 
                     // instead of calling real communication routines.
                     vm->sendrecv(sendBuffer, sendBufferSize, theirPET,
                                      recvBuffer, recvBufferSize, theirPET);
     
                   }    // l loop


                if (k < sendReps) {
                    sendIndex[1]++;
                    sendItemPtr += sendStride[1];
                    for (j=1; j<sendRank-2; j++) {
                        if (sendIndex[j] >= sendRepCount[j]) {
                            sendIndex[j] = 0;
                            sendIndex[j+1]++;
                            sendItemPtr -= (sendRepCount[j]*sendStride[j]);
                            sendItemPtr += sendStride[j+1];
                        }
                    }
                }
                if (k < recvReps) {
                    recvIndex[1]++;
                    recvItemPtr += recvStride[1];
                    for (j=1; j<recvRank-2; j++) {
                        if (recvIndex[j] >= recvRepCount[j]) {
                            recvIndex[j] = 0;
                            recvIndex[j+1]++;
                            recvItemPtr -= (recvRepCount[j]*recvStride[j]);
                            recvItemPtr += recvStride[j+1];
                        }
                    }
                }

            }      // end of k loop
#endif
          }        // XP loop
         }         // address loop
        }          // packing branch

        // reset the useOptions to the default in case it has been overwritten
        useOptions = options;

      }            // communication (PET) loop, variable i
    }              // SYNC branch

// -----------------------------------------------------------
// Now the BIG branch for asynchronous communications
// -----------------------------------------------------------

    if (useOptions & ESMC_ROUTE_OPTION_ASYNC) {

      // this section computes how many possible outstanding communications
      // request we are going to generate, which is different for each 
      // packing type.   after this section we allocate the handles,
      // pack the data, and initiate all communications.  then we go back
      // and loop again, waiting for the communication to be confirmed as
      // complete, and unpack if needed.

      myPET = vm->getMypet();
      rc = ct->ESMC_CommTableGetCount(&commCount);

      sendXPCount = 0;
      recvXPCount = 0;

      // if we are packing by pet, all data going to or coming from any
      // other pet is accomplished in a single communication, so the number
      // of handles is simply the number of PETs in the comm table.
      if (useOptions & ESMC_ROUTE_OPTION_PACK_PET) {
        maxReqCount = commCount;
      }
  
      // if we are packing by XP, then we loop up to the maximum count of
      // either send or receive by PETs.  This logic has been moved into
      // a rather specialized counting method.
      if (useOptions & ESMC_ROUTE_OPTION_PACK_XP) {
        rc = ESMC_RouteGetSumMaxXPsPerPET(&maxReqCount);
      }

      // TODO: not implemented yet, but here is where we could create a
      // strided handle which describes the layout in memory and does
      // no actual packing before calling the communication routines.
      if (useOptions & ESMC_ROUTE_OPTION_PACK_VECTOR) {
          // TODO: use same code as PACK_XP case for number of possible
          // outstanding requests possible?
          maxReqCount = 0;
      }

      // if we are not packing at all, each non-contiguous region described
      // by each XP must be sent individually.  This logic has been moved
      // into a rather specialized counting method.
      if (useOptions & ESMC_ROUTE_OPTION_PACK_NOPACK) {
        rc = ESMC_RouteGetSumMaxRegionsPerXP(&maxReqCount);
        maxReqCount *= numAddrs;
      }

      // the maxReqCount value has been computed based on the assumption 
      // that a single sendRecv call will handle one send and one receive
      // per outstanding request handle.  if these are broken up into separate
      // sends and receives, each with their own handle, this number can be
      // up to twice the size (which is a safe upper bound) but should probably
      // be computed differently above.

      // do a simple sanity check in case the value is complete out of bounds.
      // (this count is going to be used below for an allocation, which can
      //  be confusing if it fails because of an invalid value.)
      if ((maxReqCount < 0) || (maxReqCount > (1<<24))) {
        sprintf(msgbuf, "computed #reqs too small or large: %d\n", maxReqCount);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
        return (rc);
      }

      // if the total number of possible outstanding communications requests
      // is less than the limit, use local stack buffers.  if it is larger,
      // allocate off the heap to avoid stack size problems.
      if (maxReqCount <= STACKLIMIT) {
        handle = stackHandle;
        sendBufferList = stackSendBuffer;
        recvBufferList = stackRecvBuffer;
      } else {
        handle = new ESMCI::VMK::commhandle*[maxReqCount];
        sendBufferList = new char*[maxReqCount];
        recvBufferList = new char*[maxReqCount];
      }

      // allocate any necessary arrays for specific options
      if (useOptions & ESMC_ROUTE_OPTION_PACK_XP) {
        madeSendBufList = new bool[maxReqCount];
        madeRecvBufList = new bool[maxReqCount];
      } else if (useOptions & ESMC_ROUTE_OPTION_PACK_PET) {
        // these might not be used, but in the case where we change packing
        // strategies on the fly, allocate enough to cover that case.
        madeSendBufList = new bool[commCount];
        madeRecvBufList = new bool[commCount];
      } else {
        madeSendBufList = madeRecvBufList = NULL;
      }
      
// -----------------------------------------------------------------------
// For asynchronous communications we have two separate loops:
//  loop #1: issue non-blocking sendrecv() calls
//  loop #2: wait for local non-blocking comms to finish
// --> loop #1 follows:
// -----------------------------------------------------------------------
      
      // reset request counter
      int req=0;

      // loop over each destination in the comm table
      for (i=0; i<commCount; i++) {

        // Find out theirPET id for this entry in the comm table.  If it's not
        // needed then no communication is necessary -- go to the next entry.
        rc = ct->ESMC_CommTableGetPartner(i, &theirPET, &needed);
        if (!needed) continue;

        // some sanity checking to be sure we have enough async buffers
        if (req > maxReqCount) {
          sprintf(msgbuf, "not enough async bufs; have %d and index now %d (must be < %d)\n",
                           maxReqCount, req, maxReqCount);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
          return (rc);
        }

        // find number of xpackets to be communicated with this PET
	rc = recvRT->ESMC_RTableGetCount(theirPET, &recvXPCount);
	rc = sendRT->ESMC_RTableGetCount(theirPET, &sendXPCount);
        maxXPCount = MAX(recvXPCount, sendXPCount);

// -----------------------------------------------------------
// Branch again, this time based on packing option
// First up is packing by PET
// -----------------------------------------------------------

        // reset options only for packing by PET and if the maximum XPCount is 1
        if ((useOptions & ESMC_ROUTE_OPTION_PACK_PET) && (maxXPCount == 1)) {
          useOptions = (ESMC_RouteOptions)(useOptions ^ ESMC_ROUTE_OPTION_PACK_PET);
          useOptions = (ESMC_RouteOptions)(useOptions | ESMC_ROUTE_OPTION_PACK_XP);
        }

        // First up is packing on a PET-level, where all the data that must be
        // exchanged between PETs is loaded up into single buffers for exchange
        if (useOptions & ESMC_ROUTE_OPTION_PACK_PET) {

          // uncomment for profiling
          // wtime(&starttime);
          // get corresponding send/recv xpackets from the rtable
          if (sendXPCount > 0)
            sendXPList = new ESMC_XPacket*[sendXPCount];
          else
            sendXPList = NULL;
          if (recvXPCount > 0)
            recvXPList = new ESMC_XPacket*[recvXPCount];
          else
            recvXPList = NULL;

          // fill the array of pointers so they point to the XPs for this PET
          for (ixs=0; ixs<sendXPCount; ixs++){
            rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXPList[ixs]);
          }
          for (ixr=0; ixr<recvXPCount; ixr++){
            rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXPList[ixr]);
          }

          // create the buffer to hold the sending data, and pack it 
          rc = ESMC_XPacketMakeBuffer(sendXPCount, sendXPList, nbytes, numAddrs,
                                      &sendBufferList[req], &sendBufferSize);
          rc = ESMC_XPacketPackBuffer(sendXPCount, sendXPList, dk, nbytes,
            numAddrs, sendAddr, sendBufferList[req]);

          // uncomment for profiling
          // wtime(&time);
          // timer1 += time-starttime;

          // if my PET is both the sender and receiver, there is no need
          // to allocate a separate buffer; just point both at the single
          // send buffer.
          handle[req] = NULL;
          if (myPET == theirPET) {
            recvBufferList[req] = sendBufferList[req];
          } else {
            // time to exchange data
            // create the buffer to hold the receiving data
            rc = ESMC_XPacketMakeBuffer(recvXPCount, recvXPList, nbytes,
              numAddrs, &recvBufferList[req], &recvBufferSize);

            // uncomment for profiling
            // wtime(&starttime);
            vm->sendrecv(sendBufferList[req], sendBufferSize, theirPET,
                             recvBufferList[req], recvBufferSize, theirPET,
                             &handle[req]);
            // uncomment for profiling
            // wtime(&time);
            // timer2 += time-starttime;
          }

          req++;

          // delete the lists of pointers, plus the local packing buffers
          // allocated during this loop.
          delete [] sendXPList;
          delete [] recvXPList;

	}       // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is packing by XP
// -----------------------------------------------------------

        // Next is packing on an XP-level, where all the data that must be
        // exchanged between PETs loops over all the XPs that must be exchanged,
        // communicating them one at a time.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_XP) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            madeSendBufList[req] = madeRecvBufList[req] = false;

            // load up the corresponding send/recv xpackets from the rtables
            if (ixs < sendXPCount) {
              rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
              rc = sendXP->ESMC_XPacketGet(&sendRank, &sendOffset,
                                           &sendContigLength, sendStride,
                                           sendRepCount, &sendBufferIndex);

              // test contiguity of xpacket data 
              // if sending data is contiguous, set the pointer into the data;
              // otherwise create a buffer and pack it if necessary
              if ((numAddrs == 1) && sendXP->ESMC_XPacketIsContig()) {
                  sendContig = true;
                  sendBufferList[req] = (char *)sendAddr[0]+(sendOffset*nbytes); 
                  sendBufferSize = sendContigLength * nbytes;
                  for (k=0; k<sendRank-1; k++)
                      sendBufferSize *= sendRepCount[k];
              } else {
                  sendContig = false;
                  rc = ESMC_XPacketMakeBuffer(1, &sendXP, nbytes, numAddrs,
                    &sendBufferList[req], &sendBufferSize);
                  rc = ESMC_XPacketPackBuffer(1, &sendXP, dk, nbytes, numAddrs,
                                              sendAddr, sendBufferList[req]);
                  madeSendBufList[req] = true;
              }

            } else {  // nothing to more send, but data will be received
              sendBufferList[req] = NULL;
              sendBufferSize = 0;
            }

            if (ixr < recvXPCount) {
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              rc = recvXP->ESMC_XPacketGet(&recvRank, &recvOffset, 
                                           &recvContigLength, recvStride,
                                           recvRepCount, &recvBufferIndex);

              // test contiguity of xpacket data 
              // the receive buffer is a bit more complicated - it depends both
              // on whether the receive buffer is contig and also if the sender
              // and receiver are the same PET.
              if ((numAddrs == 1) && recvXP->ESMC_XPacketIsContig()) {
                  recvContig = true;
                  recvBufferList[req] = (char *)recvAddr[0]+(recvOffset*nbytes); 
                  recvBufferSize = recvContigLength * nbytes;
                  for (k=0; k<recvRank-1; k++)
                      recvBufferSize *= recvRepCount[k];
              } else {
                  recvContig = false;
                  // make a separate receive buffer if necessary (if the data
                  // isn't contig and can't be moved directly to where it will
                  // finally need to be.
                  if (myPET != theirPET) {
                      rc = ESMC_XPacketMakeBuffer(1, &recvXP, nbytes, numAddrs,
                                                  &recvBufferList[req], &recvBufferSize);
                      madeRecvBufList[req] = true;
                  } else
                      recvBufferSize = sendBufferSize;
              }

            } else {   // nothing more to receive, but data will be sent
              recvBufferList[req] = NULL;
              recvBufferSize = 0;
            }

            // the case where we are moving data around in the same PET 
            handle[req] = NULL;
            if (myPET == theirPET) {
              // if the receive buffer is contig, you can move the data directly
              // to where it belongs.  this could be done with memcpy,
              // unpack, or sendrecv.  we're using the latter for now.
              if (recvContig) {
                vm->sendrecv(sendBufferList[req], sendBufferSize, theirPET,
                                 recvBufferList[req], recvBufferSize, theirPET);
              } else {
                // otherwise, the send buffer is a copy of what needs to be
                // unpacked; no data movement is needed before unpacking.
                recvBufferList[req] = sendBufferList[req];
              }
            } else {
              // myPET != theirPET 
              // now move the data 
              vm->sendrecv(sendBufferList[req], sendBufferSize, theirPET,
                               recvBufferList[req], recvBufferSize, theirPET,
                               &handle[req]);
            }
            req ++;  

          }      // XP loop
	}        // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is MPI_type_vector packing
// -----------------------------------------------------------

        if (useOptions & ESMC_ROUTE_OPTION_PACK_VECTOR) {
           // if you wanted to send multiple XPs with one communication call 
           // using the MPI-Vector equivalent, you would need code here which
           // could take an array of VMTypes instead of a single one.

           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                 "Route option PACK_VECTOR not supported yet", &rc);

        }         // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is no packing
// -----------------------------------------------------------

        // Next is no packing, which means that each contiguous chunk of data
        // described by an XP is sent as a separate communication.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_NOPACK) {

         // loop over the addresses, doing the send/recv loops for each one
         for (n=0; n<numAddrs; n++) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            // load up the corresponding send/recv xpackets from the rtables
            if (ixs < sendXPCount) {
              rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
              rc = sendXP->ESMC_XPacketGet(&sendRank, &sendOffset,
                                           &sendContigLength, sendStride,
                                           sendRepCount, &sendBufferIndex);
              sendContigRank = sendXP->ESMC_XPacketGetContigRank();
            } else {
              sendXP = NULL;
              sendRank = ESMF_MAXDIM;
              ESMC_XPacketGetEmpty(&sendRank, &sendOffset,
                                   &sendContigLength, sendStride,
                                   sendRepCount, &sendBufferIndex);
              sendContigRank = 1;
            }

            if (ixr < recvXPCount) {
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              rc = recvXP->ESMC_XPacketGet(&recvRank, &recvOffset, 
                                           &recvContigLength, recvStride,
                                           recvRepCount, &recvBufferIndex);
              recvContigRank = recvXP->ESMC_XPacketGetContigRank();
            } else {
              recvXP = NULL;
              recvRank = ESMF_MAXDIM;
              ESMC_XPacketGetEmpty(&recvRank, &recvOffset,
                                   &recvContigLength, recvStride,
                                   recvRepCount, &recvBufferIndex);
              recvContigRank = 1;
            }

            // default version: all loops collapsed into a single master loop
            // simpler code, but perhaps harder for the optimizer to handle.

            // TODO: the XPs now have a contigRank value which says up to which
            // rank the slabs are contig; so if we want to minimize the number
            // of communication calls (at the cost of more complicated code), 
            // we can multiple the contig length times the rep_count[n] up to
            // n < contigRank-1, and then start the loop which computes the
            // total number of reps (send/recv calls) at contigRank-1 instead 
            // of at 0.
            
            sendReps = 1; 
            for (k=sendContigRank-1; k<sendRank-1; k++)
                sendReps *= sendRepCount[k];
            
            recvReps = 1; 
            for (k=recvContigRank-1; k<recvRank-1; k++)
                recvReps *= recvRepCount[k];

            maxReps = MAX(sendReps, recvReps);
            
            sendItemPtr = sendOffset; 
            sendBufferSize = sendContigLength*nbytes;
            for (k=0; k<sendContigRank-1; k++) 
                sendBufferSize *= sendRepCount[k];

            recvItemPtr = recvOffset; 
            recvBufferSize = recvContigLength*nbytes;
            for (k=0; k<recvContigRank-1; k++) 
                recvBufferSize *= recvRepCount[k];

            for (k=0; k<sendRank-1; k++) 
                sendIndex[k] = 0;
            for (k=0; k<recvRank-1; k++) 
                recvIndex[k] = 0;

            for (k=0; k<maxReps; k++) {
                if (k < sendReps) {
                  // set up sendbuf 
                  sendBufferList[req] = (char *)sendAddr[n] +
                                                          (sendItemPtr*nbytes);
                } else if (k == sendReps) {
                  sendBufferList[req] = NULL;
                  sendBufferSize = 0;
                } 

                if (k < recvReps) {
                  // set up recvbuf 
                  recvBufferList[req] = (char *)recvAddr[n] +
                                                          (recvItemPtr*nbytes);
                } else if (k == recvReps) {
                  recvBufferList[req] = NULL;
                  recvBufferSize = 0;
                } 


                // time to exchange data
                handle[req] = NULL;
                if (myPET == theirPET) {
                  vm->sendrecv(sendBufferList[req], sendBufferSize, theirPET,
                               recvBufferList[req], recvBufferSize, theirPET);
                } else {
                  vm->sendrecv(sendBufferList[req], sendBufferSize, theirPET,
                               recvBufferList[req], recvBufferSize, theirPET,
                               &handle[req]);
                }
                req ++;

                if (k < sendReps) {
                    sendIndex[0]++;
                    sendItemPtr += sendStride[0];
                    for (j=0; (j<sendRank-2) && (sendIndex[j]>=sendRepCount[j]);
                         j++) {
                        sendIndex[j] = 0;
                        sendIndex[j+1]++;
                        sendItemPtr -= (sendRepCount[j]*sendStride[j]);
                        sendItemPtr += sendStride[j+1];
                    }
                }
                if (k < recvReps) {
                    recvIndex[0]++;
                    recvItemPtr += recvStride[0];
                    for (j=0; (j<recvRank-2) && (recvIndex[j]>=recvRepCount[j]);
                         j++) {
                        recvIndex[j] = 0;
                        recvIndex[j+1]++;
                        recvItemPtr -= (recvRepCount[j]*recvStride[j]);
                        recvItemPtr += recvStride[j+1];
                    }
                }

            }      // end of k loop
          }        // XP loop
         }         // address loop
        }          // packing branch

        // reset the useOptions to the default in case it has been overwritten
        useOptions = options;

      }            // communication (PET) loop, variable i

// -----------------------------------------------------------------------
// For asynchronous communications we have two separate loops:
//  loop #1: issue non-blocking sendrecv() calls
//  loop #2: wait for local non-blocking comms to finish
// --> loop #2 follows:
// -----------------------------------------------------------------------

      // reset request counter
      req=0;

      // loop over each destination in the comm table
      for (i=0; i<commCount; i++) {

        // Find out theirPET id for this entry in the comm table.  If it's not
        // needed then no communication is necessary -- go to the next entry.
        rc = ct->ESMC_CommTableGetPartner(i, &theirPET, &needed);
        if (!needed) continue;

        // find total number of xpackets
	rc = recvRT->ESMC_RTableGetCount(theirPET, &recvXPCount);
	rc = sendRT->ESMC_RTableGetCount(theirPET, &sendXPCount);
        maxXPCount = MAX(recvXPCount, sendXPCount);

// -----------------------------------------------------------
// Branch again, this time based on packing option
// First up is packing by PET
// -----------------------------------------------------------

        // reset options only for packing by PET and if the maximum XPCount is 1
        if ((useOptions & ESMC_ROUTE_OPTION_PACK_PET) && (maxXPCount == 1)) {
          useOptions = (ESMC_RouteOptions)
            (useOptions ^ ESMC_ROUTE_OPTION_PACK_PET);
          useOptions = (ESMC_RouteOptions)
            (useOptions | ESMC_ROUTE_OPTION_PACK_XP);
        }

        // First up is packing on a PET-level, where all the data that must be
        // exchanged between PETs is loaded up into single buffers for exchange
        if (useOptions & ESMC_ROUTE_OPTION_PACK_PET) {
          // uncomment for profiling
          // wtime(&starttime);
          // get corresponding send/recv xpackets from the rtable
          if (recvXPCount > 0)
            recvXPList = new ESMC_XPacket*[recvXPCount];
          else
            recvXPList = NULL;

          // fill the array of pointers so they point to the XPs for this PET
          for (ixr=0; ixr<recvXPCount; ixr++){
            rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXPList[ixr]);
          }

          // if my PET is both the sender and receiver, we did an immediate
          // copy - nothing to wait for.  otherwise, call the wait routine 
          // to be sure the communication has completed.
          if (myPET != theirPET) {
            vm->commwait(&handle[req]);
          }
          // uncomment for profiling
          // wtime(&time);
          // timer2 += time-starttime;
          // starttime = time;

          // whether myPET = theirPET or not, we still have to unpack the 
          // receive buffer to move the data into the final location.
          rc = ESMC_XPacketUnpackBuffer(recvXPCount, recvXPList, dk, nbytes,
                                       numAddrs, recvBufferList[req], recvAddr);

          // uncomment for profiling
          // wtime(&time);
          // timer3 += time-starttime;
          
          // delete the individual buffers for this transfer
          delete [] recvXPList;
          delete [] sendBufferList[req];
          if (myPET != theirPET) 
              delete [] recvBufferList[req];

          req++;

        }

// -----------------------------------------------------------
// Branching still on packing option
// Next up is packing by XP
// -----------------------------------------------------------

        // Next is packing on an XP-level, where all the data that must be
        // exchanged between PETs loops over all the XPs that must be exchanged,
        // communicating them one at a time.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_XP) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            // load up the corresponding recv xpackets from the rtables
            if (ixr < recvXPCount) {

              // return whether XP describes a completely contig region
              // of memory or not.  if yes, no unpacking is needed; the 
              // data was transferred directly to the final destination;
              // if no, then we have made a separate buffer, transferred
              // the data there, and we need to copy it where it needs to go.
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              recvContig = recvXP->ESMC_XPacketIsContig() && (numAddrs == 1);

            } else {
              // in this case we do not want have a buffer to be unpacked, so
              // lie and say that empty data is contig.  slightly strange, yes.
              // but below it successfully avoids the unpack buffer code.
              recvContig = true; 
            }

            // time to retrieve data -- only necessary if myPET != theirPET
            // this has to be checked whether we're sending or receiving,
            // to be sure the vm has received the completed transfer and
            // removed this handle from its pending request list.
            // (if the pet numbers were the same, we did an immediate copy and
            // didn't start an async communication, so nothing to wait for.)
            if (myPET != theirPET) {
              vm->commwait(&handle[req]);
            }
           
            // if the receive buffer is not contig, we still need to unpack
            // the data from the receive buffer into the final location.
            if (!recvContig) {
              rc = ESMC_XPacketUnpackBuffer(1, &recvXP, dk, nbytes, numAddrs,
                                            recvBufferList[req], recvAddr);
            }

            // free buffers if allocated
            if (madeSendBufList[req])
              delete [] sendBufferList[req];
            if (madeRecvBufList[req])
              delete [] recvBufferList[req];

            req++;
 
          }      // XP loop
	}        // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is MPI_type_vector packing
// -----------------------------------------------------------

        if (useOptions & ESMC_ROUTE_OPTION_PACK_VECTOR) {
           // if you wanted to send multiple XPs with one communication call 
           // using the MPI-Vector equivalent, you would need code here which
           // could take an array of VMTypes instead of a single one.

           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                 "Route option PACK_VECTOR not supported yet", &rc);

        }         // packing branch

// -----------------------------------------------------------
// Branching still on packing option
// Next up is no packing
// -----------------------------------------------------------

        // Next is no packing, which means that each contiguous chunk of data
        // described by an XP is sent as a separate communication.
        if (useOptions & ESMC_ROUTE_OPTION_PACK_NOPACK) {

         // loop over the addresses, doing the send/recv loops for each one
         for (n=0; n<numAddrs; n++) {

          // loop over the XPs
          for (m=0, ixs=0, ixr=0; m<maxXPCount; m++, ixs++, ixr++){

            // load up the corresponding send/recv xpackets from the rtables
            if (ixs < sendXPCount) {
              rc = sendRT->ESMC_RTableGetEntry(theirPET, ixs, &sendXP);
              rc = sendXP->ESMC_XPacketGet(&sendRank, NULL, NULL, NULL, 
                                           sendRepCount, NULL);
              sendContigRank = sendXP->ESMC_XPacketGetContigRank();

              sendReps = 1;
              for (k=sendContigRank-1; k<sendRank-1; k++)
                  sendReps *= sendRepCount[k];

            } else {
              sendReps = 0;
            }

            if (ixr < recvXPCount) {
              rc = recvRT->ESMC_RTableGetEntry(theirPET, ixr, &recvXP);
              rc = recvXP->ESMC_XPacketGet(&recvRank, NULL, NULL, NULL, 
                                           recvRepCount, NULL);
              recvContigRank = recvXP->ESMC_XPacketGetContigRank();
 
              recvReps = 1;
              for (k=recvContigRank-1; k<recvRank-1; k++)
                  recvReps *= recvRepCount[k];

            } else {
              recvReps = 0;
            }

            maxReps = MAX(sendReps, recvReps);

            for (l=0; l<maxReps; l++) {
         
              // wait to be sure communication has finished.
              // only necessary if myPET != theirPET, because for same-PET
              // transfers we do an immediate operation and not async.
              if (myPET != theirPET) 
                vm->commwait(&handle[req]);

              req++;

            }      // l loop
          }        // XP loop
         }         // address loop
        }          // packing branch

        // reset the useOptions to the default in case it has been overwritten
        useOptions = options;

      }            // communication (PET) loop, variable i

      // if we are not using the local stack buffers, free these. 
      if (maxReqCount > STACKLIMIT) {
          delete [] handle; 
          delete [] sendBufferList;
          delete [] recvBufferList;
      }
      // free any necessary arrays for specific options
      if ((useOptions & ESMC_ROUTE_OPTION_PACK_XP) ||
          (useOptions & ESMC_ROUTE_OPTION_PACK_PET)) {
        delete [] madeSendBufList;
        delete [] madeRecvBufList;
      }

    }              // ASYNC branch


    //printf("End of Route run on PET %d\n", mypet);
    return rc;

 } // end ESMC_RouteRun


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeHalo"
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
      ESMCI::DELayout *delayout,   // in  - pointer to the DELayout 
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
    ESMC_Logical boundary[ESMF_MAXIGRIDDIM][2];
    int nde[ESMF_MAXIGRIDDIM], DEpos[ESMF_MAXIGRIDDIM];
    int my_global_start[ESMF_MAXDIM], my_start[ESMF_MAXDIM];
    int my_XPcount, their_XPcount;
    int rc; 
    int i, j, k, nextxp, start;
    int their_de, decount, dummy;
    int theirMatchingPET;

    // Calculate the sending table.  If this DE is not part of the sending
    // TODO: this assumes a 2D layout?  (certainly < 3D)
    delayout->getDeprecated(&decount, NULL, NULL, NULL, 0, NULL, NULL, NULL,
      nde, ESMF_MAXIGRIDDIM);

    // Calculate the sending table.
 
    // get "my" AI out of the AI_exc array
    // TODO:  this is NOT going to work for data dims which are not
    //  equal the igrid dims, e.g. a 2d igrid with 4d data.
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
      delayout->getDELocalInfo(their_de, DEpos, ESMF_MAXIGRIDDIM, NULL, 0, NULL,
        0, NULL, NULL);

      // get the actual pet number for use later on.
      delayout->getDEMatchPET(their_de, *vm, NULL, &theirMatchingPET, 1);
      
      // get "their" AI out of the AI_tot array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_tot[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      for (j=0; j<rank; j++) {
        boundary[j][0] = ESMF_FALSE;
        boundary[j][1] = ESMF_FALSE;
        if (periodic[j]==ESMF_TRUE) {
          if (DEpos[j] == 0) 
            boundary[j][0] = ESMF_TRUE;
          if (DEpos[j] == nde[j]-1) 
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
        if (intersect_XP.ESMC_XPacketIsEmpty()) {
          continue;
        }

#if 0
        // debug
        printf("in halo, ready to translate global to local, AI tot:\n");
        for (int l=0; l<ESMF_MAXDIM; l++)
            ESMC_AxisIndexPrint(&my_AI_tot[l]);
#endif
  
        // translate from global to local data space
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                               rank, my_global_start);

#if 0
        // debug
        printf("in halo, xp:\n");
        intersect_XP.ESMC_XPacketPrint("");
#endif

        // load the intersecting XPacket into the sending RTable
        sendRT->ESMC_RTableSetEntry(theirMatchingPET, &intersect_XP);
        ct->ESMC_CommTableSetPartner(theirMatchingPET);
      }
  
      // free XPs allocated by XPacketFromAxisIndex() routine above
      delete [] their_XP;
    }

    // free the old exclusive my_XP before computing the total my_XP
    delete [] my_XP;

    // Calculate the receiving table.
 
    // figure out my boundary array
    delayout->getDELocalInfo(my_DE, DEpos, ESMF_MAXIGRIDDIM, NULL, 0, NULL, 0,
      NULL, NULL);
    for (j=0; j<rank; j++) {
      boundary[j][0] = ESMF_FALSE;
      boundary[j][1] = ESMF_FALSE;
      if (periodic[j] == ESMF_TRUE) {
        if (DEpos[j] == 0) 
          boundary[j][0] = ESMF_TRUE;
        if (DEpos[j] == nde[j]-1) 
          boundary[j][1] = ESMF_TRUE;
      }
    }

    rc = ESMC_XPacketFromAxisIndex(my_AI_tot, rank, global_count, boundary,
                                   &my_XP, &my_XPcount);

    // calculate my_Start for the different XPs
    int myStart[5][2];
    for (i=0; i<my_XPcount; i++) {
      myStart[i][0] = my_global_start[0];
      myStart[i][1] = my_global_start[1];
    }

    // there's always 1, which is OK with the default setting
    nextxp = 0;

    // if periodic along the first axis and this piece along boundary:
    for (i=0; i<rank; i++) {
      if (boundary && (boundary[i][0] == ESMF_TRUE)) {
        nextxp++;
        myStart[nextxp][i] = my_global_start[i] + global_count[i];
      }
      if (boundary && (boundary[i][1] == ESMF_TRUE)) {
        nextxp++;
        myStart[nextxp][i] = my_global_start[i] - global_count[i];
      }
    }

    // loop over DE's from layout to calculate receive table
    for (k=0; k<decount; k++) {
      their_de = k;

      // get actual PET number to put into route table
      delayout->getDEMatchPET(their_de, *vm, NULL, &theirMatchingPET, 1);
      
      // get "their" AI out of the AI_exc array
      for (j=0; j<rank; j++) {
        their_AI[j] = AI_exc[their_de + j*AI_count];
      }
 
      // calculate "their" XPacket in the sense of the global data
      rc = ESMC_XPacketFromAxisIndex(their_AI, rank, global_count, NULL, 
                                     &their_XP, &their_XPcount);

      // calculate the intersection
      start = 0;
      if (my_DE == their_de) start=1;
      for (i=start; i<my_XPcount; i++) {

        intersect_XP.ESMC_XPacketIntersect(&my_XP[i], &their_XP[0]);

        // if there's no intersection, no need to add an entry here
        if (intersect_XP.ESMC_XPacketIsEmpty()) 
          continue;
         
        // translate from global to local
        my_global_start[0] = myStart[i][0];
        my_global_start[1] = myStart[i][1];
        intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot,
                                               rank, my_global_start);

        // load the intersecting XPacket into the receiving RTable
        recvRT->ESMC_RTableSetEntry(theirMatchingPET, &intersect_XP);
        ct->ESMC_CommTableSetPartner(theirMatchingPET);
      }

      // free their_XP allocated in XPacketFromAxisIndex above.
      delete [] their_XP;
    }
    //ESMC_RoutePrint();
 
    // and delete the total my_XP
    delete [] my_XP;  


    return rc;

 } // end ESMC_RoutePrecomputeHalo


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeRedist"
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

    ESMC_Logical  hasSrcData,   // in  - has source data on this DE?
    ESMCI::DELayout *srcDELayout, // in  - pointer to the source DELayout
    int mySrcDE,                // in  - DE identifier in the source
    int srcDECount,             // in  - number of DEs in source
    ESMC_AxisIndex *srcGlobalCompAIperDEperRank,  // in  - per DE, per rank
    ESMC_AxisIndex *mySrcGlobalTotalAIperRank,    // in - 1 AI list for src
    //! TODO: this must be mySrcGlobalAllocAIperRank!!

    ESMC_Logical  hasDstData,   // in  - has destination data on this DE?
    ESMCI::DELayout *dstDELayout, // in  - pointer to the receive DELayout
    int myDstDE,                // in  - DE identifier in the destination
    int dstDECount,             // in  - number of DEs in destination
    ESMC_AxisIndex *dstGlobalCompAIperDEperRank,  // in  - per DE, per rank
    ESMC_AxisIndex *myDstGlobalTotalAIperRank) {  // in  - 1 AI list for dst
    //! TODO: ditto

//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    //! TODO:  this needs to be intersectLocalAllocAIperRank!!!
    ESMC_AxisIndex intersectGlobalCompAIperRank[ESMF_MAXDIM];
    ESMC_AxisIndex intersectLocalCompAIperRank[ESMF_MAXDIM];
    ESMC_AxisIndex intersectLocalTotalAIperRank[ESMF_MAXDIM];

    ESMC_AxisIndex mySrcGlobalCompAIperRank[ESMF_MAXDIM];
    ESMC_AxisIndex myDstGlobalCompAIperRank[ESMF_MAXDIM];
    ESMC_AxisIndex theirSrcGlobalCompAIperRank[ESMF_MAXDIM];
    ESMC_AxisIndex theirDstGlobalCompAIperRank[ESMF_MAXDIM];

    ESMC_XPacket intersectXP;
    int i, k, rc;
    int didsomething;
    int theirDE, theirMatchingPET;

    // set this here, because if neither send or recv are > 0 then we
    // do nothing here.
    rc = ESMF_SUCCESS;
    didsomething = 0;

    // TODO: 
    // start of precompute overhaul.   the proposed plan:
    // - the rank of the AIs is the same as the data rank (not igrid rank) 
    // - the AIs are already ordered to match the data dims (any reordering
    //    required by the datamap has already been applied)
    // - (0,0) is offset for computational region in local space
    // - loop thru theirAIglobal doing AI intersect in global space, making
    //   a new AI list in global space
    // - change to local (by subtracting global min for this DE) using 
    //   mySrcGlobalTotalAI (NOT comp here - so the AI is now relative to
    //   the total indexing and not comp indexing)
    // - call AI to XP now; needs simply intersect local AI.  if we use the
    //   strides to store the local total sizes (is this a good idea?) then
    //   we don't need any other args.  otherwise we need the local total 
    //   counts per rank.
    // - then add to route table as before.  (de to pet code remains, other
    //   code stays the same).
    //
    //  if AxisIndex object morphs into taking a rank and containing an
    //   array of mins, maxs, etc - then the "perRank" can go away.
    //  (or an array of old AIs if access is better that way).
    //
    // this should allow any dimensionality of data array to be supported.
    // the rewritten route code already handles up to 7d xpackets.
    //


    // Calculate the sending table.  If this DE is not part of the sending
    // layout skip this loop.
    if (hasSrcData == ESMF_TRUE) {
 
      // TODO: this isn't used anywhere - do we really care?  maybe for debug?
        didsomething++;
 
        // get myAIs here from srcGlobalCompAIperDEperRank
        for (k=0; k<rank; k++) {
          mySrcGlobalCompAIperRank[k] = 
            srcGlobalCompAIperDEperRank[mySrcDE + k*srcDECount];
        }

        // loop over DE's from receiving layout to calculate send table
        for (i=0; i<dstDECount; i++) {
            theirDE = i;
  
            // get the parent PET identifier for this DE in the recv layout
            // this code is in DE-space but the matching PET is used in the
            // routetable, which is by PETs
            dstDELayout->getDEMatchPET(theirDE, *vm, NULL,
              &theirMatchingPET, 1);
  
            // get "their" AI out of the dstAI array
            for (k=0; k<rank; k++) {
              theirDstGlobalCompAIperRank[k] = 
                   dstGlobalCompAIperDEperRank[theirDE + k*dstDECount];
            }
   
            // intersect the AI lists
            //  this routine needs to return 0 if no intersect, 1 if it did.
            if (!ESMC_AxisIndexIntersect(rank, 
                                         mySrcGlobalCompAIperRank,
                                         theirDstGlobalCompAIperRank,
                                         intersectGlobalCompAIperRank))
                continue;
  
            // TODO:  Total now bad; Alloc good
            //   This needs to be 2 parts:  part 1 is GlobalComp to LocalComp
            //   part 2 is LocalComp to LocalAlloc.

            // translate global comp AIs to local comp AIs
            rc = ESMC_AxisIndexGlobalToLocal(rank, 
                                             intersectGlobalCompAIperRank,
                                             mySrcGlobalCompAIperRank,
                                             intersectLocalCompAIperRank);
      
            // TODO: we think we don't need this now.
            //// translate local comp AIs to local total AIs
            //rc = ESMC_AxisIndexCompToTotal(rank, 
            //                               intersectLocalCompAIperRank, 
            //                               mySrcGlobalCompAIperRank,
            //                               mySrcGlobalTotalAIperRank,
            //                               intersectLocalTotalAIperRank);
      
            // then:
            // (assumes strides are set to the local total counts in each dim;
            //  otherwise need the local total counts as either an int array or
            //  in another AI list)
            rc = intersectXP.ESMC_XPacketFromCompAIs(rank, 
                                           intersectLocalCompAIperRank, 
                                           mySrcGlobalCompAIperRank,
                                           mySrcGlobalTotalAIperRank);

            // load the intersecting XPacket into the sending RTable
            sendRT->ESMC_RTableSetEntry(theirMatchingPET, &intersectXP);
            ct->ESMC_CommTableSetPartner(theirMatchingPET);
 
        }
    }

    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout skip this loop completely.
    if (hasDstData == ESMF_TRUE) {
 
        // TODO: this isn't used anywhere - do we really care?  maybe for debug?
        didsomething++;
 
        // get myAIs here from dstGlobalCompAIperDEperRank
        for (k=0; k<rank; k++) {
            myDstGlobalCompAIperRank[k] = 
              dstGlobalCompAIperDEperRank[myDstDE + k*dstDECount];
        }

        // loop over DE's from sending layout to calculate receive table
        for (i=0; i<srcDECount; i++) {
            theirDE = i;
  
            // get the parent PET identifier for this DE in the send layout
            // this code is in DE-space but the matching PET is used in the
            // routetable, which is by PETs
            srcDELayout->getDEMatchPET(theirDE, *vm, NULL, &theirMatchingPET,
              1);

            // get "their" AI out of the dstAI array
            for (k=0; k<rank; k++) {
                theirSrcGlobalCompAIperRank[k] = 
                     srcGlobalCompAIperDEperRank[theirDE + k*srcDECount];
            }

            // intersect the AI lists
            //  this routine needs to return 0 if no intersect, 1 if it did.
            if (!ESMC_AxisIndexIntersect(rank, 
                                         myDstGlobalCompAIperRank,
                                         theirSrcGlobalCompAIperRank,
                                         intersectGlobalCompAIperRank))
                continue;

            // TODO:  Total now bad; Alloc good
            //   This needs to be 2 parts:  part 1 is GlobalComp to LocalComp
            //   part 2 is LocalComp to LocalAlloc.

            // translate global comp AIs to local comp AIs
            rc = ESMC_AxisIndexGlobalToLocal(rank, 
                                             intersectGlobalCompAIperRank,
                                             myDstGlobalCompAIperRank,
                                             intersectLocalCompAIperRank);
      
            // create a memory-relative xpacket from the comp AIs
            rc = intersectXP.ESMC_XPacketFromCompAIs(rank, 
                                           intersectLocalCompAIperRank, 
                                           myDstGlobalCompAIperRank,
                                           myDstGlobalTotalAIperRank);


            // load the intersecting XPacket into the receiving RTable
            recvRT->ESMC_RTableSetEntry(theirMatchingPET, &intersectXP);
            ct->ESMC_CommTableSetPartner(theirMatchingPET);
 
        }
    }

    //printf("end of RoutePrecomputeRedist:\n");
    //this->ESMC_RoutePrint();

    return rc;

 } // end ESMC_RoutePrecomputeRedist


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeRedistV"
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeRedistV - initialize a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeRedistV(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                   // in  - rank of data in both Fields
      ESMC_Logical  hasDstData,   // in  - has destination data on this DE?
      int dstMyDE,                // in  - DE identifier in the DELayout of
                                  //       the destination Field
      ESMC_Logical dstVector,     // in  - ESMC_Logical identifier denoting
                                  //       whether or not the destination
                                  //       Field has been distributed as a vector
      ESMC_AxisIndex *dstCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the destination
                                  //       Field covering the computational
                                  //       domain
      ESMC_AxisIndex *dstTotalAI, // in  - array of axis indices for all DE's
                                  //       in the DELayout for the destination
                                  //       Field covering the total domain
      int dstAICount,             // in  - number of sets of AI's in the dst
                                  //       array
      int *dstAICountPerDE,       // in  - array of the number of AI's per DE
                                  //       in the destination field
      int *dstGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the destination
                                  //       Field
      int dstGSCount,             // in -  number of sets of global counts
      int *dstGlobalCount,        // in  - array of global strides for each
                                  //       direction for the receiving Field
      ESMCI::DELayout *dstdeLayout, // in  - pointer to the rcv DELayout
      ESMC_Logical  hasSrcData,   // in  - has source data on this DE?
      int srcMyDE,                // in  - DE identifier in the DELayout of
                                  //       the source Field
      ESMC_Logical srcVector,     // in  - ESMC_Logical identifier denoting
                                  //       whether or not the source Field has
                                  //       been distributed as a vector
      ESMC_AxisIndex *srcCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the source
                                  //       Field covering the computational
                                  //       domain
      ESMC_AxisIndex *srcTotalAI, // in  - array of axis indices for all DE's
                                  //       in the DELayout for the source
                                  //       Field covering the total domain
      int srcAICount,             // in  - number of sets of AI's in the src
                                  //       array
      int *srcAICountPerDE,       // in  - array of the number of AI's per DE
                                  //       in the source field
      int *srcGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the source
                                  //       Field
      int srcGSCount,             // in -  number of sets of global counts
      int *srcGlobalCount,        // in  - array of global strides for each
                                  //       direction for the source Field
      ESMCI::DELayout *srcdeLayout) { // in  - pointer to the src DELayout
//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      This routine differs from PrecomputeRedist mostly in that there can
//      be an array of AxisIndices per DE.
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
    int i, k, m, n, rc;
    int myDEStartCount, theirDEStartCount;
    int theirDE, *theirMatchingPET, theirDECount;

    // set this here, because if neither send or recv are > 0 then we
    // do nothing here.
    rc = ESMF_SUCCESS;

    // Set a different default value for this route
    this->ESMC_RouteSetOptions((ESMC_RouteOptions) (ESMC_ROUTE_OPTION_ASYNC |
                               ESMC_ROUTE_OPTION_PACK_PET));

    // Calculate the sending table.  If this DE is not part of the sending
    // layout skip this loop.
    if (hasSrcData == ESMF_TRUE) {

      // get the number of destination DEs 
      dstdeLayout->getDeprecated(&theirDECount, NULL, NULL, NULL, 0, NULL, NULL,
        NULL, NULL, 0); 

      theirMatchingPET = new int[theirDECount];
      for (i=0; i<theirDECount; i++) {
	// get the parent DE identifier for this DE in the src layout
	dstdeLayout->getDEMatchPET(i, *vm, NULL, &theirMatchingPET[i], 1);
        
	if (theirMatchingPET[i] != i)
	  printf("theirDE = %d, parentDE = %d\n", i, theirMatchingPET[i]);
      }

      // get the starting count in the AI list for myDE
      myDEStartCount = 0;
      for (i=0; i<srcMyDE; i++) myDEStartCount += srcAICountPerDE[i];
                                  
      // loop over the number of AI's for my DE
      for (n=0; n<srcAICountPerDE[srcMyDE]; n++) {  
                                  
        // get "my" AI out of the srcAI array
        // TODO:  this is NOT going to work for data dims which are not
        //  equal the igrid dims, e.g. a 2d igrid with 4d data.
        for (k=0; k<rank; k++) {
          myAI[k]          =      srcCompAI[n + myDEStartCount + k*srcAICount];
          myTotalAI[k]     =     srcTotalAI[n + myDEStartCount + k*srcAICount];     
          myGlobalStart[k] = srcGlobalStart[srcMyDE + k*srcGSCount];
        }

        // TODO: check that myAI is not empty -- if so, continue to the recving table
                                        
        // calculate "my" (local DE's) XPacket in the sense of the global data
        rc = ESMC_XPacketFromAxisIndex(myAI, rank, srcGlobalCount,
                                       NULL, &myXP, &myXPCount);
    
        // loop over DE's from receiving layout to calculate send table
        theirDEStartCount = 0;
        for (i=0; i<theirDECount; i++) {
          theirDE = i;
    
          // loop over the number of AI's for their DE
          for (m=0; m<dstAICountPerDE[theirDE]; m++) {

            // get "their" AI out of the dstAI array
            for (k=0; k<rank; k++) {
              theirAI[k]     =  dstCompAI[m + theirDEStartCount + k*dstAICount];
            }

            // TODO: check that theirAI is not empty  -- if so, loop on i

            // calculate "their" XPacket in the sense of the global data
            rc = ESMC_XPacketFromAxisIndex(theirAI, rank, dstGlobalCount,
                                           NULL, &theirXP, &theirXPCount);

            // calculate the intersection
            if (intersectXP.ESMC_XPacketIntersect(&myXP[0], &theirXP[0])==ESMF_FAILURE) {
	      delete [] theirXP;
	      continue;
	    }

            // if there's no intersection, no need to add an entry here
            if (intersectXP.ESMC_XPacketIsEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] theirXP;
            continue;         // TODO: loop on m -- is this continue
            }

            // translate from global to local data space
            if (srcVector == ESMF_FALSE)
              intersectXP.ESMC_XPacketGlobalToLocal(&intersectXP, myTotalAI,
                                                    rank, myGlobalStart);
            if (srcVector == ESMF_TRUE)
              intersectXP.ESMC_XPacketSetOffset(n);

            // load the intersecting XPacket into the sending RTable
            sendRT->ESMC_RTableSetEntry(theirMatchingPET[i], &intersectXP);
            ct->ESMC_CommTableSetPartner(theirMatchingPET[i]);

            // free XPs allocated by XPacketFromAxisIndex() routine above
            delete [] theirXP;
          }   // next AI for theirDE

          theirDEStartCount += dstAICountPerDE[theirDE];
        }   // next DE

        // free this myXP before computing the next myXP
        delete [] myXP;

      }     // next AI for myDE

      delete [] theirMatchingPET;
    }       // end of sending table

    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout skip this loop completely.
    if (hasDstData == ESMF_TRUE) {

      // get the number of source DEs
      srcdeLayout->getDeprecated(&theirDECount, NULL, NULL, NULL, 0, NULL, NULL,
        NULL, NULL, 0);

      theirMatchingPET = new int[theirDECount];

      for (i=0; i<theirDECount; i++) {
	// get the parent DE identifier for this DE in the src layout
	srcdeLayout->getDEMatchPET(i, *vm, NULL, &theirMatchingPET[i], 1);
        
	if (theirMatchingPET[i] != i)
	  printf("theirDE = %d, parentDE = %d\n", i, theirMatchingPET[i]);
      }

      // get the starting count in the AI list for myDE
      myDEStartCount = 0;
      for (i=0; i<dstMyDE; i++) myDEStartCount += dstAICountPerDE[i];

      // loop over the number of AI's for my DE
      for (n=0; n<dstAICountPerDE[dstMyDE]; n++) { 

        // get "my" AI out of the dstAI array 
        for (k=0; k<rank; k++) {
          myAI[k]          =      dstCompAI[n + myDEStartCount + k*dstAICount];
          myTotalAI[k]     =     dstTotalAI[n + myDEStartCount + k*dstAICount];     
          myGlobalStart[k] = dstGlobalStart[dstMyDE + k*dstGSCount];
        }   
            
        // calculate "my" (local DE's) XPacket in the sense of the global data
        rc = ESMC_XPacketFromAxisIndex(myAI, rank, dstGlobalCount,
                                       NULL, &myXP, &myXPCount);
            
        // TODO: check that myAI is not empty -- if so, continue to the end

        // loop over DE's from sending layout to calculate receive table
        theirDEStartCount = 0;
        for (i=0; i<theirDECount; i++) {
          theirDE = i;

          // loop over the number of AI's for their DE
          for (m=0; m<srcAICountPerDE[theirDE]; m++) {

            // get "their" AI out of the srcAI array
            for (k=0; k<rank; k++) {
              theirAI[k]     =  srcCompAI[m + theirDEStartCount + k*srcAICount];
            }

            // TODO: check that theirAI is not empty  -- if so, loop on i

            // calculate "their" XPacket in the sense of the global data
            rc = ESMC_XPacketFromAxisIndex(theirAI, rank, srcGlobalCount,
                                           NULL, &theirXP, &theirXPCount);

            // calculate the intersection
            if (intersectXP.ESMC_XPacketIntersect(&myXP[0], &theirXP[0])==ESMF_FAILURE) {
	      delete [] theirXP;
	      continue;
	    }

            // if there's no intersection, no need to add an entry here
            if (intersectXP.ESMC_XPacketIsEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] theirXP;
            continue;
          }

            // translate from global to local
            if (dstVector==ESMF_FALSE)
              intersectXP.ESMC_XPacketGlobalToLocal(&intersectXP, myTotalAI,
                                                    rank, myGlobalStart);
            if (dstVector == ESMF_TRUE)
              intersectXP.ESMC_XPacketSetOffset(n);

            // load the intersecting XPacket into the receiving RTable
            recvRT->ESMC_RTableSetEntry(theirMatchingPET[i], &intersectXP);
            ct->ESMC_CommTableSetPartner(theirMatchingPET[i]);

            // free XPs allocated by XPacketFromAxisIndex() routine above
            delete [] theirXP;
          }   // next AI for theirDE

          theirDEStartCount += srcAICountPerDE[theirDE];
        }   // next DE

        // free this myXP before computing the next myXP
        delete [] myXP;

      }     // next AI for myDE

      delete [] theirMatchingPET;

    }       // end of recv table
            
    return rc;
 } // end ESMC_RoutePrecomputeRedistV

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeRedistA2A"
//BOP
// !IROUTINE:  ESMC_RoutePrecomputeRedistA2A - initialize a Route
//
// !INTERFACE:
      int ESMC_Route::ESMC_RoutePrecomputeRedistA2A(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                   // in  - rank of data in both Fields
      ESMC_Logical  hasDstData,   // in  - has destination data on this DE?
      int dstMyDE,                // in  - DE identifier in the DELayout of
                                  //       the destination Field
      ESMC_AxisIndex *dstCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the destination
                                  //       Field covering the computational
                                  //       domain
      int dstAICount,             // in  - number of sets of AI's in the dst
                                  //       array
      int *dstAICountPerDE,       // in  - array of the number of AI's per DE
                                  //       in the destination field
      int *dstGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the destination
                                  //       Field
      int dstGSCount,             // in -  number of sets of global counts
      int *dstGlobalCount,        // in  - array of global strides for each
                                  //       direction for the receiving Field
      ESMCI::DELayout *dstdeLayout, // in  - pointer to the rcv DELayout
      ESMC_Logical  hasSrcData,   // in  - has source data on this DE?
      int srcMyDE,                // in  - DE identifier in the DELayout of
                                  //       the source Field
      ESMC_AxisIndex *srcCompAI,  // in  - array of axis indices for all DE's
                                  //       in the DELayout for the source
                                  //       Field covering the computational
                                  //       domain
      int srcAICount,             // in  - number of sets of AI's in the src
                                  //       array
      int *srcAICountPerDE,       // in  - array of the number of AI's per DE
                                  //       in the source field
      int *srcGlobalStart,        // in  - array of global starting indices
                                  //       for all DE's in the DELayout and in
                                  //       each direction for the source
                                  //       Field
      int srcGSCount,             // in -  number of sets of global counts
      int *srcGlobalCount,        // in  - array of global strides for each
                                  //       direction for the source Field
      ESMCI::DELayout *srcdeLayout) { // in  - pointer to the src DELayout

//
// !DESCRIPTION:
//      Initializes a Route with send and receive RouteTables.
//      This routine differs from PrecomputeRedist mostly in that there can
//      be an array of AxisIndices per DE.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex myAI[ESMF_MAXDIM];
    ESMC_XPacket *theirXP = NULL;
    ESMC_XPacket intersectXP;
    int myXPCount, theirXPCount;
    int myGlobalStart[ESMF_MAXDIM];
    int i, k, m, n, rc;
    int myDEStartCount, theirDEStartCount;
    int theirDE, *theirMatchingPET, theirDECount;
    int myCount, lastmatch, totalAI;
    permuteLocal *myOffsetTbl;
    permuteGlobal *dstOffsetTbl, *srcOffsetTbl;

    // set this here, because if neither send or recv are > 0 then we
    // do nothing here.
    rc = ESMF_SUCCESS;

    // Set a different default value for this route
    this->ESMC_RouteSetOptions((ESMC_RouteOptions) (ESMC_ROUTE_OPTION_ASYNC |
                               ESMC_ROUTE_OPTION_PACK_PET));

    // Calculate the sending table.  If this DE is not part of the sending
    // layout skip this loop.
    if (hasSrcData == ESMF_TRUE) {

      // get the number of destination DEs
      dstdeLayout->getDeprecated(&theirDECount, NULL, NULL, NULL, 0, NULL, NULL,
        NULL, NULL, 0);

      theirMatchingPET = new int[theirDECount];
      for (i=0; i<theirDECount; i++) {
        // get the parent DE identifier for this DE in the src layout
        dstdeLayout->getDEMatchPET(i, *vm, NULL, &theirMatchingPET[i], 1);
        
        if (theirMatchingPET[i] != i)
          printf("theirDE = %d, parentDE = %d\n", i, theirMatchingPET[i]);
      }

      // get the starting count in the AI list for myDE
      myDEStartCount = 0;
      for (i=0; i<srcMyDE; i++) myDEStartCount += srcAICountPerDE[i];

      // extract indices for the local igrid, calculate the offset and sort them
      myCount = srcAICountPerDE[srcMyDE];
      myOffsetTbl = new permuteLocal[myCount];
      for (n=0, m=myDEStartCount; n< myCount; n++, m++) {
          myOffsetTbl[n].offset = srcCompAI[m].min+
          srcCompAI[m+srcAICount].min*srcGlobalCount[0];
          myOffsetTbl[n].index=n;
      }

      // Sort the offset in ascending order,
      qsort(myOffsetTbl,myCount, sizeof(permuteLocal), compare1);

      // totalAI count for the destination igrid
      totalAI = 0;
      for (i=0; i<theirDECount; i++) totalAI += dstAICountPerDE[i];
      dstOffsetTbl = new permuteGlobal[totalAI];

      // Calculate offsets from dstIndicesArry and sort them, keep the dest. de
      // and local index in the dest de.
      for (n=0, i=0; n<theirDECount; n++) {
          for (m=0; m<dstAICountPerDE[n]; m++,i++) {
              dstOffsetTbl[i].offset = dstCompAI[i].min+
              dstCompAI[i+dstAICount].min*dstGlobalCount[0];
              dstOffsetTbl[i].index=m;
              dstOffsetTbl[i].de=n;
          }
      }

      qsort(dstOffsetTbl, totalAI, sizeof(permuteGlobal), compare2);

      // Find intersection between myOffsetTbl and dstOffsetTbl (both are
      // sorted by offset
      lastmatch=0;
      for (n=0; n<myCount; n++) {
         for (m=lastmatch; m<totalAI; m++) {
           if (myOffsetTbl[n].offset < dstOffsetTbl[m].offset) {
             lastmatch=m;
             break;
           } else if (myOffsetTbl[n].offset == dstOffsetTbl[m].offset) {
               lastmatch = m+1;
               for (k=0; k<rank; k++) {
                 myAI[k]= srcCompAI[myOffsetTbl[n].index + myDEStartCount +
                          k*srcAICount];
               }
               ESMC_XPacketFromAxisIndex(myAI, rank, srcGlobalCount,
                                         NULL, &theirXP, &theirXPCount);
               theirXP[0].ESMC_XPacketSetOffset(myOffsetTbl[n].index);
               theirXP[0].ESMC_XPacketSetIndex(dstOffsetTbl[m].index);
               // load the intersecting XPacket into the sending RTable
               i = dstOffsetTbl[m].de;
               sendRT->ESMC_RTableSetEntry(theirMatchingPET[i],
                                           &theirXP[0]);
               ct->ESMC_CommTableSetPartner(theirMatchingPET[i]);
               delete [] theirXP;
               break;
           }
         }
      }

      // Need to sort the sendRT so that the XPackets are ordered in the sequence of
      // of the destination igrid.
      sendRT->ESMC_RTableSort();

      delete [] theirMatchingPET;
      delete [] myOffsetTbl;
      delete [] dstOffsetTbl;

    }       // end of sending table


    // Calculate the receiving table.  If this DE is not part of the receiving
    // layout skip this loop completely.
    if (hasDstData == ESMF_TRUE) {

      // get the number of source DEs
      srcdeLayout->getDeprecated(&theirDECount, NULL, NULL, NULL, 0, NULL, NULL,
        NULL, NULL, 0);

      theirMatchingPET = new int[theirDECount];

      for (i=0; i<theirDECount; i++) {
        // get the parent DE identifier for this DE in the src layout
        srcdeLayout->getDEMatchPET(i, *vm, NULL, &theirMatchingPET[i], 1);
        
        if (theirMatchingPET[i] != i)
          printf("theirDE = %d, parentDE = %d\n", i, theirMatchingPET[i]);
      }

      // get the starting count in the AI list for myDE
      myDEStartCount = 0;
      for (i=0; i<dstMyDE; i++) myDEStartCount += dstAICountPerDE[i];

      // extract indices for the local igrid, calculate the offset and sort them
      myCount = dstAICountPerDE[dstMyDE];
      myOffsetTbl = new permuteLocal[myCount];
      for (n=0, m=myDEStartCount; n< myCount; n++, m++) {
          myOffsetTbl[n].offset = dstCompAI[m].min*dstGlobalCount[0]+
          dstCompAI[m+dstAICount].min;
          myOffsetTbl[n].index=n;
      }

      // Sort the offset in ascending order,
      qsort(myOffsetTbl,myCount, sizeof(permuteLocal), compare1);

      // totalAI count for the source igrid
      totalAI = 0;
      for (i=0; i<theirDECount; i++) totalAI += srcAICountPerDE[i];
      srcOffsetTbl = new permuteGlobal[totalAI];

      // Calculate offsets from srcCompAI and sort them, keep the dest. de
      // and local index in the dest de.
      for (n=0, i=0; n<theirDECount; n++) {
          for (m=0; m<srcAICountPerDE[n]; m++,i++) {
              srcOffsetTbl[i].offset = srcCompAI[i].min*srcGlobalCount[0]+
              srcCompAI[i+srcAICount].min;
              srcOffsetTbl[i].index=m;
              srcOffsetTbl[i].de=n;
          }
      }

      qsort(srcOffsetTbl, totalAI, sizeof(permuteGlobal), compare2);

      // Find intersection between myOffsetTbl and srcOffsetTbl (both are
      // sorted by offset
      lastmatch=0;
      for (n=0; n<myCount; n++) {
         for (m=lastmatch; m<totalAI; m++) {
           if (myOffsetTbl[n].offset < srcOffsetTbl[m].offset) {
             lastmatch=m;
             break;
           } else if (myOffsetTbl[n].offset == srcOffsetTbl[m].offset) {
               lastmatch = m+1;
               for (k=0; k<rank; k++) {
                 myAI[k]= dstCompAI[myOffsetTbl[n].index + myDEStartCount +
                          k*srcAICount];
               }
               ESMC_XPacketFromAxisIndex(myAI, rank, srcGlobalCount,
                                         NULL, &theirXP, &theirXPCount);
               theirXP[0].ESMC_XPacketSetOffset(myOffsetTbl[n].index);
               theirXP[0].ESMC_XPacketSetIndex(myOffsetTbl[n].index);
               // load the intersecting XPacket into the sending RTable
               i = srcOffsetTbl[m].de;
               recvRT->ESMC_RTableSetEntry(theirMatchingPET[i],
                                           &theirXP[0]);
               ct->ESMC_CommTableSetPartner(theirMatchingPET[i]);
               delete [] theirXP;
               break;
            }
         }
      }

      // Need to sort the sendRT so that the XPackets are ordered in the sequence of
      // of the destination igrid.
      recvRT->ESMC_RTableSort();

      delete [] theirMatchingPET;
      delete [] myOffsetTbl;
      delete [] srcOffsetTbl;
    }       // end of recving table

    return rc;
 } // end ESMC_RoutePrecomputeRedistA2A


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeRegrid"
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
      ESMCI::DELayout *delayout_rcv,   // in  - pointer to the rcv DELayout
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
      ESMCI::DELayout *delayout_snd) { // in  - pointer to the snd DELayout 
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
    int their_de, their_matching_pet, their_decount;

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
      //  equal the igrid dims, e.g. a 2d igrid with 4d data.
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
      delayout_rcv->getDeprecated(&their_decount, NULL, NULL, NULL, 0, NULL,
        NULL, NULL, NULL, 0);
      
      for (i=0; i<their_decount; i++) {
          their_de = i;

          // get the parent PET identifier for this DE in the rcv layout
          delayout_rcv->getDEMatchPET(their_de, *vm, NULL, &their_matching_pet,
            1);
          
 //         printf("Match1: %d, %d\n", their_de, their_matching_pet);
          //their_matching_pet = their_de;     // temporarily
          if (their_matching_pet != their_de) 
	     printf("regrid: theirDE = %d, parentDE = %d\n", 
                      their_de, their_matching_pet);
          //their_de = their_matching_pet;     // temporarily

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
          if (intersect_XP.ESMC_XPacketIsEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] their_XP;
              continue;
          }

          // translate from global to local data space
          intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                                 rank, my_global_start);

          // load the intersecting XPacket into the sending RTable
          sendRT->ESMC_RTableSetEntry(their_matching_pet, &intersect_XP);
          ct->ESMC_CommTableSetPartner(their_matching_pet);
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

          // get the parent PET identifier for this DE in the snd layout
          delayout_snd->getDEMatchPET(their_de, *vm, NULL, &their_matching_pet,
            1);
          
  //        printf("Match2: %d, %d\n", their_de, their_matching_pet);
          //their_matching_pet = their_de;     // temporarily
          if (their_matching_pet != their_de) 
	     printf("regrid: theirDE = %d, parentDE = %d\n", 
                     their_de, their_matching_pet);
          //their_de = their_matching_pet;     // temporarily

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
          if (intersect_XP.ESMC_XPacketIsEmpty()) {
              // free XPs allocated by XPacketFromAxisIndex() routine above
              delete [] their_XP;
              continue;
          }

          // translate from global to local
          intersect_XP.ESMC_XPacketGlobalToLocal(&intersect_XP, my_AI_tot, 
                                                 rank, my_global_start);

          // load the intersecting XPacket into the receiving RTable
          recvRT->ESMC_RTableSetEntry(their_matching_pet, &intersect_XP);
          ct->ESMC_CommTableSetPartner(their_matching_pet);

          // free XPs allocated by XPacketFromAxisIndex() routine above
          delete [] their_XP;
        }

        // free the rcv my_XP
        delete [] my_XP;
    }

    //printf("end of RoutePrecomputeRegrid:\n");
    //this->ESMC_RoutePrint();

    return rc;

 } // end ESMC_RoutePrecomputeRegrid


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrecomputeDomList"
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
      int rank,                       // in - rank of data
      ESMCI::DELayout *srcDELayout,     // in - Source DELayout
      ESMCI::DELayout *dstDELayout,     // in - Destination DELayout
      ESMC_DomainList *srcDomainList, // in - array of axis indices for all DEs
                                      //      in the DELayout for the receiving
                                      //      Field
      ESMC_DomainList *dstDomainList, // in - array of axis indices for all DEs
                                      //      in the DELayout for the receiving
                                      //      Field
      ESMC_Logical *hasSrcData,
      ESMC_Logical *hasDstData) {
//
// !DESCRIPTION:
//      Initializes a Route from a DomainList with send and receive RouteTables.
//      Returns error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    ESMC_AxisIndex myAI[ESMF_MAXDIM], theirAI[ESMF_MAXDIM];
    ESMC_XPacket *myXP = NULL;
    ESMC_XPacket *theirXP = NULL;
    int myXPcount, theirXPcount;
    int globalCount[ESMF_MAXDIM];
    int theirPET[1], nmatch;
    int rc;  
    int j, k, theirOffset, offset, count;
    int theirDE;

    for (j=0; j<rank; j++)
      globalCount[j] = 0;

    // Calculate the sending table.
    // loop over DE's from DomainList to send to
    if (*hasSrcData == ESMF_TRUE) {
      for (k=0; k<srcDomainList->num_domains; k++) {
        theirDE = srcDomainList->ESMC_DomainListGetDE(k);
        //theirDE = srcDomainList->domains[k].DE;
        // get "my" AI
        for (j=0; j<rank; j++) 
          myAI[j] = srcDomainList->ESMC_DomainListGetAI(k, j);
          //myAI[j] = srcDomainList->domains[k].ai_list[j];
      
        // calculate "my" XPacket from the AxisIndices -- in this case the
        // AIs are in local space and so is the XPacket
        rc = ESMC_XPacketFromAxisIndex(myAI, rank, globalCount, NULL,
                                       &myXP, &myXPcount);
      
        // get PET from DE here.  Single DE per PET.  TODO: fix this
        dstDELayout->getDEMatchPET(theirDE, *vm, &nmatch, theirPET, 1);
        
        // load the XPacket into the sending RTable
        sendRT->ESMC_RTableSetEntry(*theirPET, myXP);
        ct->ESMC_CommTableSetPartner(*theirPET);

        // free each XP before allocating another in XPacketFromAxisIndex
        delete [] myXP;
      }
    }

    // Calculate the receiving table.
    // loop over DE's from DomainList to receive from
    offset = 0;
    if (*hasDstData == ESMF_TRUE) {
      for (k=0; k<dstDomainList->num_domains; k++) {
        theirDE = dstDomainList->ESMC_DomainListGetDE(k);
        //theirDE = dstDomainList->domains[k].DE;
        // get "their" AI
        count = 1;
        for (j=0; j<rank; j++) {
          theirAI[j] = dstDomainList->ESMC_DomainListGetAI(k, j);
          //theirAI[j] = dstDomainList->domains[k].ai_list[j];
          count = count * (theirAI[j].max - theirAI[j].min + 1);
        }
 
        // calculate "their" XPacket from the AxisIndices -- in this case the
        // AIs are in local space and so is the XPacket
        rc = ESMC_XPacketFromAxisIndex(theirAI, rank, globalCount, NULL,
                                       &theirXP, &theirXPcount);
      
        // modify the XPacket so it gets stored immediately after the last one
        theirOffset = theirXP->ESMC_XPacketGetOffset();
        theirOffset += offset;
        theirXP->ESMC_XPacketSetOffset(theirOffset);
        offset += count;

        // get PET from DE here.  Single DE per PET.  TODO: fix this
        srcDELayout->getDEMatchPET(theirDE, *vm, &nmatch, theirPET, 1);
        
        // load the XPacket into the sending RTable
        recvRT->ESMC_RTableSetEntry(*theirPET, theirXP);
        ct->ESMC_CommTableSetPartner(*theirPET);

        // free each XP before allocating another in XPacketFromAxisIndex
        delete [] theirXP;
      }
    }
 
    //ESMC_RoutePrint();
 
    return rc;

 } // end ESMC_RoutePrecomputeDomainList


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteValidate"
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
      int srcbufcount,              // in - number of src buffers, usually 1
      int *srcbufsizes,             // in - item count per src buffer
      int dstbufcount,              // in - number of dst buffers, usually 1
      int *dstbufsizes,             // in - item count per dst buffer
      const char *options) const {  // in - validate options
//
// !DESCRIPTION:
//      Validates that a Route is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc;
    int i, j, k;
    int npets, xpcount;
    int minitem, maxitem;
    ESMC_XPacket *xp;

// TODO:
// ok we can still validate base object, but mostly this needs to broadcast
// the buffer size and the number of xpackets it expects to send, and
// the item counts in each xpacket.   the code needs to match up the
// sends and receives and be sure they are the same number of bytes,
// and make sure that no offsets end up going past the item count in
// the local buffers - either on the send or receive side.

// this could be called at the end of Precompute automatically, to
// flag problems (although would we have the item count for the buffersize
// at this time?  should have, i'd think...)

    rc = this->ESMC_Base::ESMC_Validate(options);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, 
                                              &rc)) return (rc);

    // add more code here.
   
    // TODO: first pass at helping find route-table-related bugs, which
    // are pretty painful to debug at this point.
    // 1. do what we can locally.   we have item counts and we can verify
    // that xpackets do not reference memory outside the buffer.
    // 2. next most common problem - mismatches between number of sends
    // and receives or sizes of sends/receives on each PET.  for that we
    // have to broadcast the sizes and check against what we expect to do.
 
    // for now just do number 1.

    // figure out how many pets total we might be sending to and receiving from
    rc = ct->ESMC_CommTableGetCount(&npets);

    // for send table:
    for (i=0; i<npets; i++) {
        rc = sendRT->ESMC_RTableGetCount(i, &xpcount);
        if (xpcount == 0) continue;

        for (j=0; j<xpcount; j++) {
            rc = sendRT->ESMC_RTableGetEntry(i, j, &xp);
            rc = xp->ESMC_XPacketMinMax(&minitem, &maxitem);
            for (k=0; k<srcbufcount; k++) {
                // i think this should really be >=, but try > for now.
                if ((minitem < 0) || (maxitem > srcbufsizes[k])) {
                    printf("ERROR: pet %d, send xp %d, item %d > size %d of buffer %d\n",
                                        i, j, maxitem, srcbufsizes[k], k);
                    return ESMF_FAILURE;
                }
            }
        }
    }

    // for recv table:
    for (i=0; i<npets; i++) {
        rc = recvRT->ESMC_RTableGetCount(i, &xpcount);
        if (xpcount == 0) continue;

        for (j=0; j<xpcount; j++) {
            rc = recvRT->ESMC_RTableGetEntry(i, j, &xp);
            rc = xp->ESMC_XPacketMinMax(&minitem, &maxitem);
            for (k=0; k<dstbufcount; k++) {
                // ditto
                if ((minitem < 0) || (maxitem > dstbufsizes[k])) {
                    printf("ERROR: pet %d, recv xp %d, item %d > size %d of buffer %d\n",
                                        i, j, maxitem, dstbufsizes[k], k);
                    return ESMF_FAILURE;
                }
            }
        }
    }


    return ESMF_SUCCESS;

 } // end ESMC_RouteValidate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RoutePrint"
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

    int rc = ESMC_RC_NOT_IMPL;
    char msgbuf[ESMF_MAXSTR];

    sprintf(msgbuf,"Route print:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    sprintf(msgbuf," Routeid = %d\n", routeid);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    sprintf(msgbuf," Route Options = %d\n", this->options);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    sprintf(msgbuf," Recv item count: %d\n", recvitems);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    // uncomment for profiling
    // printf("Timing: Packing %lf, SendRecv: %lf, Unpack: %lf\n", 
    //	   timer1*1000, timer2*1000, timer3*1000);

    if (options == NULL) return ESMF_SUCCESS;

    // TODO: print something about the attached VM?
    //sprintf(msgbuf," VM =\n");
    ////ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    //printf(msgbuf);
    //vm->VMPrint("");
    {
      int myid = vm->getLocalPet();
      int petcount = vm->getPetCount();
      sprintf(msgbuf, "VM: my pet = %d of %d\n", myid, petcount);
      //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      printf(msgbuf);
    }

    sprintf(msgbuf," Send table:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    rc = sendRT->ESMC_RTablePrint(options);

    sprintf(msgbuf," Recv table:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    rc = recvRT->ESMC_RTablePrint(options);

    sprintf(msgbuf," Comm table:\n");
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
    rc = ct->ESMC_CommTablePrint(options);

    return rc;

 } // end ESMC_RoutePrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Route()"
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
      void) { 
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:

  // default constructor is fine.
  
 } // end ESMC_Route

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_Route()"
//BOP
// !IROUTINE:  ~ESMC_Route - native C++ destructor
//
// !INTERFACE:
      ESMC_Route::~ESMC_Route(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS: 

  // default destructor is fine.

 } // end ~ESMC_Route
