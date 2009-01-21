// $Id: ESMC_XPacket.C,v 1.68.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC XPacket method implementation (body) file
#define ESMF_FILENAME "ESMC_XPacket.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ XPacket methods declared
// in the companion file ESMC_XPacket.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>
 #include <ESMC_LogErr.h>

// for printf
#include <stdio.h>
#include <assert.h>
#include <string.h>
 // associated class definition file
#include <ESMC_XPacket.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
              "$Id: ESMC_XPacket.C,v 1.68.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XPacket routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketCopy"
//BOP
// !IROUTINE:  ESMC_XPacketCopy - Copy contents from an existing XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
	 ESMC_XPacket* xpacket) { // Input
//
// !DESCRIPTION:
//     Copy the contents of xpacket to this XPacket.  
//
//EOP
// !REQUIREMENTS:  

	int nrank, noffset, ncontig_length;
	int nstride[ESMF_MAXDIM], nrep_count[ESMF_MAXDIM],bufindex;

	xpacket->ESMC_XPacketGet(&nrank, &noffset, &ncontig_length,
				 nstride, nrep_count, &bufindex);
	single = 1;
        offset = noffset;
	rank = nrank;
	contig_length = ncontig_length;
        for (int i=0; i<rank; i++) {
            stride[i] = nstride[i];
	    rep_count[i] = nrep_count[i];
	}
        block_index = bufindex;

    return ESMF_SUCCESS;

 } // end ESMC_XPacketCopy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketSet"
//BOP
// !IROUTINE:  ESMC_XPacketGet - Get values back out of an exchange packet
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nrank,             // out, single int
      int *noffset,           // out, single int
      int *ncontig_length,    // out, single int
      int *nstride,           // out, array of rank-1 ints
      int *nrep_count,        // out, array of rank-1 ints
      int *bufindex) {        // out, single int
//
// !DESCRIPTION:
//     Returns the contents of XPacket member.  Allow incoming values to 
//     be NULL so caller can query for only the items they need.
//
//EOP
// !REQUIREMENTS:  

    if (nrank) *nrank = rank;
    if (noffset) *noffset = offset;
    if (ncontig_length) *ncontig_length = contig_length;
    if (nstride) {
        for (int i=0; i<rank; i++) 
            nstride[i] = stride[i];

    }
    if (nrep_count) {
        for (int i=0; i<rank; i++) 
            nrep_count[i] = rep_count[i];
    }
    if (bufindex) *bufindex = block_index;

    return ESMF_SUCCESS;

 } // end ESMC_XPacketGet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketSet"
//BOP
// !IROUTINE:  ESMC_XPacketSet - set <Value> for a XPacket
//
// !INTERFACE:
      //int ESMC_XPacket::ESMC_XPacketSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the XPacket member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

    //return ESMF_FAILURE;

 //} // end ESMC_XPacketSet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketSetContig"
//BOPI
// !IROUTINE:  ESMC_XPacketSetContig - Compute if an XP is contiguous
//
// !INTERFACE:
      void ESMC_XPacket :: ESMC_XPacketSetContig() {
//
// !RETURN VALUE:
//     None.
//
// !DESCRIPTION:
//
//   Determine if the region of memory described by this XP is contiguous.
//   For higher dimensional blocks, it might be possible that lower dim
//   slabs are contig, so instead of a boolean, this stores the highest rank
//   at which the data can be moved in a single contiguous memory transfer.
//
//EOPI
// !REQUIREMENTS:  AAAn.n.n

    int i, j, reps;

    // start out assuming only the contig_length items are together.
    contigrank = 1;

    // if the packet describes an empty region, return here.
    if (contig_length == 0)
        return;

    // if the counts are 1 in all dimensions, then this has to be a single
    // block of memory.
    reps = 1;
    for (i=0; i<rank-1; i++)
        reps *= rep_count[i];

    if (reps == 1) {
        contigrank = rank;
        return;
    }

    // it still might be contig even if the repcounts are > 1, if the strides
    // for each dim match the size of the previous block of memory.

    // no, return leaving contigrank = 1
    if (contig_length != stride[0]) 
        return;

    // for each time thru this loop where the strides times the rep_counts
    // match the next stride up, that slab is contig so bump up the rank.
    for (j=1; j<rank-2; j++) {
       if (stride[j] != rep_count[j-1] * stride[j-1]) 
           return;
      
       contigrank++;
    }
   

    // you get here if the entire block is contig, but by this point
    // contigrank should have been incremented the proper amount

    return;

 } // end ESMC_XPacketSetContig


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketSetDefault"
//BOP
// !IROUTINE:  ESMC_XPacketSetDefault - Initialize an XPacket
//
// !INTERFACE:
      int ESMC_XPacket :: ESMC_XPacketSetDefault(
//
// !RETURN VALUE:
//     integer return code
//
// !ARGUMENTS:
      int nrank,
      int noffset,
      int ncontig_length,
      int *nstride,
      int *nrep_count) {
//
// !DESCRIPTION:
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_XPacket.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    rank = nrank;
    offset = noffset;
    contig_length = ncontig_length;
    for (int i=0; i<rank; i++) {
      stride[i] = nstride[i];
      rep_count[i] = nrep_count[i];
    }

    ESMC_XPacketSetContig();
        
    return ESMF_SUCCESS;

 } // end ESMC_XPacketSetDefault


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketSetEmpty"
//BOPI
// !IROUTINE:  ESMC_XPacketSetEmpty - Initialize an XPacket with zero values
//
// !INTERFACE:
      int ESMC_XPacket :: ESMC_XPacketSetEmpty() {
//
// !RETURN VALUE:
//     integer return code
//
//
// !DESCRIPTION:
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_XPacket.h)
//
//EOPI
// !REQUIREMENTS:  AAAn.n.n

    rank          = 0;
    offset        = 0;
    contig_length = 0;
    for(int i=0; i<ESMF_MAXDIM; i++) {
       rep_count[i] = 0;
       stride[i]    = 0;
    }
    contigrank = 1;

    return ESMF_SUCCESS;

 } // end ESMC_XPacketSetEmpty


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketGetEmpty"
//BOPI
// !IROUTINE:  ESMC_XPacketGetEmpty - Set values which match an empty XP
//
// !INTERFACE:
      int ESMC_XPacketGetEmpty(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nrank,             // in/out, single int - required to have a value
      int *noffset,           // out, single int
      int *ncontig_length,    // out, single int
      int *nstride,           // out, array of rank ints
      int *nrep_count,        // out, array of rank ints
      int *bufindex) {        // out, single int, buffer num
//
// !DESCRIPTION:
//     Returns the values of what an empty XPacket would return.
//     nrank must come in with the max rank valid for the
//     strides and rep_counts. 
//
//EOPI
// !REQUIREMENTS:  

    // Initialize return code
    int rc = ESMC_RC_NOT_IMPL;

    int i;

    if (!nrank) {
        // TODO: add real error handling.  this is a required input.
        // even though it will be set to 0 before exiting.  it limits the
        // loop reps for the strides and repcounts.
        return ESMF_FAILURE;
    }

    if (nstride) {
        for (i=0; i<*nrank; i++) 
            nstride[i] = 0;
    }

    // TODO: this loop should really only be to rank-1; all use of rep_count
    //  are rank-1, but until we fix it everywhere it needs to be rank long.
    if (nrep_count) {
        for (i=0; i<*nrank; i++) 
            nrep_count[i] = 0;
    }

    // after using nrank, set it to 0 (and all the rest)
    *nrank = 0;
    if (noffset) *noffset = 0;
    if (ncontig_length) *ncontig_length = 0;
    if (bufindex) *bufindex = 0;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketGetEmpty

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketMakeBuffer"
//BOPI
// !IROUTINE:  ESMC_XPacketMakeBuffer - make a buffer for data described by XPs
//
// !INTERFACE:
      int ESMC_XPacketMakeBuffer(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int xpCount,                   // in  - count of xp's to pack
      ESMC_XPacket **xpList,         // in  - list of xp's to pack
      int nbytes,                    // in  - number of bytes per item
      int numAddrs,                  // in  - number of addrs to apply this to
      char **buffer,                 // out - new buf, must be del'd by caller 
      int *bufferSize) {             // out - size of the buffer
//
// !DESCRIPTION:
//      Makes a buffer with the space required to hold data from a list of XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMC_RC_NOT_IMPL;
    int i, j;
    int xpDataSize;
    int rank, offset, contigLength, stride[ESMF_MAXDIM], repCount[ESMF_MAXDIM];
    char msgbuf[ESMF_MAXSTR];

    // initialize the buffer size to 0
    *bufferSize = 0;

    // handle the case where there are no XPs
    if ((xpCount == 0) || (xpList == NULL)) {
        *buffer = NULL;
        return ESMF_SUCCESS;
    }

    // loop over the XPs adding the size of the data to the buffer size
    // all the counts and sizes in an XP are in terms of number of items
    for (i=0; i<xpCount; i++) {

      if (xpList[i]->ESMC_XPacketIsEmpty())
          continue;

      rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                      stride, repCount, NULL);

      //  number of items represented by this XP
      xpDataSize = contigLength;
      for (j=0; j<rank-1; j++) {
        xpDataSize *= repCount[j];
      }

      *bufferSize += xpDataSize;
    }

    // the buffer count so far is in #items; convert to bytes
    *bufferSize *= nbytes;

    // and now multiply by the number of separate addresses this will be
    // applied to.
    *bufferSize *= numAddrs;

    // allocate the buffer, or set explicitly to NULL
    if (*bufferSize > 0) {
      *buffer = new char[*bufferSize];
    } else {
      *buffer = NULL;
    }

    return ESMF_SUCCESS;

 } // end ESMC_XPacketMakeBuffer


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketDoRBuffer"
//BOPI
// !IROUTINE:  ESMC_XPacketDoRBuffer - pack or unpack a flat buffer from/to XPs
//
// !INTERFACE:
      int ESMC_XPacketDoRBuffer(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PackUnpackFlag packflag,  // in  - move XPs from/to the buffer
      int xpCount,                   // in  - count of XPs to pack/unpack
      ESMC_XPacket **xpList,         // in  - list of XPs to pack/unpack
      int nbytes,                    // in  - number of bytes per item
      int numAddrs,                  // in  - number of addrs to apply this to
      void **dataAddr,               // in  - list of addresses to be un/packed
      char *buffer) {                // in  - buffer to pack/unpack into
//
// !DESCRIPTION:
//    Packs an already allocated buffer with data described by one or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;
    int xpItemCount, totalRepCount;
    int rank, offset, contigLength, stride[ESMF_MAXDIM], repCount[ESMF_MAXDIM];
    int itemPtr, contigBytes, index[ESMF_MAXDIM], bufindex;
    char *dataPtr, *bufPtr;
    char msgbuf[ESMF_MAXSTR];

    if ((xpCount == 0) || (xpList == NULL))
        return ESMF_SUCCESS;

    // make a local pointer we can increment without affecting the original.
    bufPtr = buffer;

    // outer loop:  does nothing if there is only 1 address.  however
    // if you want to pack all the xpackets for one address together before
    // moving on to the next address, then change this loop to be 
    // from 1 to numAddrs.  right now this is a no-op.
    for (m=0; m<1; m++) {

      // loop over the XPs, packing the data into the buffer or unpacking the
      // buffer into multiple memory regions described by the XPs.
      for (i=0; i<xpCount; i++) {

	if (xpList[i]->ESMC_XPacketIsSingle()) {
	  offset = xpList[i]->ESMC_XPacketGetOffset();
	  for (l=0; l<numAddrs; l++) {
	    dataPtr = (char *)dataAddr[l] + (offset*nbytes);
	    if (packflag == ESMC_BUFFER_PACK)  {
	      // slow   memcpy(bufPtr, dataPtr, nbytes);
              if (nbytes == 8)
                 *(ESMC_R8 *)bufPtr = *(ESMC_R8 * )dataPtr;
              if (nbytes == 4)
                 *(ESMC_R4 *)bufPtr = *(ESMC_R4 * )dataPtr;
              }
	    else  {
	      // slow   memcpy(dataPtr, bufPtr, nbytes);
              if (nbytes == 8)
                 *(ESMC_R8 *)dataPtr = *(ESMC_R8 * )bufPtr;
              if (nbytes == 4)
                 *(ESMC_R4 *)dataPtr = *(ESMC_R4 * )bufPtr;
              }
	    bufPtr += nbytes;
	  }
	  continue;
	}
    
        // make sure this XP isn't empty before proceeding.
        if (xpList[i]->ESMC_XPacketIsEmpty())
            continue;
  
        rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                        stride, repCount, &bufindex);
  
        // loops for all ranks collapsed into a single master loop
        // if timing shows an advantage, the inner-most loop could be unrolled
        // to give the optimizer some help.
  
        // total number of contig regions which will be moved
        totalRepCount = 1; 
        for (k=0; k<rank-1; k++) {
            totalRepCount *= repCount[k];
            index[k] = 0;
        }
  
        // initial value for item offset, plus size of contig buffer in 
        // bytes (xp values are computed and stored as number of items)
        itemPtr = offset;
        contigBytes = contigLength * nbytes;
  
        // for each contig region which must be moved...
        for (k=0; k<totalRepCount; k++) {
  
            // inner loop:  this configuration of loops packs each xpacket
            // across all addresses before moving on to the next xpacket.
            // it saves the computations of repcount and contig length, but
            // it also hopscotches across memory more - so it isn't clear
            // which loop configuration is the win.  this routine could be
            // configured to take a flag saying whether to pack with the
            // outer loop or the inner loop.   to pack at the outer loop,
            // set the upper m limit to numAddrs and set the following l limit
            // to 1.     the other change is the dataAddr[] index must change
            // from l to m.

            // for each address in our list, apply the xpacket. 
            for (l=0; l<numAddrs; l++) {
  
                // move data into the buffer or out of the buffer
                dataPtr = (char *)dataAddr[l] + (itemPtr*nbytes);
                if (packflag == ESMC_BUFFER_PACK) { 
                    // Using memcpy may be slow but lack of data
                    // set to test; so keep the memcpy for now.
                    memcpy(bufPtr, dataPtr, contigBytes);
                    //
                    // Proposed replacement :
                    // for (i=0; i<contigLength; i++) {
                    //   if (nbytes == 8)
                    //     *(ESMC_R8 *)bufPtr = *(ESMC_R8 * )dataPtr; 
                    //   if (nbytes == 4)
                    //     *(ESMC_R4 *)bufPtr = *(ESMC_R4 * )dataPtr;
                    //   bufPtr += nbytes;
                    //   dataPtr += nbytes;
                    // }
                    }
                else {
                    // Using memcpy may be slow but lack of data
                    // set to test; so keep the memcpy for now.
                    memcpy(dataPtr, bufPtr, contigBytes);
                    //
                    // Proposed replacement :
                    // for (i=0; i<contigLength; i++) {
                    //   if (nbytes == 8)
                    //     *(ESMC_R8 *)dataPtr = *(ESMC_R8 * )bufPtr;
                    //   if (nbytes == 4)
                    //     *(ESMC_R4 *)dataPtr = *(ESMC_R4 * )bufPtr;
                    //   bufPtr += nbytes;
                    //   dataPtr += nbytes;
                    // }
                    }
                bufPtr += contigBytes;     // take off when use the replacement
            }
  
            // increment the innermost counter 
            index[0]++;
            itemPtr += stride[0];
  
            // and now roll up the index, thru all ranks
            // at the end of a loop for one rank, roll back the pointer to
            // the start of the previous loop and add in the next stride.
            for (j=0; (j<rank-2) && (index[j] >= repCount[j]); j++) {
                index[j] = 0;
                index[j+1]++;
                itemPtr -= (repCount[j]*stride[j]);
                itemPtr += stride[j+1];
            }   // end of j (per-rank, "odometer-rollover" or "carry-bit") loop
  
        }     // end of k (total rep count) loop
      }     // end of i (XP) loop
    }     // end of m (numAddr) loop

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketDoRBuffer


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketDoIBuffer"
//BOPI
// !IROUTINE:  ESMC_XPacketDoIBuffer - pack or unpack a flat buffer from/to XPs
//
// !INTERFACE:
      int ESMC_XPacketDoIBuffer(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PackUnpackFlag packflag,  // in  - move XPs from/to the buffer
      int xpCount,                   // in  - count of XPs to pack/unpack
      ESMC_XPacket **xpList,         // in  - list of XPs to pack/unpack
      int nbytes,                    // in  - number of bytes per item
      int numAddrs,                  // in  - number of addrs to apply this to
      void **dataAddr,               // in  - list of addresses to be un/packed
      char *buffer) {                // in  - buffer to pack/unpack into
//
// !DESCRIPTION:
//    Packs an already allocated buffer with data described by one or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;
    int xpItemCount, totalRepCount;
    int rank, offset, contigLength, stride[ESMF_MAXDIM], repCount[ESMF_MAXDIM];
    int itemPtr, contigBytes, index[ESMF_MAXDIM], bufindex;
    char *dataPtr, *bufPtr;
    char msgbuf[ESMF_MAXSTR];

    if ((xpCount == 0) || (xpList == NULL))
        return ESMF_SUCCESS;

    // make a local pointer we can increment without affecting the original.
    bufPtr = buffer;

    // outer loop:  does nothing if there is only 1 address.  however
    // if you want to pack all the xpackets for one address together before
    // moving on to the next address, then change this loop to be 
    // from 1 to numAddrs.  right now this is a no-op.
    for (m=0; m<1; m++) {

      // loop over the XPs, packing the data into the buffer or unpacking the
      // buffer into multiple memory regions described by the XPs.
      for (i=0; i<xpCount; i++) {

	if (xpList[i]->ESMC_XPacketIsSingle()) {
	  offset = xpList[i]->ESMC_XPacketGetOffset();
	  for (l=0; l<numAddrs; l++) {
	    dataPtr = (char *)dataAddr[l] + (offset*nbytes);
	    if (packflag == ESMC_BUFFER_PACK)
	      // slow   memcpy(bufPtr, dataPtr, nbytes);
              if (nbytes == 1)
                 *(ESMC_I1 *)bufPtr = *(ESMC_I1 * )dataPtr;
              if (nbytes == 2)
                 *(ESMC_I2 *)bufPtr = *(ESMC_I2 * )dataPtr;
              if (nbytes == 4)
                 *(ESMC_I4 *)bufPtr = *(ESMC_I4 * )dataPtr;
              if (nbytes == 8)
                 *(ESMC_I8 *)bufPtr = *(ESMC_I8 * )dataPtr;
	    else
	      // slow   memcpy(dataPtr, bufPtr, nbytes);
              if (nbytes == 1)
                 *(ESMC_I1 *)dataPtr = *(ESMC_I1 * )bufPtr;
              if (nbytes == 2)
                 *(ESMC_I2 *)dataPtr = *(ESMC_I2 * )bufPtr;
              if (nbytes == 4)
                 *(ESMC_I4 *)dataPtr = *(ESMC_I4 * )bufPtr;
              if (nbytes == 8)
                 *(ESMC_I8 *)dataPtr = *(ESMC_I8 * )bufPtr;
	    bufPtr += nbytes;
	  }
	  continue;
	}
    
        // make sure this XP isn't empty before proceeding.
        if (xpList[i]->ESMC_XPacketIsEmpty())
            continue;
  
        rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                        stride, repCount, &bufindex);
  
        // loops for all ranks collapsed into a single master loop
        // if timing shows an advantage, the inner-most loop could be unrolled
        // to give the optimizer some help.
  
        // total number of contig regions which will be moved
        totalRepCount = 1; 
        for (k=0; k<rank-1; k++) {
            totalRepCount *= repCount[k];
            index[k] = 0;
        }
  
        // initial value for item offset, plus size of contig buffer in 
        // bytes (xp values are computed and stored as number of items)
        itemPtr = offset;
        contigBytes = contigLength * nbytes;
  
        // for each contig region which must be moved...
        for (k=0; k<totalRepCount; k++) {
  
            // inner loop:  this configuration of loops packs each xpacket
            // across all addresses before moving on to the next xpacket.
            // it saves the computations of repcount and contig length, but
            // it also hopscotches across memory more - so it isn't clear
            // which loop configuration is the win.  this routine could be
            // configured to take a flag saying whether to pack with the
            // outer loop or the inner loop.   to pack at the outer loop,
            // set the upper m limit to numAddrs and set the following l limit
            // to 1.     the other change is the dataAddr[] index must change
            // from l to m.

            // for each address in our list, apply the xpacket. 
            for (l=0; l<numAddrs; l++) {
  
                // move data into the buffer or out of the buffer
                dataPtr = (char *)dataAddr[l] + (itemPtr*nbytes);
                if (packflag == ESMC_BUFFER_PACK) {
                    // Using memcpy may be slow but lack of data
                    // set to test; so keep the memcpy for now.
                    memcpy(bufPtr, dataPtr, contigBytes);
                    //
                    // Proposed replacement :
                    // for (i=0; i<contigLength; i++) {
                    //   if (nbytes == 1)
                    //   *(ESMC_I1 *)bufPtr = *(ESMC_I1 * )dataPtr;
                    //   if (nbytes == 2)
                    //   *(ESMC_I2 *)bufPtr = *(ESMC_I2 * )dataPtr;
                    //   if (nbytes == 4)
                    //   *(ESMC_I4 *)bufPtr = *(ESMC_I4 * )dataPtr;
                    //   if (nbytes == 8)
                    //   *(ESMC_I8 *)bufPtr = *(ESMC_I8 * )dataPtr;
                    //   bufPtr += nbytes;
                    //   dataPtr += nbytes;
                    // }
                    }
                else {
                    // Using memcpy may be slow but lack of data
                    // set to test; so keep the memcpy for now.
                    memcpy(dataPtr, bufPtr, contigBytes);
                    //
                    // Proposed replacement :
                    // for (i=0; i<contigLength; i++) {
                    //   if (nbytes == 1)
                    //   *(ESMC_I1 *)dataPtr = *(ESMC_I1 * )bufPtr;
                    //   if (nbytes == 2)
                    //   *(ESMC_I2 *)dataPtr = *(ESMC_I2 * )bufPtr;
                    //   if (nbytes == 4)
                    //   *(ESMC_I4 *)dataPtr = *(ESMC_I4 * )bufPtr;
                    //   if (nbytes == 8)
                    //   *(ESMC_I8 *)dataPtr = *(ESMC_I8 * )bufPtr;
                    //   bufPtr += nbytes;
                    //   dataPtr += nbytes;
                    // }
                    }
                bufPtr += contigBytes;     // take off when use the replacement
            }
  
            // increment the innermost counter 
            index[0]++;
            itemPtr += stride[0];
  
            // and now roll up the index, thru all ranks
            // at the end of a loop for one rank, roll back the pointer to
            // the start of the previous loop and add in the next stride.
            for (j=0; (j<rank-2) && (index[j] >= repCount[j]); j++) {
                index[j] = 0;
                index[j+1]++;
                itemPtr -= (repCount[j]*stride[j]);
                itemPtr += stride[j+1];
            }   // end of j (per-rank, "odometer-rollover" or "carry-bit") loop
  
        }     // end of k (total rep count) loop
      }     // end of i (XP) loop
    }     // end of m (numAddr) loop

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketDoIBuffer


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketPackBuffer"
//BOPI
// !IROUTINE:  ESMC_XPacketPackBuffer - pack a buffer with data described by XPs
//
// !INTERFACE:
      int ESMC_XPacketPackBuffer(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS
      int xpCount,               // in  - count of xp's to pack
      ESMC_XPacket **xpList,     // in  - list of xp's to pack
      ESMC_TypeKind dk,          // in  - data kind
      int nbytes,                // in  - number of bytes per item
      int numAddrs,              // in  - number of addrs to apply this to
      void **dataAddr,           // in  - list of addresses to be packed
      char *buffer) {            // in  - buffer to pack into
//
// !DESCRIPTION:
//    Packs an already allocated buffer with data described by one or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    switch (dk) {
       case ESMC_TYPEKIND_I1: 
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I2:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I4:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I8:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_R4:
        return ESMC_XPacketDoRBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_R8:
        return ESMC_XPacketDoRBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       //case ESMF_C16:
       // return ESMC_XPacketDoCBuffer(ESMC_BUFFER_PACK, xpCount, xpList,
       //                           nbytes, numAddrs, dataAddr, buffer);
       // break;

       default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                     "Unknown TypeKind", NULL);
        return NULL;
        }

 } // end ESMC_XPacketPackBuffer


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketUnpackBuffer"
//BOPI
// !IROUTINE:  ESMC_XPacketUnpackBuffer - unpack a buffer with data described by XPs
//
// !INTERFACE:
      int ESMC_XPacketUnpackBuffer(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int xpCount,            // in  - count of xp's to unpack
      ESMC_XPacket **xpList,  // in  - list of xp's to unpack
      ESMC_TypeKind dk,       // in  - data kind 
      int nbytes,             // in  - number of bytes per item
      int numAddrs,           // in  - number of addrs to apply this to
      char *buffer,           // in  - raw buffer to unpack
      void **dataAddr) {      // in  - list of addrs to be unpacked into
//
// !DESCRIPTION:
//    Unpacks an already filled buffer into a data array described by one 
//    or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    switch (dk) {
       case ESMC_TYPEKIND_I1:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I2:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I4:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_I8:
        return ESMC_XPacketDoIBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_R4:
        return ESMC_XPacketDoRBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       case ESMC_TYPEKIND_R8:
        return ESMC_XPacketDoRBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
                                  nbytes, numAddrs, dataAddr, buffer);
        break;

       //case ESMF_C16:
       // return ESMC_XPacketDoCBuffer(ESMC_BUFFER_UNPACK, xpCount, xpList,
       //                           nbytes, numAddrs, dataAddr, buffer);
       // break;

       default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                     "Unknown TypeKind", NULL);
        return NULL;
        }

 } // end ESMC_XPacketUnpackBuffer


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketIntersect"
//BOP
// !IROUTINE:  ESMC_XPacketIntersect - intersection of two XPackets
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketIntersect(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XPacket *xpacket1,      // in  - first XPacket
      ESMC_XPacket *xpacket2) {    // in  - second XPacket
//
// !DESCRIPTION:
//      Finds the intersection of two XPackets, which is itself an XPacket.
//      Assumes the XPackets have the same strides.  TODO: always true?  If not?
//      Returns an XPacket and error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i;
    char msgbuf[ESMF_MAXSTR];

    // check that the xpacket ranks are the same
    if (xpacket1->rank != xpacket2->rank) {
      return ESMF_FAILURE;
    }
    if (xpacket1->ESMC_XPacketIsSingle() &&
	xpacket2->ESMC_XPacketIsSingle()) {
      if (xpacket1->ESMC_XPacketGetOffset() == 
	  xpacket2->ESMC_XPacketGetOffset()) {
	this->ESMC_XPacketCopy(xpacket1);
	return ESMF_SUCCESS;
      } else {
	return ESMF_FAILURE;
      }
    }
    // set offset, contig_length, and rep_counts to zero as default
    this->ESMC_XPacketSetEmpty();
    this->rank = xpacket1->rank;

    // debug
    //printf("xpacket1: ");
    //xpacket1->ESMC_XPacketPrint("");
    //printf("xpacket2: ");
    //xpacket2->ESMC_XPacketPrint("");

    // check that the xpacket strides are the same
    if (this->rank > 0) { 
      for (i=0; i<xpacket1->rank-1; i++) {
        if (xpacket1->stride[i] != xpacket2->stride[i]) {
          return ESMF_FAILURE;
        }
        this->stride[i] = xpacket1->stride[i];
      }
    }

    // switch based on xpacket rank  TODO: is this necessary?
    switch (this->rank) {
      case 1:
        {
          if (xpacket1->offset >= xpacket2->offset)
            this->offset  = xpacket1->offset;
          else
            this->offset  = xpacket2->offset;
          if (xpacket1->contig_length <= xpacket2->offset)
            this->contig_length = xpacket1->contig_length;
          else
            this->contig_length = xpacket2->contig_length;
          this->rep_count[0] = 1;
        }
      break;
      case 2:
        {
          // implementation of efficient intersection calculation from
          // thesis by Ramaswamy
          int r1, r2;
          int xp1_right  = xpacket1->offset + xpacket1->contig_length - 1;
          int xp2_right  = xpacket2->offset + xpacket2->contig_length - 1;
          int intersect1 = (xpacket2->offset - xp1_right + xpacket1->stride[0]-1)
                         /  xpacket1->stride[0];  // rounding to nearest integer
          if (intersect1 < 0) intersect1 = 0;
          int intersect2 = (xpacket1->offset - xp2_right + xpacket2->stride[0]-1)
                         /  xpacket2->stride[0];  // rounding to nearest integer
          if (intersect2 < 0) intersect2 = 0;
          int i1=intersect1;
          int L1_left  = xpacket1->offset  + i1*xpacket1->stride[0];
          int L1_right = xp1_right         + i1*xpacket1->stride[0];
          int i2 = (i1*xpacket1->stride[0] + xpacket1->offset -xp2_right)
                 /  xpacket2->stride[0];
          if (i2 < intersect2) i2 = intersect2;
          int L2_left  = xpacket2->offset  + i2*xpacket2->stride[0];
          int L2_right = xp2_right         + i2*xpacket2->stride[0];
          if (L1_left >= L2_left)
            this->offset  = L1_left;
          else
            this->offset  = L2_left;
          if (L1_right <= L2_right)
            this->contig_length = L1_right - this->offset + 1;
          else
            this->contig_length = L2_right - this->offset + 1;

          // take the abs value of overlap rep counts, and pick
          // the smaller positive one. 
          r1 = xpacket1->rep_count[0] - i1;
          //if (r1 < 0) r1 = -r1;
          r2 = xpacket2->rep_count[0] - i2;
          //if (r2 < 0) r2 = -r2;
          this->rep_count[0] = (r1 < r2) ? r1 : r2;

          // for now, just check here for a real intersection
          if ((this->contig_length <= 0) || (this->rep_count[0] <= 0)) {
            this->offset = 0;
            this->contig_length = 0;
            this->rep_count[0] = 0;
          }
        }
      break;
      case 3:
      case 4:
      case 5:
      default:
        sprintf(msgbuf, "no code to handle xpacket rank %d yet\n", this->rank);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
        return (rc);
    } 

    // debug
    //printf("intersect: ");
    //this->ESMC_XPacketPrint("");

    ESMC_XPacketSetContig();
        
    return ESMF_SUCCESS;

 } // end ESMC_XPacketIntersect


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketFromAxisIndex"
//BOP
// !IROUTINE:  ESMC_XPacketFromAxisIndex - calculate an XPacket from an AxisIndex
//
// !INTERFACE:
      int ESMC_XPacketFromAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_AxisIndex *indexlist,  // in  - set of global AxisIndices
      int size_axisindex,         // in  - size of AxisIndex array
      int *global_count,          // in -  array of global counts per dimension
      ESMC_Logical (*boundary)[2], // in - per dim, is left/right periodic?
      ESMC_XPacket **xp_list,     // out - list of xp's created
      int *xp_count) {            // out - count of xp's created
//
// !DESCRIPTION:
// WARNING: 
//  Older routine; slowly being replaced by the one directly below.
//
//      Translates a set of AxisIndices into one or more XPackets.
//      If internal or non-periodic boundary, will only return 1 xp.
//      If external along a periodic boundary, may return multiple xp's.
//      If 'boundary' comes in as NULL, default to non-periodic.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int i, j, nxp, nextxp;
    ESMC_XPacket *xps;
    int rc = ESMF_FAILURE;
    int single = 1;
    char msgbuf[ESMF_MAXSTR];

    // Check if there is only a single element in AI, 
    for (i = 0; i < size_axisindex; i++) {
      if (indexlist[i].max == indexlist[i].min)
	continue;
      else {
	single = 0;
	break;
      }
    }
    // single element XPacket
    if (single == 1) {
        xps = new ESMC_XPacket[1];
        *xp_list = xps;
        *xp_count = 1;
        xps[0].rank = size_axisindex;
        xps[0].single = 1;
	xps[0].contig_length = 1;
        xps[0].contigrank = 1;
	for (i = 0; i < size_axisindex-1; i++) {
	  xps[0].stride[i]=indexlist[i].stride;
	  xps[0].rep_count[i]=1;
	}
        if (size_axisindex == 2)
	  xps[0].offset = indexlist[1].min*indexlist[0].stride
	    + indexlist[0].min;
	else
          xps[0].offset  = indexlist[2].min * indexlist[1].stride 
	          * indexlist[0].stride
                  + indexlist[1].min * indexlist[0].stride
                  + indexlist[0].min;
	return ESMF_SUCCESS;
    }

    // switch based on array rank  TODO: is this necessary?
    switch (size_axisindex) {
      case 2:
        {
          int boundary_l[2];
          int boundary_r[2];

          nxp = 1;
          if (boundary) {
              for (i=0; i<size_axisindex; i++) {
                if (boundary[i][0] == ESMF_TRUE) nxp++;
                if (boundary[i][1] == ESMF_TRUE) nxp++;
              }
          }

          xps = new ESMC_XPacket[nxp];
        
          *xp_list = xps; 
          *xp_count = nxp;

          // there's always 1.
          nextxp = 0;

          xps[nextxp].rank = 2;
          xps[nextxp].offset  = indexlist[1].min * indexlist[0].stride
                              + indexlist[0].min;
          xps[nextxp].contig_length = indexlist[0].max - indexlist[0].min + 1;
          xps[nextxp].stride[0] = indexlist[0].stride;
          xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;

          // if periodic along the first axis and this piece along boundary:
          for (i=0; i<size_axisindex; i++) {
            if (boundary && (boundary[i][0] == ESMF_TRUE)) {
 
              nextxp++;

              xps[nextxp].rank = 2;

              boundary_l[0] = indexlist[0].min;
              boundary_l[1] = indexlist[1].min;
              boundary_r[0] = indexlist[0].max;
              boundary_r[1] = indexlist[1].max;
              boundary_l[i] = boundary_l[i] + global_count[i];
              boundary_r[i] = boundary_r[i] + global_count[i];
    
              xps[nextxp].offset  = boundary_l[1]*indexlist[0].stride 
                                  + boundary_l[0];
              xps[nextxp].contig_length = boundary_r[0] - boundary_l[0] + 1;
              xps[nextxp].stride[0] = indexlist[0].stride;
              xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;
            }
            if (boundary && (boundary[i][1] == ESMF_TRUE)) {
 
              nextxp++;

              xps[nextxp].rank = 2;

              boundary_l[0] = indexlist[0].min;
              boundary_l[1] = indexlist[1].min;
              boundary_r[0] = indexlist[0].max;
              boundary_r[1] = indexlist[1].max;
              boundary_l[i] = boundary_l[i] - global_count[i];
              boundary_r[i] = boundary_r[i] - global_count[i];
    
              xps[nextxp].offset  = boundary_l[1]*indexlist[0].stride
                                  + boundary_l[0];
              xps[nextxp].contig_length = boundary_r[0] - boundary_l[0] + 1;
              xps[nextxp].stride[0] = indexlist[0].stride;
              xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;
            }
          }
        }
      break;

      case 3:
        {
          int boundary_l[3];
          int boundary_r[3];

          nxp = 1;
          if (boundary) {
              for (i=0; i<size_axisindex; i++) {
                if (boundary[i][0] == ESMF_TRUE) nxp++;
                if (boundary[i][1] == ESMF_TRUE) nxp++;
              }
          }

          xps = new ESMC_XPacket[nxp];
        
          *xp_list = xps; 
          *xp_count = nxp;

          // there's always 1.
          nextxp = 0;

          xps[nextxp].rank = 3;
          xps[nextxp].offset  = indexlist[2].min * indexlist[1].stride * indexlist[0].stride
                              + indexlist[1].min * indexlist[0].stride
                              + indexlist[0].min;
          xps[nextxp].contig_length = indexlist[0].max - indexlist[0].min + 1;
          xps[nextxp].stride[0] = indexlist[0].stride;
          xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;
          xps[nextxp].stride[1] = indexlist[1].stride * indexlist[0].stride;
          xps[nextxp].rep_count[1] = indexlist[2].max - indexlist[2].min + 1;


          // if periodic and this piece along boundary:
          for (i=0; i<size_axisindex; i++) {
            if (boundary && (boundary[i][0] == ESMF_TRUE)) {
 
              nextxp++;

              xps[nextxp].rank = 3;

              boundary_l[0] = indexlist[0].min;
              boundary_l[1] = indexlist[1].min;
              boundary_l[2] = indexlist[2].min;
              boundary_r[0] = indexlist[0].max;
              boundary_r[1] = indexlist[1].max;
              boundary_r[2] = indexlist[2].max;
              boundary_l[i] = boundary_l[i] + global_count[i];  // ??
              boundary_r[i] = boundary_r[i] + global_count[i];  // ??
    
              xps[nextxp].offset  = boundary_l[2]*indexlist[1].stride 
                                  + boundary_l[1]*indexlist[0].stride 
                                  + boundary_l[0];
              xps[nextxp].contig_length = boundary_r[0] - boundary_l[0] + 1;
              xps[nextxp].stride[0] = indexlist[0].stride;
              xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;
              xps[nextxp].stride[1] = indexlist[1].stride;
              xps[nextxp].rep_count[1] = indexlist[2].max - indexlist[2].min + 1;
            }
            if (boundary && (boundary[i][1] == ESMF_TRUE)) {
 
              nextxp++;

              xps[nextxp].rank = 3;

              boundary_l[0] = indexlist[0].min;
              boundary_l[1] = indexlist[1].min;
              boundary_l[2] = indexlist[2].min;
              boundary_r[0] = indexlist[0].max;
              boundary_r[1] = indexlist[1].max;
              boundary_r[2] = indexlist[2].max;
              boundary_l[i] = boundary_l[i] - global_count[i];
              boundary_r[i] = boundary_r[i] - global_count[i];
    
              xps[nextxp].offset  = boundary_l[2]*indexlist[1].stride 
                                  + boundary_l[1]*indexlist[0].stride 
                                  + boundary_l[0];
              xps[nextxp].contig_length = boundary_r[0] - boundary_l[0] + 1;
              xps[nextxp].stride[0] = indexlist[0].stride;
              xps[nextxp].rep_count[0] = indexlist[1].max - indexlist[1].min + 1;
              xps[nextxp].stride[1] = indexlist[1].stride;
              xps[nextxp].rep_count[1] = indexlist[2].max - indexlist[2].min + 1;
            }
          }
        }
      break;

      default:
        sprintf(msgbuf, "no code to handle %d AxisIndices yet\n", size_axisindex);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
        return (rc);
        break;
    } 

    for (j=0; j<nxp; j++) 
        xps[j].ESMC_XPacketSetContig();

    return ESMF_SUCCESS;

 } // end ESMC_XPacketFromAxisIndex


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketFromCompAIs"
//BOP
// !IROUTINE:  ESMC_XPacketFromCompAIs - calculate XPacket from AxisIndexList
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketFromCompAIs(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int AIrank,                             // in - data rank
      ESMC_AxisIndex *intersectLocalCompAIperRank,
      ESMC_AxisIndex *srcGlobalCompAIperRank,
      ESMC_AxisIndex *srcGlobalTotalAIperRank) {
      // TODO: change this GlobalAllocAI
//
// !DESCRIPTION:
//      Translates a list of AxisIndex types, one per rank, into a single
//      XPacket. The strides must be input as the total / allocate size
//      for this local array. 
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int i;
    int rc = ESMF_FAILURE;
    char msgbuf[ESMF_MAXSTR];
    ESMC_AxisIndex *aip;  // shorter alias
    int diffs[ESMF_MAXDIM][2];
    ESMC_AxisIndex memspace[ESMF_MAXDIM];

    // compute the lower halo widths for each axis the hard way: 
    for (i=0; i<AIrank; i++) {
       diffs[i][0] = srcGlobalCompAIperRank[i].min - 
                     srcGlobalTotalAIperRank[i].min; 
    }

    // shift the origin that these numbers are relative to; it was (0,0)
    // was the corner of the computational area; now (0,0) is the corner of
    // memory/allocation area. 
    for (i=0; i<AIrank; i++) {
      memspace[i].min = intersectLocalCompAIperRank[i].min + diffs[i][0];
      memspace[i].max = intersectLocalCompAIperRank[i].max + diffs[i][0];

      memspace[i].stride = srcGlobalTotalAIperRank[i].max -
                           srcGlobalTotalAIperRank[i].min + 1;
    }

#if 0
          this->rank = 2;
          this->offset  = 
                  indexlist[1].min * indexlist[0].stride
                + indexlist[0].min;

          this->contig_length = indexlist[0].max - indexlist[0].min + 1;

          this->stride[0] = indexlist[0].stride;

          this->rep_count[0] = indexlist[1].max - indexlist[1].min + 1;


          this->rank = 3;
          this->offset  = 
                  indexlist[2].min * indexlist[1].stride * indexlist[0].stride
                + indexlist[1].min * indexlist[0].stride
                + indexlist[0].min;

          this->contig_length = indexlist[0].max - indexlist[0].min + 1;

          this->stride[0] = indexlist[0].stride;

          this->rep_count[0] = indexlist[1].max - indexlist[1].min + 1;

          this->stride[1] = indexlist[1].stride * indexlist[0].stride;

          this->rep_count[1] = indexlist[2].max - indexlist[2].min + 1;
#endif


    this->rank = AIrank;
    this->contig_length = memspace[0].max - memspace[0].min + 1;

    this->stride[0] = memspace[0].stride;
    for (i=1; i<rank-1; i++)
          this->stride[i] = this->stride[i-1] * memspace[i].stride;

    for (i=0; i<rank-1; i++)
          this->rep_count[i] = memspace[i+1].max - memspace[i+1].min + 1;

    this->offset = memspace[0].min;
    for (i=1; i<rank; i++)
          this->offset += memspace[i].min * this->stride[i-1];

    this->ESMC_XPacketSetContig();

    return ESMF_SUCCESS;

 } // end ESMC_XPacketFromAxisIndex


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketGlobalToLocal"
//BOP
// !IROUTINE:  ESMC_XPacketGlobalToLocal - get a local XPacket from a global one
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketGlobalToLocal(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XPacket *global_XP,     // in  - global XPacket
      ESMC_AxisIndex *indexlist,   // in  - set of local AxisIndices
      int rank,                    // in  - rank of AxisIndex array
      int *global_start) {         // in  - array of global starting numbers
                                   //       per dimension
//
// !DESCRIPTION:
//      Translates a global XPacket into a local XPacket.
//      Returns an XPacket and error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    char msgbuf[ESMF_MAXSTR];
    int i;

    // switch based on array rank  TODO: is this necessary?
    switch (rank) {
      case 1:
        sprintf(msgbuf, "no code to handle %d rank yet\n", rank);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
        return (rc);
      case 2:
        {
          int my_stride = indexlist[0].max - indexlist[0].min + 1;
          int my_row = global_XP->offset/indexlist[0].stride;
          int my_left = global_XP->offset - my_row*indexlist[0].stride 
                      - global_start[0];
          int my_right = (global_XP->offset+global_XP->contig_length)
                       - my_row*indexlist[0].stride - global_start[0];
          my_row      = my_row - global_start[1];
          this->offset  = my_row*my_stride + my_left;
          this->contig_length = my_right - my_left;
          this->stride[0] = my_stride;
          this->rep_count[0] = global_XP->rep_count[0];
        }
      break;
      case 3:
      case 4:
      case 5:
      default:
        sprintf(msgbuf, "no code to handle %d rank yet\n", rank);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
        return (rc);
    } 

    ESMC_XPacketSetContig();

    return ESMF_SUCCESS;

 } // end ESMC_XPacketGlobalToLocal

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketMinMax"
//BOPI
// !IROUTINE:  ESMC_XPacketMinMax - return min and max item number touched
//
// !INTERFACE:
      int ESMC_XPacket :: ESMC_XPacketMinMax(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *minitem,        // out  - lowest item number referenced
      int *maxitem) {      // out  - highest item number referenced
//
// !DESCRIPTION:
//    For debugging use, to check on references beyond the range of an input.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int i, j, k, l, m;
    int xpItemCount, totalRepCount;
    int itemPtr, contigItems, index[ESMF_MAXDIM];
    char *dataPtr;
    char msgbuf[ESMF_MAXSTR];

    
    // empty xpacket
    if (rep_count[0] == 0) {
        if (minitem) *minitem = 0; 
        if (maxitem) *maxitem = 0;
        return ESMF_SUCCESS;
    }

    // min is offset, unless any of the strides are negative
    //  (which is not expected).

    if (minitem) *minitem = offset;
 
    // total number of contig regions which will be moved
    totalRepCount = 1; 
    for (k=0; k<rank-1; k++) {
        totalRepCount *= rep_count[k];
        index[k] = 0;
    }
  
    // initial value for item offset, plus size of contig buffer in 
    // bytes (xp values are computed and stored as number of items)
    itemPtr = offset;
    contigItems = contig_length;
  
    // for each contig region which must be moved...
    for (k=0; k<totalRepCount-1; k++) {
  
        // increment the innermost counter 
        index[0]++;
        itemPtr += stride[0];
    
        // and now roll up the index, thru all ranks
        // at the end of a loop for one rank, roll back the pointer to
        // the start of the previous loop and add in the next stride.
        for (j=0; (j<rank-2) && (index[j] >= rep_count[j]); j++) {
            index[j] = 0;
            index[j+1]++;
            itemPtr -= (rep_count[j]*stride[j]);
            itemPtr += stride[j+1];
        }   // end of j (per-rank, "odometer-rollover" or "carry-bit") loop
    
    }     // end of k (total rep count) loop

    if (maxitem) *maxitem = itemPtr + contigItems;

    return ESMF_SUCCESS;

 } // end ESMC_XPacketMinMax



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketPrint"
//BOP
// !IROUTINE:  ESMC_XPacketPrint - Print an XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int indent,                // how far to indent
      const char *options) {     // print options
//
// !DESCRIPTION:
//     Print the contents of an XPacket
//
//EOP
// !REQUIREMENTS:  

    int i;
    char msgbuf[ESMF_MAXSTR];
    char tempbuf[ESMF_MAXSTR];

    if (indent > 0)  
        sprintf(msgbuf, "XPacket: rank=%d, offset=%d, crank=%d, clen=%d, ", 
                         rank, offset, contigrank, contig_length);
    else
        sprintf(msgbuf, "   XPacket: rank=%d, offset=%d, crank=%d, clen=%d, ", 
                             rank, offset, contigrank, contig_length);

    printf(msgbuf);
 
    sprintf(tempbuf,"strides=(");
    strcpy(msgbuf, tempbuf);
    for (i=0; i<rank-2; i++) {
      sprintf(tempbuf,"%d,", stride[i]);
      strcat(msgbuf, tempbuf);
    }
    sprintf(tempbuf,"%d), reps=(", stride[i]);
    strcat(msgbuf, tempbuf);
    for (i=0; i<rank-2; i++) {
      sprintf(tempbuf,"%d,", rep_count[i]);
      strcat(msgbuf, tempbuf);
    }
    sprintf(tempbuf,"%d)\n", rep_count[i]); 
    strcat(msgbuf, tempbuf);
    
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    return ESMF_SUCCESS;

 } // end ESMC_XPacketPrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketPrint"
//BOP
// !IROUTINE:  ESMC_XPacketPrint - Print an XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) {     // print options
//
// !DESCRIPTION:
//     Print the contents of an XPacket
//
//EOP
// !REQUIREMENTS:  

    int i;
    char msgbuf[ESMF_MAXSTR];
    char tempbuf[ESMF_MAXSTR];

    sprintf(msgbuf, "XPacket: rank=%d, offset=%d, crank=%d, clen=%d, ", 
                         rank, offset, contigrank, contig_length);
    printf(msgbuf);
 
    sprintf(tempbuf,"strides=(");
    strcpy(msgbuf, tempbuf);
    for (i=0; i<rank-2; i++) {
      sprintf(tempbuf,"%d,", stride[i]);
      strcat(msgbuf, tempbuf);
    }
    sprintf(tempbuf,"%d), reps=(", stride[i]);
    strcat(msgbuf, tempbuf);
    for (i=0; i<rank-2; i++) {
      sprintf(tempbuf,"%d,", rep_count[i]);
      strcat(msgbuf, tempbuf);
    }
    sprintf(tempbuf,"%d)\n", rep_count[i]); 
    strcat(msgbuf, tempbuf);
    
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);

    return ESMF_SUCCESS;

 } // end ESMC_XPacketPrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacketIsEmpty"
//BOP
// !IROUTINE:  ESMC_XPacketIsEmpty - Return true if xp is empty, false otherwise
//
// !INTERFACE:
      bool ESMC_XPacket::ESMC_XPacketIsEmpty(
//
// !RETURN VALUE:
//    True/False boolean return code
//
// !ARGUMENTS:
      void) {
//     none
//
// !DESCRIPTION:
//     Return true if XP describes an empty region.
//
//EOP
// !REQUIREMENTS:  

    if (rep_count[0] == 0)
        return true;
  
    return false;

 } // end ESMC_XPacketIsEmpty

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XPacket()"
//BOP
// !IROUTINE:  ESMC_XPacket - native C++ constructor
//
// !INTERFACE:
      ESMC_XPacket::ESMC_XPacket(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
	block_index = 0;
	single = 0;

 } // end ESMC_XPacket

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_XPacket()"
//BOP
// !IROUTINE:  ~ESMC_XPacket - native C++ destructor
//
// !INTERFACE:
      ESMC_XPacket::~ESMC_XPacket(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ~ESMC_XPacket
