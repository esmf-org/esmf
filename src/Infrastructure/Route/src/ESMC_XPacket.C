// $Id: ESMC_XPacket.C,v 1.49 2005/02/28 16:39:54 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
              "$Id: ESMC_XPacket.C,v 1.49 2005/02/28 16:39:54 nscollins Exp $";
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
#define ESMC_METHOD "ESMC_XPacketGet"
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
      int *nrank,     // out, single ints
      int *noffset,     // out, single ints
      int *ncontig_length,    // out, single ints
      int *nstride,  // out, array of rank ints
      int *nrep_count) {    // out, array of rank ints
//
// !DESCRIPTION:
//     Returns the contents of XPacket member.
//
//EOP
// !REQUIREMENTS:  

    *nrank = rank;
    *noffset = offset;
    *ncontig_length = contig_length;
    for (int i=0; i<rank; i++) {
      nstride[i] = stride[i];
      nrep_count[i] = rep_count[i];
    }

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
      int *nrank,             // in/out, single int
      int *noffset,           // out, single int
      int *ncontig_length,    // out, single int
      int *nstride,           // out, array of rank ints
      int *nrep_count) {      // out, array of rank ints
//
// !DESCRIPTION:
//     Returns the values of what an empty XPacket would return.
//     nrank must come in with the max rank valid for the
//     strides and rep_counts. 
//
//EOPI
// !REQUIREMENTS:  

    int i;

    for (i=0; i<*nrank; i++) {
      nstride[i] = 0;
      nrep_count[i] = 0;
    }

    // after using nrank, set it to 0 (and all the rest)
    *nrank = 0;
    *noffset = 0;
    *ncontig_length = 0;

    return ESMF_SUCCESS;

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
      int VMType,                    // in  - datatype
      int nbytes,                    // in  - number of bytes per item
      char **buffer,                 // out - new buf, must be del'd by caller 
      int *bufferSize) {             // out - size of the buffer
//
// !DESCRIPTION:
//      Makes a buffer with the space required to hold data from a list of XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
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

    // loop over the number of XPs, adding the size of the data to the buffer size
    for (i=0; i<xpCount; i++) {
      rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                      stride, repCount);

      xpDataSize = contigLength;
      for (j=0; j<rank-1; j++) {
        xpDataSize *= repCount[j];
      }
      *bufferSize += xpDataSize;
    }

    *bufferSize *= nbytes;

    // allocate the buffer
    if (*bufferSize > 0) {
      *buffer = new char[*bufferSize];
    } else {
      *buffer = NULL;
    }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketMakeBuffer


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
// !ARGUMENTS:
      int xpCount,                   // in  - count of xp's to pack
      ESMC_XPacket **xpList,         // in  - list of xp's to pack
      int VMType,                    // in  - datatype
      int nbytes,                    // in  - number of bytes per item
      void *dataAddr,                // in  - address of the data to be packed
      char *buffer) {                // in  - buffer to pack into
//
// !DESCRIPTION:
//    Packs an already allocated buffer with data described by one or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k;
    int xpItemCount, itemCount;
    int rank, offset, contigLength, stride[ESMF_MAXDIM], repCount[ESMF_MAXDIM];
    char *dataPtr;
    char msgbuf[ESMF_MAXSTR];

    // make it a no-op to ask to pack an empty xp list
    if ((xpCount == 0) || (xpList == NULL))
        return ESMF_SUCCESS;

    // loop over the XPs, packing the data into the buffer
    for (i=0; i<xpCount; i++) {
      rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                      stride, repCount);

      // check the number of data items to be packed from this XP -- mostly
      // making sure it's not 0
      xpItemCount = xpList[i] ? contigLength : 0;
      for (j=0; j<rank-1; j++) {
        xpItemCount *= repCount[j];
      }

      // copy into the buffer
      if (xpItemCount > 0) {
        switch (rank) {
          case 2:
            itemCount = offset;
            for (j=0; j<repCount[0] ; j++, itemCount += stride[0]) {

              dataPtr = (char *)dataAddr + (itemCount*nbytes);
              memcpy(buffer, dataPtr, contigLength*nbytes);
              buffer += contigLength*nbytes;
            }
            break;
          case 3:
            for (k=0; k<repCount[1]; k++, offset += stride[1]) {
              itemCount = offset;
              for (j=0; j<repCount[0]; j++, itemCount += stride[0]) {

                dataPtr = (char *)dataAddr + (itemCount*nbytes);
                memcpy(buffer, dataPtr, contigLength*nbytes);
                buffer += contigLength*nbytes;
              }
            }
            break;
          default:
            sprintf(msgbuf, "no code to handle rank %d yet\n", rank);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                                  msgbuf, &rc);
            return (rc);
        }   // rank switch
      }     // check for non-zero data items from this XP
    }       // loop over XPs

    rc = ESMF_SUCCESS;
    return rc;

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
      int xpCount,                   // in  - count of xp's to unpack
      ESMC_XPacket **xpList,         // in  - list of xp's to unpack
      int VMType,                    // in  - datatype
      int nbytes,                    // in  - number of bytes per item
      char *buffer,                  // in  - raw buffer to unpack
      void *dataAddr) {              // in  - base addr of array to be unpacked into
//
// !DESCRIPTION:
//    Unpacks an already filled buffer into a data array described by one 
//    or more XPs.
//
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k;
    int xpItemCount, itemCount;
    int rank, offset, contigLength, stride[ESMF_MAXDIM], repCount[ESMF_MAXDIM];
    char *dataPtr;
    char msgbuf[ESMF_MAXSTR];

    // make it a no-op to unpack an empty XP list
    if ((xpCount == 0) || (xpList == NULL))
        return ESMF_SUCCESS;

    // loop over the XPs, unpacking the data from the buffer into the data array
    for (i=0; i<xpCount; i++) {
      rc = xpList[i]->ESMC_XPacketGet(&rank, &offset, &contigLength,
                                      stride, repCount);

      // check the number of data items to be packed from this XP -- mostly
      // making sure it's not 0
      xpItemCount = xpList[i] ? contigLength : 0;
      for (j=0; j<rank-1; j++) {
        xpItemCount *= repCount[j];
      }

      // copy into the data array
      if (xpItemCount > 0) {
        switch (rank) {
          case 2:
            itemCount = offset;
            for (j=0; j<repCount[0] ; j++, itemCount += stride[0]) {

              dataPtr = (char *)dataAddr + (itemCount*nbytes);
              memcpy(dataPtr, buffer, contigLength*nbytes);
              buffer += contigLength*nbytes;
            }
            break;
          case 3:
            for (k=0; k<repCount[1]; k++, offset += stride[1]) {
              itemCount = offset;
              for (j=0; j<repCount[0]; j++, itemCount += stride[0]) {

                dataPtr = (char *)dataAddr + (itemCount*nbytes);
                memcpy(dataPtr, buffer, contigLength*nbytes);
                buffer += contigLength*nbytes;
              }
            }
            break;
          default:
            sprintf(msgbuf, "no code to handle rank %d yet\n", rank);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                                  msgbuf, &rc);
            return (rc);
        }   // rank switch
      }     // check for non-zero data items from this XP
    }       // loop over XPs

    rc = ESMF_SUCCESS;
    return rc;

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

    rc = ESMF_SUCCESS;
    return rc;

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
    char msgbuf[ESMF_MAXSTR];

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

    rc = ESMF_SUCCESS;
    return rc;

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

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketGlobalToLocal


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

    sprintf(msgbuf, "XPacket: rank=%d, offset=%d, contig_length=%d, ", 
                         rank, offset, contig_length);
    printf(msgbuf);
 
    sprintf(tempbuf,"strides=(");
    strcpy(msgbuf, tempbuf);
    for (i=0; i<rank-2; i++) {
      sprintf(tempbuf,"%d,", stride[i]);
      strcat(msgbuf, tempbuf);
    }
    sprintf(tempbuf,"%d), rep_count=(", stride[i]);
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
#define ESMC_METHOD "ESMC_XPacketEmpty"
//BOP
// !IROUTINE:  ESMC_XPacketEmpty - Return true (1) if xp is empty, 0 otherwise
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketEmpty(
//
// !RETURN VALUE:
//    True/False (1)/(0) return code
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

    int i;

    if (rep_count[0] == 0)
        return 1;
  
    return 0;

 } // end ESMC_XPacketEmpty

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
