// $Id: ESMC_XPacket.C,v 1.20 2003/03/21 23:02:32 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC XPacket method implementation (body) file

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
 #include <ESMC.h>

// for printf
#include <stdio.h>
#include <assert.h>
 // associated class definition file
 #include <ESMC_XPacket.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
              "$Id: ESMC_XPacket.C,v 1.20 2003/03/21 23:02:32 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XPacket routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketInit - Initialize an XPacket
//
// !INTERFACE:
      int ESMC_XPacket :: ESMC_XPacketInit(
//
// !RETURN VALUE:
//     integer return code
//
// !ARGUMENTS:
      int nrank,
      int nleft,
      int nright,
      int *nstrides,
      int *nnum) {
//
// !DESCRIPTION:
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_XPacket.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    rank = nrank;
    left = nleft;
    right = nright;
    for (int i=0; i<rank; i++) {
      strides[i] = nstrides[i];
      num[i] = nnum[i];
    }

    return ESMF_SUCCESS;

 } // end ESMC_XPacketInit


//-----------------------------------------------------------------------------
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
      int *nleft,     // out, single ints
      int *nright,    // out, single ints
      int *nstrides,  // out, array of rank ints
      int *nnum) {    // out, array of rank ints
//
// !DESCRIPTION:
//     Returns the contents of XPacket member.
//
//EOP
// !REQUIREMENTS:  

    *nrank = rank;
    *nleft = left;
    *nright = right;
    for (int i=0; i<rank; i++) {
      nstrides[i] = strides[i];
      nnum[i] = num[i];
    }

    return ESMF_SUCCESS;

 } // end ESMC_XPacketGet

//-----------------------------------------------------------------------------
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

    // check that the xpacket ranks are the same
    if (xpacket1->rank != xpacket2->rank) {
      return ESMF_FAILURE;
    }
    this->rank = xpacket1->rank;

    // debug
    printf("xpacket1: ");
    xpacket1->ESMC_XPacketPrint();
    printf("xpacket2: ");
    xpacket2->ESMC_XPacketPrint();

    // check that the xpacket strides are the same
    if (this->rank > 0) { 
      for (i=0; i<xpacket1->rank-1; i++) {
        if (xpacket1->strides[i] != xpacket2->strides[i]) {
          return ESMF_FAILURE;
        }
        this->strides[i] = xpacket1->strides[i];
      }
    }

    // set left, right, and nums to zero as default
    this->left = 0;
    this->right = 0;
    if (this->rank > 0) { 
      for (i=0; i<xpacket1->rank-1; i++) {
        this->num[i] = 0;
      }
    }

    // switch based on xpacket rank  TODO: is this necessary?
    switch (this->rank) {
      case 1:
        {
          if (xpacket1->left >= xpacket2->left)
            this->left  = xpacket1->left;
          else
            this->left  = xpacket2->left;
          if (xpacket1->right <= xpacket2->left)
            this->right = xpacket1->right;
          else
            this->right = xpacket2->right;
          this->num[0] = 1;
        }
      break;
      case 2:
        {
          // implementation of efficient intersection calculation from
          // thesis by Ramaswamy
          int intersect1 = (xpacket2->left-xpacket1->right+xpacket1->strides[0]-1)
                         /  xpacket1->strides[0];  // rounding to nearest integer
          if (intersect1 < 0) intersect1 = 0;
          int intersect2 = (xpacket1->left-xpacket2->right+xpacket2->strides[0]-1)
                         /  xpacket2->strides[0];  // rounding to nearest integer
          if (intersect2 < 0) intersect2 = 0;
          int i1=intersect1;
          int L1_left  = xpacket1->left  + i1*xpacket1->strides[0];
          int L1_right = xpacket1->right + i1*xpacket1->strides[0];
          int i2 = (i1*xpacket1->strides[0] + xpacket1->left - xpacket2->right)
                 / xpacket2->strides[0];
          if (i2 < intersect2) i2 = intersect2;
          int L2_left  = xpacket2->left  + i2*xpacket2->strides[0];
          int L2_right = xpacket2->right + i2*xpacket2->strides[0];
          if (L1_left >= L2_left)
            this->left  = L1_left;
          else
            this->left  = L2_left;
          if (L1_right <= L2_right)
            this->right = L1_right;
          else
            this->right = L2_right;
          if (xpacket1->num[0]-i1 <= xpacket2->num[0]-i2) 
            this->num[0] = xpacket1->num[0]-i1;
          else
            this->num[0] = xpacket2->num[0]-i2;
          // for now, just check here for a real intersection
          if (this->left > this->right) {
            this->left = 0;
            this->right = 0;
            this->num[0] = 0;
          }
        }
      break;
      case 3:
        {
          printf("no code to handle xpacket rank %d yet\n", this->rank);
        }
      break;
      case 4:
        {
          printf("no code to handle xpacket rank %d yet\n", this->rank);
        }
      break;
      case 5:
        {
          printf("no code to handle xpacket rank %d yet\n", this->rank);
        }
      break;
    } 

    // debug
    printf("intersect: ");
    this->ESMC_XPacketPrint();

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketIntersect


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketFromAxisIndex - calculate an XPacket from an AxisIndex
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketFromAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_AxisIndex *indexlist,  // in  - set of AxisIndices
      int size_axisindex) {       // in  - size of AxisIndex array
//
// !DESCRIPTION:
//      Translates a set of AxisIndices into a corresponding XPacket.
//      Returns an XPacket and error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i;

    // switch based on array rank  TODO: is this necessary?
    switch (size_axisindex) {
      case 1:
        {
          printf("no code to handle %d AxisIndices yet\n", size_axisindex);
        }
      break;
      case 2:
        {
          int global_l[2];
          int global_r[2];
          this->rank = 2;
          // calculate global lefts and rights for the index space
          for (i=0; i<size_axisindex; i++) {
    printf("incoming AxisIndex: [%d] left=%d, right=%d, gstart=%d, max=%d\n",
     i, indexlist[i].l, indexlist[i].r, indexlist[i].gstart, indexlist[i].max);
            global_l[i] = indexlist[i].l + indexlist[i].gstart;
            global_r[i] = indexlist[i].r + indexlist[i].gstart;
          }
          this->left  = global_l[1]*indexlist[0].max + global_l[0];
          this->right = global_l[1]*indexlist[0].max + global_r[0];
          this->strides[0] = indexlist[0].max;
          this->num[0] = indexlist[1].r - indexlist[1].l + 1;
     printf("outgoing ");
     this->ESMC_XPacketPrint();
        }
      break;
      case 3:
        {
          printf("no code to handle %d AxisIndices yet\n", size_axisindex);
        }
      break;
      case 4:
        {
          printf("no code to handle %d AxisIndices yet\n", size_axisindex);
        }
      break;
      case 5:
        {
          printf("no code to handle %d AxisIndices yet\n", size_axisindex);
        }
      break;
    } 

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketFromAxisIndex


//-----------------------------------------------------------------------------
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
      ESMC_AxisIndex *indexlist,   // in  - set of AxisIndices
      int rank) {                  // in  - rank of AxisIndex array
//
// !DESCRIPTION:
//      Translates a global XPacket into a local XPacket.
//      Returns an XPacket and error code if problems are found.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;

    // switch based on array rank  TODO: is this necessary?
    switch (rank) {
      case 1:
        {
          printf("no code to handle %d rank yet\n", rank);
        }
      break;
      case 2:
        {
          int my_stride = indexlist[0].r - indexlist[0].l + 1;
          int my_row = global_XP->left/indexlist[0].max;
          int my_left = global_XP->left - my_row*indexlist[0].max
                      - indexlist[0].gstart;
          int my_right = global_XP->left - my_row*indexlist[0].max
                       - indexlist[0].gstart;
          this->left  = my_row*my_stride + my_left;
          this->right = my_row*my_stride + my_right;
          this->strides[0] = my_stride;
          this->num[0] = global_XP->num[0];
        }
      break;
      case 3:
        {
          printf("no code to handle %d rank yet\n", rank);
        }
      break;
      case 4:
        {
          printf("no code to handle %d rank yet\n", rank);
        }
      break;
      case 5:
        {
          printf("no code to handle %d rank yet\n", rank);
        }
      break;
    } 

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_XPacketGlobalToLocal


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketPrint - Print an XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketPrint(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//     none
//
// !DESCRIPTION:
//     Print the contents of an XPacket
//
//EOP
// !REQUIREMENTS:  

    int i;

    printf("XPacket: rank=%d, left=%d, right=%d, ", rank, left, right);
 
    printf("strides=(");
    for (i=0; i<rank-1; i++) 
      printf("%d,", strides[i]);
    printf("%d), num=(", strides[i]);
    for (int i=0; i<rank-1; i++) 
      printf("%d,", num[i]);
    printf("%d)\n", num[i]); 

    return ESMF_SUCCESS;

 } // end ESMC_XPacketPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketEmpty - Return true (1) if xp is empty, 0 otherwise
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketEmpty(void) {
//
// !RETURN VALUE:
//    True/False (1)/(0) return code
//
// !ARGUMENTS:
//     none
//
// !DESCRIPTION:
//     Return true if XP describes an empty region.
//
//EOP
// !REQUIREMENTS:  

    int i;

    if (num[0] == 0)
        return 1;
  
    return 0;

 } // end ESMC_XPacketEmpty

//-----------------------------------------------------------------------------
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
//BOP
// !IROUTINE:  ~ESMC_XPacket - native C++ destructor
//
// !INTERFACE:
      ESMC_XPacket::~ESMC_XPacket(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
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
