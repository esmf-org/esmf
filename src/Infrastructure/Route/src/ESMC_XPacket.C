// $Id: ESMC_XPacket.C,v 1.8 2003/03/12 18:53:28 nscollins Exp $
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

 // associated class definition file
 #include <ESMC_XPacket.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
              "$Id: ESMC_XPacket.C,v 1.8 2003/03/12 18:53:28 nscollins Exp $";
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
