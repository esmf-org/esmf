// $Id: ESMC_XPacket.h,v 1.10 2003/03/12 20:33:44 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF XPacket C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_XPacket_H
 #define ESMC_XPacket_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_Array.h>

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_XPacket - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ XPacket members and declares method 
// signatures (prototypes).  The companion file ESMC_XPacket.C contains
// the definitions (full code bodies) for the XPacket methods.
//
// XPackets are used to give the source and destination locations for
//  calls to Route - they allow scatter/gather behavior, or copy to
//  intermediate contiguous buffers, depending on the machine characteristics.
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_XPacket;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_XPacket {    // does *not* inherit from base.

   private:
     // one of these per memory transfer

     int rank;
     int left;
     int right;
     int strides[ESMF_MAXDIM];
     int num[ESMF_MAXDIM];
     
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_XPacketInit(int rank, int left, int right, int *strides, int *num);

 // accessor methods for class members
    int ESMC_XPacketGet(int *nrank, int *nleft, int *nright, 
                                                int *nstrides, int *nnum);
    //int ESMC_XPacketSet(<value type>  value);

 // miscellaneous fun stuff
    int ESMC_XPacketIntersect(struct ESMC_XPacket *xpacket1, 
                              struct ESMC_XPacket *xpacket2);
    int ESMC_XPacketTransFromAxisIndex(struct ESMC_AxisIndex *indexlist,
                                       int size_axisindex);
 
 // native C++ constructors/destructors
	ESMC_XPacket(void);
	~ESMC_XPacket(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_XPacket


 #endif  // ESMC_XPacket_H
