// $Id: ESMC_XPacket.h,v 1.5 2003/03/11 01:48:02 cdeluca Exp $
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
 #include <ESMC_Route.h> 
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
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_XPacketConfig;
 class ESMC_XPacket;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_XPacketConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_XPacket : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     int destid;             // one XPacket per destination?
     int chunkcount;
      
     // linked list of these, one per chunk:
     void *base_addr;        // this one I'm still not sure of
     int rank;               // do you think each xpacket should have
                             // chunks with different rank?  it almost
                             // infers different data being moved, instead
                             // of parts of the same array, but maybe that's
                             // reasonable and more general
//   ESMC_MemAxis tuple;     the memaxis stuff, based on rank
     
     ESMC_XPacket *nextp;

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
 // ESMC_XPacketCreate and ESMC_XPacketDestroy are declared below,
 // outside the ESMC_XPacket declaration
    int ESMC_XPacketConstruct(int arg1);      // internal only, deep class
    int ESMC_XPacketDestruct(void);           // internal only, deep class

 // optional configuration methods
    int ESMC_XPacketGetConfig(ESMC_XPacketConfig *config) const;
    int ESMC_XPacketSetConfig(const ESMC_XPacketConfig *config);

 // accessor methods for class members
    //int ESMC_XPacketGet(<value type> *value) const;
    //int ESMC_XPacketSet(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_XPacketValidate(const char *options) const;
    int ESMC_XPacketPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_XPacket(void);
	~ESMC_XPacket(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_XPacket

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_XPacket object itself. E.g. if Create
// were a method, the ESMC_XPacket object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_XPacket object.

 ESMC_XPacket *ESMC_XPacketCreate(int arg1, int *rc);// interface only, deep class
 int ESMC_XPacketDestroy(ESMC_XPacket *xpacket); // interface only, deep class

 #endif  // ESMC_XPacket_H
