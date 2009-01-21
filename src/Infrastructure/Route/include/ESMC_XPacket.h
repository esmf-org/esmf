// $Id: ESMC_XPacket.h,v 1.42.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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
 #include <ESMC_LocalArray.h>

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

 enum ESMC_PackUnpackFlag { ESMC_BUFFER_PACK, ESMC_BUFFER_UNPACK};

 // class declaration type
 class ESMC_XPacket {    // does *not* inherit from base.

   private:
     // one of these blocks describes an N-d hyperslab of data
     // embedded in a larger N-d array.
     
     int rank;                       // dimensionality of hyperslab
     int offset;                     // item offset from base of memory block
     int contig_length;              // #contig items in fastest varying dim
     int contigrank;                 // highest rank for which block is contig

     // TODO: we are going to need an index number for the case where
     // multiple addresses are passed into each call to the run routine.
     // (for example, loose bundles with multiple fields, or fields
     // with multiple logical blocks)?  if multiple addresses are passed in,
     // the list must be in the same order at Store() time as at Run() time.
     // proposed new member of the xpacket would be 'block_index' to indicate
     // the address index this xpacket applies to.  (if the block don't have
     // identical index orders, or ranks, or whatever, we may have to
     // compute a separate xpacket per block.)
  
     int block_index;               // if multiple addrs, which address

     // TODO: there exists the case in loose bundles where all the fields
     // contain identical data layouts, in all aspects:  scalar/vector,
     // data rank, data type, interleave, index order, etc.  if so, we can
     // build xpackets based on inspecting data from the first field/block 
     // and then at run time apply it to all fields/blocks sequentially.  
     // proposed new member of the xpacket would be 'congruent'. 

     bool congruent;                // if true, xpackets apply to all addrs

     // TODO: stride and rep_count are really only ESMF_MAXDIM-1 because
     // contig_length implicitly stores the first rep_count, and the first
     // stride is always 1 item.  if we fix this in the class, we need to
     // find and fix all interfaces which get and set those values and
     // make sure they are also declared maxdim-1.

     int stride[ESMF_MAXDIM];        // number of items to skip in each dim-1
     int rep_count[ESMF_MAXDIM];     // repeat count for each dim-1

     int single;                      // flag for single element     
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_XPacketSetDefault(int rank, int offset, int contig_length, 
                               int *stride, int *rep_count);

    int ESMC_XPacketCopy(ESMC_XPacket *xpacket);

 // accessor methods for class members
    int ESMC_XPacketGet(int *nrank, int *noffset, int *ncontig_length, 
                        int *nstride, int *nrep_count, int *bufindex);
    //int ESMC_XPacketSet(<value type>  value);

 // get/set methods for internal data
    int ESMC_XPacketSetEmpty(void);

    int ESMC_XPacketSetOffset(int offset) { this->offset = offset;
                                            return ESMF_SUCCESS; }

    int ESMC_XPacketGetOffset(void) { return this->offset; }
    int ESMC_XPacketSetIndex(int index) { this->block_index=index;
                                          return ESMF_SUCCESS;}
    int ESMC_XPacketGetIndex(void) { return this->block_index;}
    // returns true if entire xp describes a single chunk of mem
    bool ESMC_XPacketIsContig(void) { return (this->contigrank == this->rank); }

    // all slabs up to this rank number are contig.  (since the contig_length
    // describes at least the first dim and it has an implicit stride of 1,
    // the minimum rank that can be returned here is 1.)
    int ESMC_XPacketGetContigRank(void) { return this->contigrank; }

    // computes the contig rank based on the rank, strides, rep_count 
    // alread in the xpacket.
    void ESMC_XPacketSetContig(void);

    bool ESMC_XPacketIsEmpty();      // returns true if the xp is empty
    bool ESMC_XPacketIsSingle() { return (this->single==1); }

 // miscellaneous fun stuff
    int ESMC_XPacketIntersect(ESMC_XPacket *xpacket1, 
                              ESMC_XPacket *xpacket2);
    int ESMC_XPacketGlobalToLocal(ESMC_XPacket *global_XP,
                                  ESMC_AxisIndex *indexlist, int rank,
                                  int *global_start);
    int ESMC_XPacketFromCompAIs(int AIrank,
                                ESMC_AxisIndex *intersectLocalCompAIperRank,
                                ESMC_AxisIndex *srcGlobalCompAIperRank,
                                ESMC_AxisIndex *srcGlobalTotalAIperRank);
                                // TODO: change name here Total->Alloc

    int ESMC_XPacketMinMax(int *minitems, int *maxitems);
    int ESMC_XPacketPrint(const char *options);
    int ESMC_XPacketPrint(int indent=0, const char *options="");

 // native C++ constructors/destructors
	ESMC_XPacket(void);
	~ESMC_XPacket(void);
  
  
    friend int ESMC_XPacketFromAxisIndex(struct ESMC_AxisIndex *indexlist,
                                         int size_axisindex, int *global_count,
                                         ESMC_Logical (*boundary)[2],
                                         ESMC_XPacket **xp_list, int *xp_count);
    friend int ESMC_XPacketMakeBuffer(int xpCount, ESMC_XPacket **xpList,
                                      int nbytes, int numAddrs, char **buffer,
                                      int *bufferSize);
    friend int ESMC_XPacketPackBuffer(int xpCount, ESMC_XPacket **xpList,
      ESMC_TypeKind dk, int nbytes, int numAddrs, void **dataAddr,
      char *buffer);
    friend int ESMC_XPacketUnpackBuffer(int xpCount, ESMC_XPacket **xpList,
      ESMC_TypeKind dk, int nbytes, char *buffer, int numAddrs,
      void **dataAddr);
    friend int ESMC_XPacketDoBuffer(ESMC_PackUnpackFlag packflag, 
                                    int xpCount, ESMC_XPacket **xpList,
                                    int nbytes, int numAddrs, void **dataAddr,
                                    char *buffer);

    friend int ESMC_XPacketGetEmpty(int *nrank, int *noffset, 
                                    int *ncontig_length, int *nstride, 
                                    int *nrep_count, int *bufindex);

// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_XPacket

 // non-class method - creates a list of xp's to return
    int ESMC_XPacketFromAxisIndex(struct ESMC_AxisIndex *indexlist,
                                  int size_axisindex, int *global_count,
                                  ESMC_Logical (*boundary)[2],
                                  ESMC_XPacket **xp_list, int *xp_count);
    int ESMC_XPacketMakeBuffer(int xpCount, ESMC_XPacket **xpList,
                               int nbytes, int numAddrs, 
                               char **buffer, int *bufferSize);
    int ESMC_XPacketPackBuffer(int xpCount, ESMC_XPacket **xpList, 
      ESMC_TypeKind dk, int nbytes, int numAddrs, void **dataAddr, 
      char *buffer);
    int ESMC_XPacketUnpackBuffer(int xpCount, ESMC_XPacket **xpList,
      ESMC_TypeKind dk, int nbytes, int numAddrs, char *buffer, 
      void **dataAddr);
    int ESMC_XPacketDoBuffer(ESMC_PackUnpackFlag packflag, 
                             int xpCount, ESMC_XPacket **xpList,
                             int nbytes, int numAddrs, 
                             void **dataAddr, char *buffer);
    int ESMC_XPacketGetEmpty(int *nrank, int *noffset, int *ncontig_length, 
                             int *nstride, int *nrep_count, int *bufindex);

 #endif  // ESMC_XPacket_H
