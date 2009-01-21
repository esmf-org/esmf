// $Id: ESMC_SparseMsg.h,v 1.2.2.2 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_SparseMsg_h
#define ESMC_SparseMsg_h

#include <vector>
#include <map>

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

// 
// !USES:
#include <ESMC_VM.h>
#include <GridUtil/include/ESMC_Ptypes.h>
#include <GridUtil/include/ESMC_Exception.h>

namespace ESMC {

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC::SparseMsg_h - Sparse Message buffer communication primitive
//
// !DESCRIPTION:
//
// This class solves the 'inverse communication problem', where we know
// the data that we must send (and to whom), but we must discover what
// data we are to receive.  This object provides the declaration of sends,
// and determines the receive pattern.  Buffers are provided for copying
// data into and out of; we proceed to pack the in buffer, communicate
// the data, and then unpack it.
// Call sequence:
// --setPattern:  This uses VM_Reduce_scatter to determine
// how many messages will be coming to the current pet.
// Make sure that the proc list does not contain duplicates!!
// +++
// --setSizes: This may be called either once, or, if the object
// is reused for different sized messages, more than once.  If
// messages will always be the same size, call once.  If the pattern
// stays the same, but sizes differ, call each time, but save
// on establishing the pattern.
// ***
// --resetBuffers: call just before loading the buffer (if reusing)
// -- pack buffer
// -- ask if filled()
// --communicate: sends send bufs and loads into receive
// -- unpack buffers
// -- make sure empty()
// go back to *** if sizes not changed, or ++++ if sizes change
// If sizes change, call resetSizes, which puts keeps the pattern, but
// clears the send/receive buffers.
//
// Alternately, this object can be thought of as a finite-state machine:
// When constructed, it starts in state=BASE.
// Valide transitions in this finite state machine are:
// BASE->PATTERN(setPattern), PATTERN->SIZES(setSizes), SIZES->PATTERN(resetSizes)
// 
//
///EOP
//-----------------------------------------------------------------------------
class SparseMsg {
public:
 SparseMsg(ESMCI::VM &vm);
 ~SparseMsg();

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC::SparseMsg::buffer - buffer to store messages
//
// !DESCRIPTION:
//     This class manages the pack and unpack buffers.
//   The design is a simple UChar buffer, with beg,end delimiting
//   its extents.  The pointer cur points to the fill spot or
//   the unpack spot, depending on which role the buffer is filling.
//
///EOP
//-----------------------------------------------------------------------------
 class buffer {
 public:
   friend class SparseMsg;
   // Copy size bytes into destination.  Update current
   // location in buffer.
   void pop(UChar *dest, UInt size) {
     UChar *end = cur + size;
     while (cur != end) *dest++ = *cur++;
   }
   // Copy size bytes into buffer.  Update current
   // location in buffer.
   void push(const UChar *src, UInt size) {
     UChar *end = cur + size;
     while (cur != end) *cur++ = *src++;
   }

   // Size of message
   UInt msg_size() { return msize;}

 private:
   UChar *beg, *end, *cur;
   int msize;   // actual message size
   UInt bsize;  // allocated size (with rounding to word boundary)
   UInt pet;    // Pet this buffer goes to or comes from.
 };

 // Set up the pattern of messages.  Communicates to determine
 // How many receives to post.  
 // num = number of pets I will send to
 // proc = list of pet numbers.
 // These pet numbers should be UNIQUE!!!!
 // If the process
 void setPattern(UInt num, UInt *pets);

 // Must be called after setPattern.  Sizes both the
 // send and receive buffers.
 // First, loop the arrays and set up the send information.
 // Next, communicates (using pattern above to post IRecv's)
 // the sizes of buffers to be sent.
 // Lastly, sizes the receive buffer.
 // Num and procs must match setPattern call
 void setSizes(UInt *sizes);

 // Reset the current points on buffers.  Call before
 // Loading up data (if the buffer is to be reused with the
 // same sizes).
 void resetBuffers();

 // Remove the send buffers; puts object back into PATTEN state
 void resetSizes();

 // Get the buffer for sending to proc
 buffer *getSendBuffer(UInt pet) {
   std::map<UInt,buffer*>::iterator bi =
     petToOutBuffer.find(pet);
   if (bi == petToOutBuffer.end())
     throw Ex() << "getSendBuffer, pet:" << pet << " not found";
   return bi->second;
 }

 // Get the buffer for unpacking from proc
 buffer *getRecvBuffer(UInt pet) {
   std::map<UInt, buffer*>::iterator bi =
     petToInBuffer.find(pet);
   if (bi == petToInBuffer.end())
     throw Ex() << "getRecvBuffer, pet:" << pet << " not found";
   return bi->second;
 }

 UInt numRecv() { return num_incoming; }

 // Send out the messages, copy into recv blocks
 void communicate();

 // Iterators to incoming pet messages.  These are guarenteed to
 // be sorted by pet number.
 UInt *inPet_begin() {return &inPets[0];}
 UInt *inPet_end() {return &inPets[num_incoming];}

 // Is the send buffer filled to capacity?
 bool filled();

 // Is the receive buffer depleted?
 bool empty();

 private:

 // We keep track of the state of the object as a mechanism to 
 // ensure the user of this object doesn't incorrectly use this
 // object.
 typedef enum { BASE=0,  // Object has been created, no pattern
                PATTERN, // Pattern has been established, no sizes
                SIZE     // Size has been set
              } sparseState;   

 SparseMsg(const SparseMsg &); // disallow copying
 SparseMsg &operator=(const SparseMsg&); // disallow assignment
 ESMCI::VM &vm; // reference since one should not change the VM
              // Not const since VM is not written to enforce const.
 std::vector<buffer> outBuffers;
 std::vector<buffer> inBuffers;

 // These maps allow lookup from pet to buffer.  They could
 // be arrays, but opting for smaller memory footprint, I use a map.
 std::map<UInt, buffer*> petToOutBuffer;
 std::map<UInt, buffer*> petToInBuffer;
 
 // List of pets we are receiving from.
 std::vector<UInt> inPets;

 // Number of pets we are sending to.
 UInt nsend;

 // Pointer to the block of send buffers;
 UChar *sendBuf;
 // Pointer to the block of recv buffers;
 UChar *recvBuf;

 // Number of pets we are receiving from;
 UInt num_incoming;

 int rank, npet;

 // If one of the send procs is self, save the send idx.  The last receive block
 //  will be set aside for receiving self.
 bool sendself;
 UInt self_idx; // my index in the send buffers

 // Error checking state tracker.
 sparseState obj_state;
};

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC::SparsePack
//
// !DESCRIPTION:
//     Type safe packing object.  To pack a complex object, simply specialize
//   this class.  For instance,
//
//   template<>
//   class SparsePack<MeshObj> {
//     SparsePack(buffer &buf, MeshObj &obj) {
//     buf.push((UChar*) obj.id, sizeof(id));
//
//     // Recursively call the relationlist specialization
//     SparsePack<MeshObjRelationList>(buf, obj.Relations); 
//     }
//   };
//
///EOP
//-----------------------------------------------------------------------------
template<class T>
class SparsePack {
public:
  SparsePack(SparseMsg::buffer &buf, T t) {buf.push(reinterpret_cast<UChar*>(&t), sizeof(T));}
  SparsePack() {}
  static UInt size() { return sizeof(T);}
};

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC::SparseUnpack
//
// !DESCRIPTION:
//     Type safe unpacking object.  This object is the dual of SparsePack and
//   is used to unpack a value/object.
//
///EOP
//-----------------------------------------------------------------------------
template<class T>
class SparseUnpack {
public:
  SparseUnpack(SparseMsg::buffer &buf, T &t) {buf.pop(reinterpret_cast<UChar*>(&t), sizeof(T));}
  SparseUnpack() {}
  static UInt size() { return sizeof(T);}
};


} // namespace

#endif  // SparseMsg_h
