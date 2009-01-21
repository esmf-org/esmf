// $Id: ESMC_SparseMsg.C,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMC_SparseMsg.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ SparseMsg methods.
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <algorithm>
#include <iostream>
#include <iterator>

// associated class definition file
#include <GridUtil/include/ESMC_SparseMsg.h>
#include <GridUtil/include/ESMC_Exception.h>
#include <ESMC_LogErr.h>
#include <ESMC_VMKernel.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_SparseMsg.C,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $";
//-----------------------------------------------------------------------------

//

namespace ESMC {

// Helper function to round up to a double word boundary
static UInt round_to_dword(UInt size) {
  UInt dwsz = sizeof(void*)*4;
  UInt rm = size % dwsz;
  return rm ? size + (dwsz - rm) : size;
}
//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::SparseMsg"
//BOPI
// !IROUTINE:  SparseMsg - Constructor
//
// !INTERFACE:
SparseMsg::SparseMsg(
//
// !RETURN VALUE:

//
// !ARGUMENTS:
     ESMCI::VM &_vm)     // Virtual machine to live in
//
// !DESCRIPTION:
//     Constructor.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
  : vm(_vm),
  outBuffers(),
  inBuffers(),
  petToOutBuffer(),
  petToInBuffer(),
  inPets(),
  nsend(0),
  sendBuf(NULL),
  recvBuf(NULL),
  num_incoming(0),
  sendself(false),
  self_idx(0),
  obj_state(BASE)
{
  rank = vm.getLocalPet();
  npet = vm.getPetCount();
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::~SparseMsg"
//BOPI
// !IROUTINE:  SparseMsg - Constructor
//
// !INTERFACE:
SparseMsg::~SparseMsg()
//
// !RETURN VALUE:

//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     Constructor.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  delete [] sendBuf;
  delete [] recvBuf;
}

#undef  METHOD
#define METHOD "ESMC::SparseMsg::setPattern"
//BOPI
// !IROUTINE:  setPattern
//
// !INTERFACE:
void SparseMsg::setPattern(
//
// !RETURN VALUE:

//
// !ARGUMENTS:
  UInt num,                   // Number of processors to send to
  UInt *pet)                 // List of processor ID's
//
// !DESCRIPTION:
//     Deduces how many messages each processor will
//   recieve in the communication pattern.  Absolutely
//   essential: the pet list contain unique entries, i.e.
//   no repeats of a pet'cessor !!!!!!!!
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{

  // Error checking
  if (obj_state != BASE) throw Ex() << "SparseMsg illegal transition from state:" << obj_state << " to PATTERN";
  // Set dest proc
  nsend = num;

  // Classic performance tradeoff here.  Do I protect the user from sending
  // in non unique pet id's and invoke a performance tradeoff?  This block
  // may be deleted, as it simply verifies that the pet's are unique in the
  // list.  TODO: create debug state, and run this only in debug mode.
  {
    std::vector<UInt> petuq;
    std::copy(pet, pet+num, std::back_inserter(petuq));
    std::sort(petuq.begin(), petuq.end());
    std::vector<UInt>::iterator ue =
      std::unique(petuq.begin(), petuq.end());
    if (ue != petuq.end()) // list not unique !!!!
    throw Ex() << METHOD << ": pet list is not unique!!!!";
  }

  std::vector<int> sendto(npet, 0);
  std::vector<int> counts(npet, 1);
  for (UInt i = 0; i < num; i++) {
    sendto[pet[i]] = 1;
    if (pet[i] == (UInt) rank) {
      sendself = true;
      self_idx = i;
    }
  }

  //MPI_Reduce_scatter(&sendto[0], &num_incoming, &counts[0], MPI_INT, MPI_SUM, comm);
  vm.reduce_scatter(&sendto[0], &num_incoming, &counts[0], vmI4, vmSUM);

  // Set up send buffers (so we don't have save the proc ids
  if (nsend > 0) outBuffers.resize(nsend); else outBuffers.clear();
  for (UInt i = 0; i < nsend; i++) {
    outBuffers[i].pet = pet[i];
  }

  obj_state = PATTERN;
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::setSizes"
//BOPI
// !IROUTINE:  setSizes - Establish the message sizes
//
// !INTERFACE:
void SparseMsg::setSizes(
//
// !RETURN VALUE:

//
// !ARGUMENTS:
  UInt *sizes)
//
// !DESCRIPTION:
//     Establish and share the size of a message between pet(i,j).  Also
//     sets aside memory for this messages in the form of send/receive 
//     buffers.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  // Error checking
  if (obj_state != PATTERN) throw Ex() << "SparseMsg illegal transition from state:" << obj_state << " to SIZE";
  // First, set up send buffers
  UInt totalsize = 0;
  // Find total buffer size; round up to word boundaries
  // for each new buffer
  for (UInt i = 0; i < nsend; i++) {
    totalsize += round_to_dword(sizes[i]);
  }

  sendBuf = new UChar[totalsize+1];

  // Set up pointers into buffer
  UInt cur_loc = 0;
  for (UInt i = 0; i < nsend; i++) {
    UInt bsize = round_to_dword(sizes[i]);
    buffer &buf = outBuffers[i];
    buf.beg = outBuffers[i].cur = &sendBuf[cur_loc];
    buf.end = &sendBuf[cur_loc+bsize];
    buf.bsize = bsize;
    buf.msize = sizes[i];
    cur_loc += bsize;
    petToOutBuffer[buf.pet] = &buf;
  }
//std::cout << "last buf end:" << (int) outBuffers[nsend-1].end << ", sendbuf end:" << (int) &sendBuf[cur_loc] << std::endl;

  // Second, send sizes to receive

  // avoid allocating zero (add 1)
  //std::vector<MPI_Request> request(num_incoming+1, NULL);
  //std::vector<MPI_Status> status(num_incoming+1);
  std::vector<ESMCI::VMK::commhandle*> commhp(num_incoming+1);
  for (UInt i = 0; i < num_incoming; i++) commhp[i] =
    new ESMCI::VMK::commhandle;
  

  // Post Recieves
  std::vector<int> inSizes(num_incoming+1);
  UInt tag0 = 0;

  UInt enD = num_incoming - (sendself ? 1 : 0);

  for (UInt i = 0; i < enD; i++) {
    //MPI_Irecv(&inSizes[i], 1, MPI_INT, MPI_ANY_SOURCE, tag0, comm, &request[i]);
    vm.recv(static_cast<void*>(&inSizes[i]), sizeof(int), VM_ANY_SRC, &commhp[i], tag0);
  }

  // Sends
  for (UInt i = 0; i < nsend; i++) {
    if (!sendself || i != self_idx) {
      buffer &buf = outBuffers[i];
      //MPI_Send(&(buf.msize), 1, MPI_INT, buf.pet, tag0, comm);
      vm.send(static_cast<void*>(&(buf.msize)), sizeof(int), buf.pet, tag0);
    }
  }

  std::vector<ESMCI::VMK::status> stat(enD);
  if (num_incoming > 0) {
    for (UInt w = 0; w < enD; w++) {
      vm.commwait(&commhp[w], &stat[w]);
    }
    //ret = MPI_Waitall(enD, &request[0], &status[0]);
    //if (ret != MPI_SUCCESS) 
     // throw("Bad MPI_WaitAll in setSizes");
  }
  // Now set up true size
  if (sendself) inSizes[enD] = outBuffers[self_idx].msize;
 
  // Third, set up receive sizes
  if (num_incoming > 0) {
    inBuffers.resize(num_incoming);
    inPets.resize(num_incoming);
  } else {
    // We need these to have zero sizes since users iterate through them
    inBuffers.clear();
    inPets.clear();
  }
  
  totalsize = 0;
  for (UInt i = 0; i < enD; i++) {
    totalsize += round_to_dword(inSizes[i]);
//std::cout << "P:" << rank << ", from " << status[i].MPI_SOURCE << ", size:" << inSizes[i] << std::endl;
  }

  recvBuf = new UChar[totalsize+1]; // avoid zero allocation

  
  cur_loc = 0;

  for (UInt i = 0; i < enD; i++) {
    UInt bsize = round_to_dword(inSizes[i]);
    inBuffers[i].beg = inBuffers[i].cur = &recvBuf[cur_loc];
    inBuffers[i].end = &recvBuf[cur_loc+bsize];
    inBuffers[i].bsize = bsize;
    inBuffers[i].msize = inSizes[i];
    inBuffers[i].pet = stat[i].srcPet;
    petToInBuffer[stat[i].srcPet] = &inBuffers[i];
    inPets[i] = stat[i].srcPet;
    cur_loc += bsize;
  }

  // Buffer enD is the special receive buffer from this proc.
  // Point it straight to the send buffer for this proc.  In this
  // manner, when we copy into the send buffer, the communication is
  // done; the receive buffer has de facto been filled.
  if (sendself) {
    UInt bsize = round_to_dword(inSizes[enD]);
    inBuffers[enD].beg = inBuffers[enD].cur = outBuffers[self_idx].beg;
    inBuffers[enD].end = outBuffers[self_idx].end;
    inBuffers[enD].bsize = bsize;
    inBuffers[enD].msize = inSizes[enD];
    inBuffers[enD].pet = rank;
    petToInBuffer[rank] = &inBuffers[enD];
    inPets[enD] = rank;
  }

  // Sort inPets so unpacking can proceed in an expected manner.
  if (num_incoming > 0) std::sort(inPets.begin(), inPets.end());

  for (UInt i = 0; i < num_incoming; i++) delete commhp[i];

  obj_state = SIZE;
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::communicate"
//BOPI
// !IROUTINE:  communicate
//
// !INTERFACE:
void SparseMsg::communicate()
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     Trade messages between each proc(i,j) in the pattern.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
 // std::vector<MPI_Request> request(num_incoming+1, NULL);
  //std::vector<MPI_Status> status(num_incoming+1);

  std::vector<ESMCI::VMK::commhandle*> commhp(num_incoming+1);
  for (UInt i = 0; i < num_incoming; i++) commhp[i] =
    new ESMCI::VMK::commhandle;

  // Post Recieves
  UInt tag1 = 1;
  UInt enD = num_incoming - (sendself ? 1 : 0);
  for (UInt i = 0; i < enD; i++) {
    //MPI_Irecv(inBuffers[i].beg, inBuffers[i].msize, MPI_BYTE, inBuffers[i].pet, tag1, comm, &request[i]);
    vm.recv(static_cast<void*>(inBuffers[i].beg), inBuffers[i].msize, inBuffers[i].pet, &commhp[i], tag1);
  }

  // Sends
  for (UInt i = 0; i < nsend; i++) {
    if (!sendself || i != self_idx)
      //MPI_Send(outBuffers[i].beg, outBuffers[i].msize, MPI_BYTE, outBuffers[i].pet, tag1, comm);
      vm.send(static_cast<void*>(outBuffers[i].beg), outBuffers[i].msize, outBuffers[i].pet, tag1);
  }

  for (UInt w = 0; w < enD; w++) {
      vm.commwait(&commhp[w]);
  }
  //ret = MPI_Waitall(enD, &request[0], &status[0]);
  //if (ret != MPI_SUCCESS) 
   // throw("Bad MPI_WaitAll in setSizes");
  for (UInt i = 0; i < num_incoming; i++) delete commhp[i];
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::filled"
//BOPI
// !IROUTINE:  filled
//
// !INTERFACE:
bool SparseMsg::filled()
//
// !RETURN VALUE:
//    bool : true = message filled to buffer size, false = not filled
//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     It is good practice to call this message after filling the buffer, since
//     a correctly filled buffer will be filled.  If this returns false,
//     there is an incorrect-ness between your sizing and packing routines.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  for (UInt i = 0; i < nsend; i++) {
    buffer &b = outBuffers[i];
//std::cout << "buffer cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
    if (b.cur > &b.beg[b.msize]) {
      //std::cout << "buffer overfilled!! cur=" << (int) b.cur << ", capacity at=" << (int) &b.beg[b.msize] << std::endl;
      return false;
    }

    if (b.cur != &b.beg[b.msize]) {
      //std::cout << "buffer underfilled!! cur=" << (int) b.cur <<
       //     ", end=" << (int) b.end << " capacity at:" << (int) &b.beg[b.msize] << std::endl;
      return false;
    }
  }

  return true;
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::empty"
//BOPI
// !IROUTINE:  empty
//
// !INTERFACE:
bool SparseMsg::empty()
//
// !RETURN VALUE:
//    bool : true = message completely unpacked, false = not unpacked
//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     It is good practice to call this message after unpacking the buffer.
//     If you have not taken the correct amount of data out of the buffer,
//     there is an error between your sizing send and the deduced unpack size.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  for (UInt i = 0; i < num_incoming; i++) {
    buffer &b = inBuffers[i];
//std::cout << "buffer cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
    if (b.cur > b.end) {
      //std::cout << "buffer overpicked!! cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
      return false;
    }

    if (b.cur != &b.beg[b.msize]) {
      //std::cout << "buffer underpicked!! cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
      return false;
    }
  }

  return true;
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::resetBuffers"
//BOPI
// !IROUTINE:  resetBuffers
//
// !INTERFACE:
void SparseMsg::resetBuffers()
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     Call this if you will use the message pattern/sizes to send
//     a new message.  It resets the cur in the buffer pointers.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  for (UInt i = 0; i < nsend; i++) {
    buffer &b = outBuffers[i];
    b.cur = b.beg;
  }
  for (UInt i = 0; i < num_incoming; i++) {
    buffer &b = inBuffers[i];
    b.cur = b.beg;
  }
}

//-----------------------------------------------------------------------------
#undef  METHOD
#define METHOD "ESMC::SparseMsg::resetSizes"
//BOPI
// !IROUTINE:  resetSizes
//
// !INTERFACE:
void SparseMsg::resetSizes()
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
// !DESCRIPTION:
//     Removes the buffers and puts the object back into the pattern
//     state.
//
//EOPI
// !REQUIREMENTS:  
//-----------------------------------------------------------------------------
{
  inPets.clear();
  inBuffers.clear();
  petToInBuffer.clear();

  delete [] sendBuf;
  delete [] recvBuf;

  obj_state = PATTERN;
}

} // namespace

