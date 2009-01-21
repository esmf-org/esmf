// $Id: ESMC_SparseMsg.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_SparseMsg.h>
#include <ESMC_Exception.h>
#include <ESMC_ParEnv.h>
#include <mpi.h>

#include <iostream>
#include <algorithm>

namespace ESMCI {
namespace MESH {

static UInt round_to_dword(UInt size) {
  UInt dwsz = sizeof(void*)*4;
  UInt rm = size % dwsz;
  return rm ? size + (dwsz - rm) : size;
}

SparseMsg::SparseMsg() :
  outBuffers(),
  inBuffers(),
  procToOutBuffer(),
  procToInBuffer(),
  inProcs(),
  nsend(0),
  sendBuf(NULL),
  recvBuf(NULL),
  comm(Par::Comm()),
  num_incoming(0),
  sendself(false),
  self_idx(0)
{
  rank = Par::Rank();
  nproc = Par::Size();
}

SparseMsg::~SparseMsg() {
  delete [] sendBuf;
  delete [] recvBuf;
}

void SparseMsg::setPattern(UInt num, const UInt *proc) {

  UInt csize = Par::Size();

  // Set dest proc
  nsend = num;

  std::vector<int> sendto(nproc, 0);
  std::vector<int> counts(nproc, 1);
  for (UInt i = 0; i < num; i++) {
    ThrowRequire(proc[i] < csize);
    sendto[proc[i]] = 1;
    if (proc[i] == (UInt) rank) {
      sendself = true;
      self_idx = i;
    }
  }

  !Par::Serial() ? MPI_Reduce_scatter(&sendto[0], &num_incoming, &counts[0], MPI_INT, MPI_SUM, comm)
    : num_incoming = sendto[0];
//std::cout << "Proc:" << rank << "to receive " << num_incoming << " messages" << std::endl;

  // Set up send buffers (so we don't have save the proc ids
  if (nsend > 0) outBuffers.resize(nsend); else outBuffers.clear();
  for (UInt i = 0; i < nsend; i++) {
    outBuffers[i].proc = proc[i];
  }
}

void SparseMsg::setSizes(UInt *sizes) {
  // First, set up send buffers
  UInt totalsize = 0;
  // Find total buffer size; round up to word boundaries
  // for each new buffer
  for (UInt i = 0; i < nsend; i++) {
    totalsize += round_to_dword(sizes[i]);
  }

  // Allocate send buffer.  Delete in case already done
  delete [] sendBuf;

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
    procToOutBuffer[buf.proc] = &buf;
  }
//std::cout << "last buf end:" << (int) outBuffers[nsend-1].end << ", sendbuf end:" << (int) &sendBuf[cur_loc] << std::endl;

  // Second, send sizes to receive

  // avoid allocating zero (add 1)
  std::vector<MPI_Request> request(num_incoming+1, NULL);
  std::vector<MPI_Status> status(num_incoming+1);

  // Post Recieves
  std::vector<int> inSizes(num_incoming+1);
  UInt tag0 = 0;

  UInt enD = num_incoming - (sendself ? 1 : 0);

  for (UInt i = 0; i < enD; i++) {
    MPI_Irecv(&inSizes[i], 1, MPI_INT, MPI_ANY_SOURCE, tag0, comm, &request[i]);
  }

  // Sends
  for (UInt i = 0; i < nsend; i++) {
    if (!sendself || i != self_idx) {
      buffer &buf = outBuffers[i];
      MPI_Send(&(buf.msize), 1, MPI_INT, buf.proc, tag0, comm);
    }
  }

  int ret;
  if (enD > 0) {
    ret = MPI_Waitall(enD, &request[0], &status[0]);
    if (ret != MPI_SUCCESS) 
      throw("Bad MPI_WaitAll in setSizes");
  }
  // Now set up true size
  if (sendself) inSizes[enD] = outBuffers[self_idx].msize;
 
  // Third, set up receive sizes
  if (num_incoming > 0) {
    inBuffers.resize(num_incoming);
    inProcs.resize(num_incoming);
  } else {
    // We need these to have zero sizes since users iterate through them
    inBuffers.clear();
    inProcs.clear();
  }
  
  totalsize = 0;
  for (UInt i = 0; i < enD; i++) {
    totalsize += round_to_dword(inSizes[i]);
//std::cout << "P:" << rank << ", from " << status[i].MPI_SOURCE << ", size:" << inSizes[i] << std::endl;
  }
  delete [] recvBuf;

  recvBuf = new UChar[totalsize+1]; // avoid zero allocation

  
  cur_loc = 0;

  // Buffer zero is the special receive buffer from this proc.
  // Point it straight to the send buffer for this proc.  The
  // user won't even know what happened to them.
  for (UInt i = 0; i < enD; i++) {
    UInt bsize = round_to_dword(inSizes[i]);
    inBuffers[i].beg = inBuffers[i].cur = &recvBuf[cur_loc];
    inBuffers[i].end = &recvBuf[cur_loc+bsize];
    inBuffers[i].bsize = bsize;
    inBuffers[i].msize = inSizes[i];
    inBuffers[i].proc = status[i].MPI_SOURCE;
    procToInBuffer[status[i].MPI_SOURCE] = &inBuffers[i];
    inProcs[i] = status[i].MPI_SOURCE;
    cur_loc += bsize;
  }
  if (sendself) {
    UInt bsize = round_to_dword(inSizes[enD]);
    inBuffers[enD].beg = inBuffers[enD].cur = outBuffers[self_idx].beg;
    inBuffers[enD].end = outBuffers[self_idx].end;
    inBuffers[enD].bsize = bsize;
    inBuffers[enD].msize = inSizes[enD];
    inBuffers[enD].proc = rank;
    procToInBuffer[rank] = &inBuffers[enD];
    inProcs[enD] = rank;
  }

  // Sort inProcs to be conformal with CommRel
  if (num_incoming > 0) std::sort(inProcs.begin(), inProcs.end());
//std::cout << "last buf end:" << (int) inBuffers[num_incoming-1].end << ", sendbuf end:" << (int) &recvBuf[cur_loc] << std::endl;

}

void SparseMsg::communicate() {

  std::vector<MPI_Request> request(num_incoming+1, NULL);
  std::vector<MPI_Status> status(num_incoming+1);

  // Post Recieves
  UInt tag1 = 1;
  UInt enD = num_incoming - (sendself ? 1 : 0);
  for (UInt i = 0; i < enD; i++) {
    MPI_Irecv(inBuffers[i].beg, inBuffers[i].msize, MPI_BYTE, inBuffers[i].proc, tag1, comm, &request[i]);
  }

  // Sends
  for (UInt i = 0; i < nsend; i++) {
    if (!sendself || i != self_idx)
      MPI_Send(outBuffers[i].beg, outBuffers[i].msize, MPI_BYTE, outBuffers[i].proc, tag1, comm);
  }

  int ret;
  enD >0 ? ret = MPI_Waitall(enD, &request[0], &status[0]) : ret = MPI_SUCCESS;
  if (ret != MPI_SUCCESS) 
    throw("Bad MPI_WaitAll in setSizes");
  
}

bool SparseMsg::filled() {
  for (UInt i = 0; i < nsend; i++) {
    buffer &b = outBuffers[i];
    //UInt dist = b.end - b.cur;

/*
    if (dist != 0) {
        std::cout << "buffer end-cur dist =" << dist << ", msize=" << b.msize << std::endl;
    }
*/
//std::cout << "buffer cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
    if (b.cur > b.end) {
      //std::cout << "buffer overfilled!! cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
      return false;
    }

    if (b.cur != &b.beg[b.msize]) {
     // std::cout << "buffer to " << b.proc << " underfilled!! cur=" << (int) b.cur << ", end=" << (int) b.end << std::endl;
      return false;
    }
  }

  return true;
}

bool SparseMsg::empty() {
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

void SparseMsg::resetBuffers() {
  for (UInt i = 0; i < nsend; i++) {
    buffer &b = outBuffers[i];
    b.cur = b.beg;
  }
  for (UInt i = 0; i < num_incoming; i++) {
    buffer &b = inBuffers[i];
    b.cur = b.beg;
  }
}

void SparseMsg::buffer::push(const UChar * src, UInt size) {
  for (UInt i = 0; i < size; i++) {
    *cur++ = *src++;
  }
}

void SparseMsg::buffer::pop(UChar *dest, UInt size) {
  for (UInt i = 0; i < size; i++) {
    *dest++ = *cur++;
  }
}

UInt SparseMsg::commSize() {
  return Par::Size();
}

UInt SparseMsg::commRank() {
  return Par::Rank();
}

} //namespace
} //namespace
