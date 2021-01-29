// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_DDir.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>

#include <limits>
#include <map>
#include <iostream>
#include <algorithm>
#include <mpi.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

template<typename HASH>
DDir<HASH>::DDir() :
hash_func(),
my_managed()
{
}

template<typename HASH>
DDir<HASH>::DDir(UInt ngid, const UInt gid[], const UInt lid[]) :
hash_func(),
my_managed()
{
  Create(ngid, gid, lid);
}

template<typename HASH>
void DDir<HASH>::Create(UInt ngid, const UInt gid[], const UInt lid[]) 
{

  // Delete anything, in case we are setting up again.
  // Set up the structure by passing my gid's to the processor who
  // needs them.
  std::vector<dentry>().swap(my_managed);

  int csize, rank;
  MPI_Comm_size(Par::Comm(), &csize);
  MPI_Comm_rank(Par::Comm(), &rank);

  // Find local min,max and global
  int lmin = std::numeric_limits<UInt>::max(),
       lmax = 0;
  for (UInt i = 0; i < ngid; i++) {
    if (gid[i] < (UInt) lmin) lmin = gid[i];
    if (gid[i] > (UInt) lmax) lmax = gid[i];
  }

  int t_gmin, t_gmax;
  MPI_Allreduce(&lmin, &t_gmin, 1, MPI_INT, MPI_MIN, Par::Comm());
  MPI_Allreduce(&lmax, &t_gmax, 1, MPI_INT, MPI_MAX, Par::Comm());
  gmin = t_gmin; gmax = t_gmax;

  // Loop gids, set up sends
  std::vector<UInt> to_proc; // procs I will send to

  std::vector<UInt> send_sizes_all(csize, 0);
  for (UInt i = 0; i < ngid; i++) {
    UInt tproc = hash_func(gid[i], csize, gmin, gmax);
    to_proc.push_back(tproc);
//std::cout << "P:" << rank << " to proc=" << tproc << std::endl;
    // gid
    send_sizes_all[tproc] += SparsePack<UInt>::size();
    // lid 
    send_sizes_all[tproc] += SparsePack<UInt>::size();
//std::cout << "tprocsize:" << send_sizes_all[tproc] << std::endl;
  }

  // Uniq the proc list
  std::sort(to_proc.begin(), to_proc.end());
  to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());

  UInt nsend = to_proc.size();
  std::vector<UInt> sizes(nsend, 0);
  for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
  send_sizes_all.clear();  // free memory

  SparseMsg msg;

  msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);

  msg.setSizes(nsend > 0 ? &sizes[0] : NULL);

  // Now pack
//std::vector<UInt> sizest(nsend, 0);
  for (UInt i = 0; i < ngid; i++) {
    UInt proc = hash_func(gid[i], csize, gmin, gmax);
    SparseMsg::buffer &b = *msg.getSendBuffer(proc);
    // gid, lid, origin_proc
    SparsePack<UInt>(b, gid[i]);
    SparsePack<UInt>(b, lid[i]);
//sizest[proc] += sizeof(UInt)*3;
  }
/*
for (UInt i = 0; i < nsend; i++) {
if (sizes[i] != sizest[i]) std::cout << "sizes mismatch:" << sizes[i] << ", " << sizest[i] << std::endl;
}
*/

  if (!msg.filled()) Throw() << "Message not filled, P:" << rank << ", DDir()";

  msg.communicate();

  // Now unpack
  for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
   UInt proc = *p;
   SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

   // Deduce the message size
   UInt nmsg = b.msg_size() / (2*SparsePack<UInt>::size());
   for (UInt m = 0; m < nmsg; m++) {
     dentry d;
     SparseUnpack<UInt>(b, d.gid);
     SparseUnpack<UInt>(b, d.origin_lid);
     d.origin_proc = proc;

     my_managed.push_back(d);
   }
  }

  if (!msg.empty()) Throw() << "DDir, msg unpack didnt empty buffer!";

  // Now, sort the list
  std::sort(my_managed.begin(), my_managed.end(), std::less<dentry>());
  
  my_managed.erase(std::unique(my_managed.begin(), my_managed.end()), my_managed.end());
  

}

template<typename HASH>
void DDir<HASH>::Print(std::ostream &os) {
  int csize, rank;
  MPI_Comm_size(Par::Comm(), &csize);
  MPI_Comm_rank(Par::Comm(), &rank);

  os << "P:" << rank << " manages:" << std::endl;

  UInt nent = my_managed.size();

  for (UInt i = 0; i < nent; i++) {
    dentry &de = my_managed[i];
    os << "(gid=" << de.gid << ", lid=" << de.origin_lid << ", op=" << de.origin_proc << std::endl;
  }
}

// A less function that only cares about gid.  Picks off first instance of gid
template<typename HASH>
class dentry_less : public std::binary_function<typename DDir<HASH>::dentry,typename DDir<HASH>::dentry,bool> {
public:
  dentry_less() {}
  bool operator()(const typename DDir<HASH>::dentry &l, const typename DDir<HASH>::dentry& r) {
    return l.gid < r.gid;
  }
};


template<typename HASH>
void DDir<HASH>::RemoteGID(UInt ngid, const UInt gid[], UInt orig_proc[], UInt lid[]) {

  // First, forward the requests
  int csize = Par::Size(), rank = Par::Rank();


  // ONly thing escaping the first phase, belo
  std::vector<dentry> requests;
  UInt req_size;

  { // temporary namespace 
    // Loop gids, set up sends
    std::vector<UInt> to_proc; // procs I will send to
  
    std::vector<UInt> send_sizes_all(csize, 0);
    for (UInt i = 0; i < ngid; i++) {
      UInt tproc = hash_func(gid[i], csize, gmin, gmax);
      to_proc.push_back(tproc);
      // gid
      send_sizes_all[tproc] += SparsePack<UInt>::size();
    }
  
    // Uniq the proc list
    std::sort(to_proc.begin(), to_proc.end());
    to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());
  
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
    send_sizes_all.clear();  // free memory
  
    SparseMsg msg;
  
    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < ngid; i++) {
      UInt proc = hash_func(gid[i], csize, gmin, gmax);
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
      // gid, lid, origin_proc
      SparsePack<UInt>(b, gid[i]);
    }
  
    if (!msg.filled()) Throw() << "RemoteGID, Message not filled, P:" << rank << ", DDir()";
  
    msg.communicate();
  
    // Now unpack
    for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
     UInt proc = *p;
     SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
  
     // Deduce the message size
     UInt nmsg = b.msg_size() / (1*SparsePack<UInt>::size());
     for (UInt m = 0; m < nmsg; m++) {
       dentry d;
       SparseUnpack<UInt>(b, d.gid);
       d.req_proc = proc;
  
       requests.push_back(d);
     }
    }
  
    if (!msg.empty()) Throw() << "RemoteGID, DDir, msg unpack didnt empty buffer!";
  
  } // delete msg
  
    // Service the requests
  req_size = requests.size();
  for (UInt r = 0; r < req_size; r++) {
    dentry &req = requests[r];
    typename std::vector<dentry>::iterator ei = std::lower_bound(my_managed.begin(), my_managed.end(), req, dentry_less<HASH>());
    if (ei == my_managed.end()) Throw() << "processor=" << rank << " could not find gid=" << req.gid<<" even though it's the processor that should contain it. It's likely that that gid isn't in the directory.";
    dentry &ser = *ei;
    if (req.gid != ser.gid) Throw() << "P:" << rank << " could not service request, gids not equal:"
                     << req.gid << ", " << ser.gid << std::endl;
    req.origin_lid = ser.origin_lid;
    req.origin_proc = ser.origin_proc;
  }

  // Build the reply.  Important: the ordering (per proc) of requests is the same
  // as the original request gid's.  When we send back, we use this same
  // ordering so that we can unpack straight into the request buffer.
  {
    SparseMsg msg;
    std::vector<UInt> to_proc; // procs I will send to

    std::vector<UInt> send_sizes_all(csize, 0);
    for (UInt i = 0; i < req_size; i++) {
      dentry &req = requests[i];
      UInt tproc = req.req_proc; // back to requestor
      to_proc.push_back(tproc);
      // lid
      send_sizes_all[tproc] += SparsePack<UInt>::size();
      // origin proc
      send_sizes_all[tproc] += SparsePack<UInt>::size();
      //send_sizes_all[tproc] += SparsePack<UInt>::size();
    }

    // Uniq the proc list
    std::sort(to_proc.begin(), to_proc.end());
    to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());
  
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
    send_sizes_all.clear();  // free memory

    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < req_size; i++) {
      dentry &req = requests[i];
      UInt proc = req.req_proc; // we send back to requestor
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
      // lid, origin_proc
      SparsePack<UInt>(b, req.origin_lid);
      SparsePack<UInt>(b, req.origin_proc);
      //SparsePack<UInt>(b, req.gid);
    }
  
    if (!msg.filled()) Throw() << "RemoteGID, returning requests, Message not filled, P:" << rank << ", DDir()";
  
    msg.communicate();

    // Now unpack.  This unpacking is a bit wierd.  We loop the original gid's,
    // and then get the processor number so that we know which buffer to pick from
    // next.  In this way, we unpack the buffers in the correct order so that the
    // requested gid's line up with our receive order.

    for (UInt i = 0; i < ngid; i++) {
      UInt proc = hash_func(gid[i], csize, gmin, gmax);
      SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
      // lid
      SparseUnpack<UInt>(b, lid[i]);
      // origin proc
      SparseUnpack<UInt>(b, orig_proc[i]);
      //SparseUnpack<UInt>(b, lgid);
      
//if (lgid != gid[i]) std::cout << "P:" << rank << ", lgid=" << lgid << ", gid=" << gid[i] << std::endl;
    }

    if (!msg.empty()) Throw() << "RemoteGID, returning requests, DDir, msg unpack didnt empty buffer!";
  

  } // msg

}

template<typename HASH>
void DDir<HASH>::RemoteGID(UInt ngid, const UInt gid[], std::vector<dentry> &response, bool some_dest) {

  response.clear();
  
  std::vector<dentry> tresponse;
  
  // First, forward the requests
  int csize = Par::Size(), rank = Par::Rank();

  // ONly thing escaping the first phase, belo
  std::vector<dentry> requests;
  UInt req_size;
  
  { // temporary namespace 
    // Loop gids, set up sends
    std::vector<UInt> to_proc; // procs I will send to
  
    std::vector<UInt> send_sizes_all(csize, 0);
    for (UInt i = 0; i < ngid; i++) {
      UInt tproc = hash_func(gid[i], csize, gmin, gmax);
      
      std::vector<UInt>::iterator lb =
        std::lower_bound(to_proc.begin(), to_proc.end(), tproc);
      
      if (lb == to_proc.end() || *lb != tproc)
        to_proc.insert(lb, tproc);
      
      // gid
      send_sizes_all[tproc] += SparsePack<UInt>::size();
    }
  
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];

    SparseMsg msg;
  
    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < ngid; i++) {
      UInt proc = hash_func(gid[i], csize, gmin, gmax);
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
      // gid, lid, origin_proc
      SparsePack<UInt>(b, gid[i]);
    }
  
    if (!msg.filled()) Throw() << "RemoteGID, Message not filled, P:" << rank << ", DDir()";
  
    msg.communicate();
  
    // Now unpack
    for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
     UInt proc = *p;
     SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
  
     // Deduce the message size
     UInt nmsg = b.msg_size() / (1*SparsePack<UInt>::size());
     for (UInt m = 0; m < nmsg; m++) {
       dentry d;
       SparseUnpack<UInt>(b, d.gid);
       d.req_proc = proc;
       requests.push_back(d);
     }
    }
  
    if (!msg.empty()) Throw() << "RemoteGID, DDir, msg unpack didnt empty buffer!";
  
  } // delete msg
  
  // Service the requests
  req_size = requests.size();
  for (UInt r = 0; r < req_size; r++) {
    dentry &req = requests[r];
    typename std::vector<dentry>::iterator ei = std::lower_bound(my_managed.begin(), my_managed.end(), req, dentry_less<HASH>());
    
    // Build a response
    while (ei != my_managed.end() && ei->gid == req.gid) {
      
      dentry ser = *ei;
      ser.req_proc = req.req_proc;
      
      // Use the response vector locally to build replies (clear later before
      // receiving response)
      tresponse.push_back(ser);

      ++ei; 
    }
    
  }

  // Build the reply.  Important: the ordering (per proc) of requests is the same
  // as the original request gid's.  When we send back, we use this same
  // ordering so that we can unpack straight into the request buffer.
  
  UInt res_size = tresponse.size();
  
  {
    SparseMsg msg;
    std::vector<UInt> to_proc; // procs I will send to
    
    std::vector<UInt> send_sizes_all(csize, 0);
    
    for (UInt i = 0; i < res_size; i++) {
      
      dentry &req = tresponse[i];
      UInt tproc = req.req_proc; // back to requestor
      
      std::vector<UInt>::iterator plb = 
        std::lower_bound(to_proc.begin(), to_proc.end(), tproc);
      
      if (plb == to_proc.end() || *plb != tproc)
        to_proc.insert(plb, tproc);
      
      // gid
      send_sizes_all[tproc] += SparsePack<UInt>::size();
      // lid
      send_sizes_all[tproc] += SparsePack<UInt>::size();
      // origin proc
      send_sizes_all[tproc] += SparsePack<UInt>::size();
      
    }
  
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
    send_sizes_all.clear();  // free memory

    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < res_size; i++) {
      
      dentry &req = tresponse[i];
      
      UInt proc = req.req_proc; // we send back to requestor
      
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
      
      // gid, lid, origin_proc
      SparsePack<UInt>(b, req.gid);
      SparsePack<UInt>(b, req.origin_lid);
      SparsePack<UInt>(b, req.origin_proc);
      
    }
  
    if (!msg.filled()) Throw() << "RemoteGID, returning requests, Message not filled, P:" << rank << ", DDir()";
  
    msg.communicate();

    tresponse.clear();
    
    // Now unpack
    for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
     UInt proc = *p;
     SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

     while (!b.empty()) {
       
       dentry res;
       
       // gid, lid, origin_proc
       SparseUnpack<UInt>(b, res.gid);
       SparseUnpack<UInt>(b, res.origin_lid);
       SparseUnpack<UInt>(b, res.origin_proc);
       
       tresponse.push_back(res);
       
     }
     
    }
  
    if (!msg.empty()) Throw() << "RemoteGID, DDir, msg unpack didnt empty buffer!";

  } // msg

  // Order the responses so they line up with gids.  
  // TODO: this could be deduced in the unpacking phase, with some effort.
  // Would be slightly more efficient.
  std::sort(tresponse.begin(), tresponse.end());
  
  tresponse.erase(std::unique(tresponse.begin(), tresponse.end()), tresponse.end());
  
  response.reserve(tresponse.size());
  
  for (UInt i = 0; i < ngid; i++) {
    
    dentry d(gid[i], 0, 0);
//Par::Out() << "add_gid=" << gid[i] << std::endl;     
    typename std::vector<dentry>::iterator dlb = 
      std::lower_bound(tresponse.begin(), tresponse.end(), d);
    
    // If some_dest is true and we can't find a place to send this gid,
    // then just send it back to here.
    if (some_dest && (dlb == tresponse.end() || dlb->gid != gid[i])) {
      response.push_back(dentry(gid[i], 0, Par::Rank(), 0));
    }
    
    while (dlb != tresponse.end() && dlb->gid == gid[i]) {
//Par::Out() << "\tadding_gid " << dlb->gid << std::endl;
      response.push_back(*dlb);
      ++dlb;
    }
    
  } // i
  
}

template<typename HASH>
DDir<HASH>::~DDir() {
  // Love stl.  Nothing to do
}

template <typename HASH>
void DDir<HASH>::clear() {
  std::vector<dentry>().swap(my_managed);
}


// ****** instantiations

template class DDir<DDir_lin_hash>;

} // namespace ESMCI
