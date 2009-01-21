// $Id: ESMC_DistDir.C,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $
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
#define ESMC_FILENAME "ESMC_DistDir.h"
//==============================================================================
//
// ESMC DistDir method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Distributed directory implementation.
//
//-----------------------------------------------------------------------------

#include <GridUtil/include/ESMC_DistDir.h>
#include <GridUtil/include/ESMC_SparseMsg.h>


#include <limits>
#include <map>
#include <iostream>
#include <algorithm>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_DistDir.C,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $";
//-----------------------------------------------------------------------------


namespace ESMC {

template<typename id_type, typename hash>
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC::DistDir::DistDir"
//BOP
// !IROUTINE:  ESMC::DistDir
//
// !INTERFACE:
DistDir<id_type,hash>::DistDir(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
ESMCI::VM &_vm,           // Virtual machine for this object
UInt ngid,              // number of global id's to register
const id_type gid[],    // array of the global id's
const id_type lid[])    // local id's corresponding to the registered gid's
//
//
// !DESCRIPTION:
//     Create a distributed directory object.
//    
//EOP
//-----------------------------------------------------------------------------
: vm(_vm),
hash_func(),
gmin(0), gmax(0),
npet(0), rank(0),
my_managed()
{
  // Set up the structure by passing my gid's to the pet'cessor who
  // manages them.

  {
    int npet_i, rank_i; // signed to unsigned helpers
    npet_i = vm.getPetCount();
    rank_i = vm.getLocalPet();
    npet = npet_i; rank = rank_i;
  }
  
  // Find local min,max and global
  id_type lmin = std::numeric_limits<id_type>::max(),
       lmax = 0;
  for (UInt i = 0; i < ngid; i++) {
    // If this is a signed type, make sure the value is positive.
    if (is_unsigned<id_type>::value && gid[i] < 0) throw Ex() << "DistDir: gid[" << i << "]=" << gid[i] << " < 0.  Only unsigned values implemented.";
    if (gid[i] < lmin) lmin = gid[i];
    if (gid[i] > lmax) lmax = gid[i];
  }

  {
    int gmin_i, gmax_i;
    // The expensive part of the algorithm.  Declare the minimum and maximum
    // overall pets of the id's.
    vm.allreduce(&lmin, &gmin_i, 1, vmI4, vmMIN);
    vm.allreduce(&lmax, &gmax_i, 1, vmI4, vmMAX);
    //MPI_Allreduce(&lmin, &t_gmin, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
    //MPI_Allreduce(&lmax, &t_gmax, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
    gmin = gmin_i; gmax = gmax_i; // Using UInt for correctness.
  }

  // Loop gids, set up sends
  std::vector<UInt> to_pet; // pets I will send to

  std::vector<UInt> send_sizes_all(npet, 0);
  for (UInt i = 0; i < ngid; i++) {
    UInt tpet = hash_func(gid[i], npet, gmin, gmax);
    std::vector<UInt>::iterator lb = std::lower_bound(to_pet.begin(), to_pet.end(), tpet);
    if (lb == to_pet.end() || *lb != tpet)
      to_pet.insert(lb, tpet);
    // gid
    send_sizes_all[tpet] += SparsePack<id_type>::size();
    // lid 
    send_sizes_all[tpet] += SparsePack<id_type>::size();
  }

  UInt nsend = to_pet.size();
  std::vector<UInt> sizes(nsend, 0);
  for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_pet[i]];
  send_sizes_all.clear();  // free memory

  SparseMsg msg(vm);

  msg.setPattern(nsend, nsend > 0 ? &to_pet[0] : NULL);

  msg.setSizes(nsend > 0 ? &sizes[0] : NULL);

  // Now pack
  for (UInt i = 0; i < ngid; i++) {
    UInt pet = hash_func(gid[i], npet, gmin, gmax);
    SparseMsg::buffer &b = *msg.getSendBuffer(pet);
    // gid, lid
    SparsePack<id_type>(b, gid[i]);
    SparsePack<id_type>(b, lid[i]);
  }

  if (!msg.filled()) throw Ex() << "Message not filled, P:" << rank << ", DistDir()";

  msg.communicate();

  // Now unpack
  for (UInt *p = msg.inPet_begin(); p != msg.inPet_end(); ++p) {
   UInt pet = *p;
   SparseMsg::buffer &b = *msg.getRecvBuffer(pet);

   // Deduce the message size
   UInt nmsg = b.msg_size() / (2*SparsePack<id_type>::size());
   for (UInt m = 0; m < nmsg; m++) {
     dentry d;
     SparseUnpack<id_type>(b, d.gid);
     SparseUnpack<id_type>(b, d.origin_lid);
     d.origin_pet = pet;

     my_managed.push_back(d);
   }
  }

  if (!msg.empty()) throw Ex() << "DistDir, msg unpack didnt empty buffer!";

  // Now, sort the list
  std::sort(my_managed.begin(), my_managed.end(), std::less<dentry>());
  

}

template<typename id_type, typename hash>
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC::DistDir::Print"
//BOP
// !IROUTINE:  ESMC::Print
//
// !INTERFACE:
void DistDir<id_type,hash>::Print()
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
//
// !DESCRIPTION:
//     Print the contents of the directory.  I'm not really crazy about this 
//   function, and we probably will eventually make it better or nuke it.  
//   For instance, we should probably be overload << for an arbitrary
//   ostream instead of printing to std::cout.
//   But for now it will help for debugging.  
//
//EOP
//-----------------------------------------------------------------------------
{
  std::cout << "P:" << rank << " manages:" << std::endl;

  UInt nent = my_managed.size();

  for (UInt i = 0; i < nent; i++) {
    dentry &de = my_managed[i];
    std::cout << "P:" << rank << "(gid=" << de.gid << ", lid=" << de.origin_lid << ", op=" << de.origin_pet << ")" << std::endl;
  }
}

template<typename id_type, typename hash>
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC::DistDir::RemoteGID"
//BOP
// !IROUTINE:  ESMC::RemoteGID
//
// !INTERFACE:
void DistDir<id_type,hash>::RemoteGID(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
UInt ngid,                 // number of global id's 
const id_type gid[],       // list of global id's to query
UInt orig_pet[],           // (out) pet of queryied indices
id_type lid[],             // (out) local id's of queried indices
bool id_found[])              // (out) true=found, false=not in directory
//
//
// !DESCRIPTION:
//     Query information about a list of global id's.  Return the origin pet
//   and the original local id.
//    
//EOP
//-----------------------------------------------------------------------------
{
  // Cache the local requests (after communication)
  std::vector<dentry> requests;
  UInt req_size;

  { // Encapsulate send phase of requests.
    // Loop gids, set up sends
    std::vector<UInt> to_pet; // pets I will send to
  
    std::vector<UInt> send_sizes_all(npet, 0);
    for (UInt i = 0; i < ngid; i++) {
      UInt tpet = hash_func(gid[i], npet, gmin, gmax);
      std::vector<UInt>::iterator lb = std::lower_bound(to_pet.begin(), to_pet.end(), tpet);
      if (lb == to_pet.end() || *lb != tpet)
        to_pet.insert(lb, tpet);
      // gid
      send_sizes_all[tpet] += SparsePack<id_type>::size();
    }
  
  
    UInt nsend = to_pet.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_pet[i]];
    send_sizes_all.clear();  // free memory
  
    SparseMsg msg(vm);
  
    msg.setPattern(nsend, nsend > 0 ? &to_pet[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < ngid; i++) {
      UInt pet = hash_func(gid[i], npet, gmin, gmax);
      SparseMsg::buffer &b = *msg.getSendBuffer(pet);
      // gid, lid, origin_pet
      SparsePack<id_type>(b, gid[i]);
    }
  
    if (!msg.filled()) throw Ex() << "RemoteGID, Message not filled, P:" << rank << ", DistDir()";
  
    msg.communicate();
  
    // Now unpack
    for (UInt *p = msg.inPet_begin(); p != msg.inPet_end(); ++p) {
     UInt pet = *p;
     SparseMsg::buffer &b = *msg.getRecvBuffer(pet);
  
     // Deduce the message size
     UInt nmsg = b.msg_size() / (1*SparsePack<id_type>::size());
     for (UInt m = 0; m < nmsg; m++) {
       dentry d;
       SparseUnpack<id_type>(b, d.gid);
       d.req_pet = pet;
  
       requests.push_back(d);
     }
    }
  
    if (!msg.empty()) throw Ex() << "RemoteGID, DistDir, msg unpack didnt empty buffer!";
  
  } // Phase 1 complete
  
    // Service the requests
  req_size = requests.size();
  for (UInt r = 0; r < req_size; r++) {
    dentry &req = requests[r];
    typename std::vector<dentry>::iterator ei = std::lower_bound(my_managed.begin(), my_managed.end(), req, dentry_less());
    // If we hit the end of the list, or if our first hit is a gid larger than our gid,
    // the directory doesn't contain the requested gid.
    if (ei == my_managed.end() || req.gid != ei->gid) { 
       req.valid = false;
    } else {
      dentry &ser = *ei;
      // valid request; update the info.
      req.origin_lid = ser.origin_lid;
      req.origin_pet = ser.origin_pet;
    }
  }

  // Build the reply.  Important: the ordering (per pet) of requests is the same
  // as the original request gid's.  When we send back, we use this same
  // ordering so that we can unpack straight into the request buffer.
  { // encapsulate the return message
    SparseMsg msg(vm);
    std::vector<UInt> to_pet; // pets I will send to

    std::vector<UInt> send_sizes_all(npet, 0);
    for (UInt i = 0; i < req_size; i++) {
      dentry &req = requests[i];
      UInt tpet = req.req_pet; // back to requestor
      std::vector<UInt>::iterator lb = std::lower_bound(to_pet.begin(), to_pet.end(), tpet);
      if (lb == to_pet.end() || *lb != tpet)
        to_pet.insert(lb, tpet);
      // lid
      send_sizes_all[tpet] += SparsePack<id_type>::size();
      // origin pet
      send_sizes_all[tpet] += SparsePack<UInt>::size();
      // Valid == 1, Invalid == 0.  Send as a UInt, because I have
      // no idea what a practical binary representation for a bool is.
      send_sizes_all[tpet] += SparsePack<UInt>::size();
    }

  
    UInt nsend = to_pet.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_pet[i]];
    send_sizes_all.clear();  // free memory

    msg.setPattern(nsend, nsend > 0 ? &to_pet[0] : NULL);
  
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);
  
    // Now pack
    for (UInt i = 0; i < req_size; i++) {
      dentry &req = requests[i];
      UInt pet = req.req_pet; // we send back to requestor
      SparseMsg::buffer &b = *msg.getSendBuffer(pet);
      // lid, origin_pet,valid
      SparsePack<id_type>(b, req.origin_lid);
      SparsePack<UInt>(b, req.origin_pet);
      UInt valid = req.valid ? 1 : 0;
      SparsePack<UInt>(b, valid);
    }
  
    if (!msg.filled()) throw Ex() << "RemoteGID, returning requests, Message not filled, P:" << rank << ", DistDir()";
  
    msg.communicate();

    // Now unpack.  This unpacking is a bit wierd.  We loop the original gid's,
    // and then get the pet'cessor number so that we know which buffer to pick from
    // next.  In this way, we unpack the buffers in the correct order so that the
    // requested gid's line up with our receive order.

    for (UInt i = 0; i < ngid; i++) {
      UInt pet = hash_func(gid[i], npet, gmin, gmax);
      SparseMsg::buffer &b = *msg.getRecvBuffer(pet);
      // lid
      SparseUnpack<id_type>(b, lid[i]);
      // origin pet
      SparseUnpack<UInt>(b, orig_pet[i]);
      // valid
      UInt valid;
      SparseUnpack<UInt>(b, valid);
      id_found[i] = (valid == 1 ? true : false);
    }

    if (!msg.empty()) throw Ex() << "RemoteGID, returning requests, DistDir, msg unpack didnt empty buffer!";

  } // Done returning the message

}

template<typename id_type, typename hash>
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC::DistDir::~DistDir"
//BOP
// !IROUTINE:  ESMC::~DistDir
//
// !INTERFACE:
DistDir<id_type,hash>::~DistDir()
//
// !RETURN VALUE:
//    
// !ARGUMENTS:
//
// !DESCRIPTION:
//    
//EOP
//-----------------------------------------------------------------------------
{
  // Love STL.  Nothing to do
}

// Instantiate the standard candidates.  To implement a new type,
// simply write the instantiation here.
template class DistDir<UInt, DistDir_hash<UInt> >;
template class DistDir<int, DistDir_hash<int> >;

} // namespace

