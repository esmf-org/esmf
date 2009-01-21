// $Id: ESMC_GlobalIds.C,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_GlobalIds.h>
#include <ESMC_ParEnv.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_Exception.h>

#include <mpi.h> 
#include <bitset>
 

namespace ESMCI {
namespace MESH {


void GlobalIds(const std::vector<long> &current_ids,
                          std::vector<long> &new_ids)
{
  Trace __trace("get_global_ids(const std::vector<MeshObj::id_type> &current_ids, std::vector<MeshObj::id_type> &new_ids)");
  UInt rank = Par::Rank();
  UInt nproc = Par::Size();

  // Find the open 'holes' in the numbering scheme.
  // Use a rendezvous decomposition holding the hashed bit positions to compute holes
  // and then forward them to the needy procs.

//Par::Out() << " ggi, used:" << current_ids.size() << ", needed:" << new_ids.size() << std::endl;


  // First trade how many id's are needed
  int nnew_ids_l = new_ids.size();


  std::vector<int> nnew_ids_all(nproc, 0);

  if (nproc > 1) {
    MPI_Allgather(&nnew_ids_l, 1, MPI_INT, &nnew_ids_all[0], 1, MPI_INT, Par::Comm());
  } else {
    nnew_ids_all[0] = nnew_ids_l;
  }

  // Set up the intervals (for reply, later)
  std::vector<int> new_id_disp(nproc+1, 0);
  for (UInt i = 0; i < nproc; i++) 
    new_id_disp[i+1] = new_id_disp[i] + nnew_ids_all[i];

  int nnew_ids = new_id_disp[nproc];

  if (nnew_ids == 0) return; // nothing new needed.

  // Find the possible range the new id's will live in,
  // namely 1 to cur_used + requested
  int nused_ids_l = current_ids.size();
  int nused_ids;
  // On proc zero, say that I used id 0 so that it doesn't end up being used.
  if (Par::Rank() == 0) nused_ids_l++;

  if (nproc > 1) {
    MPI_Allreduce(&nused_ids_l, &nused_ids, 1, MPI_INT, MPI_SUM, Par::Comm()); 
  } else {
    nused_ids = nused_ids_l;
  }

  // We know that there are enough free integers between 1 and:
  UInt max_new_id = nused_ids + nnew_ids;


//Par::Out() << " ggi, max_new_id:" << max_new_id << std::endl;
  // Send my local ints off
  SparseMsg msg;

  UInt ids_per_proc = (max_new_id + nproc - 1) / nproc;

  long id_base = ids_per_proc*rank; // base id for this proc

  // each bit represents the id (rank*ids_per_proc+1) + which bit
  const UInt NBITS = 8;

  // tools to divide by 8 and take rem
  const UInt SHIFT = 3;
  const ULong MASK = 0x07;

  UInt nbits_per_proc = (ids_per_proc + NBITS) / NBITS;
  std::vector<std::bitset<NBITS> > id_bits(nbits_per_proc, 0);
//Par::Out() << "ggi, nbpp=" << nbits_per_proc << std::endl;
  { // send to rendezvous
    std::vector<UInt> send_procs;
    std::vector<UInt> send_sizes_all(nproc, 0);
    for (int i = 0; i < nused_ids_l; i++ ) {
      ULong id;
      // fake id 0 on proc 0 to avoid assigning it.
      if (Par::Rank() == 0 && i == nused_ids_l-1) id = 0;
        else id = current_ids[i];
      if (id > max_new_id) continue; // don't care about this one
      UInt proc = std::min((UInt)(id / ids_per_proc), Par::Size()-1);
  
      // insert new processor (cheap map)
      std::vector<UInt>::iterator lb = std::lower_bound(send_procs.begin(), send_procs.end(), proc);
      if (lb == send_procs.end() || *lb != proc)
        send_procs.insert(lb, proc);
  
//Par::Out() << "size id=" << id << " for " << proc << std::endl;
      send_sizes_all[proc] += SparsePack<ULong>::size();

    }

    UInt nsend = send_procs.size();
  
    msg.setPattern(nsend, nsend == 0 ? NULL : &send_procs[0]);
  
    std::vector<UInt> send_sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++)
      send_sizes[i] = send_sizes_all[send_procs[i]];
  
    msg.setSizes(nsend == 0 ? NULL : &send_sizes[0]);

    // pack
    for (int i = 0; i < nused_ids_l; i++ ) {
      ULong id;
      // fake id 0 on proc 0 to avoid assigning it.
      if (Par::Rank() == 0 && i == nused_ids_l-1) id = 0;
        else id = current_ids[i];
      if (id > max_new_id) continue; // don't care about this one
      UInt proc = std::min((UInt)(id / ids_per_proc), Par::Size()-1);
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
//Par::Out() << "pack id=" << id << " for " << proc << std::endl;
      SparsePack<ULong>(b, id);
    }
    if (!msg.filled()) Throw() << "P:" << Par::Rank() << " neg id, not filled!";

    msg.communicate();

    // unpack
    for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
      SparseMsg::buffer &b = *msg.getRecvBuffer(*p);

      UInt nids = b.msg_size() / SparsePack<ULong>::size();
//std::cout << "Proc:" << Par::Rank() << " managing " << nids << " ids" << std::endl;
       
      for (UInt i = 0; i < nids; i++) {
        ULong id;
        SparseUnpack<ULong>(b, id);
        UInt bas = (id - id_base) >> SHIFT;
        UInt of = (id - id_base) & MASK;
        id_bits[bas].set(of);
//std::cout << "Proc:" << Par::Rank() << ", id=" << id << ", bas=" << bas << ", of:" << of << std::endl;
      }
    }
    if (!msg.empty()) Throw() << "neg id, not empty!";

  } // send ids to rendezvous

  // Now (locally) find unused id's
  int num_avail_l = 0;
  for (UInt i = 0; i < nbits_per_proc; i++) {
    id_bits[i].flip(); // expose holes
    num_avail_l += id_bits[i].count();
  }

  // We counted a few extra at the end:
  num_avail_l -= (nbits_per_proc*NBITS - ids_per_proc);

  //std::cout << "P:" << Par::Rank() << ", numavail=" << num_avail_l << std::endl;

  std::vector<int> num_avail(nproc);
  if (nproc > 1) {
    MPI_Allgather(&num_avail_l, 1, MPI_INT, &num_avail[0], 1, MPI_INT, Par::Comm());
  } else {
    num_avail[0] = num_avail_l;
  }

  UInt new_disp_l = 0;  // Where this proc provides avail
  for (UInt i = 0; i < rank; i++) {
    new_disp_l += num_avail[i];
  }
  UInt new_disp_end_l = new_disp_l + num_avail_l;



/*
Par::Out() << "offset table:" << std::endl;
std::copy(new_id_disp.begin(), new_id_disp.end(), std::ostream_iterator<int>(Par::Out(), " "));
Par::Out() << "my disp_l=" << new_disp_l << ", my end=" << new_disp_end_l << std::endl;
*/
  // Now pack and send ids
  {
    SparseMsg msg;
    std::vector<UInt> send_procs;
    std::vector<UInt> send_sizes_all(nproc, 0);

    
    if ( new_disp_l < (UInt) new_id_disp[nproc]) {
      for (UInt p = 0; p < nproc && new_disp_end_l > (UInt) new_id_disp[p]  ; p++) {
        if ( new_disp_l > (UInt) new_id_disp[p+1]) continue; 

        UInt nidsend = std::min(new_id_disp[p+1] - new_disp_l, (UInt) num_avail_l);

        send_procs.push_back(p);
        
        send_sizes_all[p] += nidsend; // multiply later by size of id
   
//Par::Out() << " sendinig " << nidsend << " to " << p << std::endl;
//Par::Out() << " new p+1- my loc=" << new_id_disp[p+1] - new_disp_l << std::endl;
        new_disp_l += nidsend;
        num_avail_l -= nidsend;
      } // proc
    } // if 

    UInt nsend = send_procs.size();
     msg.setPattern(nsend, nsend == 0 ? NULL : &send_procs[0]);
     std::vector<UInt> send_sizes(nsend);
     for (UInt i = 0; i < nsend; i++) 
       send_sizes[i] = send_sizes_all[send_procs[i]]*SparsePack<long>::size();

     msg.setSizes(nsend == 0 ? NULL : &send_sizes[0]);

     // Packing loop
     UInt local_offset = 0;
     for (UInt i = 0; i < nsend; i++) {
       UInt proc = send_procs[i];
       SparseMsg::buffer &b = *msg.getSendBuffer(proc);
       UInt nids = send_sizes_all[send_procs[i]];
//Par::Out() << "sproc=" << proc << " nids =" << nids << ", local off=" << local_offset << std::endl;

       for (UInt j = 0; j < nids; j++) {

         // Advance to a set bit
         UInt bas, of;
         do {
          bas = local_offset >> SHIFT;
           of = local_offset & MASK;
//std::cout << "bas,of=" << bas << "," << of << std::endl;
           if (local_offset > ids_per_proc)
             Throw() << "P:" << Par::Rank() << "num_ids_per_proc=" << ids_per_proc << " but local_offset=" << local_offset << std::endl;
           local_offset++;
//Par::Out() << "inc local offset:" << local_offset << std::endl;
         } while(!id_bits[bas].test(of));
//Par::Out() << "found bit, (bas,of) =" << bas << "," << of << std::endl;

         // Manufacture the id
         long id = id_base + local_offset - 1;
if (id == 0) Throw() << "Id is zero, why?? from proc:" << Par::Rank() << ".  id_base=" << id_base << ", loffset=" << local_offset;
         SparsePack<long>(b, id);
//Par::Out() << "sending id=" << id << std::endl;
         
       } // ids for a proc

     } // procs to send to

     if (!msg.filled())
       Throw() << "get global id, sending ids, buffer not filled!";

     msg.communicate();

     // Now unpack the ids
     std::vector<long> incoming_ids; incoming_ids.reserve(nnew_ids_l);

     UInt num_in = 0;
     for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
       UInt proc = *p;
       SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

       UInt nids = b.msg_size() / SparsePack<long>::size();
       for (UInt i = 0; i < nids; i++) {
         long id;
         SparseUnpack<long>(b, id);
//Par::Out() << "P:" << Par::Rank() << ", got id=" << id << " from P:" << proc << std::endl;
         if (num_in >= (UInt) nnew_ids_l) 
           Throw() << "P:" << Par::Rank() << " num_new ids needed:" << nnew_ids_l << ", but up to:" << num_in + 1;
         new_ids[num_in++] = id;
if (id == 0) Throw() << "Id is zero, why?? from proc:" << proc;
       } // nids
     } // inProc

     if (!msg.empty())
       Throw() << "get global ids, receiving ids, buffer not empty!";

     if (new_ids.size() != num_in)
           Throw() << "P:" << Par::Rank() << " num_new ids needed:" << nnew_ids_l << ", but only got:" << num_in;
 
  } // send new ids

  // Check: num ids in should be 

}

} // namespace
} // namespace
