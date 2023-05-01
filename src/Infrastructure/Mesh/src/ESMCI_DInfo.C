// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <limits>
#include <string>
#include <ostream>
#include <iterator>

#include <iostream>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "VM/include/ESMC_VM.h"

#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <Mesh/include/ESMCI_DInfo.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;
using namespace std;

// Set of templated calls to support different id types in allreduce
template<class IDTYPE>
static void _get_global_minmax(IDTYPE lmin, IDTYPE lmax, IDTYPE &t_gmin, IDTYPE &t_gmax) {
  Throw() << "Unsupported id type.";
}

template<>
static void _get_global_minmax(int lmin, int lmax, int &t_gmin, int &t_gmax) {
  MPI_Allreduce(&lmin, &t_gmin, 1, MPI_INT, MPI_MIN, Par::Comm());
  MPI_Allreduce(&lmax, &t_gmax, 1, MPI_INT, MPI_MAX, Par::Comm());
}

// Set of templated call to calculate proc from id and global info
template<class IDTYPE>
static UInt _calc_proc_from_id(IDTYPE id, UInt petCount, IDTYPE gmin, IDTYPE gmax) {
  //std::cout << "id ="<<id<<" petCount="<<petCount<<" gmin="<<gmin<<" gmax="<<gmax<<std::endl;

  UInt num_per_proc = ((gmax-gmin+1) + petCount -1) / petCount;
  UInt proc = (id-gmin) / num_per_proc;
  return proc > petCount-1 ? petCount-1 : proc; // If it's bigger than the largest Pet, then just return the largest
}



// Set up DInfo clas to be searched
template<class IDTYPE, class INFOTYPE>
void DInfo<IDTYPE, INFOTYPE> :: commit() {

  // if already committed, then leave
  if (is_committed) return;

  // Get comm info
  // TODO: Switch this to VM based calls, but need to see if that jives with sparse message stuff
  int petCount=Par::Size();
  int localPet=Par::Rank();

  // Find local min,max and global
  IDTYPE lmin = std::numeric_limits<IDTYPE>::max(), lmax = 0;
  for (auto i = 0; i < staging.size(); i++) {
    if (staging[i].id < lmin) lmin = staging[i].id;
    if (staging[i].id > lmax) lmax = staging[i].id;
  }
  
  // Get global min and max id
  _get_global_minmax(lmin, lmax, gmin, gmax);
  
  // Loop ids, set up sends
  std::vector<UInt> to_proc; // procs I will send to
  std::vector<UInt> send_sizes_all(petCount, 0);
  for (auto i = 0; i < staging.size(); i++) {
    UInt tproc = _calc_proc_from_id(staging[i].id, petCount, gmin, gmax);

    // Add to send list 
    to_proc.push_back(tproc);
      
    // Add size to send
    // send_sizes_all[tproc] += sizeof(IDTYPE);
    // send_sizes_all[tproc] += sizeof(INFOTYPE);
    send_sizes_all[tproc] += SparsePack<IDTYPE>::size(); 
    send_sizes_all[tproc] += SparsePack<INFOTYPE>::size(); 
  }

  // Uniq the proc list
  std::sort(to_proc.begin(), to_proc.end());
  to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());

  // Calc sizes going to each processor
  UInt nsend = to_proc.size();
  std::vector<UInt> sizes(nsend, 0);
  for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
  send_sizes_all.clear();  // free memory

  // Set up communication pattern
  SparseMsg msg;
  msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  msg.setSizes(nsend > 0 ? &sizes[0] : NULL);


  // Now pack entries to send
  for (auto i = 0; i < staging.size(); i++) {
    UInt tproc = _calc_proc_from_id(staging[i].id, petCount, gmin, gmax);

    // Get buffer for tproc
    SparseMsg::buffer &b = *msg.getSendBuffer(tproc);
    
    // Pack data to send
    SparsePack<IDTYPE>(b, staging[i].id);
    SparsePack<INFOTYPE>(b, staging[i].info);; 
  }

  // Complain if we don't have the amount of data we expect
  if (!msg.filled()) Throw() << "Message not filled, P:" << localPet << ", DDir()";

  // Empty the staging vector to save memory
  std::vector<DInfoEntry>().swap(staging);

  // Communicate data
  msg.communicate();

  // Calc. size of one packed entry
  UInt packed_entry_size = SparsePack<IDTYPE>::size() +  SparsePack<INFOTYPE>::size(); 
  
  // Now unpack
  for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
   UInt proc = *p;
   SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

   // Deduce the message size
   UInt num_msgs = b.msg_size()/packed_entry_size;
   for (auto m = 0; m < num_msgs; m++) {
     IDTYPE id;
     INFOTYPE info;
     SparseUnpack<IDTYPE>(b, id);
     SparseUnpack<INFOTYPE>(b, info);

     // NOT DOING, BUT KEEPING IN CASE WE NEED TO LATER
     //  d.origin_proc = proc;
     
     searchable.push_back(DInfoEntry(id,info));
   }
  }

  // I didn't unpack as much as I expected, so complain
  if (!msg.empty()) Throw() << "DInfo unpack did not empty buffer!";

  // Sort the search list
  std::sort(searchable.begin(), searchable.end(), std::less<DInfoEntry>());
  
  // Get rid of repeated entries
  searchable.erase(std::unique(searchable.begin(), searchable.end()), searchable.end());
  
  
  // Mark as committed
  is_committed=true;
}

//template<> void DInfo<int, double> :: commit();

// Get information from a distributed info directory. Must
// be called in unison across the current VM. Since the search involves communication
// it's more efficient to do it in a large chunk like this. 
// in:
//    num_search - the number of ids to search
//    search_ids - the ids to search (of size num_search)
// out:
//    search_info - the info at each id (of size num_search)
// 
template<class IDTYPE, class INFOTYPE>
void DInfo<IDTYPE, INFOTYPE> :: search(int num_search,
                                       IDTYPE *search_ids,
                                       INFOTYPE *out_search_info) {
#undef  ESMC_METHOD
#define ESMC_METHOD "DInfo::search()"

  // Error return codes
  int localrc;

}

  
// Print searchable list
// (for debugging mostly)
template<class IDTYPE, class INFOTYPE>
void DInfo<IDTYPE, INFOTYPE> :: print_searchable() {

  
  // std::cout << Par::Rank() << "#  size="<<searchable.size()<<std::endl;
  // std::cout << std::endl;
  
  // Output list
  for (auto d: searchable) {
    std::cout << Par::Rank() << "#  id="<<d.id<<" info="<<d.info<<std::endl;
  }
  
}

//template<> void DInfo<int, double> :: print_searchable();

// Explicitly Instantiate some versions of this class for testing, etc.
template class DInfo<int, double>;



