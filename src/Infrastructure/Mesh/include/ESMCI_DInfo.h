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

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_DInfo_H
#define ESMCI_DInfo_H

#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "VM/include/ESMC_VM.h"

#include <limits>
#include <string>
#include <ostream>
#include <iterator>
#include <iostream>
#include <vector>

#include "ESMCI_CoordSys.h"
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>




//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


//==============================================================================
//
// DInfo: A templated distributed information class used for setting and getting information
// associated with a numeric id. The id should be relatively
// dense between it's min and max, because the ids are spread evenly across
// procs between it's min and max. The IDTYPE AND INFOTYPE classes need to
// have both == (equality) and < (less than) operators declared. 
//
//==============================================================================



using namespace ESMCI;

template <class IDTYPE, class INFOTYPE>
class DInfo {


  //private:
public:

  class DInfoEntry{
  public:
    IDTYPE id;
    INFOTYPE info;

    DInfoEntry(IDTYPE _id, INFOTYPE _info): id(_id), info(_info) {}

    // Less 
    bool operator<(const DInfoEntry &rhs) const {
      if (id != rhs.id) {
        return id < rhs.id;
      } 
      return info < rhs.info;
    }

    // Do we need this? 
    bool operator==(const DInfoEntry &rhs) const {
      return (id == rhs.id &&
              info == rhs.info);
    }

    
  };
  
  std::vector<DInfoEntry> staging; // A place to accumulate ids and info before setting
                                   // up for searching
  std::vector<DInfoEntry> searchable; // After committing this will contain a sorted list
                                      // for searching

  bool is_committed; // has this DInfo object been committed, so it's searchable
  IDTYPE gmin,gmax; // global min and max of id in staging (only valid when committed)

  
public:
  
  // Full constructor
  DInfo(): is_committed(false) {}

  // Reserve space in staging area
  void reserve(int _num) {
    staging.reserve(_num);
  }

  // Set up to enable searching
  void commit();

  // Add an entry
  void add(IDTYPE _id, INFOTYPE _info) {
    staging.push_back(DInfoEntry(_id,_info));
  }


  // Reset back to empty (pre-commit) state, but like a vector hold onto any memory that happens to be there
  void clear() {
    staging.clear();
    searchable.clear();
    is_committed=false;
  }
  
  
  // Search info in directory
  // (Because the search involves communication it's more efficient to do it in a chunk))
  void search(int num_search,
              IDTYPE *search_ids,
              bool  error_on_not_found, 
              INFOTYPE not_found_info_val,
              INFOTYPE *out_search_info);
  
  // Output searchable list (probably mostly for debugging)
  void print_searchable();
  
};


////
//// IMPLEMENTATION OF ABOVE METHODS
//// (Because this is a templated class that I'd like to be useful with a
////  general set of template types, the whole implementation needs to be
////  in the header file, so the compiler can see it wherever it's used.)


// Set of templated calls to support different id types in allreduce
template<class IDTYPE>
void DINFO_get_global_minmax(IDTYPE lmin, IDTYPE lmax, IDTYPE &t_gmin, IDTYPE &t_gmax) {
  Throw() << "Unsupported id type.";
}

template<>
void DINFO_get_global_minmax(int lmin, int lmax, int &t_gmin, int &t_gmax) {
  MPI_Allreduce(&lmin, &t_gmin, 1, MPI_INT, MPI_MIN, Par::Comm());
  MPI_Allreduce(&lmax, &t_gmax, 1, MPI_INT, MPI_MAX, Par::Comm());
}

// Set of templated call to calculate proc from id and global info
template<class IDTYPE>
UInt DINFO_calc_proc_from_id(IDTYPE id, UInt petCount, IDTYPE gmin, IDTYPE gmax) {
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
  DINFO_get_global_minmax(lmin, lmax, gmin, gmax);
  
  // Loop ids, set up sends
  std::vector<UInt> to_proc; // procs I will send to
  std::vector<UInt> send_sizes_all(petCount, 0);
  for (auto i = 0; i < staging.size(); i++) {

    // Figure out which proc this id would be on
    UInt tproc = DINFO_calc_proc_from_id(staging[i].id, petCount, gmin, gmax);

    // Add to send list 
    to_proc.push_back(tproc);
      
    // Add size to send
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
  std::vector<UInt>().swap(send_sizes_all); // free memory

  // Set up communication pattern
  SparseMsg msg;
  msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
  msg.setSizes(nsend > 0 ? &sizes[0] : NULL);


  // Now pack entries to send
  for (auto i = 0; i < staging.size(); i++) {

    // Figure out which proc this id would be on
    UInt tproc = DINFO_calc_proc_from_id(staging[i].id, petCount, gmin, gmax);

    // Get buffer for tproc
    SparseMsg::buffer &b = *msg.getSendBuffer(tproc);
    
    // Pack data to send
    SparsePack<IDTYPE>(b, staging[i].id);
    SparsePack<INFOTYPE>(b, staging[i].info);; 
  }

  // Complain if we don't have the amount of data we expect
  if (!msg.filled()) Throw() << "In DInfo class message buffer unexpectedly not filled";

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

  // Sort the search list to enable fast searching later
  //  std::sort(searchable.begin(), searchable.end(), std::less<DInfoEntry>()); // I don't think we need to specify less
  std::sort(searchable.begin(), searchable.end());
  
  // Get rid of repeated entries
  searchable.erase(std::unique(searchable.begin(), searchable.end()), searchable.end());
  
  
  // Mark as committed
  is_committed=true;
}


// A less function object that only cares about id. Used below in lower_bound search by just id.
template <class IDTYPE, class INFOTYPE>
class DInfoEntry_just_id_less : public std::binary_function<typename DInfo<IDTYPE, INFOTYPE>::DInfoEntry,typename  DInfo<IDTYPE, INFOTYPE>::DInfoEntry, bool> {
public:
  DInfoEntry_just_id_less() {}
  bool operator()(const typename DInfo<IDTYPE, INFOTYPE>::DInfoEntry &l, const typename DInfo<IDTYPE, INFOTYPE>::DInfoEntry &r) {
    return l.id < r.id;
  }
};


// Get information from a distributed info directory. Must
// be called in unison across the current VM. Since the search involves communication
// it's more efficient to do it in a large chunk like this.
// 
// NOTES:
//     + This interface is designed to work in a situation where there
//       are not repeated ids with different info. If such a cases exists
//       and this method is searching for it, then it will return an error.
//       (For searching with repeated entries see the other search method)
//      
//     + If no entry exists for a given id, then this method won't fill
//       the corresponding entry in the output. This can be detected
//       by setting the info array to a value before passing it in. 
//   
// in:
//    num_search - the number of ids to search
//    search_ids - the ids to search (of size num_search)
//    error_on_not_found - if true, throw an error if any search id isn't found. 
//    not_found_info_val - if error_on_not_found=false, the value that unfilled info entries will be set to (e.g. if an id isn't found). 
//   
// out:
//    search_info - the info at each id (of size num_search)
// 
// TODO: NEED TO ADD ERROR IF REPEATED ENTRIES!!! 
template<class IDTYPE, class INFOTYPE>
void DInfo<IDTYPE, INFOTYPE> :: search(int num_search,
                                       IDTYPE *search_ids,
                                       bool  error_on_not_found, 
                                       INFOTYPE not_found_info_val,
                                       INFOTYPE *out_search_info) {
#undef  ESMC_METHOD
#define ESMC_METHOD "DInfo::search()"


  // If not committed, then complain
  if (!is_committed) {
    Throw() << "DInfo object hasn't been committed, so unable to search it.";
  }
  
  // This search doesn't support repeated ids, so make sure there aren't any
  if (searchable.size() > 1) {

    // Loop starting with second entry checking if any ids are the same, but info isn't
    for (auto i=1; i<searchable.size(); i++) {
      if ((searchable[i-1].id == searchable[i].id) &&
          !(searchable[i-1].info == searchable[i].info)) {
        Throw() << "This search method doesn't support repeated ids with different associated info.";
      }
    }
  }

  
  // Get info about current VM
  int petCount = Par::Size();
  int localPet = Par::Rank();
  
  // Declare entries to be sent as requests and their associated procs
  std::vector<DInfoEntry> requests;
  std::vector<UInt> request_procs;
  UInt req_size;


  // Send ids to procs where corresponding info should be   
  { // vv Make block so temp. memory goes away
    
    // Loop search_ids, calculate sizes
    std::vector<UInt> to_proc; // procs I will send to
    std::vector<UInt> send_sizes_all(petCount, 0); // Sizes going to each proc
    for (auto i = 0; i < num_search; i++) {

      // Figure out which proc this id would be on
      UInt proc = DINFO_calc_proc_from_id(search_ids[i], petCount, gmin, gmax);

      // Add proc to list
      to_proc.push_back(proc);

      // Size of search ids
      send_sizes_all[proc] += SparsePack<IDTYPE>::size();
    }
  
    // Uniq the proc list
    std::sort(to_proc.begin(), to_proc.end());
    to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());

    // Calc sizes going to each processor
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (auto i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
    std::vector<UInt>().swap(send_sizes_all); // free memory

    // Set up communication pattern
    SparseMsg msg;
    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);

    // Loop search_ids, pack to send
    for (auto i = 0; i < num_search; i++) {
      
      // Figure out which proc this id would be on
      UInt proc = DINFO_calc_proc_from_id(search_ids[i], petCount, gmin, gmax);

      // Get buffer going to that proc
      SparseMsg::buffer &b = *msg.getSendBuffer(proc);
      
      // Pack id to send
      SparsePack<IDTYPE>(b, search_ids[i]);
    }

    // Complain if we don't have the amount of data we expect
    if (!msg.filled()) Throw() << "In DInfo class message unexpectedly not filled";

    // Communicate data
    msg.communicate();

    // Calc. size of one packed id
    UInt packed_id_size = SparsePack<IDTYPE>::size();
    
    // Now unpack ids
    for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
     UInt proc = *p;

     // Get buffer for proc
     SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

     // Deduce the message size
     UInt num_msgs = b.msg_size()/packed_id_size;

     // Loop over messages from proc and unpack
     for (UInt m = 0; m < num_msgs; m++) {

       // Unpack request id
       IDTYPE id;
       SparseUnpack<IDTYPE>(b, id);

       // Record request id and initially mark info as unset
       requests.push_back(DInfoEntry(id,not_found_info_val)); 

       // Record the proc that the request should be retured to
       request_procs.push_back(proc);
     }
    }

    // Complain if we don't have the amount of data we expect
    if (!msg.empty()) Throw() << "In DInfo class message buffer unexpectedly not emptied completely";

  
  } // ^^ Make block so temp. memory goes away
  
     
  // Loop through the requests looking them up in this PET's entries
  for (auto r = 0; r < requests.size(); r++) {
    DInfoEntry &req = requests[r];

    // Search for the request in this PETs searchable list
    typename std::vector<DInfoEntry>::iterator ei = std::lower_bound(searchable.begin(),
                                                                     searchable.end(),
                                                                     req,
                                                                     DInfoEntry_just_id_less<IDTYPE,INFOTYPE>());
    // If within list
    if (ei != searchable.end()) {
       DInfoEntry &lb_req = *ei;

       // If the ids match, then we've found an answer, so set value
       if (lb_req.id == req.id) {
         req.info = lb_req.info;
       } else {
         // If requested throw an error, otherwise just leave set to no_found_info_val
         if (error_on_not_found) {
           Throw() << " Searched for id=" << req.id<<" not found in the search object.";
         }
       }
    } else { // else not found
      // If requested throw an error, otherwise just leave set to no_found_info_val
      if (error_on_not_found) {
        Throw() << " Searched for id=" << req.id<<" not found in the search object.";
      }
    } 
  }

  
  // Build the reply.  Important: the ordering (per proc) of requests is the same
  // as the original request gid's.  When we send back, we use this same
  // ordering so that we can unpack straight into the request buffer.

  { // vv Make block so temp. memory goes away

    // Loop requests calc. sizes and set of procs being send to
    std::vector<UInt> to_proc; // procs I will send to
    std::vector<UInt> send_sizes_all(petCount, 0);
    for (auto r = 0; r < requests.size(); r++) {
      DInfoEntry &req = requests[r]; // request
      UInt ret_proc = request_procs[r]; // corresponding return proc

      // Record which proc this request is going back to
      to_proc.push_back(ret_proc);

      // Record info size
      send_sizes_all[ret_proc] += SparsePack<INFOTYPE>::size();
    }

    // Uniq the proc list
    std::sort(to_proc.begin(), to_proc.end());
    to_proc.erase(std::unique(to_proc.begin(), to_proc.end()), to_proc.end());

    // Calc sizes going to each processor
    UInt nsend = to_proc.size();
    std::vector<UInt> sizes(nsend, 0);
    for (UInt i = 0; i < nsend; i++) sizes[i] = send_sizes_all[to_proc[i]];
    std::vector<UInt>().swap(send_sizes_all); // free memory

    // Set up communication pattern
    SparseMsg msg;
    msg.setPattern(nsend, nsend > 0 ? &to_proc[0] : NULL);
    msg.setSizes(nsend > 0 ? &sizes[0] : NULL);

    // Now pack info to be send back to requestor
    for (auto r = 0; r < requests.size(); r++) {
      DInfoEntry &req = requests[r]; // request
      UInt ret_proc = request_procs[r]; // corresponding return proc

      // Get send buffer
      SparseMsg::buffer &b = *msg.getSendBuffer(ret_proc);

      // Pack info
      SparsePack<INFOTYPE>(b, req.info);
    }

    // Complain if we don't have the amount of data we expect
    if (!msg.filled()) Throw() << "In DInfo class message buffer unexpectedly not filled";    

    // Communicate data
    msg.communicate();

    // Now unpack.  This unpacking is a bit weird.  We loop the original search ids,
    // and then get the processor number so that we know which buffer to pick from.
    // In this way, we unpack the buffers in the correct order so that the info we're
    // unpacking from the send comes out in the correct order.
    for (UInt i = 0; i < num_search; i++) {
      
      // Figure out which proc this search id would be on
      UInt proc = DINFO_calc_proc_from_id(search_ids[i], petCount, gmin, gmax);

      // Get buffer for this proc
      SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

      // Unpack info into output array
      SparseUnpack<INFOTYPE>(b, out_search_info[i]);
    }

    // Complain if we don't have the amount of data we expect
    if (!msg.empty()) Throw() << "In DInfo class message buffer unexpectedly not emptied completely";
  

  } // ^^ Make block so temp. memory goes away ^^
}


  
// Print searchable list
// (for debugging mostly)
template<class IDTYPE, class INFOTYPE>
void DInfo<IDTYPE, INFOTYPE> :: print_searchable() {
  
  // Output list
  for (auto d: searchable) {
    std::cout << Par::Rank() << "#  id="<<d.id<<" info="<<d.info<<std::endl;
  }
  
}


#endif // ESMCI_DInfo_H
