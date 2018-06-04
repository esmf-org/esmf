// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_DDir_h
#define ESMCI_DDir_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>

#include <cmath>
#include <iostream>


// Creates a distributed directory.  This will lookup off processor GID's
// and tell the requestor the processor and local id where the GID resides.
// Utilizes a hash function to create a rendezvous directory structure.
// It is assumed that an id is an unsigned int for this design.

namespace ESMCI {


// Base class for a hash function.  operator() should
// return the assigned processor for a gid, given
// the range of all possible gid's.  Defualt is linear.
// Proc returned is between 0 and nproc-1.
class DDir_lin_hash {
public:
DDir_lin_hash() {}
virtual ~DDir_lin_hash() {}
virtual UInt operator()(UInt gid, UInt nproc, UInt min, UInt max) const {
  UInt num_per_proc = ((max-min+1) + nproc -1) / nproc;
  UInt proc = (gid-min) / num_per_proc;
  return proc > nproc-1 ? nproc-1 : proc;
}
};


/**
 * Distributed directory of indices.  Provides query operations
 * so a user can find out what processor and local index a given
 * global index has.  The object does this without having to gather
 * the entire directory ever on a single processor.
*/
template<typename HASH=DDir_lin_hash>
class DDir {
public:
// Create the DDIR.
// gid = all locally (owned or resident id's)
// lid = local id's of corresponding gid
// _hf user may provide a hash function
DDir(UInt ngid, const UInt gid[], const UInt lid[]);
DDir();

void Create(UInt ngid, const UInt gid[], const UInt lid[]);
  
~DDir();

// Structure for my managed directory entries.
struct dentry {
dentry(UInt _gid=0, UInt _origin_lid = 0, UInt _origin_proc = 0, UInt _req_proc = 0) :
  gid(_gid), origin_lid(_origin_lid), origin_proc(_origin_proc), req_proc(_req_proc) 
  {}
UInt gid;
UInt origin_lid;
UInt origin_proc;
UInt req_proc; // used when requesting

// total sort.  Used for ordering.  A weaker sort
// is used for lookup (gid only).
bool operator<(const dentry &rhs) const {
  if (gid != rhs.gid) {
    return gid < rhs.gid;
  } else if (origin_proc != rhs.origin_proc) {
    return origin_proc < rhs.origin_proc;
  }
  return origin_lid < rhs.origin_lid; // (not sure why a mesh would have several copies)
}

#if 0
bool operator==(const dentry &rhs) const {
  return (gid == rhs.gid && 
          origin_proc == rhs.origin_proc);
}
#endif


bool operator==(const dentry &rhs) const {
  return (gid == rhs.gid && origin_lid == rhs.origin_lid &&
          origin_proc == rhs.origin_proc);
}

bool operator!=(const dentry &rhs) const {
  return !(*this == rhs);
}

};

// For the given global id's, request the owning processors and the
// owner's local id's.  To be used when one doesn't care about duplicates
void RemoteGID(UInt ngid, const UInt gid[], UInt orig_proc[], UInt lid[]);


/*
 * Returns an entry for each DDir entry (in the case that multiple processors share
 * a gid).  Responses will be sorted by gid, but will not line up directly, since
 * there could be multiple responses for a single gid.
 * If some_dest = true, then objects that do not have an entry in the directory
 * are sent to themselves, i.e. the same processor; lid is set to zero. 
 */
void RemoteGID(UInt ngid, const UInt gid[], std::vector<dentry> &response, bool some_dest = false);

void Print(std::ostream &);

void clear();

private:
const HASH hash_func;
UInt gmin, gmax; // global min and max of id's

// List of entries.  The list is sorted.
std::vector<dentry> my_managed;

};

} // namepsace

#endif
