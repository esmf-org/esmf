// $Id: ESMC_DDir.h,v 1.1 2007/08/07 17:47:55 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_DDir_h
#define ESMC_DDir_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObj.h>

#include <cmath>
#include <iostream>


// Creates a distributed directory.  This will lookup off processor GID's
// and tell the requestor the processor and local id where the GID resides.
// Utilizes a hash function to create a rendezvous directory structure.
// It is assumed that an id is an unsigned int for this design.

namespace ESMCI {
namespace MESH {


// Base class for a hash function.  operator() should
// return the assigned processor for a gid, given
// the range of all possible gid's.  Defualt is linear.
// Proc returned is between 0 and nproc-1.
class DDir_lin_hash {
public:
DDir_lin_hash() {}
virtual ~DDir_lin_hash() {}
virtual UInt operator()(UInt gid, UInt nproc, UInt min, UInt max) const {
  double frac = (double) (gid-min)/(max-min);
  UInt val = (UInt) std::floor(frac*nproc);
//std::cout << "frac=" << frac << ", val=" << val << std::endl;
  return std::min(val, nproc-1);
}
};

// This hash does not depend on the max idx, so it may
// be reused as new ids are created without having to redistribute
// the hash table.
class DDir_mod_hash {
public:
DDir_mod_hash() {}
virtual ~DDir_mod_hash() {}
virtual UInt operator()(UInt gid, UInt nproc, UInt min, UInt max) const {
  return (gid - min) % nproc;
  double frac = (double) (gid-min)/(max-min);
  UInt val = (UInt) std::floor(frac*nproc);
//std::cout << "frac=" << frac << ", val=" << val << std::endl;
  return std::min(val, nproc-1);
}
};

/**
 * Distributed directory of indices.  Provides query operations
 * so a user can find out what processor and local index a given
 * global index has.  The object does this without having to gather
 * the entire directory ever on a single processor.
*/
template<typename HASH>
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

};

// For the given global id's, request the owning processors and the
// owner's local id's.
void RemoteGID(UInt ngid, const UInt gid[], UInt orig_proc[], UInt lid[]);

void Print();

private:
const HASH hash_func;
UInt gmin, gmax; // global min and max of id's

// List of entries.  The list is sorted.
std::vector<dentry> my_managed;
};

} // namepsace
} // namepsace

#endif
