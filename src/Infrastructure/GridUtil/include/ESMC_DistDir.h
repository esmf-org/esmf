// $Id: ESMC_DistDir.h,v 1.7.2.2 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF DistDir C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document petessing.)
//-----------------------------------------------------------------------------
//
#ifndef ESMC_DistDir_h
#define ESMC_DistDir_h

//-----------------------------------------------------------------------------
#include <GridUtil/include/ESMC_Ptypes.h>
#include <GridUtil/include/ESMC_traits.h>

#include <cmath>
#include <iostream>
#include <vector>
#include <functional>

#include <VM/include/ESMC_VM.h>

namespace ESMC {

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC::DistDir_hash
//  
// !DESCRIPTION:
//     A hash function object for the distributed directory.  To create
//   a different hash function, simply implement an object
//   with the operator() signature below.
//   We use a function object here to avoid indirect function lookups
//   when the function is used (see Scott Meyer's 'Effective STL').  This 
//   strategy can be 600% faster than using function pointers.
//     This particular object hashes a global id to pet'cessors.  The 
//   default implementation uses a linear map between max,min.  If your
//   indices are distributed nonuniformly, the object could end up with
//   a poor load balance of the directory; at worst, all indices could end
//   up on one pet'cessor and swamp memory.  In general, however, for
//   uniformly distributed global indices, behavior will be near optimal.
///EOP
//-------------------------------------------------------------------------
template <typename id_type>
class DistDir_hash {
public:
DistDir_hash() {}
UInt operator()(id_type gid, UInt npet, id_type min, id_type max) const {
  UInt num_per_pet = ((max-min+1) + npet -1) / npet;
  UInt pet = (gid-min) / num_per_pet;
  return pet > npet-1 ? npet-1 : pet;
}
};

    
//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC::DistDir
//  
// !DESCRIPTION:
//     A distributed directory object.  Allows lookup of global id's in a 
//   scalable, parallel manner.  For a detailed description of the algorithm,
//   see the paper "Communication support for adaptive computation," by
//   Bruce Henderickson and Ali Pinar.
//   The object allows lookup of off pet'cessor GID's
//   and tell the requestor the pet'cessor and local id where the GID resides.
//   Utilizes a hash function to create a rendezvous directory structure.

//   The local and global id's are templated types id_type. The global 
//   id must represent an int (just maybe of different sizes).
//   Features;  1) Be a representation of a non-negative int.
//            2) Support gid = 0;
//            3) Support std::numeric_limits<id_type>::max
//            4) All the usual <, ==
//
//   It may be useful later to make lid it's own template parameter,
//   since (i,j,k), for instance, could be a useful local id.
//
//   To implement your own hash, simply implement a function object
//   with this signature of DistDir_hash above, and create the distdir
//   with your has function as the second argument.
//
//   Example of use:
//   ------------------------
//   std::vector<UInt> my_gid;
//   std::vector<UInt> my_lid(ngid);
//
//   ... set up local and global id's on this pet
//
//   // Create the distributed directory
//   ESMC::DistDir<UInt> dir(*vm, ngid, &my_gid[0], &my_lid[0]);
//   
//   .... Now we need to query some global id's
//   std::vector<UInt> query_gid;
//   std::vector<UInt> query_lid;
//   std::vector<UInt> owning_pet;
//   bool *valid = new bool[query_gid.size()]; // don't use vector<bool>
//   ... assign which gid to query
//   dir.RemoteGID(query_gid.size(), &query_gid[0], &owning_pet[0], &query_lid[0], valid);
//   delete [] valid;
//
//   if (valid[i]) process_id(query_gid[i], owning_pet[i], query_lid[i]); ...
//   ------------------------
//   To implement your own hash, simply implement a function object
//  
///EOP
//-------------------------------------------------------------------------
template<typename id_type, typename hash=DistDir_hash<id_type> >
class DistDir {
public:
// Create the DDIR.
// gid = all locally (owned or resident id's)
// lid = local id's of corresponding gid
DistDir(ESMCI::VM &vm, UInt ngid, const id_type gid[], const id_type lid[]);
  
~DistDir();

// For the given global id's, request the owning pet'cessors and the
// owner's local id's.
void RemoteGID(UInt ngid, const id_type gid[], UInt orig_pet[], id_type lid[], bool id_found[]);

void Print();

private:
// Structure for my managed directory entries.  This object is
// also used to service requests for directory entries.
struct dentry {
dentry() :
origin_lid(0),
origin_pet(0),
req_pet(0),
valid(true) {}

id_type gid;          // global id of entry
id_type origin_lid;   // original local id of entry
UInt origin_pet;      // original pet
UInt req_pet;         // Who is the requesting pet?
bool valid;           // Is the request in the directory (=true) else false

// Total sort.  Used for ordering.  A weaker sort
// is used for lookup (gid only).  
bool operator<(const dentry &rhs) const {
  if (gid != rhs.gid) {
    return gid < rhs.gid;
  } else if (origin_pet != rhs.origin_pet) {
    return origin_pet < rhs.origin_pet;
  }
  return origin_lid < rhs.origin_lid; // (not sure why a mesh would have several copies)
}

};

// A sort function that only uses global id.  This is used for looking up a global id in the
// local list of managed id's.  By only using gid as the sort key, the function will allow
// std::lower_bound to point to the first entry with the given gid.  If there are multiple of
// these, they will be at the subsequent locations.  Again, we use a function object for this
// comparison rather than a function pointer due to the 600% speedup of this approach.
class dentry_less : public std::binary_function<dentry,dentry,bool> {
public:
  dentry_less() {}
  bool operator()(const dentry &l, const dentry& r) {
    return l.gid < r.gid;
  }
};
ESMCI::VM &vm;
const hash hash_func;
UInt gmin, gmax; // global min and max of id's
UInt npet, rank;

// List of entries.  The list is sorted so that 
// lower_bound may be used for servicing requests (log(n) lookup).
std::vector<dentry> my_managed;
};


} // namespace


#endif
