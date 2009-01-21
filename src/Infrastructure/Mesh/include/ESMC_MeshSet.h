// $Id: ESMC_MeshSet.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_MeshSet_h
#define ESMC_MeshSet_h

#include <ESMC_MeshTypes.h>

#include <string>

// A class to ease the definition of poritions of a mesh.  These may be used
// to specify iteration subsets of the mesh, or field subsets, etc....

namespace ESMCI {
namespace MESH {

class Mesh;
class Context;

class MeshSet {
public:
  friend class Mesh;
private:
  // Define a set on a mesh.  Sets are always of a given object type, usually
  // element sets of side sets.
  MeshSet(UInt objtype, const std::string &name);

public:
  const std::string &Name() const { return mname; }
  MeshSet &AddSubset(const MeshSet &rhs);
  void AddIOPart(UInt io_id);
  void Commit(); // actually imprint contexts on objects
private:
  std::string mname;
  UInt context_id;
  Context context; // the bits of all subsets + this
  std::vector<const MeshSet*> subsets; // all sets included
  std::vector<UInt> io_ids;
  bool committed;
}; 

} // namespace
} // namespace


#endif
