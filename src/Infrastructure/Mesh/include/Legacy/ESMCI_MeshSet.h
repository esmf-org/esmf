// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshSet_h
#define ESMCI_MeshSet_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

#include <string>

// A class to ease the definition of poritions of a mesh.  These may be used
// to specify iteration subsets of the mesh, or field subsets, etc....

namespace ESMCI {

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


#endif
