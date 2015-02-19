// $Id$
// Earth System Modeling Framework
// Copyright 2002-2015, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_Search_h
#define ESMCI_MBMesh_Search_h

#include <list>

#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_SearchFlags.h>
#include <Mesh/include/ESMCI_MBMesh.h>

#include <vector>


namespace ESMCI {

#if 0
// The return type from search.  A list of source grid node to
// destination grid element pairs
struct MBMesh_Search_node_result {
  EntityHandle *node;  // Take this out when everything converted to PntList?

  double pcoord[3];  // parametric coord of node in elem
};
#endif


struct MBMesh_Search_result {
  EntityHandle src_elem;
  std::vector<EntityHandle> dst_elems;

  // AN IDEA would be to do some kind of inheritence instead of having nodes and elems in the same struct

  bool operator<(const MBMesh_Search_result &rhs) const {
    return src_elem < rhs.src_elem;
  }
  bool operator==(const MBMesh_Search_result &rhs) const {
    return src_elem == rhs.src_elem;
  }
  bool operator!=(const MBMesh_Search_result &rhs) const {
    return !(*this == rhs);
  }
};
typedef std::vector<MBMesh_Search_result*> MBMesh_SearchResult;


//void PrintSearchResult(const SearchResult &result);

//void DestroySearchResult(SearchResult &sres);

} //namespace

#endif
