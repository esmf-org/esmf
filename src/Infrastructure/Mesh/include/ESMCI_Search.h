// $Id: ESMCI_Search.h,v 1.10 2011/01/05 20:05:44 svasquez Exp $
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Search_h
#define ESMCI_Search_h

#include <list>

#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_SearchFlags.h>

#include <vector>


namespace ESMCI {

class Mesh;

// The return type from search.  A list of source grid node to
// destination grid element pairs
struct Search_node_result {
  const MeshObj *node;
  double pcoord[3];  // parametric coord of node in elem
};


struct Search_result {
  const MeshObj *elem;
  std::vector<Search_node_result> nodes;
  std::vector<const MeshObj *> elems;

  // AN IDEA would be to do some kind of inheritence instead of having nodes and elems in the same struct

  bool operator<(const Search_result &rhs) const {
    return elem->get_id() < rhs.elem->get_id();
  }
  bool operator==(const Search_result &rhs) const {
    return elem->get_id() == rhs.elem->get_id();
  }
  bool operator!=(const Search_result &rhs) const {
    return !(*this == rhs);
  }
};
typedef std::vector<Search_result*> SearchResult;


  void OctSearchElems(const Mesh &meshA, int unmappedactionA, const Mesh &meshB, int unmappedactionB, 
                      double stol, SearchResult &result);

void OctSearch(const Mesh &src, const Mesh &dest, UInt dst_obj_type, int unmappedaction, SearchResult &result,
            double stol = 1e-8, std::vector<const MeshObj*> *to_investigate = NULL, OTree *box = NULL);

void PrintSearchResult(const SearchResult &result);

void DestroySearchResult(SearchResult &sres);

} //namespace

#endif
