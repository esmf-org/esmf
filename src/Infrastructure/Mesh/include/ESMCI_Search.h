// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_SearchFlags.h>

#include "PointList/include/ESMCI_PointList.h"

#include <vector>


namespace ESMCI {

class Mesh;

// The return type from search.  A list of source grid node to
// destination grid element pairs
struct Search_node_result {
  const MeshObj *node;
  int dst_gid;
  double pcoord[3];  // parametric coord of node in elem
};


struct Search_result {
  const MeshObj *elem;
  std::vector<Search_node_result> nodes;
  std::vector<const MeshObj *> elems;

  UInt dst_gid;
  UInt src_gid;


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


 void OctSearch(const Mesh &src, PointList &dst_pl, MAP_TYPE mtype, UInt dst_obj_type, int unmappedaction, SearchResult &result, double stol, std::vector<int> *revised_dst_loc=NULL, OTree *box_in=NULL);

  void OctSearchElems(const Mesh &meshA, int unmappedactionA, const Mesh &meshB, int unmappedactionB, 
                      double stol, SearchResult &result);


void SearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result);

void ParSearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result);

void SearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchResult &result);

void ParSearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchResult &result);

void PrintSearchResult(const SearchResult &result);

void DestroySearchResult(SearchResult &sres);

} //namespace

#endif
