// $Id: ESMC_Search.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Search_h
#define ESMC_Search_h

#include <list>

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObj.h>
#include <ESMC_Mesh.h>


namespace ESMCI {
namespace MESH {

// The return type from search.  A list of source grid node to
// destination grid element pairs
struct Search_node_result {
  const MeshObj *node;
  double pcoord[3];  // parametric coord of node in elem
};
struct Search_result {
  const MeshObj *elem;
  std::vector<Search_node_result> nodes;
};
typedef std::vector<Search_result*> SearchResult;


void Search(const Mesh &src, const Mesh &dest, UInt dst_obj_type, SearchResult &result,
            double stol = 1e-8, std::vector<const MeshObj*> *to_investigate = NULL);

void PrintSearchResult(const SearchResult &result);


} //namespace
} //namespace

#endif
