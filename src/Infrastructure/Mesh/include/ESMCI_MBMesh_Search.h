// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_Search_h
#define ESMCI_MBMesh_Search_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/Regridding/ESMCI_SearchFlags.h>
#include <Mesh/include/ESMCI_MBMesh.h>


#include <list>
#include <vector>

namespace ESMCI {

  /////// TODO: THIS ISN'T USED RIGHT NOW, SO GET RID OF EVENTUALLY //////////


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

#endif // ESMF_MOAB

#endif
