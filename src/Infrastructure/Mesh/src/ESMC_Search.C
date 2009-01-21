// $Id: ESMC_Search.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Search.h>
#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Mapping.h>

#include <ESMC_MeshUtils.h>

#include <iostream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>

#include <ESMC_BBox.h>

namespace ESMCI {
namespace MESH {

// Store the index and a found flag for the
// dimension.
struct Search_index {
  Search_index() : index(0), found_flag(false) {}
  Search_index(UInt _index, const double *_sort_val) :
    index(_index), found_flag(false) {
    sort_val[0] = _sort_val[0];
    sort_val[1] = _sort_val[1];
    sort_val[2] = _sort_val[2];
    best_dist = std::numeric_limits<double>::max();
    best_elem = NULL;
    investigated = false;
  }
  Search_index(UInt _index, double _sort_val) :
     index(_index), found_flag(false) {
     sort_val[0] = sort_val[1] = sort_val[2] = _sort_val;
     best_dist = std::numeric_limits<double>::max();
     best_elem = NULL;
    investigated = false;
   }
  UInt index;
  double sort_val[3];  // coordinate
  bool found_flag;
  const MeshObj *best_elem;
  Search_node_result best_snr;
  double best_dist;
  bool investigated;
/*
  bool operator<(const Search_index &rhs) const
    { return sort_val < rhs.sort_val; }
*/
  friend std::ostream& operator <<(std::ostream &os, const Search_index &obj);
};

class Search_Less : public std::binary_function<Search_index*,Search_index*,bool> {
public:
  Search_Less(int _dim) : dim(_dim) {}
  bool operator()(const Search_index* l, const Search_index* r) {
    return l->sort_val[dim] < r->sort_val[dim];
  }
private:
  const int dim;
};

class Search_Res_Less : public std::binary_function<Search_result*,Search_result,bool> {
public:
  Search_Res_Less() {}
  bool operator()(const Search_result* l, const Search_result* r) {
    return l->elem->get_id() < r->elem->get_id();
  }
};


std::ostream &operator <<(std::ostream &os, const Search_index &obj) {
  os << "(val=" << obj.sort_val[0] << ", " << obj.sort_val[1] << ", " << obj.sort_val[2] << ", index=" << obj.index << ")";
  os << "investigated=" << obj.investigated << ", best_dist=" << obj.best_dist;
  return os;
}

void delAndNullifyFound(Search_index *&si) {
  // May already be NULL
  if (si && si->found_flag) {
    delete si;
    si = NULL;
  }
}

class Search_found : public std::unary_function<Search_index*,bool> {
public:
  Search_found(){}
  bool operator()(const Search_index* r) {
    return r->found_flag;
  }
};

// The main routine
void Search(const Mesh &src, const Mesh &dest, UInt dst_obj_type, SearchResult &result, double stol,
     std::vector<const MeshObj*> *to_investigate) {
  Trace __trace("Search(const Mesh &src, const Mesh &dest, UInt dst_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *to_investigate");


  MEField<> &coord_field = *src.GetCoordField();
  
  // Destination coordinate is only a _field, not an MEField, since there
  // are no master elements.
  _field *dcptr = dest.Getfield("coordinates_1");
  ThrowRequire(dcptr);
  _field &dstcoord_field = *dcptr;
   
  //MEField<> &dstcoord_field = *dest.GetCoordField();

  if (src.spatial_dim() != dest.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }

  // TODO: only grab objects in the current interpolation realm.

  // Get a bounding box for the dest mesh.
  BBox dstBBox(dstcoord_field, dest);

  // Load the destination objects into a list
  std::vector<const MeshObj*> dest_nlist;
  {

    if (to_investigate == NULL) {
      MeshDB::const_iterator ni = dest.obj_begin(), ne = dest.obj_end();
      for (; ni != ne; ++ni) {
        if (dst_obj_type & ni->get_type())
          dest_nlist.push_back(&*ni);
      }
    } else {
      std::vector<const MeshObj*>::const_iterator ni = to_investigate->begin(),
                       ne = to_investigate->end();
      for (; ni != ne; ++ni) dest_nlist.push_back(*ni);
      if (dest_nlist.size() == 0) return;
    }

  }

  UInt sdim = dest.spatial_dim();
  
  // Build the search table
  std::vector<std::vector<Search_index*> > sidx(sdim);

  // Set aside proper space
  for (UInt i = 0; i < sdim; i++) sidx[i].reserve(dest.num_nodes());

  // Load up the nodes in the search table
  std::vector<const MeshObj*>::const_iterator ni = dest_nlist.begin(),
            ne = dest_nlist.end();
  UInt node_num = 0;
  
  for (; ni != ne; ++ni) {
  
    // Get the coords
    const MeshObj &node = **ni;
    
    double *node_coord = dstcoord_field.data(node);
  
    Search_index *si = new Search_index(node_num, &node_coord[0]);
    
    for (UInt i = 0; i < sdim; i++) {  
      sidx[i].push_back(si);
    }
    node_num++;
    
  } // for ni

  // Now sort each list
  for (UInt d = 0; d < sdim; d++) 
    std::sort(sidx[d].begin(), sidx[d].end(), Search_Less(d));


  // Loop through the source mesh elements, trying to find candidate points
  // TODO: loop kernels
  KernelList::const_iterator ki = src.set_begin(), ke = src.set_end();
  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;
    
    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;
    
    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();
    
    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());
    
    std::vector<double> node_coord(cme.num_functions()*src.spatial_dim());
    
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
   
     BBox bounding_box(coord_field, elem, 0.5);
  
     // First check to see if the box even intersects the dest mesh bounding
     // box.  
     if (!BBoxIntersect(dstBBox, bounding_box, 1e-4)) {
       //std::cout << "eleminate elem" << std::endl;
       //std::cout << bounding_box;
       continue;
     }
  
     GatherElemData(cme, coord_field, elem, &node_coord[0]);
  
     std::set<Search_index*> candidates[3];
     bool empty_dimension = false;
     for (UInt d = 0; d < sdim; d++) { 
       Search_index sl(0, bounding_box.getMin()[d]-stol);
       Search_index su(0, bounding_box.getMax()[d]+stol);
       std::vector<Search_index*>::iterator lb =
             std::lower_bound(sidx[d].begin(), sidx[d].end(), &sl, Search_Less(d));
       std::vector<Search_index*>::iterator ub =
             std::upper_bound(sidx[d].begin(), sidx[d].end(), &su, Search_Less(d));
  
     // If list is empty, we can move on
       if (lb == ub) {
  //std::cout << "empty dim:" << std::endl;
         empty_dimension = true;
         break;
       }
     // Copy 
       for (; lb != ub; lb++) {
         candidates[d].insert(*lb);
       }
  
     } // dimension
  
     if (empty_dimension) continue;  // no point in continuing
  
  //std::cout << " candid size:" << candidates[0].size() << ", " << candidates[1].size() << std::endl;
  
     // Now, we intersect the dimensions
     std::set<Search_index*> final_candidates;
     std::set_intersection(candidates[0].begin(), candidates[0].end(),
                           candidates[1].begin(), candidates[1].end(),
                           std::inserter(final_candidates, final_candidates.begin()));
     if (final_candidates.size() == 0) continue;
     if (sdim == 3) {
       candidates[0] = final_candidates;
       final_candidates.clear();
       std::set_intersection(candidates[0].begin(), candidates[0].end(),
                           candidates[2].begin(), candidates[2].end(),
                           std::inserter(final_candidates, final_candidates.begin()));
     }
     if (final_candidates.size() == 0) continue;
  //std::cout << " final candid size:" << final_candidates.size() << std::endl;
  
     // We have some candidates, so loop through and see if they work
     Search_result *sr = new Search_result;
     sr->elem = &elem;
     for (std::set<Search_index*>::iterator ci = final_candidates.begin(); ci != final_candidates.end(); ci++) { 
       Search_index &si = *(*ci);
       si.investigated = true;
       const MappingBase &map = GetMapping(elem);
       double pcoord[3];
       double dist;
       bool in = map.is_in_cell(&node_coord[0], si.sort_val, &pcoord[0], &dist);
  //std::cout << "in = " << in << std::endl;
  //std::cout << "dist=" << dist << std::endl;
       Search_node_result snr;
       snr.node = dest_nlist[si.index];
       std::copy(pcoord, pcoord+sdim, &snr.pcoord[0]);
       if (in) {
         si.found_flag = true;
         sr->nodes.push_back(snr);
       } else if(dist < si.best_dist) {
         // Set up fallback candidate.
         si.best_dist = dist;
         si.best_elem = &elem;
         si.best_snr = snr;
       }
     } // final candidates
     if (sr->nodes.size() > 0) {
       result.push_back(sr);
     } else delete sr;
  
     // Remove the found guys.  Remember, all three lists point to the same structure.
     // Hence, for lower dim, just remove if found.  For last dim, delete the pointer and erase
     // everyone in the list with a NULL pointer.
     for (UInt d = 0; d < sdim; d++) {
       if (d == sdim-1) {
         std::for_each(sidx[d].begin(), sidx[d].end(), delAndNullifyFound);
         sidx[d].erase(std::remove(sidx[d].begin(), sidx[d].end(), static_cast<Search_index*>(0)), sidx[d].end());
       } else {
         sidx[d].erase(std::remove_if(sidx[d].begin(), sidx[d].end(), Search_found()), sidx[d].end());
       }
     }
     
      // Get the bounding box
    } // for ei
  }
  
    // Sort search result for lookup
  std::sort(result.begin(), result.end(), Search_Res_Less());

  // Let us see if any pointers are left to erase
  UInt nres = 0;
  UInt nbad = 0;
  const bool try_again = true;
  std::vector<const MeshObj*> iagain;
  std::vector<Search_index*>::iterator si = sidx[0].begin(), se = sidx[0].end();
  for (; si != se; ++si) {
    nbad++;
    if ((*si)->best_elem == NULL) {
      std::cout << "Could not locate:" << **si << ", with found_flag=" << (*si)->found_flag << std::endl;
      if (try_again) iagain.push_back(dest_nlist[(*si)->index]);
    } else {
      Search_result *srtmp = new Search_result;
      srtmp->elem = (*si)->best_elem;
      SearchResult::iterator sri = std::lower_bound(result.begin(), result.end(), srtmp, Search_Res_Less());
      if (sri == result.end() || (*sri)->elem != srtmp->elem) {
      // didn't finnd element in list, must add
        srtmp->nodes.push_back((*si)->best_snr);
        result.insert(sri, srtmp);
      } else {
        // Just push back
        (*sri)->nodes.push_back((*si)->best_snr);
      }
      //std::cout << "Resolved:Could not locate:" << **si << ", dist=" << (*si)->best_dist << std::endl;
      nres++;
    }
    delete *si;
  }
  std::cout << "Search, found " << nbad << " bad cases, but resolved " << nres << " of these." << std::endl;

  // Remove storage for dest_nlist.
  std::vector<const MeshObj*>().swap(dest_nlist);

  if (try_again && iagain.size() != 0) {
     if (stol > 1e-6) {
       std::cout << "Bailing, since stol=" << stol << " which is above limit" << std::endl;
     } else {
       std::cout << "Going again to resolve " << iagain.size() << " items, with stol=" << stol*1e-2 << std::endl;
       Search(src, dest, dst_obj_type, result, stol*1e+2, &iagain);
     }
  }
    
}

void PrintSearchResult(const SearchResult &result) {
  SearchResult::const_iterator si = result.begin(), se = result.end();
  for (; si != se; ++si) {
    Search_result *sr = *si;
    std::cout << "Source Element:" << sr->elem->get_id() << std::endl;
    for (std::vector<Search_node_result>::iterator sni = sr->nodes.begin(); sni != sr->nodes.end(); sni++) {
      std::cout << "\tNode:" << sni->node->get_id() << ", pcoord:";
      std::cout << sni->pcoord[0] << ", " << sni->pcoord[1] << ", " << sni->pcoord[2] << std::endl;
    }
  }
}

} // namespace
} // namespace
