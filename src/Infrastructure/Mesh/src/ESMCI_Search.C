// $Id: ESMCI_Search.C,v 1.16 2010/12/04 00:11:39 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
 
#include <iostream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include <Mesh/include/ESMCI_BBox.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Search.C,v 1.16 2010/12/04 00:11:39 oehmke Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

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
    is_in=false;
    elem_masked=false;
  }
  Search_index(UInt _index, double _sort_val) :
     index(_index), found_flag(false) {
     sort_val[0] = sort_val[1] = sort_val[2] = _sort_val;
     best_dist = std::numeric_limits<double>::max();
     best_elem = NULL;
    investigated = false;
    is_in=false;
    elem_masked=false;
   }
  UInt index;
  double sort_val[3];  // coordinate
  bool found_flag;
  const MeshObj *best_elem;
  Search_node_result best_snr;
  double best_dist;
  bool investigated;
  bool is_in;
  bool elem_masked;

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


/*--------------------------------------------------------------------------*/
// Octree node search
/*--------------------------------------------------------------------------*/


static int num_intersecting(const Mesh &src, const BBox &dstBBox, double btol, double nexp) {
  
  int ret = 0;

  MEField<> &coord_field = *src.GetCoordField();
  
  KernelList::const_iterator ki = src.set_begin(), ke = src.set_end();
  
  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;
    
    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;
    
    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();
    
    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());
    
    std::vector<double> node_coord(cme.num_functions()*src.spatial_dim());
    
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
   
     BBox bounding_box(coord_field, elem, nexp);
  
     // First check to see if the box even intersects the dest mesh bounding
     // box.  
     if (BBoxIntersect(dstBBox, bounding_box, btol)) ++ret;
  
    }
    
  }
  return ret;
}

static void populate_box(OTree *box, const Mesh &src, const BBox &dstBBox, double btol, double nexp) {

  MEField<> &coord_field = *src.GetCoordField();

  // Get spatial dim of mesh
  UInt sdim = src.spatial_dim();
  
  KernelList::const_iterator ki = src.set_begin(), ke = src.set_end();
  
  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;
    
    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;
    
    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();
    
    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());
    
    std::vector<double> node_coord(cme.num_functions()*src.spatial_dim());
    
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
   
     BBox bounding_box(coord_field, elem, nexp);
  
     // First check to see if the box even intersects the dest mesh bounding
     // box.  
     if (BBoxIntersect(dstBBox, bounding_box, btol)) {
       
       double min[3], max[3];
       
       min[0] = bounding_box.getMin()[0] - btol;
       min[1] = bounding_box.getMin()[1] - btol;
       if (sdim >2) min[2] = bounding_box.getMin()[2] - btol;
       else min[2] = - btol;

       max[0] = bounding_box.getMax()[0] + btol;
       max[1] = bounding_box.getMax()[1] + btol;
       if (sdim >2) max[2] = bounding_box.getMax()[2] + btol;
       else  max[2] = btol;
    
       /*
       if (elem.get_id() == 2426) {
         std::cout << "elem 2426, bbox=" << bounding_box << std::endl;
       }*/

       // Add element to search tree
       box->add(min, max, (void*)&elem);

     }
  
    }
    
  }
}

struct OctSearchNodesData {

Search_node_result snr;
bool investigated;
double coords[3];
double best_dist;
MEField<> *src_cfield;
MEField<> *src_mask_field_ptr;
MeshObj *elem;
bool is_in;
  bool elem_masked;
};

static int found_func(void *c, void *y) {
  MeshObj &elem = *static_cast<MeshObj*>(c);
  OctSearchNodesData &si = *static_cast<OctSearchNodesData*>(y);

  // if we already have some one, then make sure this guy has a smaller id 
  if (si.is_in && (elem.get_id()>si.elem->get_id())) return 0; 

  // Get kernel
  const Kernel &ker = *elem.GetKernel();

    // Setup for source masks, if used
  std::vector<double> src_node_mask;
  MasterElement<> *mme;
  MEField<> *src_mask_field_ptr = si.src_mask_field_ptr;
  if (src_mask_field_ptr != NULL) {
    mme=GetME(*src_mask_field_ptr, ker)(METraits<>());
    src_node_mask.resize(mme->num_functions(),0.0);
  }
  
    // Set src mask status
    bool elem_masked=false;    
    if (src_mask_field_ptr != NULL && !src_node_mask.empty()) {
      GatherElemData<>(*mme, *src_mask_field_ptr, elem, &src_node_mask[0]);
      for (int i=0; i< mme->num_functions(); i++) {
	if (src_node_mask[i] > 0.5) {
	  elem_masked=true;
	  break;
	}
      }
    }
 
    // If we're masked and we already have someone then continue
    // NOTE: if we ever care about finding the lowest gid masked elem
    //       will have to change this
    if (elem_masked && si.is_in) return 0; 

    // Do the is_in calculation
  const MappingBase &map = GetMapping(elem);
  double pcoord[3];
  double dist;
    
  const MeshObjTopo *etopo = GetMeshObjTopo(elem);
  
  MasterElement<> &cme = *GetME(*si.src_cfield, ker)(METraits<>());
      
  std::vector<double> node_coord(cme.num_functions()*etopo->spatial_dim);
      
  GatherElemData<>(cme, *si.src_cfield, elem, &node_coord[0]);
    
  bool in = map.is_in_cell(&node_coord[0], si.coords, &pcoord[0], &dist);

  // if we're too far away don't even consider this as a fall back candidate
  if (!in && (dist > 1.0E-10)) return 0;

  // In or close enough, so set as a candidate, until someone better comes along...
  if (in) {
    std::copy(pcoord, pcoord+etopo->spatial_dim, &si.snr.pcoord[0]);
    si.best_dist = 0.0;
    si.elem = &elem;
    si.is_in=true;
    si.elem_masked=elem_masked;
  } else if (!si.is_in && (dist < si.best_dist)) {
    // Set up fallback candidate.
    std::copy(pcoord, pcoord+etopo->spatial_dim, &si.snr.pcoord[0]);
    si.best_dist = dist;
    si.elem = &elem;
    si.elem_masked=elem_masked;
  }

  // Mark that something is in struct
  si.investigated = true;
    
  //  return in&&!elem_masked ? 1 : 0;
  return 0;
}

// The main routine
  void OctSearch(const Mesh &src, const Mesh &dest, UInt dst_obj_type, int unmappedaction, SearchResult &result, double stol,
     std::vector<const MeshObj*> *to_investigate,OTree *box_in) {
  Trace __trace("Search(const Mesh &src, const Mesh &dest, UInt dst_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *to_investigate");

  //std::cout << "Start octree search" << std::endl;

  MEField<> &coord_field = *src.GetCoordField();

  MEField<> *src_mask_field_ptr = src.GetField("mask");

  
  // Destination coordinate is only a _field, not an MEField, since there
  // are no master elements.
  _field *dcptr = dest.Getfield("coordinates_1");
  ThrowRequire(dcptr);
  _field &dstcoord_field = *dcptr;

  // Get destination mask field
  _field *dmptr = dest.Getfield("mask_1");
  _field &dstmask_field = *dmptr;
   
  //MEField<> &dstcoord_field = *dest.GetCoordField();

  if (src.spatial_dim() != dest.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }

  // TODO: only grab objects in the current interpolation realm.

  // Load the destination objects into a list
  std::vector<const MeshObj*> dest_nlist;
  if (dmptr == NULL){ // No dest masks

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

  } else { // dest masks exist
    if (to_investigate == NULL) {
      MeshDB::const_iterator ni = dest.obj_begin(), ne = dest.obj_end();
      for (; ni != ne; ++ni) {
	if (dst_obj_type & ni->get_type()) {
	  // Get mask value
	  double *m=dstmask_field.data(*ni);
	  
	  // Only put objects in if they're not masked
	  if (*m < 0.5) {
	    dest_nlist.push_back(&*ni);
	  }
	}
      }
    } else {
      std::vector<const MeshObj*>::const_iterator ni = to_investigate->begin(),
                       ne = to_investigate->end();
      for (; ni != ne; ++ni) {
	// Get mask value
	double *m=dstmask_field.data(**ni);

	// Only put objects in if they're not masked
	if (*m < 0.5) {
          dest_nlist.push_back(*ni);
	}
      }
      if (dest_nlist.size() == 0) return;
    }
  }

  // Get a bounding box for the dest mesh.
  BBox dstBBox(dstcoord_field, dest);
  
  OTree *box;
  
  const double normexp = 0.15;
  const double dstint = 1e-8;
  
  if (!box_in) {
    int num_box = num_intersecting(src, dstBBox, dstint, normexp);
  
    box=new OTree(num_box); 

    populate_box(box, src, dstBBox, dstint, normexp);

    box->commit();

  } else box = box_in;
  
  UInt sdim = dest.spatial_dim();
  
  std::vector<const MeshObj *> again;
  
  std::set<Search_result> tmp_sr; // store in map for quick lookup
  
  // Loop the destination points, find hosts.
  for (UInt p = 0; p < dest_nlist.size(); ++p) {
    
    const MeshObj &node = *dest_nlist[p];
    
    
    const double *c = dstcoord_field.data(node);
    
    double pmin[3], pmax[3];
    
    pmin[0] = c[0]-stol;
    pmin[1] = c[1] - stol;
    pmin[2] = sdim == 3 ? c[2]-stol : -stol;
    
    pmax[0] = c[0] + stol;
    pmax[1] = c[1] + stol;
    pmax[2] = sdim == 3 ? c[2]+stol : +stol;
    
    OctSearchNodesData si;
    si.snr.node = &node;
    si.investigated = false;
    si.best_dist = std::numeric_limits<double>::max();
    si.src_cfield = &coord_field; 
    si.src_mask_field_ptr = src_mask_field_ptr; 
    si.is_in=false;
    si.elem_masked=false;
    si.elem=NULL;  

    // The node coordinates.
    si.coords[0] = c[0]; si.coords[1] = c[1]; si.coords[2] = (sdim == 3 ? c[2] : 0.0);
    
    
    box->runon(pmin, pmax, found_func, (void*)&si);
    
    if (!si.investigated) {
      again.push_back(&node);
    } else {
      if (si.elem_masked) {
	if (unmappedaction == ESMC_UNMAPPEDACTION_ERROR) {
	  Throw() << " Some destination points cannot be mapped to source grid";
	} else if (unmappedaction == ESMC_UNMAPPEDACTION_IGNORE) {
	  // don't do anything
	} else {
	  Throw() << " Unknown unmappedaction option";
	}
      } else {
	Search_result sr; sr.elem = si.elem;
	std::set<Search_result>::iterator sri =
	  tmp_sr.lower_bound(sr);
	if (sri == tmp_sr.end() || *sri != sr) {
	  sr.nodes.push_back(si.snr);
	  tmp_sr.insert(sri, sr);
	} else {
	  // std::cout << "second choice" << std::endl;
	  std::vector<Search_node_result> &r
	    = const_cast<std::vector<Search_node_result>&>(sri->nodes);
	  r.push_back(si.snr);	  
	  //std::cout << "size=" << sri->nodes.size() << std::endl;
	}
      }
    }
    
  } // for dest nodes
  
  {
    // Build seach res
    std::set<Search_result>::iterator si = 
      tmp_sr.begin(), se = tmp_sr.end();
    
    for (; si != se; ++si)
      result.push_back(new Search_result(*si));
  }
  
  std::set<Search_result>().swap(tmp_sr);
  std::vector<const MeshObj*>().swap(dest_nlist);

  if (!again.empty()) {
     if (stol > 1e-6) {
	if (unmappedaction == ESMC_UNMAPPEDACTION_ERROR) {
	  Throw() << " Some destination points cannot be mapped to source grid";	} else if (unmappedaction == ESMC_UNMAPPEDACTION_IGNORE) {
	  // don't do anything
	} else {
	  Throw() << " Unknown unmappedaction option";
	}
     } else {
       OctSearch(src, dest, dst_obj_type, unmappedaction, result, stol*1e+2, &again, box);
     }
  }

  if (!box_in) delete box;

}


// Search for ELEMS BEGIN --------------------------------
// NOTE::This finds the list of meshB elements which intersect with each meshA element and returns
//       it in sres

static int num_intersecting_elems(const Mesh &meshA, const BBox &meshBBBox, double btol, double nexp) {
  
  int ret = 0;

  MEField<> &coord_field = *meshA.GetCoordField();
  
  KernelList::const_iterator ki = meshA.set_begin(), ke = meshA.set_end();
  
  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;
    
    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;
    
    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();
    
    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());
    
    std::vector<double> node_coord(cme.num_functions()*meshA.spatial_dim());
    
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
   
     BBox bounding_box(coord_field, elem, nexp);
  
     // First check to see if the box even intersects the meshB mesh bounding
     // box.  
     if (BBoxIntersect(meshBBBox, bounding_box, btol)) ++ret;
  
    }
    
  }
  return ret;
}

  static void populate_box_elems(OTree *box, SearchResult &result, const Mesh &meshA, const BBox &meshBBBox, double btol, double nexp) {

  MEField<> &coord_field = *meshA.GetCoordField();

  // Get spatial dim of mesh
  UInt sdim = meshA.spatial_dim();
  
  KernelList::const_iterator ki = meshA.set_begin(), ke = meshA.set_end();
  
  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;
    
    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;
    
    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();
    
    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());
    
    std::vector<double> node_coord(cme.num_functions()*meshA.spatial_dim());
    
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
   
     BBox bounding_box(coord_field, elem, nexp);
  
     // First check to see if the box even intersects the meshB mesh bounding
     // box.  
     if (BBoxIntersect(meshBBBox, bounding_box, btol)) {
       
       // Create Search result
       Search_result *sr=new Search_result();       
       sr->elem=&elem;
       sr->elems.clear();

       // Add it to results list
       result.push_back(sr);

       // Add it to tree
       double min[3], max[3];
       
       min[0] = bounding_box.getMin()[0] - btol;
       min[1] = bounding_box.getMin()[1] - btol;
       if (sdim >2) min[2] = bounding_box.getMin()[2] - btol;
       else min[2] = - btol;

       max[0] = bounding_box.getMax()[0] + btol;
       max[1] = bounding_box.getMax()[1] + btol;
       if (sdim >2) max[2] = bounding_box.getMax()[2] + btol;
       else  max[2] = btol;
    
       /*
       if (elem.get_id() == 2426) {
         std::cout << "elem 2426, bbox=" << bounding_box << std::endl;
       }*/

       // Add element to search tree
       box->add(min, max, (void*)sr);
     }
  
    }
    
  }
}



struct OctSearchElemsData {
  const MeshObj *meshB_elem;
  bool found;
};

static int found_func_elems(void *c, void *y) {
  Search_result *sr = static_cast<Search_result*>(c);
  OctSearchElemsData *si = static_cast<OctSearchElemsData*>(y);


  // It might make sense to do something here to trim down the 
  // number of candidates beyond just those that intersect the 
  // minmax box of the search element. However, I'm not sure
  // that there is anything that would be more efficient than
  // just gathering them all and letting the clipping code
  // handle the detection of true intersection as is what
  // is currently being done. 

  sr->elems.push_back(si->meshB_elem);
  si->found=true;

  // Keep searching
  return 0;
}

// The main routine
// This constructs the list of meshB elements which intersects with each meshA element and returns
// this list in result. Each search_result in result contains a meshA element in elem and a list of intersecting meshB
// elements in elem  This function is symmertrical with regard to meshA or meshB, and when used
// for regrid either src or dest mesh may be used for either

  void OctSearchElems(const Mesh &meshA, int unmappedactionA, const Mesh &meshB, int unmappedactionB, 
                      double stol, SearchResult &result) {
  Trace __trace("OctSearchElems(const Mesh &meshA, const Mesh &meshB, UInt meshB_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *to_investigate");

  // Mesh A fields
  MEField<> &coord_field = *meshA.GetCoordField();
  MEField<> *meshA_mask_field_ptr = meshA.GetField("mask");

  
  // Mesh B fields
  MEField<> *dcptr = meshB.GetCoordField(); 
  ThrowRequire(dcptr);
  MEField<> &meshBcoord_field = *dcptr;

  // Get destination mask field
  MEField<> *dmptr = meshB.GetField("mask");
  MEField<> &meshBmask_field = *dmptr;
   

  if (meshA.spatial_dim() != meshB.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }

  // TODO: only grab objects in the current interpolation realm.

  // Load the meshBination objects into a list
  std::vector<const MeshObj*> meshB_elist;
  if (dmptr == NULL){ // No meshB masks
      MeshDB::const_iterator ei = meshB.elem_begin(), ee = meshB.elem_end();
      for (; ei != ee; ++ei) {
          meshB_elist.push_back(&*ei);
      }

      if (meshB_elist.size() == 0) return;

  } else { // meshB masks exist
      Throw() << " ERROR Masking not yet implemented for conservative";

      MeshDB::const_iterator ei = meshB.elem_begin(), ee = meshB.elem_end();
      for (; ei != ee; ++ei) {

	// Check the mask value of all the nodes in the elem, only
	// add if none are masked
#if 0
	  // Get mask value
	  double *m=meshBmask_field.data(*ni);
	  
	  // Only put objects in if they're not masked
	  if (*m < 0.5) {
	    meshB_elist.push_back(&*ei);
	  }
#endif

      }
  }
  if (meshB_elist.size() == 0) return;
  

  // Get a bounding box for the meshB mesh.
  BBox meshBBBox(meshBcoord_field, meshB);
  
  // declare some variables
  OTree *box=NULL;  
  const double normexp = 0.15;
  const double meshBint = 1e-8;
 
  // EVENTUALLY SKIP MASKED ELEMENTS WHEN ADDING SOURCE TO TREE
 
  // Count number of elements in tree
  int num_box = num_intersecting_elems(meshA, meshBBBox, meshBint, normexp);
  
  // Construct box tree
  box=new OTree(num_box); 
  
  // Construct search result list 
  result.reserve(num_box);
  
  // Fill tree with search result structs to fill
  // with intesecting elements
  populate_box_elems(box, result, meshA, meshBBBox, meshBint, normexp);
  box->commit();
  
  // Dimension of meshB
  UInt sdim = meshB.spatial_dim();
    
  
  // Loop the mesh B elements, find the corresponding mesh A elements
  bool meshB_elem_not_found=false;
  for (UInt p = 0; p < meshB_elist.size(); ++p) {
    
    const MeshObj &meshB_elem = *meshB_elist[p];
    
    BBox meshB_bbox(meshBcoord_field, meshB_elem);

    double min[3], max[3];       
    min[0] = meshB_bbox.getMin()[0] - stol;
    min[1] = meshB_bbox.getMin()[1] - stol;
    if (sdim >2) min[2] = meshB_bbox.getMin()[2] - stol;
    else min[2] = - stol;

    max[0] = meshB_bbox.getMax()[0] + stol;
    max[1] = meshB_bbox.getMax()[1] + stol;
    if (sdim >2) max[2] = meshB_bbox.getMax()[2] + stol;
    else  max[2] = stol;

    OctSearchElemsData si;
    si.meshB_elem=&meshB_elem;
    si.found=false;

    box->runon(min, max, found_func_elems, (void*)&si);

    if (!si.found) {
      meshB_elem_not_found=true;
    } 

  } // for mesh B elems
  
  // Check for meshB elements which haven't been intersected
  if (meshB_elem_not_found) {
    if (unmappedactionB == ESMC_UNMAPPEDACTION_ERROR) {
      Throw() << " Some mesh B elements do not intersect with mesh A";	
    } else if (unmappedactionB == ESMC_UNMAPPEDACTION_IGNORE) {
      // don't do anything
    } else {
      Throw() << " Unknown unmappedaction option";
    }
  }

  // Check for meshA elements which haven't been intersected
  // MIGHT BE MORE EFFICIENT TO CHECK IF MATRIX ROW SUMS TO 1.0
  if (unmappedactionA == ESMC_UNMAPPEDACTION_ERROR) {
    SearchResult::iterator sb = result.begin(), se = result.end();
    for (; sb != se; sb++) {
      Search_result &sr = **sb;

      if (sr.elems.empty()) {
	Throw() << " Some mesh A elements do not intersect with mesh B";	
      }
    }
  }

  // Get rid of box tree
  if (box != NULL) delete box;
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


void DestroySearchResult(SearchResult &result) {
  // Delete contents of search results
  SearchResult::const_iterator si = result.begin(), se = result.end();
  for (; si != se; ++si) {
    Search_result *sr = *si;
    delete sr;
  }
 
  // Empty search results
  result.clear();
}

} // namespace
