// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_OTree.h>

#include "PointList/include/ESMCI_PointList.h"

#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include <Mesh/include/Legacy/ESMCI_BBox.h>



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {


extern bool mathutil_debug;

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
 /* XMRKX */

  static int num_intersecting(const Mesh &src, bool on_sph, const BBox &dstBBox, double btol, double nexp) {

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

      BBox bounding_box(coord_field, elem, nexp, on_sph);

     // First check to see if the box even intersects the dest mesh bounding
     // box.
     if (BBoxIntersect(dstBBox, bounding_box, btol)) ++ret;
    }

  }
  return ret;
}

  static void populate_box(OTree *box, const Mesh &src, bool on_sph, const BBox &dstBBox, double btol, double nexp) {

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

      BBox bounding_box(coord_field, elem, nexp, on_sph);

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
  bool set_dst_status;
};

static int found_func(void *c, void *y) {
  MeshObj &elem = *static_cast<MeshObj*>(c);
  OctSearchNodesData &si = *static_cast<OctSearchNodesData*>(y);


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
  if (si.snr.dst_gid==ESMF_REGRID_DEBUG_MAP_NODE) {
    printf("Checking dst pnt id=%d vs. elem id=%d\n",si.snr.dst_gid,elem.get_id());
  }
#endif

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
    // if (elem_masked && si.is_in) return 0;

    // Instead of the above, if this element is masked then skip altogether
    // this prevents problems with bad coords in masked elements
    if (!si.set_dst_status && elem_masked) return 0;

    // Do the is_in calculation
  const MappingBase &map = GetMapping(elem);
  double pcoord[3]={0.0,0.0,0.0};
  double dist;

  const MeshObjTopo *etopo = GetMeshObjTopo(elem);

  MasterElement<> &cme = *GetME(*si.src_cfield, ker)(METraits<>());

  std::vector<double> node_coord(cme.num_functions()*etopo->spatial_dim);

  GatherElemData<>(cme, *si.src_cfield, elem, &node_coord[0]);


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
  if (si.snr.dst_gid==ESMF_REGRID_DEBUG_MAP_NODE) {
     mathutil_debug=true;
  }
#endif

  bool in = map.is_in_cell(&node_coord[0], si.coords, &pcoord[0], &dist);


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
  if (si.snr.dst_gid == ESMF_REGRID_DEBUG_MAP_NODE) {
    printf("Mapping dst_id=%d dst_coords=%f %f %f in=%d pcoords=%f %f %f dist=%e s_elem=%d [",si.snr.dst_gid,si.coords[0],si.coords[1],si.coords[2],in,pcoord[0],pcoord[1],pcoord[2],dist,elem.get_id());

    double coords[3*40];
    int num_nds;
    int ids[40];

    get_elem_coords_and_ids(&elem, si.src_cfield, etopo->spatial_dim, 40, &num_nds, coords, ids);

    for (int i=0; i<num_nds; i++) {
      double *pnt=coords+3*i;

      printf("%d ",ids[i]);
    }
    printf("]\n");
    fflush(stdout);

    mathutil_debug=false;
  }
#endif


  // if we're too far away don't even consider this as a fall back candidate
  if (!in && (dist > 1.0E-8)) return 0;


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
  MEField<> *meshA_mask_field_ptr = meshA.GetField("elem_mask");


  // Mesh B fields
  MEField<> *dcptr = meshB.GetCoordField();
  ThrowRequire(dcptr);
  MEField<> &meshBcoord_field = *dcptr;

  // Get destination mask field
  MEField<> *dmptr = meshB.GetField("elem_mask");
  MEField<> &meshBmask_field = *dmptr;

  if (meshA.spatial_dim() != meshB.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }


  // Load the meshB objects into a list
  std::vector<const MeshObj*> meshB_elist;
  if (dmptr == NULL){ // No meshB masks
      MeshDB::const_iterator ei = meshB.elem_begin(), ee = meshB.elem_end();
       for (; ei != ee; ++ei) {
          meshB_elist.push_back(&*ei);
      }
  } else { // meshB masks exist
      MeshDB::const_iterator ei = meshB.elem_begin(), ee = meshB.elem_end();
      for (; ei != ee; ++ei) {
          // Get mask value
          double *m=meshBmask_field.data(*ei);
        
          // Only put objects in if they're not masked
          if (*m < 0.5) {
            meshB_elist.push_back(&*ei);
          }
      }
  }

  if (meshB_elist.size() == 0) return;


  // Get a bounding box for the meshB mesh.
  // TODO: NEED TO MAKE BOUNDING BOX ONLY DEPEND ON NON-MASKED ELEMENTS
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

    BBox meshB_bbox(meshBcoord_field, meshB_elem, normexp);

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
    if (unmappedactionB == ESMCI_UNMAPPEDACTION_ERROR) {
      Throw() << " Some mesh B elements do not intersect with mesh A";  
    } else if (unmappedactionB == ESMCI_UNMAPPEDACTION_IGNORE) {
      // don't do anything
    } else {
      Throw() << " Unknown unmappedaction option";
    }
  }

  // Check for meshA elements which haven't been intersected
  // MIGHT BE MORE EFFICIENT TO CHECK IF MATRIX ROW SUMS TO 1.0
  if (unmappedactionA == ESMCI_UNMAPPEDACTION_ERROR) {
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

BBox bbox_from_pl(PointList &dst_pl) {

    // Init min to biggest double
    double min[3];
    min[0]=std::numeric_limits<double>::max();
    min[1]=std::numeric_limits<double>::max();
    min[2]=std::numeric_limits<double>::max();

    // Init max to smallest double
    double max[3];
    max[0]=-std::numeric_limits<double>::max();
    max[1]=-std::numeric_limits<double>::max();
    max[2]=-std::numeric_limits<double>::max();

    // Calc min max from point list depending on dim
    if (dst_pl.get_coord_dim()==2) {
      for(int i=0; i<dst_pl.get_curr_num_pts(); i++) {
        const double *coords=dst_pl.get_coord_ptr(i);

        if (coords[0] < min[0]) min[0]=coords[0];
        if (coords[1] < min[1]) min[1]=coords[1];

        if (coords[0] > max[0]) max[0]=coords[0];
        if (coords[1] > max[1]) max[1]=coords[1];
      }
    } else if (dst_pl.get_coord_dim()==3) {
      for(int i=0; i<dst_pl.get_curr_num_pts(); i++) {
        const double *coords=dst_pl.get_coord_ptr(i);

        if (coords[0] < min[0]) min[0]=coords[0];
        if (coords[1] < min[1]) min[1]=coords[1];
        if (coords[2] < min[2]) min[2]=coords[2];

        if (coords[0] > max[0]) max[0]=coords[0];
        if (coords[1] > max[1]) max[1]=coords[1];
        if (coords[2] > max[2]) max[2]=coords[2];
      }
    } else {
      Throw() << "unsupported number of coordinate dimensions \n";
    }

    // Create BBox
    return BBox(dst_pl.get_coord_dim(), min, max);
}

  // Loop through search results and clamp them to be within the cell to ensure monotonicity.
  // Do this after the search so we are not changing which cell a point is mapped to. Also, so we only
  // do it once per cell.
  void ClampPCoords(int pdim, SearchResult &result) {

    // Process results depending on parametric dimension
    SearchResult::iterator sb = result.begin(), se = result.end();
    if (pdim == 2) {
      for (; sb != se; sb++) {
        Search_result *sr = *sb;

        // Get number of nodes in element
        const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*(sr->elem));
        int num_nodes=topo->num_nodes;

        // Clamp based on shape indicated by the number of nodes
        if (num_nodes == 3) { // Tri
          for (int i= 0; i<sr->nodes.size(); i++) {
            double *pcoord=sr->nodes[i].pcoord;

            // Clamp into the correct range
            if (pcoord[0] < 0.0) pcoord[0]=0.0;
            else if (pcoord[0] > 1.0) pcoord[0]=1.0;

            if (pcoord[1] < 0.0) pcoord[1]=0.0;
            else if (pcoord[1] > 1.0) pcoord[1]=1.0;

            // If over the slanty side, then fix that too
            if (pcoord[0] + pcoord[1] > 1.0) {
              // Distance outside
              double dist=(pcoord[0]+pcoord[1])-1.0;

              // Try taking 1/2 off, otherwise take what we can
              if (((pcoord[0]-0.5*dist) >= 0.0) &&
                  ((pcoord[1]-0.5*dist) >= 0.0)) {
                pcoord[0]=pcoord[0]-0.5*dist;
                pcoord[1]=pcoord[1]-0.5*dist;
              } else {
                if (dist < pcoord[0]) {
                  pcoord[0]=pcoord[0]-dist;
                } else {
                  double extra=dist-pcoord[0];
                  pcoord[0]=0.0; // take as much from here as we can
                  pcoord[1]=pcoord[1]-extra; // take the rest from here
                }
              }
            }
          }
        } else if (num_nodes == 4) { // Quad
          for (int i= 0; i<sr->nodes.size(); i++) {
            double *pcoord=sr->nodes[i].pcoord;

            if (pcoord[0] < -1.0) pcoord[0]=-1.0;
            else if (pcoord[0] > 1.0) pcoord[0]=1.0;

            if (pcoord[1] < -1.0) pcoord[1]=-1.0;
            else if (pcoord[1] > 1.0) pcoord[1]=1.0;
          }

        } else {
          Throw() << " Element has an unexpected number of nodes for a 2D shape.";
        }
      }
    } else if (pdim == 3) {
      for (; sb != se; sb++) {
        Search_result *sr = *sb;

        // Get number of nodes in element
        const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*(sr->elem));
        int num_nodes=topo->num_nodes;

        // Clamp based on shape indicated by the number of nodes
        if (num_nodes == 4) { // Tetra

          // these don't work in bilinear/patch anyways right now, so
          // don't worry about it until they do.

        } else if (num_nodes == 8) { // Hex
          for (int i= 0; i<sr->nodes.size(); i++) {
            double *pcoord=sr->nodes[i].pcoord;

            if (pcoord[0] < -1.0) pcoord[0]=-1.0;
            else if (pcoord[0] > 1.0) pcoord[0]=1.0;

            if (pcoord[1] < -1.0) pcoord[1]=-1.0;
            else if (pcoord[1] > 1.0) pcoord[1]=1.0;

            if (pcoord[2] < -1.0) pcoord[2]=-1.0;
            else if (pcoord[2] > 1.0) pcoord[2]=1.0;
          }
        } else {
          Throw() << " Element has an unexpected number of nodes for a 3D shape.";
        }
      }
    }
  }


// The main routine
// dst_pl is assumed to only contain non-masked points
  void OctSearch(const Mesh &src, PointList &dst_pl, MAP_TYPE mtype, UInt dst_obj_type, int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status, double stol, std::vector<int> *revised_dst_loc, OTree *box_in) {
    Trace __trace("OctSearch(const Mesh &src, PointList &dst_pl, MAP_TYPE mtype, UInt dst_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *revised_dst_loc, OTree *box_in)");

  if (dst_pl.get_curr_num_pts() == 0)
    return;

  MEField<> &coord_field = *src.GetCoordField();
  MEField<> *src_mask_field_ptr = src.GetField("mask");

  // Set some parameters to control search
  const double normexp = 0.15;
  const double dstint = 1e-8;


  // If using great circle edges, set flag to treat as on a spherical surface when generating seach boxes
  bool on_sph=false;
  if (mtype==MAP_TYPE_GREAT_CIRCLE) {
    on_sph=true;
  }

  // Get spatial dim  and error check
  UInt sdim = src.spatial_dim();
  if (sdim != dst_pl.get_coord_dim()) {
    Throw() << "Mesh and pointlist must have same spatial dim for search";
  }

  // Fill search box tree
  OTree *box;
  if (!box_in) {
    // Get a bounding box for the dst point list
    BBox dstBBox=bbox_from_pl(dst_pl);

    // Count number of elements to go into tree
    int num_box = num_intersecting(src, on_sph, dstBBox, dstint, normexp);

    // Create tree
    box=new OTree(num_box);

    // Fill tree
    populate_box(box, src, on_sph, dstBBox, dstint, normexp);

    // Commit
    box->commit();
  } else box = box_in;

  // Get list of destination points to look at.
  std::vector<int> *dst_loc;

  // vector to hold new location if necessary
  std::vector<int> dst_loc_new;

  // Either create a new one or use list from the finer search
  if (revised_dst_loc) dst_loc=revised_dst_loc;
  else {
    dst_loc_new.reserve(dst_pl.get_curr_num_pts());

    for(int i=0; i<dst_pl.get_curr_num_pts(); i++) {
      dst_loc_new.push_back(i);
    }

    dst_loc=&dst_loc_new;
  }

  // vector to hold loc to search in future
  std::vector<int> again;

  // temp search results
  std::set<Search_result> tmp_sr;

  // Loop the destination loc, find hosts.
  for (UInt p = 0; p < dst_loc->size(); ++p) {
    int loc = (*dst_loc)[p];

    // Get info out of point list
    const double *pnt_crd=dst_pl.get_coord_ptr(loc);
    int pnt_id=dst_pl.get_id(loc);


    // Calc min max box around point
    double pmin[3], pmax[3];
    pmin[0] = pnt_crd[0]-stol;
    pmin[1] = pnt_crd[1] - stol;
    pmin[2] = sdim == 3 ? pnt_crd[2]-stol : -stol;

    pmax[0] = pnt_crd[0] + stol;
    pmax[1] = pnt_crd[1] + stol;
    pmax[2] = sdim == 3 ? pnt_crd[2]+stol : +stol;

    OctSearchNodesData si;
    si.snr.dst_gid = pnt_id;
    si.investigated = false;
    si.best_dist = std::numeric_limits<double>::max();
    si.src_cfield = &coord_field;
    si.src_mask_field_ptr = src_mask_field_ptr;
    si.is_in=false;
    si.elem_masked=false;
    si.elem=NULL;
    si.set_dst_status=set_dst_status;

    // The point coordinates.
    si.coords[0] = pnt_crd[0]; si.coords[1] = pnt_crd[1]; si.coords[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    // Set global map_type
    // TODO: pass this directly to is_in_cell mapping function
    MAP_TYPE old_sph_map_type=sph_map_type;
    sph_map_type=mtype;

    // Do Search and mapping
    box->runon(pmin, pmax, found_func, (void*)&si);

    // Reset global map_type
    sph_map_type=old_sph_map_type;

    // process output from search
    if (!si.investigated) {
      again.push_back(loc);
    } else {
      if (si.elem_masked) {
        // Mark this as unmapped due to src masking
        if (set_dst_status) {
           int dst_id=si.snr.dst_gid;

           // Set col info
           WMat::Entry col(ESMC_REGRID_STATUS_SRC_MASKED,
                               0, 0.0, 0);

           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);

           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
        }

        // This is actually handled at the top of regridding now, so that
        // we have all the destination results on their home processor, but leave
        // this in for now until we can take it out everywhere.
        if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
          Throw() << " Some destination points cannot be mapped to source grid";
        } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
          // don't do anything
        } else {
          Throw() << " Unknown unmappedaction option";
        }
      } else {
        // Mark this as mapped
        if (set_dst_status) {
           int dst_id=si.snr.dst_gid;

           // Set col info
           WMat::Entry col(ESMC_REGRID_STATUS_MAPPED,
                               0, 0.0, 0);

           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);

           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
        }

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
  std::vector<int>().swap(*dst_loc);

  if (!again.empty()) {
     if (stol > 1e-6) {
 /* XMRKX */
       // Mark anything that hasn't been mapped as unmapped
       if (set_dst_status) {
         for (UInt p = 0; p < again.size(); ++p) {
           int loc = again[p];
           int dst_id=dst_pl.get_id(loc);

           // Set col info
           WMat::Entry col(ESMC_REGRID_STATUS_OUTSIDE, 0,
                               0.0, 0);

           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);

           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
         }
       }


       if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
         Throw() << " Some destination points cannot be mapped to source grid";
       } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
         // don't do anything
       } else {
         Throw() << " Unknown unmappedaction option";
       }
     } else {
       OctSearch(src, dst_pl, mtype, dst_obj_type, unmappedaction, result, set_dst_status, dst_status, stol*1e+2, &again, box);
     }
  }

  // Exiting top of recursion, so do a few things before we leave
  if (!box_in) {
    // Clamp pcoords in results to be inside elements to avoid non-montonicity
    // Doing this after search, so changing pcoords doesn't interfer
    // with search (e.g. which are considered close)
    ClampPCoords(src.parametric_dim(), result);

    // Get rid of search structure
    delete box;
  }
}

} // namespace
