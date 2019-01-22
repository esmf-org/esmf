// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include "ESMCI_F90Interface.h"
#include <ESMCI_LogErr.h>
#include "ESMCI_Macros.h"

#include "Mesh/include/Legacy/ESMCI_BBox.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_Polynomial.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_Ftn.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>
#include <Mesh/include/Legacy/ESMCI_Phedra.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>

#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_XGridUtil.h>

#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search_EToP.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh_Mapping.h>
#include <Mesh/include/ESMCI_MBMesh_Glue.h>

// for moab Element mappings 
#include "moab/ElemEvaluator.hpp"
#include "moab/CartVect.hpp"
// for SphericalQuad
//#include "ElemUtil.hpp"

using std::vector;


// #define DEBUG_MASK
// #define DEBUG_PCOORDS
// #define DEBUG_SEARCH
// #define DEBUG_SEARCH_RESULTS
// #define DEBUG_REGRID_STATUS

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

struct SearchData {
  etop_sr snr;
  bool investigated;
  bool is_in;
  bool elem_masked;
  double coords[3];
  EntityHandle elem;
  MBMesh *mesh;
  MB_MAP_TYPE line_type;
  double dist;
  //MEField<> *src_cfield;
  // double *src_mask_field_ptr[8];
  bool set_dst_status;
};


// NOTE::This finds the list of meshB elements which intersect with each meshA
// element and returns it in sres
static int num_intersecting_elems(MBMesh *mbmp, const BBox &meshBBBox, double btol, double nexp, bool is_sph) {

  int ret = 0;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;

     const MBMesh_BBox bounding_box(mbmp, elem, nexp, is_sph);

     // First check to see if the box even intersects the meshB mesh bounding
     // box.
     if (Mixed_BBoxIntersect(bounding_box, meshBBBox, btol)) ++ret;
  }

  return ret;
}

static void populate_box_elems(OTree *box,
                               MBMesh_Search_EToP_Result_List &result,
                               MBMesh *mbmp, const BBox &meshBBBox,
                               double btol, double nexp, bool is_sph) {

  // Get spatial dim of mesh
  int sdim = mbmp->sdim;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;

    MBMesh_BBox bounding_box(mbmp, elem, nexp, is_sph);

    // First check to see if the box even intersects the meshB mesh bounding
    // box.
    if (Mixed_BBoxIntersect(bounding_box, meshBBBox, btol)) {

      // Create Search result
      MBMesh_Search_EToP_Result *sr=new MBMesh_Search_EToP_Result();
      sr->src_elem=elem;
      sr->dst_nodes.clear();

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

  int srid; MBMesh_get_gid(mbmp, sr->src_elem, &srid);
#ifdef DEBUG_SEARCH
  printf("%d# elem %d pmin/max [%f, %f], [%f, %f] \n",  Par::Rank(), srid, min[0], min[1], max[0], max[1]);
#endif
      // Add element to search tree
      box->add(min, max, (void*)sr);
    }

  }

}

static int found_func(void *c, void *y) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MBMesh_Search_EToP_found_func"

  MBMesh_Search_EToP_Result *sr = static_cast<MBMesh_Search_EToP_Result*>(c);
  SearchData *si = static_cast<SearchData*>(y);
  // NOTE: sr and si should use same mesh, if not we have big problems

  int srid; MBMesh_get_gid(si->mesh, sr->src_elem, &srid);
  int siid; MBMesh_get_gid(si->mesh, si->elem, &siid);

#ifdef DEBUG_SEARCH
  if (si->snr.dst_gid == 6) printf("%d# Search against %d [%d]\n", Par::Rank(), srid, siid);
#endif

  // from search.c
  // if we already have a source element, continue if this element has a smaller id
  if (si->is_in && (srid > siid)) return 0;


  // get coordinates of corners of element
  int num_nodes;
  int nd = si->mesh->sdim;
  int max_num_nodes = 8;
  double coords[nd*max_num_nodes];
  MBMesh_get_elem_coords(si->mesh, sr->src_elem, max_num_nodes, &num_nodes, coords);

// Setup for source masks, if used

  int localrc = 0;
  int merr = 0;

#ifdef DEBUG_MASK
  if (si->mesh->has_node_mask) {
    Range nodes;
    merr=si->mesh->mesh->get_entities_by_dimension(0, 0, nodes);
    if (merr != MB_SUCCESS) throw (ESMC_RC_MOAB_ERROR);

    int num_verts = nodes.size();
    int src_node_mask[num_verts];

  printf("%d# - elem %d has_node_mask == %s [", Par::Rank(), srid, si->mesh->has_node_mask ? "true" : "false");

    merr=si->mesh->mesh->tag_get_data(si->mesh->node_mask_val_tag, nodes, &src_node_mask);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    for (int i = 0; i < num_verts; ++i)
      printf("%d, ", src_node_mask[i]);
    printf("]\n");
  }
#endif


  Range nodes;
  bool elem_masked=false;
  if (si->mesh->has_node_mask) {
    // merr=si->mesh->mesh->get_entities_by_handle(sr->src_elem, nodes);
    merr=si->mesh->mesh->get_connectivity(&(sr->src_elem), 1, nodes);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    int num_verts = nodes.size();
    int src_node_mask[num_verts];

    if (num_nodes != num_verts)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    merr=si->mesh->mesh->tag_get_data(si->mesh->node_mask_tag, nodes, &src_node_mask);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

#ifdef DEBUG_MASK
  printf("%d# Node %d src_node_mask = [", Par::Rank(), si->snr.dst_gid);
  for (int i = 0; i < num_verts; ++i)
    printf("%d, ", src_node_mask[i]);
  printf("]\n");
#endif

    for (int i=0; i< num_verts; i++) {
      if (src_node_mask[i] > 0.5) {
        elem_masked=true;
        break;
      }
    }
  }

    // Instead of the above, if this element is masked then skip altogether
    // this prevents problems with bad coords in masked elements
    if (!si->set_dst_status && elem_masked) return 0;


  MBMappingBase *map = NULL;
  if (num_nodes == 3) {
    if (si->mesh->sdim == si->mesh->pdim)
      map = MB_POLY_Mapping<tri_shape_func, MBTraits<> >::instance();
    else
      map = MB_POLY_Mapping<tri_shape_func, MBTraits<>, 3,2>::instance();
  } else if (num_nodes == 4) {
    if (si->mesh->sdim == si->mesh->pdim)
      map = MB_POLY_Mapping<quad_shape_func, MBTraits<> >::instance();
    else
      map = MB_POLY_Mapping<quad_shape_func, MBTraits<>, 3,2>::instance();
  } else if (num_nodes == 8) {
    if (si->mesh->sdim == si->mesh->pdim)
      map = MB_POLY_Mapping<hex_shape_func, MBTraits<> >::instance();
    else {
      std::string err = "Hexagons cannot be built in 2D parametric space.";
      throw(err.c_str());
    }
  } else {
    std::string err = "Only triangles, quadrilaterals and hexagons are supported at this time.";
    throw(err.c_str());
  }

#ifdef DEBUG_PCOORDS
  printf("%d# Pcoords - Elem %d: ", Par::Rank(), srid);
  for (int i = 0; i < num_nodes; ++i) {
    printf("[");
    for (int j = 0; j < nd; ++j) {
      printf("%f, ", coords[i*nd+j]);
    }
    printf("], ");
  }
  printf("\nNode %d: [", si->snr.dst_gid);
  for (int i=0; i < nd; ++i)
    printf("%f, ", si->coords[i]);
  printf("]\n");
#endif // DEBUG_PCOORDS

//   int is_inside = 0;
//   double pcoords[3];
// 
//   if (nd == 2) {
//     // ElemEvaluator ee = ElemEvaluator(si->mesh->mesh, sr->src_elem);
//     // ee.reverse_eval(si->coords, 1e-8, 1e-6, pcoords, &is_inside);
// 
//     MBElemMap map = MBElemMap();
//     bool inside = map.cartesian_eval(coords, si->coords, num_nodes, pcoords, NULL);
//     is_inside = static_cast<int> (inside);
// 
// #ifdef DEBUG_PCOORDS
//     printf("Cartesian parametric coordinates via ESMF\n");
//     printf("  Node %d params = [%f,%f,%f]\n", si->snr.dst_gid, pcoords[0], pcoords[1], pcoords[2]);
// #endif
//     // translate pcoords from [-1,1] to [0,1]
//     // translate(pcoords);
//   } else if (nd == 3) {
//     MBElemMap map = MBElemMap();
//     bool inside = map.spherical_eval(coords, si->coords, num_nodes, pcoords, NULL);
//     is_inside = static_cast<int> (inside);
// #ifdef DEBUG_PCOORDS
//     printf("spherical parametric coordinates via ESMF\n");
//     printf("  Node %d params = [%f,%f,%f]\n", si->snr.dst_gid, pcoords[0], pcoords[1], pcoords[2]);
// #endif
//   }

  mb_sph_map_type = si->line_type;

  double dist;
  double pcoords[3];
  bool is_inside = map->is_in_cell(coords, si->coords, pcoords, &dist);

#ifdef DEBUG_PCOORDS
  printf("%d# is inside = %s\n", Par::Rank(), is_inside ? "true" : "false");
#endif

  // if we're too far away don't even consider this as a fall back candidate
  if (!is_inside && (dist > 1.0E-8)) return 0;

    // if quad or hex transform pcoords to [0,1]
    if (num_nodes > 3)
      translate(pcoords);

    // set the search data pcoords structure
    si->snr.pcoord[0] = pcoords[0];
    si->snr.pcoord[1] = pcoords[1];
    si->snr.pcoord[2] = pcoords[2];

    si->elem = sr->src_elem;
    si->elem_masked=elem_masked;

#ifdef DEBUG_PCOORDS
    printf("%d# Node %d pcoords = [", Par::Rank(), si->snr.dst_gid);
    for (int i = 0; i < nd; ++i)
      printf("%f, ", si->snr.pcoord[i]);
    printf("]\n");
#endif

  if (is_inside) {
    // sr->dst_nodes.push_back(si->snr);
    si->dist = 0.0;
    si->is_in=true;
  } else if (!si->is_in && (dist < si->dist)) {
    si->dist = dist;
  }
  
#ifdef DEBUG_SEARCH
  printf("%d# investigated %d .. keep searching\n", Par::Rank(), siid);
#endif
  // Mark that something is in struct
  si->investigated=true;

  // Keep searching
  return 0;
}


//#ifdef SPHERICAL
//  // convert to SphericalQuad?
//  vector<CartVect> verts;
//  for (int i = 0; i < num_nodes; ++i)
//    verts.push_back(CartVect(coords[i*nd+0], coords[i*nd+1], coords[i*nd+2]));
//  Element::SphericalQuad elem = Element::SphericalQuad(verts);
//
//  CartVect pos = CartVect(si->coords);
//  double tol = 1e-1;
//  bool is_inside = elem.inside_box(pos, tol);
//
//#ifdef DEBUG_PCOORDS
//  printf("\n\n  is_inside = %d\n", is_inside);
//  printf("\n  Node %d: CartVec coords = [", si->snr.dst_gid);
//  double cvcoords[3];
//  pos.get(cvcoords);
//  for (int i=0; i < nd; ++i)
//    printf("%f, ", cvcoords[i]);
//  printf("]\n");
//#endif // DEBUG_PCOORDS
//
//  CartVect pcoords_cv;
//  double pcoords[3];
//  if (is_inside) {
//    tol = 1e-8;
//    pcoords_cv = elem.ievaluate(tol, pos);
//    pcoords_cv.get(pcoords);
//  }
//#endif // SPHERICAL





// This constructs the list of pointlist nodes that intersect with each meshA
// element and returns this list in result. Each search_result in result
// contains a meshA element in elem and a list of intersecting pointlist nodes
// in dst_nodes
void MBMesh_Search_EToP(MBMesh *mbmAp,
                        PointList *mbmBp, int unmappedaction,
                        int *map_type, double stol, 
                        MBMesh_Search_EToP_Result_List &result,
                        bool set_dst_status, WMat &dst_status,
                        std::vector<int> *revised_dst_loc, OTree *box_in) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MBMesh_Search_EToP"

  Trace __trace("MBMesh_Search_EToP()");

#ifdef DEBUG_SEARCH
  std::cout << Par::Rank() << "# MBMesh_Search_EToP, stol =" << stol << std::endl;
  
  // int rc;
  // void *mbptr = (void *) mbmAp;
  // int len = 12; char fname[len];
  // sprintf(fname, "meshsearch_%d", Par::Rank());
  // MBMesh_write(&mbptr, fname, &rc, len);

#endif


  // MOAB error
  int merr;

  // leave if nothing to search
  if (mbmBp->get_curr_num_pts() == 0)
    return;

  // declare some variables
  const double normexp = 0.15;
  const double meshBint = 1e-8;
  
    // handle map type
  bool is_sph = false;
  if (*map_type == MB_MAP_TYPE_GREAT_CIRCLE) is_sph = true;

  int sdim = mbmAp->sdim;
  if (sdim != mbmBp->get_coord_dim()) {
    Throw() << "Mesh and pointlist must have same spatial dim for search";
  }

  // TODO: NEED TO MAKE BOUNDING BOX ONLY DEPEND ON NON-MASKED ELEMENTS
  
  // Get global bounding box of pointlist
  double cmin[sdim], cmax[sdim];
  build_pl_bbox(cmin, cmax, mbmBp);
  OTree *box=NULL;
  if (!box_in) {
    BBox MeshBBBox(sdim, cmin, cmax);
    
    // Count number of elements in tree
    int num_box = num_intersecting_elems(mbmAp, MeshBBBox, meshBint, normexp,   is_sph);
    
    // Construct box tree
    box=new OTree(num_box);
    
    // Construct search result list
    result.reserve(num_box);
    
    // Fill tree with search result structs to fill
    // with intersecting elements
    populate_box_elems(box, result, mbmAp, MeshBBBox, meshBint, normexp, is_sph);
    box->commit();
  } else box = box_in;


  // Get list of destination points to look at.
  std::vector<int> *dst_loc;

  // vector to hold new location if necessary
  std::vector<int> dst_loc_new;

  // Either create a new one or use list from the finer search
  if (revised_dst_loc) dst_loc=revised_dst_loc;
  else {
    dst_loc_new.reserve(mbmBp->get_curr_num_pts());

    for(int i=0; i<mbmBp->get_curr_num_pts(); i++) {
      dst_loc_new.push_back(i);
    }

    dst_loc=&dst_loc_new;
  }

  // temp search results
  std::set<MBMesh_Search_EToP_Result> tmp_sr;

  // vector to hold loc to search in future
  std::vector<int> again;

  // Loop the destination loc, find hosts.
  for (int p = 0; p < dst_loc->size(); ++p) {
    int loc = (*dst_loc)[p];

    // Get info out of point list
    const double *pnt_crd=mbmBp->get_coord_ptr(loc);
    int pnt_id=mbmBp->get_id(loc);


    // Calc min max box around point
    double pmin[3], pmax[3];
    pmin[0] = pnt_crd[0]-stol;
    pmin[1] = pnt_crd[1] - stol;
    pmin[2] = sdim == 3 ? pnt_crd[2]-stol : -stol;

    pmax[0] = pnt_crd[0] + stol;
    pmax[1] = pnt_crd[1] + stol;
    pmax[2] = sdim == 3 ? pnt_crd[2]+stol : +stol;

    SearchData si;
    si.snr.node=mbmBp->get_point(p);
    si.snr.dst_gid=pnt_id;
    si.investigated=false;
    si.dist = std::numeric_limits<double>::max();
    si.is_in=false;
    si.elem_masked=false;
    si.elem=NULL;
    si.set_dst_status=set_dst_status;
    si.mesh=mbmAp;
    if (*map_type == MB_MAP_TYPE_GREAT_CIRCLE)
      si.line_type=MB_MAP_TYPE_GREAT_CIRCLE;
    else if (*map_type == MB_MAP_TYPE_CART_APPROX)
      si.line_type=MB_MAP_TYPE_CART_APPROX;

    // The point coordinates.
    si.coords[0] = pnt_crd[0]; si.coords[1] = pnt_crd[1]; si.coords[2] = (sdim == 3 ? pnt_crd[2] : 0.0);
#ifdef DEBUG_SEARCH
    printf("%d# Point %d  pmin/max [%f, %f], [%f, %f] \n", Par::Rank(), pnt_id, pmin[0], pmin[1], pmax[0], pmax[1]);
#endif

    box->runon(pmin, pmax, found_func, (void*)&si);

    // add to dst_nodes here
    if (!si.investigated) {

      // this is the new method
      again.push_back(loc);
#ifdef DEBUG_SEARCH
printf("%d# again add node %d\n", Par::Rank(), pnt_id);
#endif
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
           
#ifdef DEBUG_REGRID_STATUS
printf("%d# dst_id %d status %d\n", Par::Rank(), dst_id, ESMC_REGRID_STATUS_SRC_MASKED);
#endif
        }

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
           WMat::Entry col(ESMC_REGRID_STATUS_MAPPED, 0, 0.0, 0);
  
           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);
  
           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
           
#ifdef DEBUG_REGRID_STATUS
printf("%d# dst_id %d status %d\n", Par::Rank(), dst_id, ESMC_REGRID_STATUS_MAPPED);
#endif
        }
          
        MBMesh_Search_EToP_Result sr; sr.src_elem = si.elem;
        std::set<MBMesh_Search_EToP_Result>::iterator sri =
          tmp_sr.lower_bound(sr);
        if (sri == tmp_sr.end() || *sri != sr) {
          sr.dst_nodes.push_back(si.snr);
          tmp_sr.insert(sri, sr);
        } else {
          std::vector<etop_sr> &r
            = const_cast<std::vector<etop_sr>&>(sri->dst_nodes);
          r.push_back(si.snr);
#ifdef DEBUG_SEARCH
std::cout << Par::Rank() << "# SECOND CHOICE, gid =" << sri->dst_nodes[sri->dst_nodes.size()-1].dst_gid << std::endl;
#endif
        }
      }
    }
  } // for pointlist

  {
    // Build seach res
    std::set<MBMesh_Search_EToP_Result>::iterator si =
      tmp_sr.begin(), se = tmp_sr.end();

    for (; si != se; ++si) {
      result.push_back(new MBMesh_Search_EToP_Result(*si));
#ifdef DEBUG_SEARCH_RESULTS
int id;
merr=mbmAp->mesh->tag_get_data(mbmAp->gid_tag, &si->src_elem, 1, &id);
printf("%d# results: add elem %d with nodes: ", Par::Rank(), id);
for (int i = 0; i < si->dst_nodes.size(); ++i)
  printf("%d, ", si->dst_nodes[i].dst_gid);
printf("\n");
#endif
    }
  }

  std::set<MBMesh_Search_EToP_Result>().swap(tmp_sr);
  std::vector<int>().swap(*dst_loc);

  if (!again.empty()) {
    if (stol > 1e-6) {
      // Mark anything that hasn't been mapped as unmapped
      if (set_dst_status) {
        for (UInt p = 0; p < again.size(); ++p) {
          int loc = again[p];
          int dst_id=mbmBp->get_id(loc);

          // Set col info
          WMat::Entry col(ESMC_REGRID_STATUS_OUTSIDE, 0,
                              0.0, 0);

          // Set row info
          WMat::Entry row(dst_id, 0, 0.0, 0);

          // Put weights into weight matrix
          dst_status.InsertRowMergeSingle(row, col);
        
#ifdef DEBUG_REGRID_STATUS
printf("%d# dst_id %d status %d\n", Par::Rank(), dst_id, ESMC_REGRID_STATUS_OUTSIDE);
#endif
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

    MBMesh_Search_EToP(mbmAp, mbmBp, unmappedaction,
                       map_type, stol*1e2, result, set_dst_status, dst_status, &again, box);
    }
  }

   // Get rid of box tree
  // if (box != NULL) delete box;

}

#endif // ESMF_MOAB
