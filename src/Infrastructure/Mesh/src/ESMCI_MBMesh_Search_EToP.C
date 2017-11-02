// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_Polynomial.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_Ftn.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/ESMCI_Phedra.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search_EToP.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_Interp.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>

#include <Mesh/include/ESMCI_MBMesh_Mapping.h>

#include "ESMCI_BBox.h"
#include "moab/ElemEvaluator.hpp"

#include "moab/CartVect.hpp"
// for SphericalQuad
//#include "ElemUtil.hpp"


using std::vector;

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
  //double best_dist;
  //MEField<> *src_cfield;
  //MEField<> *src_mask_field_ptr;
  //bool set_dst_status;
};


// NOTE::This finds the list of meshB elements which intersect with each meshA
// element and returns it in sres
static int num_intersecting_elems(MBMesh *mbmp, const BBox &meshBBBox, double btol, double nexp) {

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

     const MBMesh_BBox bounding_box(mbmp, elem, nexp);

     // First check to see if the box even intersects the meshB mesh bounding
     // box.
     if (Mixed_BBoxIntersect(bounding_box, meshBBBox, btol)) ++ret;
  }

  return ret;
}

static void populate_box_elems(OTree *box,
                               MBMesh_Search_EToP_Result_List &result,
                               MBMesh *mbmp, const BBox &meshBBBox,
                               double btol, double nexp) {

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

    MBMesh_BBox bounding_box(mbmp, elem, nexp);

    // First check to see if the box even intersects the meshB mesh bounding
    // box.
    if (Mixed_BBoxIntersect(bounding_box, meshBBBox, btol)) {

      // Create Search result
      MBMesh_Search_EToP_Result *sr=new MBMesh_Search_EToP_Result();
      sr->src_elem=elem;
      sr->dst_nodes.clear();

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

      // Add element to search tree
      box->add(min, max, (void*)sr);
    }

  }

}

static int found_func(void *c, void *y) {
  MBMesh_Search_EToP_Result *sr = static_cast<MBMesh_Search_EToP_Result*>(c);
  SearchData *si = static_cast<SearchData*>(y);
  // NOTE: sr and si should use same mesh, if not we have big problems

  int srid; MBMesh_get_gid(si->mesh, sr->src_elem, &srid);
  int siid; MBMesh_get_gid(si->mesh, si->elem, &siid);

  // from search.c
  // if we already have a source element, continue if this element has a smaller id
  if (si->is_in && (srid > siid)) return 0;


  // Debugging search results
  // get coordinates of corners of element
  int num_nodes;
  int nd = si->mesh->sdim;
  int max_num_nodes = 8;
  double coords[nd*max_num_nodes];
  MBMesh_get_elem_coords(si->mesh, sr->src_elem, max_num_nodes,
                           &num_nodes, coords);

#define DEBUG_PCOORDS

#ifdef DEBUG_PCOORDS
  printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEBUG ~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  printf("Elem %d: ", srid);
  for (int i = 0; i < num_nodes; ++i) {
    printf("[");
    for (int j = 0; j < nd; ++j) {
      printf("%f, ", coords[i*nd+j]);
    }
    printf("], ");
  }
  printf("\n  Node %d: [", si->snr.dst_gid);
  for (int i=0; i < nd; ++i)
    printf("%f, ", si->coords[i]);
  printf("]\n");
#endif // DEBUG_PCOORDS

  int is_inside = 0;
  double pcoords[3];

  if (nd == 2) {
    ElemEvaluator ee = ElemEvaluator(si->mesh->mesh, sr->src_elem);
    ee.reverse_eval(si->coords, 1e-8, 1e-6, pcoords, &is_inside);
#ifdef DEBUG_PCOORDS
    printf("Cartesian parametric coordinates via MOAB\n");
    printf("  Node %d params = [%f,%f,%f]\n", si->snr.dst_gid, pcoords[0], pcoords[1], pcoords[2]);
#endif
    // translate pcoords from [-1,1] to [0,1]
    translate(pcoords);
  } else if (nd == 3) {
    MBElemMap map = MBElemMap();
    bool inside = map.spherical_eval(coords, si->coords, num_nodes, pcoords, NULL);
    is_inside = static_cast<int> (inside);
#ifdef DEBUG_PCOORDS
    printf("spherical parametric coordinates via ESMF\n");
    printf("  Node %d params = [%f,%f,%f]\n", si->snr.dst_gid, pcoords[0], pcoords[1], pcoords[2]);
#endif
  }

  if (is_inside) {
    // set the search data pcoords structure
    si->snr.pcoord[0] = pcoords[0];
    si->snr.pcoord[1] = pcoords[1];
    si->snr.pcoord[2] = pcoords[2];


#ifdef DEBUG_PCOORDS
    printf("  Node %d pcoords = [", si->snr.dst_gid);
    for (int i = 0; i < nd; ++i)
      printf("%f, ", si->snr.pcoord[i]);
    printf("]\n");
  printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEBUG ~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
#endif

    //sr->dst_nodes.push_back(si->snr);
    si->elem = sr->src_elem;
    si->investigated=true;
    si->is_in=true;
  }
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
void MBMesh_Search_EToP(MBMesh *mbmAp, int unmappedactionA,
                        PointList *mbmBp, int unmappedactionB,
                        double stol, MBMesh_Search_EToP_Result_List &result) {
  Trace __trace("MBMesh_Search_EToP()");

  // MOAB error
  int merr;

  // TODO: NEED TO MAKE BOUNDING BOX ONLY DEPEND ON NON-MASKED ELEMENTS
  // Get global bounding box of pointlist
  int sdim = mbmBp->get_coord_dim();
  double cmin[sdim], cmax[sdim];
  build_pl_bbox(cmin, cmax, mbmBp);

  BBox MeshBBBox(sdim, cmin, cmax);

  // declare some variables
  OTree *box=NULL;
  const double normexp = 0.15;
  const double meshBint = 1e-8;

  // Create vector
  std::vector<point*> meshB_nlist;

  // NOTE: is there a has_node_mask?
  // NOTE: this can be done without the loop using the tag_get_data

  for (unsigned int i=0; i<mbmBp->get_curr_num_pts(); ++i)
    meshB_nlist.push_back(mbmBp->get_point(i));

  // Leave if nothing to search
  if (meshB_nlist.size() == 0) return;

  // Count number of elements in tree
  int num_box = num_intersecting_elems(mbmAp, MeshBBBox, meshBint, normexp);

  // Construct box tree
  box=new OTree(num_box);

  // Construct search result list
  result.reserve(num_box);

  // Fill tree with search result structs to fill
  // with intersecting elements
  populate_box_elems(box, result, mbmAp, MeshBBBox, meshBint, normexp);
  box->commit();

  // vector to hold loc to search in future
  std::vector<int> again;

  // Loop the mesh B elements, find the corresponding mesh A elements
  bool meshB_node_not_found=false;
  // Loop the destination loc, find hosts.
  for (int p = 0; p < mbmBp->get_curr_num_pts(); ++p) {
    // Get info out of point list
    const double *pnt_crd=mbmBp->get_coord_ptr(p);
    int pnt_id=mbmBp->get_id(p);


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
    si.snr.dst_gid=mbmBp->get_id(p);
    si.investigated=false;
    si.is_in=false;
    si.elem_masked=false;
    si.elem=NULL;
    si.mesh=mbmAp;

    // The point coordinates.
    si.coords[0] = pnt_crd[0]; si.coords[1] = pnt_crd[1]; si.coords[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    box->runon(pmin, pmax, found_func, (void*)&si);

/*
  double coords[3*3];
  MBMesh_get_elem_coords(mbmAp, si.src_elem, 3, 9, coords);

  for (int i = 0; i < 3; ++i) {
    std::cout << "coords = [" << coords[i*3+0] << ", " << coords[i*3+1]
              << ", " << coords[i*3+2] << "], ";
  }
  std::cout << std::endl;
*/

    // add to dst_nodes here

    if (!si.investigated) {
      // this is the new method
      again.push_back(pnt_id);
      // this is an old check
      meshB_node_not_found=true;
    } else {
      if (si.elem_masked) {
        // Mark this as unmapped due to src masking
        /*if (set_dst_status) {
           int dst_id=si.snr.dst_gid;

           // Set col info
           WMat::Entry col(ESMC_REGRID_STATUS_SRC_MASKED,
                               0, 0.0, 0);

           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);

           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
        }*/

      } else {
        // Mark this as mapped
        /*if (set_dst_status) {
           int dst_id=si.snr.dst_gid;

           // Set col info
           WMat::Entry col(ESMC_REGRID_STATUS_MAPPED,
                               0, 0.0, 0);

           // Set row info
           WMat::Entry row(dst_id, 0, 0.0, 0);

           // Put weights into weight matrix
           dst_status.InsertRowMergeSingle(row, col);
        }*/

        /*MBMesh_Search_EToP_Result sr; sr.src_elem = si.elem;
        MBMesh_Search_EToP_Result::iterator sri =
          tmp_sr.lower_bound(sr);
        if (sri == tmp_sr.end() || *sri != sr) {
          sr.dst_nodes.push_back(si.snr);
          tmp_sr.insert(sri, sr);
        } else {
          // std::cout << "second choice" << std::endl;
          std::vector<Search_node_result> &r
            = const_cast<std::vector<Search_node_result>&>(sri->dst_nodes);
          r.push_back(si.snr);
          //std::cout << "size=" << sri->nodes.size() << std::endl;
        }*/

        MBMesh_Search_EToP_Result *sr = new MBMesh_Search_EToP_Result();
        sr->src_elem = si.elem;
        sr->dst_nodes.push_back(si.snr);
        result.push_back(sr);

      }
    }
  } // for pointlist

  /*
  {
    // Build seach res
    MBMesh_Search_EToP_Result::iterator si =
      tmp_sr.begin(), se = tmp_sr.end();

    for (; si != se; ++si)
      result.push_back(new MBMesh_Search_EToP_Result(*si));
  }

  MBMesh_Search_EToP_Result.swap(tmp_sr);
  std::vector<int>().swap(*dst_loc);

  if (!again.empty()) {
    if (stol > 1e-6) {
      // Mark anything that hasn't been mapped as unmapped
      /*if (set_dst_status) {
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

    MBMesh_Search_EToP(mbmAp, unmappedactionA, mbmBp, unmappedactionB,
                       stol*1e2, result, again);
    }
  }*/

  // Check for meshB elements which haven't been intersected
  if (meshB_node_not_found) {
    if (unmappedactionB == ESMCI_UNMAPPEDACTION_ERROR) {
      Throw() << " Some destination points cannot be mapped to source grid";
    } else if (unmappedactionB == ESMCI_UNMAPPEDACTION_IGNORE) {
      // don't do anything
    } else {
      Throw() << " Unknown unmappedaction option";
    }
  }

   // Get rid of box tree
  if (box != NULL) delete box;

}

#endif // ESMF_MOAB
