// $Id: ESMCI_MeshMerge.C,v 1.1 2011/08/22 16:35:48 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_MeshMerge.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_BBox.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_SparseMsg.h>
#include <Mesh/include/ESMCI_SpaceDir.h>
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_Exception.h> 
#include <Mesh/include/ESMCI_MathUtil.h> 
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <ESMCI_VM.h>
 
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include "stdlib.h"
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshMerge.C,v 1.1 2011/08/22 16:35:48 feiliu Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {


  typedef std::vector<sintd_node *> * Sintd_nodes;
  typedef std::vector<sintd_cell *> * Sintd_cells;

  void  calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz);

  void PointMerge(int sdim, double stol, int num_pnts, std::vector<double> pnts, std::vector<int> local_ids, int *num_pnts_mrgd, std::vector<double> *pnts_mrgd);

void sew_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & midmesh, Zoltan_Struct * zz){

  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

//  {
//    Mesh & mesh = srcmesh;
//    MEField<> &coord = *mesh.GetCoordField();
//    Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
//    for (UInt i = 0; ni != ne; ++ni) {
//      const MeshObj &node = *ni;
//      double *cd = coord.data(node);
//      // Write coordinates
//      out << cd[0] << " " << cd[1] << " " << (mesh.spatial_dim() == 3 ? cd[2] : 0.0) << std::endl;
//  
//      // Increment ordinal
//      id2ord[node.get_id()] = i++;
//    }
//  }
//

  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // go through all src mesh elements
  {
    const Mesh & mesh = srcmesh;
    unsigned int ncells = 0;
    MEField<> &coord = *mesh.GetCoordField();
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];
      if(elem.get_owner() != me) continue;

      for (UInt n = 0; n < topo->num_nodes; n++) {

        const MeshObj &node = *elem.Relations[n].obj;
        for(int i = 0; i < sdim; i ++) {
          double * tmp = coord.data(node);
          cd[(n*sdim)+i] = tmp[i];
        }
        // We are not checking if the src/dst meshes are self consistent, they should be...
        //MeshObj::id_type id = elem.Relations[n].obj->get_id();
        //std::map<MeshObj::id_type,int>::iterator imi = id2ord.find(id);
        //ThrowRequire(imi != id2ord.end());
      }

      construct_sintd(0., topo->num_nodes, cd, pdim, sdim, 
        &sintd_nodes, &sintd_cells);
      ncells ++;
      delete[] cd;

    } // for ei
  } 

  // go through all dst mesh elements
  {
    const Mesh & mesh = dstmesh;
    unsigned int ncells = 0;
    MEField<> &coord = *mesh.GetCoordField();
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];
      if(elem.get_owner() != me) continue;

      for (UInt n = 0; n < topo->num_nodes; n++) {

        const MeshObj &node = *elem.Relations[n].obj;
        for(int i = 0; i < sdim; i ++) {
          double * tmp = coord.data(node);
          cd[(n*sdim)+i] = tmp[i];
        }
      }

      construct_sintd(0., topo->num_nodes, cd, pdim, sdim, 
        &sintd_nodes, &sintd_cells);
      ncells ++;
      delete[] cd;

    } // for ei
  } 

  // We now have all the genesis cells, compute the merged mesh
  compute_midmesh(sintd_nodes, sintd_cells, pdim, sdim, &midmesh);

}

// The main routine
void MeshMerge(Mesh &srcmesh, Mesh &dstmesh, Mesh **meshpp) {
    Trace __trace("MeshMerge()");

  // Set some parameters for seach, eventually move these to .h or get rid of
  // const double normexp = 0.15;
  const double search_tol = 1e-20;


  // Some error checking
  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for mesh merge";
  }  

  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "Meshes must have same parametric dim for mesh merge";
  }  

  printf("Inside MeshMerge srcmesh.spatial_dim=%d \n",srcmesh.spatial_dim());

  printf("Inside MeshMerge dstmesh.spatial_dim=%d \n",dstmesh.spatial_dim());

  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  int rc;

  // Compute global bounding box and determine if a mesh merge can be performed.
  // Get coordinate fields
  MEField<> &scoord = *srcmesh.GetCoordField();
  MEField<> &dcoord = *dstmesh.GetCoordField();
  BBox srcBBox(scoord, srcmesh);
  BBox dstBBox(dcoord, dstmesh);

  BBox gsrcBBox = BBoxParUnion(srcBBox);
  BBox gdstBBox = BBoxParUnion(dstBBox);

  bool intersected = BBoxIntersect(gsrcBBox, gdstBBox, search_tol);

  if(! intersected) {
    Throw() << "src and dst mesh do not intersect, cannot be merged to form XGrid";
  }
    
  // Create Mesh
  Mesh *meshmrgp = new Mesh();
  Mesh &meshmrg=*(meshmrgp);
  *meshpp = meshmrgp;

  meshmrg.set_parametric_dimension(sdim);
  meshmrg.set_spatial_dimension(pdim);

  // Build the rendezvous meshes and compute search result
  std::vector<Interp::FieldPair> fpairs;
  fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_CONSERVE));
  int unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
  Interp interp(srcmesh, dstmesh, meshmrgp, fpairs, unmappedaction);
  
  // Use search to figure out which elements of srcmesh overlap elements of dstmesh
  SearchResult sres;
  OctSearchElems(srcmesh, ESMC_UNMAPPEDACTION_IGNORE, dstmesh, ESMC_UNMAPPEDACTION_IGNORE, search_tol, sres);
  //SearchResult sres = interp.get_sres();
  Zoltan_Struct * zz = interp.get_zz();

  // check sres
  int nelem = sres.size();
  int gnelem;
  MPI_Allreduce(&nelem, &gnelem, 1, MPI_INT, MPI_SUM, Par::Comm());

  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;


  // Number of pnts
  int num_pnts;

  // Points
  std::vector<double> pnts;
  
  // Number of points in polys
  std::vector<int> num_pnts_in_poly;

  // Calculate polygons from search results
  calc_clipped_poly(srcmesh, dstmesh, sres, &num_pnts, &pnts, &num_pnts_in_poly, &sintd_nodes, &sintd_cells, zz);

  // if src and dst mesh are neigbors, just sew them together
  if(gnelem == 0 || (gnelem != 0 && sintd_nodes.size() == 0)) {
    sew_meshes(srcmesh, dstmesh, meshmrg, zz);
  }

  // Delete search result list
  DestroySearchResult(sres);

  // Release the Zoltan struct we used for the mid mesh
  interp.release_zz();


  }
  



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void calc_inter_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                             std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                             int *num_pnts, std::vector<double> *pnts, std::vector<int> *num_pnts_in_poly, 
                             Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz) {


// Maximum size for a supported polygon
// Since the elements are of a small 
// limited size. Fixed sized buffers seem 
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES) 

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_2D];

    // Get src coords
    get_elem_coords(src_elem, src_cfield, 2, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges2D(&num_src_nodes, src_coords);

    // if less than a triangle complain
    if (num_src_nodes<3) {
      Throw() << "Source Element is degenerate";
    }

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for tmp polygon used in intersection routine
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];

 
    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];
      
      // Get dst coords
      get_elem_coords(dst_elem, dst_cfield, 2, MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);
      
      // if no nodes then go to next
      if (num_dst_nodes<1) {
	continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_dst_nodes, dst_coords);
      
      // if less than a triangle complain
      if (num_dst_nodes<3) {
	Throw() << "Source Element is degenerate";
      }
      
     // Make sure that we aren't going to go over size of tmp buffers
     if ((num_src_nodes + num_dst_nodes) > MAX_NUM_POLY_NODES) {
       Throw() << " src and dst poly size too big for temp buffer";
     }
     
     
     // Intersect src with dst element
      // Intersect src with dst element
      intersect_convex_poly2D(num_dst_nodes, dst_coords,
                              num_src_nodes, src_coords,
                              tmp_coords,
                              &num_sintd_nodes, sintd_coords); 
     

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
	continue;
      }

      // TODO chop into triangles if necessary
      // calculate intersection area
      double sintd_areas=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords); 

      compute_sintd_nodes_cells(sintd_areas,
          num_sintd_nodes, sintd_coords, 2, 2,
          sintd_nodes, sintd_cells, zz);


      // Add element to output list
      //// Add number of points
      *num_pnts += num_sintd_nodes;

      //// Add number of points in poly
      num_pnts_in_poly->push_back(num_sintd_nodes);

      //// Add coords
      pnts->reserve(pnts->size()+2*num_sintd_nodes);
      for (int j=0; j<num_sintd_nodes; j++) {
        pnts->push_back(sintd_coords[2*j]);
        pnts->push_back(sintd_coords[2*j+1]);
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D    
  }




  void calc_clipped_poly_2D_2D_cart(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres,   int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz) {
  Trace __trace("calc_clipped_poly(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");


  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    
    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

#if 0
    // If this source element is masked then skip it
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }
#endif

    // Calculate polys
    calc_inter_2D_2D_cart(sr.elem,src_cfield,sr.elems,dst_cfield, num_pnts, pnts, num, sintd_nodes, sintd_cells, zz);


  } // for searchresult

}



  void calc_inter_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                            std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                            int *num_pnts, std::vector<double> *pnts, std::vector<int> *num_pnts_in_poly,
                            Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz) {


// Maximum size for a supported polygon
// Since the elements are of a small 
// limited size. Fixed sized buffers seem 
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES) 

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_3D];

    // Get src coords
    get_elem_coords(src_elem, src_cfield, 3, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_src_nodes, src_coords);

    // if less than a triangle complain
    if (num_src_nodes<3) {
      Throw() << "Source Element is degenerate";
    }

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for tmp polygon used in intersection routine
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

 

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];
      
      // Get dst coords
      get_elem_coords(dst_elem, dst_cfield, 3, MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);
      
      // if no nodes then go to next
      if (num_dst_nodes<1) {
	continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);
      
      // if less than a triangle complain
      if (num_dst_nodes<3) {
	Throw() << "Source Element is degenerate";
      }
      
     
     // Make sure that we aren't going to go over size of tmp buffers
     if ((num_src_nodes + num_dst_nodes) > MAX_NUM_POLY_NODES) {
       Throw() << " src and dst poly size too big for temp buffer";
     }
     
     
     // Intersect src with dst element
     intersect_convex_2D_3D_sph_gc_poly(num_dst_nodes, dst_coords,
                                        num_src_nodes, src_coords,
                                        tmp_coords,
                                        &num_sintd_nodes, sintd_coords); 
     

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
	continue;
      }

      // TODO chop into triangles if necessary
      // calculate intersection area
      double sintd_areas=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords); 

      compute_sintd_nodes_cells(sintd_areas,
          num_sintd_nodes, sintd_coords, 2, 3,
          sintd_nodes, sintd_cells, zz);


      // Add element to output list
      //// Add number of points
      *num_pnts += num_sintd_nodes;

      //// Add number of points in poly
      num_pnts_in_poly->push_back(num_sintd_nodes);

      //// Add coords
      pnts->reserve(pnts->size()+3*num_sintd_nodes);
      for (int j=0; j<num_sintd_nodes; j++) {
        pnts->push_back(sintd_coords[3*j]);
        pnts->push_back(sintd_coords[3*j+1]);
        pnts->push_back(sintd_coords[3*j+2]);
      }
    }


#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D    
  }





void calc_clipped_poly_2D_3D_sph(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres,   int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz) {
  Trace __trace("calc_clipped_poly(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");


  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    
    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

#if 0
    // If this source element is masked then skip it
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }
#endif

    // Calculate weights
    calc_inter_2D_3D_sph(sr.elem,src_cfield,sr.elems,dst_cfield, num_pnts, pnts,num, sintd_nodes, sintd_cells, zz);


  } // for searchresult

}


  void calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, int *num_pnts,  std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, Zoltan_Struct * zz) {


  // both meshes have to have the same dimensions
  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

  // Get dimension, because they're the same can just get one
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_clipped_poly_2D_2D_cart(srcmesh, dstmesh, sres, num_pnts, pnts, num, sintd_nodes, sintd_cells, zz);
    } else if (sdim==3) {
      calc_clipped_poly_2D_3D_sph(srcmesh, dstmesh, sres, num_pnts, pnts, num, sintd_nodes, sintd_cells, zz);
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 not supported for conservative regridding";
  }



}


  
  void PointMerge(int sdim, double stol, int num_pnts, std::vector<double> pnts, std::vector<int> local_ids, int *num_pnts_mrgd, std::vector<double> *pnts_mrgd) {
    
#if 0
    // Setup search tree 
    OTree *tree=new OTree(num_pnts); 
    
    // Add points
    for (int i=0; i<num_pnts; i++) {
      int p=sdim*i;
      
      double pmin[3], pmax[3];
      
      pmin[0] = pnts[p]-stol;
      pmin[1] = pnts[p+1] - stol;
      pmin[2] = sdim == 3 ? pnts[p+2]-stol : -stol;
      
      pmax[0] = pnts[p] + stol;
      pmax[1] = pnts[p+1] + stol;
      pmax[2] = sdim == 3 ? pnts[p+2]+stol : +stol;
      
      // Add element to search tree
      tree->add(pmin, pmax, (void*)NULL);
      
      
    }
#endif    


  }

  
  
} // namespace
