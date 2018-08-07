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
#include <Mesh/include/Legacy/ESMCI_BBox.h>
#include <Mesh/include/Regridding/ESMCI_ConserveInterp.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h> 
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MathUtil.h> 
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshMerge.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include <Mesh/include/Legacy/ESMCI_SpaceDir.h>
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <ESMCI_VM.h>
#include <PointList/include/ESMCI_PointList.h>
 
#include <algorithm>
#include <iterator>
#include <ostream>
#include <set>
#include <limits>
#include <vector>

#include "stdlib.h"
#include "ESMCI_LogErr.h"
#include <cstring>
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

  typedef std::vector<sintd_node *> * Sintd_nodes;
  typedef std::vector<sintd_cell *> * Sintd_cells;

  void calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map);
  void sew_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh);
  void concat_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh, const Mesh & mesh_src, const Mesh & mesh_dst, 
    SearchResult &sres, interp_mapp sres_map);
  void copy_mesh(const Mesh & dstmesh, Mesh & meshdiff);
  void diff_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & meshdiff, const Mesh & mesh_src, const Mesh & mesh_dst, SearchResult & sres, interp_mapp sres_map, double threshold);
  void dump_elem(const MeshObj & elem, int sdim, const MEField<> & coord, bool check_local=true);

// The main routine
// srcmesh is a higher priority mesh than dstmesh, srcmesh clips into dstmesh
void MeshMerge(Mesh &srcmesh, Mesh &dstmesh, Mesh **meshpp) {
  Trace __trace("MeshMerge()");
  //WriteVTKMesh(srcmesh, "srcmesh.vtk");
  //WriteVTKMesh(dstmesh, "dstmesh.vtk");

  // Set some parameters for seach, eventually move these to .h or get rid of
  // const double normexp = 0.15;
  const double search_tol = 1e-10;

  // Some error checking
  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for mesh merge";
  }  

  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "Meshes must have same parametric dim for mesh merge";
  }  

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
    Throw() << "src and dst mesh do not neighbor or intersect, cannot be merged to form XGrid";
  }
    
  // Create Mesh
  Mesh *meshmrgp = new Mesh();
  Mesh &meshmrg=*(meshmrgp);
  *meshpp = meshmrgp;

  meshmrg.set_parametric_dimension(sdim);
  meshmrg.set_spatial_dimension(pdim);

  Interp * interp=0;
  SearchResult sres;
  Mesh *mesh_src, *mesh_dst;
  int unmappedaction = ESMCI_UNMAPPEDACTION_IGNORE;
  int npet = VM::getCurrent(&rc)->getPetCount();
  // No need to compute rendezvous meshes running uni. Make sure things are scoped correctly.
  if(npet > 1){
    bool tmp_set_dst_status=false;
    IWeights tmp_dst_status;

    // Build the rendezvous meshes and compute search result
    interp = new Interp(&dstmesh, NULL, &srcmesh, NULL, NULL, true, Interp::INTERP_CONSERVE, tmp_set_dst_status, tmp_dst_status, MAP_TYPE_CART_APPROX, unmappedaction);
    

    // Get the rendevous meshes, the meaning of dst/src is flipped in interp
    mesh_dst = &(interp->get_grend().GetSrcRend());
    mesh_src = &(interp->get_grend().GetDstRend());
    
    if(false){ // Debug
      {
        std::cout << "srcmesh:\n";
        const Mesh & mesh = srcmesh;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "dstmesh:\n";
        const Mesh & mesh = dstmesh;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "rend srcmesh:\n";
        const Mesh & mesh = *mesh_src;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "rend dstmesh:\n";
        const Mesh & mesh = *mesh_dst;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
    }

    // Use search to figure out which elements of srcmesh overlap elements of dstmesh
    // each sres is an element keyed by a cell in dstmesh
    sres = interp->get_sres();
    //PrintSearchResult(sres);
  }else{
    mesh_src = &srcmesh;
    mesh_dst = &dstmesh;
    OctSearchElems(dstmesh, unmappedaction, srcmesh, unmappedaction, 1e-8, sres);
    //PrintSearchResult(sres);
  }

  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // Number of pnts
  int num_pnts;
  // Points
  std::vector<double> pnts;
  // Number of points in polys
  std::vector<int> num_pnts_in_poly;

  // Calculate polygons from search results
  // because dst(subject) mesh is not migrated, the sres is calculated from src(clip) rendezvous mesh and dst mesh
  interp_map res_map;
  calc_clipped_poly(dstmesh, *mesh_src, sres, &sintd_nodes, &sintd_cells, &res_map);

  // check sres
  int nelem[2];
  int gnelem[2];
  nelem[0] = sres.size();
  nelem[1] = sintd_nodes.size();
  MPI_Allreduce(&nelem, &gnelem, 2, MPI_INT, MPI_SUM, Par::Comm());

  // if src and dst mesh are neigbors, just sew them together
  if(gnelem[1] == 0) 
    sew_meshes(srcmesh, dstmesh, meshmrg);
  else
    concat_meshes(srcmesh, dstmesh, meshmrg, *mesh_src, *mesh_dst, sres, &res_map);

  // clean up memory
  if(interp) delete interp;
  interp_map_iter it = res_map.begin(), ie = res_map.end();
  for(; it != ie; ++it)
    delete it->second;

}

void dump_elem(const MeshObj & elem, int sdim, const MEField<> & coord, bool check_local){

  int rc;
  unsigned int npet = VM::getCurrent(&rc)->getPetCount();
  unsigned int me   = VM::getCurrent(&rc)->getLocalPet();
  unsigned int owner = elem.get_owner();
  std::cout << elem.get_id() << " Me: " << me << " Owner: " << elem.get_owner()
            << " Locally owned: " << GetAttr(elem).is_locally_owned() << "\n ";
  //if(elem.get_owner() != me) continue;
  if(check_local && !GetAttr(elem).is_locally_owned()) return;

  const MeshObjTopo *topo = GetMeshObjTopo(elem);
  double * coords = new double[topo->num_nodes*sdim];
  int pt = 0;
  for (UInt n = 0; n < topo->num_nodes; n++) {

    const MeshObj &node = *elem.Relations[n].obj;
    double * tmp = coord.data(node);
    for(int i = 0; i < sdim; i ++) {
      coords[pt++] = tmp[i];
      std::cout << tmp[i] << ',';
    }
  }
  double area = 0.;
  if(sdim == 2)
    area = area_of_flat_2D_polygon(topo->num_nodes, coords);
  if(sdim == 3)
    area = great_circle_area(topo->num_nodes, coords);
  std::cout << " area = " << area << '\n';

  delete[] coords;
}

// meshpp = dstmesh - srcmesh (srcmesh clips into dstmesh)
void MeshCreateDiff(Mesh &srcmesh, Mesh &dstmesh, Mesh **meshpp, double threshold) {
  Trace __trace("MeshCreateDiff()");
  //WriteVTKMesh(srcmesh, "srcmesh.vtk");
  //WriteVTKMesh(dstmesh, "dstmesh.vtk");

  // Set some parameters for seach, eventually move these to .h or get rid of
  // const double normexp = 0.15;
  const double search_tol = 1e-10;

  // Some error checking
  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for mesh merge";
  }  

  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "Meshes must have same parametric dim for mesh merge";
  }  

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
    Throw() << "src and dst mesh do not neighbor or intersect, cannot be merged to form XGrid";
  }
    
  // Create Mesh
  Mesh *meshdiffp = new Mesh();
  Mesh &meshdiff =*(meshdiffp);
  *meshpp = meshdiffp;

  meshdiff.set_parametric_dimension(sdim);
  meshdiff.set_spatial_dimension(pdim);

  Interp * interp=0;
  SearchResult sres;
  Mesh *mesh_src, *mesh_dst;
  int unmappedaction = ESMCI_UNMAPPEDACTION_IGNORE;
  int npet = VM::getCurrent(&rc)->getPetCount();
  // No need to compute rendezvous meshes running uni. Make sure things are scoped correctly.
  if(npet > 1){
    bool tmp_set_dst_status=false;
    IWeights tmp_dst_status;
    
    // Build the rendezvous meshes and compute search result
    interp = new Interp(&dstmesh, NULL, &srcmesh, NULL, NULL, true, Interp::INTERP_CONSERVE, tmp_set_dst_status, tmp_dst_status, MAP_TYPE_CART_APPROX, unmappedaction);

    // Get the rendevous meshes, the meaning of dst/src is flipped in interp
    mesh_dst = &(interp->get_grend().GetSrcRend());
    mesh_src = &(interp->get_grend().GetDstRend());
    
    if(false){ // Debug
      {
        std::cout << "srcmesh:\n";
        const Mesh & mesh = srcmesh;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "dstmesh:\n";
        const Mesh & mesh = dstmesh;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "rend srcmesh:\n";
        const Mesh & mesh = *mesh_src;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
      {
        std::cout << "rend dstmesh:\n";
        const Mesh & mesh = *mesh_dst;
        MEField<> &coord = *mesh.GetCoordField();
        Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          const MeshObj &elem = *ei;  
          dump_elem(elem, sdim, coord, false);
        }
      }
    }

    // Use search to figure out which elements of srcmesh overlap elements of dstmesh
    // each sres is an element keyed by a cell in dstmesh
    sres = interp->get_sres();
    //PrintSearchResult(sres);
  }else{
    mesh_src = &srcmesh;
    mesh_dst = &dstmesh;
    OctSearchElems(dstmesh, unmappedaction, srcmesh, unmappedaction, 1e-8, sres);
    //PrintSearchResult(sres);
  }

  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // Number of pnts
  int num_pnts;
  // Points
  std::vector<double> pnts;
  // Number of points in polys
  std::vector<int> num_pnts_in_poly;

  // Calculate polygons from search results
  // because dst(subject) mesh is not migrated, the sres is calculated from src(clip) rendezvous mesh and dst mesh
  interp_map res_map;
  calc_clipped_poly(dstmesh, *mesh_src, sres, &sintd_nodes, &sintd_cells, &res_map);

  // check sres
  int nelem[2];
  int gnelem[2];
  nelem[0] = sres.size();
  nelem[1] = sintd_nodes.size();
  MPI_Allreduce(&nelem, &gnelem, 2, MPI_INT, MPI_SUM, Par::Comm());

  // For differencing operation:
  // if src and dst mesh are neigbors, just copy dstmesh to meshdiff
  if(gnelem[1] == 0) 
    copy_mesh(dstmesh, meshdiff);
  else
    diff_meshes(srcmesh, dstmesh, meshdiff, *mesh_src, *mesh_dst, sres, &res_map, threshold);

  // clean up memory
  if(interp) delete interp;
  interp_map_iter it = res_map.begin(), ie = res_map.end();
  for(; it != ie; ++it)
    delete it->second;

}

void sew_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh){

  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  int rc;
  unsigned int me = VM::getCurrent(&rc)->getLocalPet();
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // go through all src mesh elements
  unsigned int ncells = 0;
  {
    //std::cout << "Traversing the source Mesh\n";
    const Mesh & mesh = srcmesh;

    // Get mask and coord field
    MEField<> *mask_field = mesh.GetField("elem_mask");
    MEField<> &coord = *mesh.GetCoordField();

    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;  //dump_elem(elem, sdim, coord);
      if(elem.get_owner() != me) continue;
      //if(!GetAttr(elem).is_locally_owned()) continue;

      if(mask_field){ // do not sew an element if it's masked out
        double *msk=mask_field->data(elem);
        if (*msk>0.5) continue;
      }

      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];

      for (UInt n = 0; n < topo->num_nodes; n++) {

        const MeshObj &node = *elem.Relations[n].obj;
        double * tmp = coord.data(node);
        for(int i = 0; i < sdim; i ++) {
          cd[(n*sdim)+i] = tmp[i];
        }
        // We are not checking if the src/dst meshes are self consistent, they should be...
        //MeshObj::id_type id = elem.Relations[n].obj->get_id();
        //std::map<MeshObj::id_type,int>::iterator imi = id2ord.find(id);
        //ThrowRequire(imi != id2ord.end());
      }
      polygon res_poly;
      coords_to_polygon(topo->num_nodes, cd, sdim, res_poly);

      construct_sintd(res_poly.area(sdim), topo->num_nodes, cd, pdim, sdim, 
        &sintd_nodes, &sintd_cells);
      ncells ++;
      delete[] cd;

    } // for ei
  } 

  //{
  //  // Debug
  //  int num_cells = sintd_cells.size();
  //  for (int i=0; i<num_cells; i++) 
  //    sintd_cells[i]->print(me, i, i);
  //}

  // go through all dst mesh elements
  {
    //std::cout << "Traversing the destination Mesh\n";
    const Mesh & mesh = dstmesh;

    // Get mask and coord field
    MEField<> *mask_field = mesh.GetField("elem_mask");
    MEField<> &coord = *mesh.GetCoordField();

    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;  //dump_elem(elem, sdim, coord);
      if(elem.get_owner() != me) continue;
      //if(!GetAttr(elem).is_locally_owned()) continue;

      if(mask_field){ // do not sew an element if it's masked out
        double *msk=mask_field->data(elem);
        if (*msk>0.5) continue;
      }

      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];

      for (UInt n = 0; n < topo->num_nodes; n++) {

        const MeshObj &node = *elem.Relations[n].obj;
        double * tmp = coord.data(node);
        for(int i = 0; i < sdim; i ++) {
          cd[(n*sdim)+i] = tmp[i];
        }
      }
      polygon res_poly;
      coords_to_polygon(topo->num_nodes, cd, sdim, res_poly);

      construct_sintd(res_poly.area(sdim), topo->num_nodes, cd, pdim, sdim, 
        &sintd_nodes, &sintd_cells);
      ncells ++;
      delete[] cd;

    } // for ei
  } 

  //{
  //  // Debug
  //  int num_cells = sintd_cells.size();
  //  for (int i=0; i<num_cells; i++) 
  //    sintd_cells[i]->print(me, i, i);
  //}

  // We now have all the genesis cells, compute the merged mesh
  compute_midmesh(sintd_nodes, sintd_cells, pdim, sdim, &mergemesh);
  //char str[64]; memset(str, 0, 64);
  //sprintf(str, "sewmesh.vtk.%d", me);
  //WriteVTKMesh(mergemesh, str);

}

// srcmesh: original src (clip) mesh    (in)
// dstmesh: original dst (subject) mesh    (in)
// mergemesh: result merged mesh (out)
// mesh_src: rendevous src mesh  (in)
// mesh_dst: rendevous dst mesh  (in)
// sres    : search result based on rendevous mesh (in)
// sres_map: search result map keyed by passive mesh element (dst mesh in this case)
void concat_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh, 
  const Mesh & mesh_src, const Mesh & mesh_dst, SearchResult &sres, interp_mapp sres_map){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshMerge.C::concat_meshes()"

  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  int rc;
  unsigned int me = VM::getCurrent(&rc)->getLocalPet();
  unsigned int npet = VM::getCurrent(&rc)->getPetCount();
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  //{
  //  std::vector<polygon> diff;
  //  double p[6] = {3.,3.5, 3.5,3.5, 3.5,4.5};
  //  double q[8] = {3.,3., 4.,3., 4.,4., 3.,4.};
  //  weiler_clip_difference(2,2,3,p,4,q,diff);
  //}

  unsigned int ncells = 0;
  // go through all src mesh elements, clip (higher priority) mesh, save all clip cells
  {
    const Mesh & mesh = srcmesh;

    // Get mask and coord field
    MEField<> *mask_field = mesh.GetField("elem_mask");
    MEField<> &coord = *mesh.GetCoordField();

    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      if(elem.get_owner() != me) continue;

      if(mask_field){ // do not sew an element if it's masked out
        double *msk=mask_field->data(elem);
        if (*msk>0.5) continue;
      }

      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];

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

      int num_nodes = topo->num_nodes;
      polygon res_poly;
      coords_to_polygon(topo->num_nodes, cd, sdim, res_poly);

      construct_sintd(res_poly.area(sdim), num_nodes, cd, pdim, sdim, &sintd_nodes, &sintd_cells);
      ncells ++;
      delete[] cd;

    } // for ei
  } 

  // Go through all dst mesh elements, subject (low priority) mesh, compute differentials
  {
    const Mesh & mesh = dstmesh;
    MEField<> &coord = *mesh.GetCoordField();
    MEField<> &clip_coord = *srcmesh.GetCoordField();
    MEField<> *elem_frac=mesh.GetField("elem_frac2");
    if (!elem_frac) Throw() << "Meshes involved in XGrid construction should have frac2 field";

    // Get mask field
    MEField<> *mask_field = mesh.GetField("elem_mask");

    // iterate through dst mesh element, construct its coordinates in 'cd'
    // used in both intersected or non-intersected cases
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;

      if(mask_field){ // do not sew an element if it's masked out
        double *msk=mask_field->data(elem);
        if (*msk>0.5) continue;
      }

      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      int subject_num_nodes = topo->num_nodes;

      double *cd = new double[sdim*topo->num_nodes];
      for (UInt n = 0; n < subject_num_nodes; n++) {
        const MeshObj &node = *elem.Relations[n].obj;
        for(int i = 0; i < sdim; i ++) {
          double * tmp = coord.data(node);
          cd[(n*sdim)+i] = tmp[i];
        }
      }

      // Get rid of degenerate edges
      if(sdim == 2)
        remove_0len_edges2D(&subject_num_nodes, cd);
      else
        remove_0len_edges3D(&subject_num_nodes, cd);

      if(subject_num_nodes < 3) continue;

      // make sure the polygons are in CCW sense before going into Weiler algorithm
      //if(sdim == 2 && area_of_flat_2D_polygon(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);
      //if(sdim == 3 && great_circle_area(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);

      // Check if this element is in sres_map
      // dump_elem(elem, sdim, coord);
      interp_map_iter it = sres_map->find(&elem);
      if(it == sres_map->end()){ // Not intersected, just add it to the list
        polygon res_poly;
        coords_to_polygon(subject_num_nodes, cd, sdim, res_poly);

        construct_sintd(res_poly.area(sdim), subject_num_nodes, cd, pdim, sdim, 
          &sintd_nodes, &sintd_cells);
        delete[] cd;
        ncells ++;
      }else{ 

        // dstpolys contains residual polygons of the original subject polygon after successive clipping
        // before the first clipping, it's the original subject polygon.
        std::vector<polygon> results, dstpolys;
        dstpolys.resize(1);
        coords_to_polygon(subject_num_nodes, cd, sdim, dstpolys[0]);
        delete[] cd;

        interp_map_range range = sres_map->equal_range(&elem);
        double fraction_deduction = 0.;

        // go through each src element that cuts into this dst element
        // Do a first cut compute total fraction reduction, if entire dst element
        // is clipped out, move onto the next dst element
        for(interp_map_iter it = range.first; it != range.second; ++it)
          fraction_deduction += it->second->fraction;
        if(fraction_deduction >= 1.0){
          double *f=elem_frac->data(elem);
          *f=0.;
          continue;
        }

        // Do spatial boolean math: difference, cut each intersected part off the dst mesh and 
        // triangulate the remaining polygon
        for(interp_map_iter it = range.first; it != range.second; ++it){

          // debug
          //const interp_res * dbg_interp_res = it->second;
          //double * master_sub_sph = new double[dbg_interp_res->num_subject_nodes*2];
          //double * master_clip_sph = new double[dbg_interp_res->num_clip_nodes*2];
          //{
          //  cart2sph(dbg_interp_res->num_subject_nodes, dbg_interp_res->subject_coords, master_sub_sph);
          //  cart2sph(dbg_interp_res->num_clip_nodes, dbg_interp_res->clip_coords, master_clip_sph);
          //}

          //if(elem.get_id() == 2378 && it->second->clip_elem->get_id() == 3559) 
          //  int nop = elem.get_id() - it->second->clip_elem->get_id();
          //if(elem.get_id() == 5497 || elem.get_id() == 6073){
          //  for(int i=0; i < dstpolys.size(); i ++)
          //    dump_polygon(dstpolys[i], true);
          //}
          // construct clip element polygon from src element
          const MeshObj & clip_elem = *(it->second->clip_elem);
          int clip_num_nodes = it->second->num_clip_nodes;
          double *clip_cd = it->second->clip_coords;

          // for each polygon in cutted dst element, compute residual diff polygons
          int num_p; int *ti, *tri_ind; double *pts, *td;
          std::vector<polygon> diff;
          for(std::vector<polygon>::const_iterator dstpoly_it = dstpolys.begin();
            dstpoly_it != dstpolys.end(); ++ dstpoly_it){

            //if(elem.get_id() == 5497 || elem.get_id() == 6073){
            //  std::cout << "polygon being clipped: " << std::endl;
            //  dump_polygon(*dstpoly_it, true);
            //  std::cout << "clip_cd: " << std::endl;
            //  dump_cart_coords(clip_num_nodes, clip_cd);
            //}

            diff.clear();

            // diff each dst element residual polygon with src element iteratively
            // this normally does not happen too deep.
            int subject_num_nodes = dstpoly_it->points.size();
            double * cd = new double[sdim*dstpoly_it->points.size()];
            polygon_to_coords(*dstpoly_it, sdim, cd);

            // Get rid of degenerate edges
            if(sdim == 2)
              remove_0len_edges2D(&subject_num_nodes, cd);
            else
              remove_0len_edges3D(&subject_num_nodes, cd);

            if(subject_num_nodes < 3) continue;
          
            double *cd_sph, *clip_cd_sph;
            if(sdim == 2) weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
            if(sdim == 3){
              
              //clip_cd_sph = new double[clip_num_nodes*2]; cart2sph(clip_num_nodes, clip_cd, clip_cd_sph);
              //cd_sph = new double[subject_num_nodes*2];   cart2sph(subject_num_nodes, cd, cd_sph);

              //weiler_clip_difference(pdim, 2, subject_num_nodes, cd_sph, clip_num_nodes, clip_cd_sph, diff);
              //std::vector<polygon> diff_cart;
              //sph2cart(diff, diff_cart);
              //diff.clear(); diff.resize(diff_cart.size()); std::copy(diff_cart.begin(), diff_cart.end(), diff.begin());
              bool left_turn = true;
              bool right_turn = false;
              rot_2D_3D_sph(subject_num_nodes, cd, &left_turn, &right_turn);
              if(right_turn)
                reverse_coord(sdim, subject_num_nodes, cd);
              rot_2D_3D_sph(clip_num_nodes, clip_cd, &left_turn, &right_turn);
              if(right_turn)
                reverse_coord(sdim, clip_num_nodes, clip_cd);

              double subject_area = great_circle_area(subject_num_nodes, cd);
              if(subject_area <= 0.) { delete[] cd; continue; }
              weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
              //std::vector<polygon> diff_sph;
              //cart2sph(diff, diff_sph);
            }
            
            delete[] cd;

            // for each non-triangular polygon in diff, use van leer's algorithm to triangulate it
            std::vector<polygon>::iterator diff_it = diff.begin(), diff_ie = diff.end();
            for(;diff_it != diff_ie; ++ diff_it){
              num_p = diff_it->points.size();
              if(num_p > 3){
                pts = new double[sdim*(diff_it->points.size())];
                polygon_to_coords(*diff_it, sdim, pts);
                //if(sdim == 2)
                //  remove_0len_edges2D(&num_p, pts);
                //else
                //  remove_0len_edges3D(&num_p, pts);
                //if(num_p < 3) continue;
                td = new double[num_p*sdim];
                ti = new int[num_p];
                tri_ind = new int[3*(num_p-2)];
                int ret=0;
                if(sdim == 2)
                  ret=triangulate_poly<GEOM_CART2D>(num_p, pts, td, ti, tri_ind);
                if(sdim == 3)
                  ret=triangulate_poly<GEOM_SPH2D3D>(num_p, pts, td, ti, tri_ind);

                // Check return code
                if (ret != ESMCI_TP_SUCCESS) {
                  if (ret == ESMCI_TP_DEGENERATE_POLY) {
                    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                         " - can't triangulate a polygon with less than 3 sides", 
                                                      ESMC_CONTEXT, &rc)) throw rc;
                  } else if (ret == ESMCI_TP_CLOCKWISE_POLY) {
                    //dump_elem(elem, sdim, coord, false);
                    //dump_elem(clip_elem, sdim, clip_coord, false);
                    if(false){
                      std::cout << "dstpoly_it->subject_cd:" << std::endl;
                      for(int npt=0; npt<subject_num_nodes; npt++) std::cout << cd[npt*3] << "," << cd[npt*3+1] << "," << cd[npt*3+2] << std::endl; 
                      cd_sph = new double[subject_num_nodes*2];   cart2sph(subject_num_nodes, cd, cd_sph); 
                      for(int npt=0; npt<subject_num_nodes; npt++) std::cout << cd_sph[npt*2] << "  " << cd_sph[npt*2+1] << std::endl; 
                      std::cout << "clip_cd:" << std::endl;
                      for(int npt=0; npt<clip_num_nodes; npt++) std::cout << clip_cd[npt*3] << "," << clip_cd[npt*3+1] << "," << clip_cd[npt*3+2] << std::endl; 
                      clip_cd_sph = new double[clip_num_nodes*2];   cart2sph(clip_num_nodes, clip_cd, clip_cd_sph); 
                      for(int npt=0; npt<clip_num_nodes; npt++) std::cout << clip_cd_sph[npt*2] << "  " << clip_cd_sph[npt*2+1] << std::endl; 
                      delete[] clip_cd_sph; 
                    }
                    char msg[1024];
                    sprintf(msg," - there was a problem (e.g. repeated points, clockwise poly, etc.) with the triangulation of the element:\n"); 
                    sprintf(msg,"%selem Id: %d clip_elem Id: %d\n", msg, elem.get_id(), clip_elem.get_id());
                    {
                      cd_sph = new double[num_p*2];   cart2sph(num_p, pts, cd_sph); 
                      for(int npt=0; npt<num_p*2; npt++) sprintf(msg,"%s %g", msg, cd_sph[npt]); 
                      delete[] cd_sph; 
                    }
                    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, msg,
                                                    ESMC_CONTEXT, &rc)) throw rc;
                  } else {
                    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                                      " - unknown error in triangulation", ESMC_CONTEXT, &rc)) throw rc;
                  }
                }

                // Add each of the triangles into the merged mesh
                unsigned num_tri_edges = 3;
                double * tri_cd = new double[sdim*num_tri_edges];
                for(int ntri = 0; ntri < num_p-2; ntri ++){
                  // copy each point of the triangle
                  std::memcpy(tri_cd, pts+tri_ind[ntri*num_tri_edges]*sdim, sdim*sizeof(double));
                  std::memcpy(tri_cd+sdim, pts+tri_ind[ntri*num_tri_edges+1]*sdim, sdim*sizeof(double));
                  std::memcpy(tri_cd+2*sdim, pts+tri_ind[ntri*num_tri_edges+2]*sdim, sdim*sizeof(double));

                  // append the triangles onto result polygon vector
                  polygon triangle;
                  coords_to_polygon(3, tri_cd, sdim, triangle);
                  results.push_back(triangle);
                  ncells ++; //debug line
                }
                delete[] pts;
                delete[] td;
                delete[] ti;
                delete[] tri_ind;
                delete[] tri_cd;
              }else{ // diff_it already points to an triangle, append to results
                results.push_back(*diff_it);
              }
            }

            //if(sdim == 3) delete[] clip_cd_sph, cd_sph;
          } // for each dst poly
          // clear dst poly vector and copy all results triangles to it to intersect with the next src element
          //if(elem.get_id() == 5497 || elem.get_id() == 6073){
          //  for(int i=0; i < results.size(); i ++){
          //    std::cout << std::endl;
          //    dump_polygon(results[i], true);
          //  }
          //}
          dstpolys.clear(); dstpolys.resize(results.size());
          std::copy(results.begin(), results.end(), dstpolys.begin());
          results.clear();
        } // iterate through each intersection pair for this dst element
        // For now, compute the remaining dst element area, compute fraction and attach to dst mesh.
        // use the fact that all src elements must be disjoint (not self-intersecting)
        double *f=elem_frac->data(elem);
        *f=1.-fraction_deduction;
        // After creeping all the clip polygons into subject (dst) polygons, finally construct mesh elements
        // based on the polygons in dstpolys vector.
        std::vector<polygon>::iterator res_it = dstpolys.begin(), res_end = dstpolys.end();
        for(;res_it != res_end; ++res_it){
          int n_pts = res_it->points.size();
          double * poly_cd = new double[sdim*n_pts];
          polygon_to_coords(*res_it, sdim, poly_cd);
          construct_sintd(res_it->area(sdim), n_pts, poly_cd, pdim, sdim, &sintd_nodes, &sintd_cells);
          delete [] poly_cd;
        }
      } // intersected dst element
    } // for ei, mesh element iterator
  } 

  // We now have all the genesis cells, compute the merged mesh
  compute_midmesh(sintd_nodes, sintd_cells, pdim, sdim, &mergemesh);
  //char str[64]; memset(str, 0, 64);
  //sprintf(str, "mergemesh.vtk.%d", me);
  //WriteVTKMesh(mergemesh, str);

}

  void MeshSetFraction(Mesh & mesh, double fraction){

    MEField<> *elem_frac=mesh.GetField("elem_frac2");
    if (!elem_frac) {
      Throw() << "Meshes used during mesh merge must support elem_frac2\n";
    }

    // iterate and set
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      double *f=elem_frac->data(elem);
      *f=fraction;
    }
  }
  
  void calc_clipped_poly_2D_2D_cart(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){
  Trace __trace("calc_clipped_poly(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get dst frac2 field
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    
    // NOTE: sr.elem is a passive element and sr.elems is a list of active elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    // If this source element is masked then skip it
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) continue; // if this is masked, then go to next search result
    }

    // Calculate polys
    double src_area;
    std::vector<int> valid;             valid.resize(sr.elems.size(),0);
    std::vector<double> wgts;           wgts.resize(sr.elems.size(), 0.0);
    std::vector<double> sintd_areas;    sintd_areas.resize(sr.elems.size(),0.0);
    std::vector<double> dst_areas;      dst_areas.resize(sr.elems.size(),0.0);

    std::vector<sintd_node *> tmp_nodes;  
    std::vector<sintd_cell *> tmp_cells;  

    std::vector<int> tmp_valid;
    std::vector<double> tmp_sintd_areas;
    std::vector<double> tmp_dst_areas;


    calc_1st_order_weights_2D_2D_cart(sr.elem,src_cfield,sr.elems,dst_cfield, dst_mask_field, dst_frac2_field,
                                      &src_area, &valid, &wgts, &sintd_areas, &dst_areas, 
                                      &tmp_valid, &tmp_sintd_areas, &tmp_dst_areas, 0, &tmp_nodes, &tmp_cells, res_map, 0);

    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (unsigned int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }

    // Count number of valid weights
    int num_valid=0;
    for (unsigned int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(*sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(*sintd_cells));

  } // for searchresult

}

void calc_clipped_poly_2D_3D_sph(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){
  Trace __trace("calc_clipped_poly(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get dst frac2 field
  MEField<> *dst_frac2_field = dstmesh.GetField("elem_frac2");


  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    
    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    // If this source element is masked then skip it
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) continue; // if this is masked, then go to next search result
    }

    // Calculate weights
    double src_area;
    std::vector<int> valid;             valid.resize(sr.elems.size(),0);
    std::vector<double> wgts;           wgts.resize(sr.elems.size(), 0.0);
    std::vector<double> sintd_areas;    sintd_areas.resize(sr.elems.size(),0.0);
    std::vector<double> dst_areas;      dst_areas.resize(sr.elems.size(),0.0);

    std::vector<sintd_node *> tmp_nodes;  
    std::vector<sintd_cell *> tmp_cells;  

    std::vector<int> tmp_valid;
    std::vector<double> tmp_sintd_areas;
    std::vector<double> tmp_dst_areas;

    calc_1st_order_weights_2D_3D_sph(sr.elem,src_cfield,sr.elems,dst_cfield, dst_mask_field, dst_frac2_field,
                                     &src_area, &valid, &wgts, &sintd_areas, &dst_areas, 
                                     &tmp_valid, &tmp_sintd_areas, &tmp_dst_areas, 
                                     0, sintd_nodes, sintd_cells, res_map, 0);

    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (unsigned int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }

    // Count number of valid weights
    int num_valid=0;
    for (unsigned int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(*sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(*sintd_cells));


  } // for searchresult

}


  void calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){

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
      calc_clipped_poly_2D_2D_cart(srcmesh, dstmesh, sres, sintd_nodes, sintd_cells, res_map);
    } else if (sdim==3) {
      calc_clipped_poly_2D_3D_sph(srcmesh, dstmesh, sres, sintd_nodes, sintd_cells, res_map);
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 not supported for conservative regridding";
  }


}

  void copy_mesh(const Mesh & dstmesh, Mesh & meshdiff){
    // Get dim info for mesh
    int sdim=dstmesh.spatial_dim();
    int pdim=dstmesh.parametric_dim();

    int rc;
    unsigned int me = VM::getCurrent(&rc)->getLocalPet();
    std::vector<sintd_node *> sintd_nodes;
    std::vector<sintd_cell *> sintd_cells;

    // go through all src mesh elements
    unsigned int ncells = 0;
    // go through all dst mesh elements
    {
      //std::cout << "Traversing the destination Mesh\n";
      const Mesh & mesh = dstmesh;

      // Get mask and coord field
      //MEField<> *mask_field = mesh.GetField("elem_mask");
      MEField<> &coord = *mesh.GetCoordField();

      Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        const MeshObj &elem = *ei;  //dump_elem(elem, sdim, coord);
        if(elem.get_owner() != me) continue;
        //if(!GetAttr(elem).is_locally_owned()) continue;

        //if(mask_field){ // do not sew an element if it's masked out
        //  double *msk=mask_field->data(elem);
        //  if (*msk>0.5) continue;
        //}

        const MeshObjTopo *topo = GetMeshObjTopo(elem);
        double *cd = new double[sdim*topo->num_nodes];

        for (UInt n = 0; n < topo->num_nodes; n++) {

          const MeshObj &node = *elem.Relations[n].obj;
          double * tmp = coord.data(node);
          for(int i = 0; i < sdim; i ++) {
            cd[(n*sdim)+i] = tmp[i];
          }
        }
        polygon res_poly;
        coords_to_polygon(topo->num_nodes, cd, sdim, res_poly);

        construct_sintd(res_poly.area(sdim), topo->num_nodes, cd, pdim, sdim, 
          &sintd_nodes, &sintd_cells);
        ncells ++;
        delete[] cd;

      } // for ei
    } 

    //{
    //  // Debug
    //  int num_cells = sintd_cells.size();
    //  for (int i=0; i<num_cells; i++) 
    //    sintd_cells[i]->print(me, i, i);
    //}

    // We now have all the genesis cells, compute the merged mesh
    compute_midmesh(sintd_nodes, sintd_cells, pdim, sdim, &meshdiff);
    //char str[64]; memset(str, 0, 64);
    //sprintf(str, "sewmesh.vtk.%d", me);
    //WriteVTKMesh(mergemesh, str);
  }

  // meshdiff is the result mesh from dstmesh clipped by srcmesh, meshdiff=dstmesh - srcmesh
  void diff_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & meshdiff, const Mesh & mesh_src, const Mesh & mesh_dst, SearchResult & sres, interp_mapp sres_map, double threshold){
  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();
  int npet = VM::getCurrent(&rc)->getPetCount();
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  //{
  //  std::vector<polygon> diff;
  //  double p[6] = {3.,3.5, 3.5,3.5, 3.5,4.5};
  //  double q[8] = {3.,3., 4.,3., 4.,4., 3.,4.};
  //  weiler_clip_difference(2,2,3,p,4,q,diff);
  //}

  unsigned int ncells = 0;

  // Go through all dst mesh elements, subject (low priority) mesh, compute differentials
  {
    const Mesh & mesh = dstmesh;
    MEField<> &coord = *mesh.GetCoordField();
    MEField<> &clip_coord = *srcmesh.GetCoordField();
    //MEField<> *elem_frac=mesh.GetField("elem_frac2");
    //if (!elem_frac) Throw() << "Meshes involved in XGrid construction should have frac2 field";

    // Get mask field
    MEField<> *mask_field = mesh.GetField("elem_mask");

    // iterate through dst mesh element, construct its coordinates in 'cd'
    // used in both intersected or non-intersected cases
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;

      if(mask_field){ // do not sew an element if it's masked out
        double *msk=mask_field->data(elem);
        if (*msk>0.5) continue;
      }

      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      int subject_num_nodes = topo->num_nodes;

      double *cd = new double[sdim*topo->num_nodes];
      for (UInt n = 0; n < subject_num_nodes; n++) {
        const MeshObj &node = *elem.Relations[n].obj;
        for(int i = 0; i < sdim; i ++) {
          double * tmp = coord.data(node);
          cd[(n*sdim)+i] = tmp[i];
        }
      }

      // Get rid of degenerate edges
      if(sdim == 2)
        remove_0len_edges2D(&subject_num_nodes, cd);
      else
        remove_0len_edges3D(&subject_num_nodes, cd);

      if(subject_num_nodes < 3) continue;

      // make sure the polygons are in CCW sense before going into Weiler algorithm
      //if(sdim == 2 && area_of_flat_2D_polygon(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);
      //if(sdim == 3 && great_circle_area(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);

      // Check if this element is in sres_map
      // dump_elem(elem, sdim, coord);
      interp_map_iter it = sres_map->find(&elem);
      if(it == sres_map->end()){ // Not intersected, just add it to the list
        polygon res_poly;
        coords_to_polygon(subject_num_nodes, cd, sdim, res_poly);

        construct_sintd(res_poly.area(sdim), subject_num_nodes, cd, pdim, sdim, 
          &sintd_nodes, &sintd_cells);
        delete[] cd;
        ncells ++;
      }else{ 

        // dstpolys contains residual polygons of the original subject polygon after successive clipping
        // before the first clipping, it's the original subject polygon.
        std::vector<polygon> results, dstpolys;
        dstpolys.resize(1);
        coords_to_polygon(subject_num_nodes, cd, sdim, dstpolys[0]);
        delete[] cd;

        interp_map_range range = sres_map->equal_range(&elem);
        double fraction_deduction = 0.;

        // go through each src element that cuts into this dst element
        // Do a first cut compute total fraction reduction, if entire dst element
        // is clipped out, move onto the next dst element
        for(interp_map_iter it = range.first; it != range.second; ++it)
          fraction_deduction += it->second->fraction;
        if(fraction_deduction >= 1.0){
          //double *f=elem_frac->data(elem);
          //*f=0.;
          continue;
        }

        // Do spatial boolean math: difference, cut each intersected part off the dst mesh and 
        // triangulate the remaining polygon
        for(interp_map_iter it = range.first; it != range.second; ++it){

          // debug
          //const interp_res * dbg_interp_res = it->second;
          //double * master_sub_sph = new double[dbg_interp_res->num_subject_nodes*2];
          //double * master_clip_sph = new double[dbg_interp_res->num_clip_nodes*2];
          //{
          //  cart2sph(dbg_interp_res->num_subject_nodes, dbg_interp_res->subject_coords, master_sub_sph);
          //  cart2sph(dbg_interp_res->num_clip_nodes, dbg_interp_res->clip_coords, master_clip_sph);
          //}

          //if(elem.get_id() == 2378 && it->second->clip_elem->get_id() == 3559) 
          //  int nop = elem.get_id() - it->second->clip_elem->get_id();

          // construct clip element polygon from src element
          const MeshObj & clip_elem = *(it->second->clip_elem);
          int clip_num_nodes = it->second->num_clip_nodes;
          double *clip_cd = it->second->clip_coords;

          // for each polygon in cutted dst element, compute residual diff polygons
          int num_p; int *ti, *tri_ind; double *pts, *td;
          std::vector<polygon> diff;
          for(std::vector<polygon>::iterator dstpoly_it = dstpolys.begin();
            dstpoly_it != dstpolys.end(); ++ dstpoly_it){

            diff.clear();

            // diff each dst element residual polygon with src element iteratively
            // this normally does not happen too deep.
            int subject_num_nodes = dstpoly_it->points.size();
            double * cd = new double[sdim*dstpoly_it->points.size()];
            polygon_to_coords(*dstpoly_it, sdim, cd);

            // Get rid of degenerate edges
            if(sdim == 2)
              remove_0len_edges2D(&subject_num_nodes, cd);
            else
              remove_0len_edges3D(&subject_num_nodes, cd);

            if(subject_num_nodes < 3) continue;
          
            double *cd_sph, *clip_cd_sph;
            if(sdim == 2) weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
            if(sdim == 3){
              
              //clip_cd_sph = new double[clip_num_nodes*2]; cart2sph(clip_num_nodes, clip_cd, clip_cd_sph);
              //cd_sph = new double[subject_num_nodes*2];   cart2sph(subject_num_nodes, cd, cd_sph);

              //weiler_clip_difference(pdim, 2, subject_num_nodes, cd_sph, clip_num_nodes, clip_cd_sph, diff);
              //std::vector<polygon> diff_cart;
              //sph2cart(diff, diff_cart);
              //diff.clear(); diff.resize(diff_cart.size()); std::copy(diff_cart.begin(), diff_cart.end(), diff.begin());

              double subject_area = great_circle_area(subject_num_nodes, cd);
              if(subject_area <= 0.) { delete[] cd; continue; }
              weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
              //std::vector<polygon> diff_sph;
              //cart2sph(diff, diff_sph);
            }
            
            delete[] cd;

            // for each non-triangular polygon in diff, use van leer's algorithm to triangulate it
            std::vector<polygon>::iterator diff_it = diff.begin(), diff_ie = diff.end();
            for(;diff_it != diff_ie; ++ diff_it){
              num_p = diff_it->points.size();
              if(num_p > 3){
                pts = new double[sdim*(diff_it->points.size())];
                polygon_to_coords(*diff_it, sdim, pts);
                td = new double[num_p*sdim];
                ti = new int[num_p];
                tri_ind = new int[3*(num_p-2)];
                if(sdim == 2)
                  triangulate_poly<GEOM_CART2D>(num_p, pts, td, ti, tri_ind);
                if(sdim == 3)
                  triangulate_poly<GEOM_SPH2D3D>(num_p, pts, td, ti, tri_ind);

                // Add each of the triangles into the merged mesh
                unsigned num_tri_edges = 3;
                double * tri_cd = new double[sdim*num_tri_edges];
                for(int ntri = 0; ntri < num_p-2; ntri ++){
                  // copy each point of the triangle
                  std::memcpy(tri_cd, pts+tri_ind[ntri*num_tri_edges]*sdim, sdim*sizeof(double));
                  std::memcpy(tri_cd+sdim, pts+tri_ind[ntri*num_tri_edges+1]*sdim, sdim*sizeof(double));
                  std::memcpy(tri_cd+2*sdim, pts+tri_ind[ntri*num_tri_edges+2]*sdim, sdim*sizeof(double));

                  // append the triangles onto result polygon vector
                  polygon triangle;
                  coords_to_polygon(3, tri_cd, sdim, triangle);
                  results.push_back(triangle);
                  ncells ++; //debug line
                }
                delete[] pts;
                delete[] td;
                delete[] ti;
                delete[] tri_ind;
                delete[] tri_cd;
              }else{ // diff_it already points to an triangle, append to results
                results.push_back(*diff_it);
              }
            }

            //if(sdim == 3) delete[] clip_cd_sph, cd_sph;
          } // for each dst poly
          // clear dst poly vector and copy all results triangles to it to intersect with the next src element
          dstpolys.clear(); dstpolys.resize(results.size());
          std::copy(results.begin(), results.end(), dstpolys.begin());
          results.clear();
        } // iterate through each intersection pair for this dst element
        // For now, compute the remaining dst element area, compute fraction and attach to dst mesh.
        // use the fact that all src elements must be disjoint (not self-intersecting)
        //double *f=elem_frac->data(elem);
        //*f=1.-fraction_deduction;
        // After creeping all the clip polygons into subject (dst) polygons, finally construct mesh elements
        // based on the polygons in dstpolys vector.
        std::vector<polygon>::iterator res_it = dstpolys.begin(), res_end = dstpolys.end();
        for(;res_it != res_end; ++res_it){
          int n_pts = res_it->points.size();
          double * poly_cd = new double[sdim*n_pts];
          polygon_to_coords(*res_it, sdim, poly_cd);
          double cell_area = res_it->area(sdim);
          if(cell_area >= threshold)
            construct_sintd(cell_area, n_pts, poly_cd, pdim, sdim, &sintd_nodes, &sintd_cells);
          delete [] poly_cd;
        }
      } // intersected dst element
    } // for ei, mesh element iterator
  } 

  // We now have all the genesis cells, compute the differential mesh
  compute_midmesh(sintd_nodes, sintd_cells, pdim, sdim, &meshdiff);
  //char str[64]; memset(str, 0, 64);
  //sprintf(str, "mergemesh.vtk.%d", me);
  //WriteVTKMesh(mergemesh, str);

  }

} // namespace
