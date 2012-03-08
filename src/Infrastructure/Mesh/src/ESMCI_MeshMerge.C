// $Id: ESMCI_MeshMerge.C,v 1.11 2012/03/08 19:41:14 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_BBox.h>
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Exception.h> 
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MathUtil.h> 
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshMerge.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_SparseMsg.h>
#include <Mesh/include/ESMCI_SpaceDir.h>
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <ESMCI_VM.h>
 
#include <algorithm>
#include <iterator>
#include <ostream>
#include <set>
#include <limits>
#include <vector>

#include "stdlib.h"
#include <cstring>
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshMerge.C,v 1.11 2012/03/08 19:41:14 feiliu Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

  struct interp_res{
    const MeshObj * clip_elem;
    int num_sintd_nodes;
    int num_subject_nodes;
    int num_clip_nodes;
    int sdim;
    double * subject_coords;
    double * clip_coords;
    double subject_elem_area;
    double clip_elem_area;
    double fraction;

    interp_res(const MeshObj * _clip_elem, int _num_sintd_nodes, int _num_subject_nodes, int _num_clip_nodes, 
      int _sdim, double * _subject_coords, double * _clip_coords, 
      double _subject_elem_area, double _clip_elem_area, double _fraction) : clip_elem(_clip_elem), 
        num_sintd_nodes(_num_sintd_nodes), num_subject_nodes(_num_subject_nodes), num_clip_nodes(_num_clip_nodes),
        sdim(_sdim), subject_elem_area(_subject_elem_area), clip_elem_area(_clip_elem_area), 
        fraction(_fraction) {

      subject_coords = new double[num_subject_nodes*sdim];
      for(int i = 0; i < num_subject_nodes*sdim; i ++) subject_coords[i] = _subject_coords[i];

      clip_coords = new double[num_clip_nodes*sdim];
      for(int i = 0; i < num_clip_nodes*sdim; i ++) clip_coords[i] = _clip_coords[i];

    }

    ~interp_res(){
      delete[] subject_coords;
      delete[] clip_coords;
    }
 
  };

  typedef std::multimap<const MeshObj *, const interp_res *> interp_map;
  typedef std::multimap<const MeshObj *, const interp_res *> * interp_mapp;
  typedef std::multimap<const MeshObj *, const interp_res *>::const_iterator interp_map_citer;
  typedef std::multimap<const MeshObj *, const interp_res *>::iterator interp_map_iter;
  typedef std::pair<std::multimap<const MeshObj *, const interp_res *>::iterator, 
                    std::multimap<const MeshObj *, const interp_res *>::iterator > interp_map_range;

  typedef std::vector<sintd_node *> * Sintd_nodes;
  typedef std::vector<sintd_cell *> * Sintd_cells;

  void calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map);
  void sew_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh);
  void concat_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh, const Mesh & mesh_src, const Mesh & mesh_dst, 
    SearchResult &sres, interp_mapp sres_map);
  void dump_elem(const MeshObj & elem, int sdim, const MEField<> & coord, bool check_local=true);

// The main routine
// srcmesh is a higher priority mesh than dstmesh, srcmesh clips into dstmesh
void MeshMerge(Mesh &srcmesh, Mesh &dstmesh, Mesh **meshpp) {
  Trace __trace("MeshMerge()");
  //WriteVTKMesh(srcmesh, "srcmesh.vtk");

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

  //printf("Inside MeshMerge srcmesh.spatial_dim=%d \n",srcmesh.spatial_dim());
  //printf("Inside MeshMerge dstmesh.spatial_dim=%d \n",dstmesh.spatial_dim());

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
    // Build the rendezvous meshes and compute search result
    std::vector<Interp::FieldPair> fpairs;
    fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_CONSERVE));
    interp = new Interp(dstmesh, srcmesh, 0, true, fpairs, unmappedaction);

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
  calc_clipped_poly(dstmesh, *mesh_src, sres, &num_pnts, &pnts, &num_pnts_in_poly, &sintd_nodes, &sintd_cells, &res_map);

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
  int npet = VM::getCurrent(&rc)->getPetCount();
  int me   = VM::getCurrent(&rc)->getLocalPet();
  int owner = elem.get_owner();
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

void sew_meshes(const Mesh & srcmesh, const Mesh & dstmesh, Mesh & mergemesh){

  // Get dim info for mesh
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // go through all src mesh elements
  unsigned int ncells = 0;
  {
    //std::cout << "Traversing the source Mesh\n";
    const Mesh & mesh = srcmesh;
    MEField<> &coord = *mesh.GetCoordField();
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;  //dump_elem(elem, sdim, coord);
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];
      if(elem.get_owner() != me) continue;
      //if(!GetAttr(elem).is_locally_owned()) continue;

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

      construct_sintd(0., topo->num_nodes, cd, pdim, sdim, 
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
    MEField<> &coord = *mesh.GetCoordField();
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;  //dump_elem(elem, sdim, coord);
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      double *cd = new double[sdim*topo->num_nodes];
      if(elem.get_owner() != me) continue;
      //if(!GetAttr(elem).is_locally_owned()) continue;

      for (UInt n = 0; n < topo->num_nodes; n++) {

        const MeshObj &node = *elem.Relations[n].obj;
        double * tmp = coord.data(node);
        for(int i = 0; i < sdim; i ++) {
          cd[(n*sdim)+i] = tmp[i];
        }
      }

      construct_sintd(0., topo->num_nodes, cd, pdim, sdim, 
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

  // go through all src mesh elements, clip (higher priority) mesh, save all clip cells
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

  // Go through all dst mesh elements, subject (low priority) mesh, compute differentials
  {
    const Mesh & mesh = dstmesh;
    unsigned int ncells = 0;
    MEField<> &coord = *mesh.GetCoordField();
    MEField<> &clip_coord = *srcmesh.GetCoordField();
    MEField<> *elem_frac=mesh.GetField("elem_frac2");
    if (!elem_frac) Throw() << "Meshes involved in XGrid construction should have frac2 field";

    // iterate through dst mesh element, construct its coordinates in 'cd'
    // used in both intersected or non-intersected cases
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
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

      // make sure the polygons are in CCW sense before going into Weiler algorithm
      //if(sdim == 2 && area_of_flat_2D_polygon(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);
      //if(sdim == 3 && great_circle_area(subject_num_nodes, cd) < 0)
      //  reverse_coord(sdim, subject_num_nodes, cd);

      // Check if this element is in sres_map
      // dump_elem(elem, sdim, coord);
      interp_map_iter it = sres_map->find(&elem);
      if(it == sres_map->end()){ // Not intersected, just add it to the list

        construct_sintd(0., subject_num_nodes, cd, pdim, sdim, 
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

          // construct clip element polygon from src element
          const MeshObj & clip_elem = *(it->second->clip_elem);
          const MeshObjTopo *topo = GetMeshObjTopo(clip_elem);
          int clip_num_nodes = topo->num_nodes;
          double *clip_cd = it->second->clip_coords;

          //double *clip_cd = new double[sdim*topo->num_nodes];
          //for (UInt n = 0; n < clip_num_nodes; n++) {

          //  const MeshObj &node = *clip_elem.Relations[n].obj;
          //  for(int i = 0; i < sdim; i ++) {
          //    double * tmp = clip_coord.data(node);
          //    clip_cd[(n*sdim)+i] = tmp[i];
          //  }
          //}

          // for each polygon in cutted dst element, compute residual diff polygons
          int num_p; int *ti, *tri_ind; double *pts, *td;
          std::vector<polygon> diff;
          for(std::vector<polygon>::iterator dstpoly_it = dstpolys.begin();
            dstpoly_it != dstpolys.end(); ++ dstpoly_it){

            // diff each dst element residual polygon with src element iteratively
            // this normally does not happen too deep.
            subject_num_nodes = dstpoly_it->points.size();
            cd = new double[sdim*dstpoly_it->points.size()];
            polygon_to_coords(*dstpoly_it, sdim, cd);
            diff.clear();
            
            double *cd_sph, *clip_cd_sph;
            if(sdim == 2) weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
            if(sdim == 3){
              clip_cd_sph = new double[clip_num_nodes*2]; cart2sph(clip_num_nodes, clip_cd, clip_cd_sph);
              cd_sph = new double[subject_num_nodes*2];   cart2sph(subject_num_nodes, cd, cd_sph);

              //weiler_clip_difference(pdim, 2, subject_num_nodes, cd_sph, clip_num_nodes, clip_cd_sph, diff);
              //std::vector<polygon> diff_cart;
              //sph2cart(diff, diff_cart);
              //diff.clear(); diff.resize(diff_cart.size()); std::copy(diff_cart.begin(), diff_cart.end(), diff.begin());

              weiler_clip_difference(pdim, sdim, subject_num_nodes, cd, clip_num_nodes, clip_cd, diff);
              std::vector<polygon> diff_sph;
              cart2sph(diff, diff_sph);
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

            if(sdim == 3) delete[] clip_cd_sph, cd_sph;
          } // for each dst poly
          // clear dst poly vector and copy all results triangles to it to intersect with the next src element
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
          construct_sintd(0., n_pts, poly_cd, pdim, sdim, &sintd_nodes, &sintd_cells);
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
  

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void calc_inter_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                             std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                             int *num_pnts, std::vector<double> *pnts, std::vector<int> *num_pnts_in_poly, 
                             Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){


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
      intersect_convex_poly2D(num_dst_nodes, dst_coords,
                              num_src_nodes, src_coords,
                              tmp_coords,
                              &num_sintd_nodes, sintd_coords); 
     

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3)
        continue;

      // TODO chop into triangles if necessary
      // calculate intersection area
      double sintd_areas=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords); 

      construct_sintd(sintd_areas,
          num_sintd_nodes, sintd_coords, 2, 2,
          sintd_nodes, sintd_cells);

      double src_area = area_of_flat_2D_polygon(num_src_nodes, src_coords);
      double dst_area = area_of_flat_2D_polygon(num_dst_nodes, dst_coords);

      // because the calling routine MeshMerge has reversed src/dst sense, reverse here too
      // to construct proper sres_map, here src->subject, dst->clip
      interp_map_iter it = res_map->find(src_elem);
      if(it != res_map->end()) { 
        // check if this is a unique intersection
        interp_map_range range = res_map->equal_range(src_elem);
        for(interp_map_iter it = range.first; it != range.second; ++it){
          if(it->second->clip_elem == dst_elem)
            Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
        }
      }
      res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, 2, src_coords, dst_coords, 
        src_area, dst_area, ((src_area == 0.)? 1.:sintd_areas/src_area) ) ) ); 

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




  void calc_clipped_poly_2D_2D_cart(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres,   int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){
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
    calc_inter_2D_2D_cart(sr.elem,src_cfield,sr.elems,dst_cfield, num_pnts, pnts, num, sintd_nodes, sintd_cells, res_map);


  } // for searchresult

}



  void calc_inter_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                            std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                            int *num_pnts, std::vector<double> *pnts, std::vector<int> *num_pnts_in_poly,
                            Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){


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
      double sintd_areas=great_circle_area(num_sintd_nodes, sintd_coords); 

      construct_sintd(sintd_areas,
          num_sintd_nodes, sintd_coords, 2, 3,
          sintd_nodes, sintd_cells);

      double src_area = great_circle_area(num_src_nodes, src_coords);
      double dst_area = great_circle_area(num_dst_nodes, dst_coords);

      //interp_map_iter it = res_map->find(dst_elem);
      //if(it != res_map->end()) { 
      //  // check if this is a unique intersection
      //  interp_map_range range = res_map->equal_range(dst_elem);
      //  for(interp_map_iter it = range.first; it != range.second; ++it){
      //    if(it->second->clip_elem == src_elem)
      //      Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
      //  }
      //}
      //res_map->insert(std::make_pair(dst_elem, new interp_res(src_elem, num_sintd_nodes, num_dst_nodes, num_src_nodes, 3, dst_coords, src_coords, 
      //  dst_area, src_area, ((src_area == 0.)? 1.:sintd_areas/src_area) ) ) ); 
      // because the calling routine MeshMerge has reversed src/dst sense, reverse here too
      // to construct proper sres_map, here src->subject, dst->clip
      interp_map_iter it = res_map->find(src_elem);
      if(it != res_map->end()) { 
        // check if this is a unique intersection
        interp_map_range range = res_map->equal_range(src_elem);
        for(interp_map_iter it = range.first; it != range.second; ++it){
          if(it->second->clip_elem == dst_elem)
            Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
        }
      }
      res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, 3, src_coords, dst_coords, 
        src_area, dst_area, ((src_area == 0.)? 1.:sintd_areas/src_area) ) ) ); 

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





void calc_clipped_poly_2D_3D_sph(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres,   int *num_pnts, std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){
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
    calc_inter_2D_3D_sph(sr.elem,src_cfield,sr.elems,dst_cfield, num_pnts, pnts,num, sintd_nodes, sintd_cells, res_map);


  } // for searchresult

}


  void calc_clipped_poly(const Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, int *num_pnts,  std::vector<double> *pnts, std::vector<int> *num, Sintd_nodes sintd_nodes, Sintd_cells sintd_cells, interp_mapp res_map){


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
      calc_clipped_poly_2D_2D_cart(srcmesh, dstmesh, sres, num_pnts, pnts, num, sintd_nodes, sintd_cells, res_map);
    } else if (sdim==3) {
      calc_clipped_poly_2D_3D_sph(srcmesh, dstmesh, sres, num_pnts, pnts, num, sintd_nodes, sintd_cells, res_map);
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 not supported for conservative regridding";
  }



}

} // namespace
