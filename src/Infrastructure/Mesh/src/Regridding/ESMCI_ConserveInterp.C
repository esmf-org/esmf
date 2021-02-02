// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_ConserveInterp.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_Polynomial.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/Legacy/ESMCI_Ftn.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/Legacy/ESMCI_Phedra.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


namespace ESMCI {


 bool debug=false;

#ifdef BOB_XGRID_DEBUG
  int global_src_id=-1;
  int global_dst_id=-1;
#endif

  //////////////// BEGIN CALC 2D 2D  WEIGHTS ////////////////

  // ORIGINAL VERSION OF THIS FUNCTION
  void calc_1st_order_weights_2D_2D_cart_orig(const MeshObj *src_elem, MEField<> *src_cfield,
                                              std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                              double *src_elem_area,
                                              std::vector<int> *valid, std::vector<double> *wgts,
                                              std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                              Mesh * midmesh,
                                              std::vector<sintd_node *> * sintd_nodes,
                                              std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {


// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES)

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_2D];
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];


    // Get src coords
    get_elem_coords_2D_ccw(src_elem, src_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);


    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges2D(&num_src_nodes, src_coords);

    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad2D(num_src_nodes, src_coords)) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // calculate dst area
    double src_area=area_of_flat_2D_polygon(num_src_nodes, src_coords);

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // Output src_elem_area
    *src_elem_area=src_area;

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];


    // Allocate something to hold areas
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];


      // Get dst coords
      get_elem_coords_2D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);


      // if no nodes then go to next
      if (num_dst_nodes<1) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_dst_nodes, dst_coords);

      // if less than a triangle skip
      if (num_dst_nodes<3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad2D(num_dst_nodes, dst_coords)) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate dst area
     dst_areas[i]=area_of_flat_2D_polygon(num_dst_nodes, dst_coords);

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
          ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
        printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
        for (int j=0; j<num_dst_nodes; j++) {
          printf(" [%f,%f] ",dst_coords[2*j],dst_coords[2*j+1]);
        }
        printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate intersection area
      sintd_areas[i]=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords);

      (*valid)[i]=1;

      // Invalidate masked destination elements
      if (dst_mask_field) {
        double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          (*valid)[i]=0;
          continue;
        }
      }
      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          (*valid)[i] = 0;
          continue;
        }
      }

      if(midmesh || res_map) {
        compute_sintd_nodes_cells(sintd_areas[i],
				  num_sintd_nodes, sintd_coords, 2, 2,
				  sintd_nodes, sintd_cells, zz);
      }

      // append result to a multi-map index-ed by passive mesh element for merging optimization
      if(res_map){
        interp_map_iter it = res_map->find(src_elem);
        if(it != res_map->end()) {
          // check if this is a unique intersection
          interp_map_range range = res_map->equal_range(src_elem);
          for(interp_map_iter it = range.first; it != range.second; ++it){
            if(it->second->clip_elem == dst_elem)
              Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
          }
        }
        int sdim = 2;
        res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, sdim, src_coords, dst_coords,
          src_area, dst_areas[i], ((src_area == 0.)? 1.:sintd_areas[i]/src_area) ) ) );
      }
    }

    if(res_map) return;     // not intended for weight calculation

    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }


    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
  }




  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_2D_cart_src_and_dst_pnts(int num_src_nodes, double *src_coords,
                                                          int num_dst_nodes, double *dst_coords,
                                                          int *valid, double *sintd_area, double *dst_area,
                                                          Mesh * midmesh,
                                                          std::vector<sintd_node *> * sintd_nodes,
                                                          std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {

// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES)
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];

    // Error checking of dst cell (e.g. is smashed) done above

    // calculate dst area
    *dst_area=area_of_flat_2D_polygon(num_dst_nodes, dst_coords);

    // if destination area is 0.0, invalidate and go to next
    if (*dst_area==0.0) {
      *valid=0;
      *sintd_area=0.0;
      *dst_area=0.0;
      return;
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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
          ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
        printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
        for (int j=0; j<num_dst_nodes; j++) {
          printf(" [%f,%f] ",dst_coords[2*j],dst_coords[2*j+1]);
        }
        printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
        *valid=0;
        *sintd_area=0.0;
        *dst_area=0.0;
        return;
      }

      // calculate intersection area
      *sintd_area=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords);

      // Mark this as valid
      *valid=1;

#if 0
      if(midmesh || res_map)
        compute_sintd_nodes_cells(sintd_areas[i],
          num_sintd_nodes, sintd_coords, 2, 2,
          sintd_nodes, sintd_cells, zz);

      // append result to a multi-map index-ed by passive mesh element for merging optimization
      if(res_map){
        interp_map_iter it = res_map->find(src_elem);
        if(it != res_map->end()) {
          // check if this is a unique intersection
          interp_map_range range = res_map->equal_range(src_elem);
          for(interp_map_iter it = range.first; it != range.second; ++it){
            if(it->second->clip_elem == dst_elem)
              Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
          }
        }
        int sdim = 2;
        res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, sdim, src_coords, dst_coords,
          src_area, dst_areas[i], ((src_area == 0.)? 1.:sintd_areas[i]/src_area) ) ) );
      }

    if(res_map) return;     // not intended for weight calculation
#endif

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_2D_cart_src_pnts(int num_src_nodes, double *src_coords,
                                                  std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                                  double *src_elem_area,
                                                  std::vector<int> *valid_list,
                                                  std::vector<double> *sintd_area_list, std::vector<double> *dst_area_list,
                                                  Mesh * midmesh,
                                                  std::vector<sintd_node *> * sintd_nodes,
                                                  std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {


    // Error checking of src cell (e.g. is smashed quad) done above

    // calculate dst area
    double src_area=area_of_flat_2D_polygon(num_src_nodes, src_coords);

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid_list)[i]=0;
        (*sintd_area_list)[i]=0.0;
        (*dst_area_list)[i]=0.0;
      }
      return;
    }

    // Output src_elem_area
    *src_elem_area=src_area;

// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES)
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];


 /* XMRKX */

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Init everything to 0s
      (*valid_list)[i]=0;
      (*sintd_area_list)[i]=0.0;
      (*dst_area_list)[i]=0.0;


      // Invalidate masked destination elements
      if (dst_mask_field) {
        double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          // Init to 0's above
          continue;
        }
      }

      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          // Init to 0's above
          continue;
        }
      }

      // Get dst coords
      get_elem_coords_2D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_dst_nodes, dst_coords);

      // if less than a triangle skip
      if (num_dst_nodes<3) {
        // Init to 0's above
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad2D(num_dst_nodes, dst_coords)) {
        // Init to 0's above
        continue;
      }

      // See if dst cell concave
      bool is_concave=false;
      if (num_src_nodes > 3) {
        bool left_turn=false;
        bool right_turn=false;

        rot_2D_2D_cart(num_dst_nodes, dst_coords, &left_turn, &right_turn);

        if (left_turn && right_turn) is_concave=true;
      }

      // If not concave, calculate intersection and intersection area for 1
      if (!is_concave) {
        int valid;
        double sintd_area;
        double dst_area;
        calc_1st_order_weights_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,
                                                           num_dst_nodes, dst_coords,
                                                           &valid, &sintd_area, &dst_area,
                                                           midmesh,
                                                           sintd_nodes,
                                                           sintd_cells, res_map, zz);

        // Set output based on validity
        if (valid==1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i]=sintd_area;
          (*dst_area_list)[i]=dst_area;
        }
        // else {
        //  Init to 0's above
        //}

    } else { // If not concave, calculate intersection and intersection area for both and combine

        // Space for temporary buffers
        double td[2*4];
        int ti[4];
        int tri_ind[6];

        // This must be a quad if not complain and exit
        // IF NOT A QUAD, THEN THE ABOVE BUFFER SIZES MUST BE CHANGED!!!
        // TO EMPHASIZE THAT IT MUST BE QUAD 4 IS PASSED IN FOR THE SIZE BELOW.
        if (num_dst_nodes != 4) Throw() << " This isn't a quad, but it should be!";
        int ret=triangulate_poly<GEOM_CART2D>(4, dst_coords, td,
                                              ti, tri_ind);
        // Error check
        // Check return code
        if (ret != ESMCI_TP_SUCCESS) {
          if (ret == ESMCI_TP_DEGENERATE_POLY) Throw() << " - can't triangulate a polygon with less than 3 sides";
          else if (ret == ESMCI_TP_CLOCKWISE_POLY) Throw() << " - clockwise polygons not supported in triangulation routine";
          else Throw() << " - unknown error in triangulation";
        }

        // Because this is a quad it will be in 2 pieces.
        double tri[6];

        // Tri 1
        tri[0]=dst_coords[2*tri_ind[0]];
        tri[1]=dst_coords[2*tri_ind[0]+1];
        tri[2]=dst_coords[2*tri_ind[1]];
        tri[3]=dst_coords[2*tri_ind[1]+1];
        tri[4]=dst_coords[2*tri_ind[2]];
        tri[5]=dst_coords[2*tri_ind[2]+1];


        // printf("Concave id=%d\n",src_elem->get_id());
        // printf("tri 1=%d %d %d\n",tri_ind[0],tri_ind[1],tri_ind[2]);
        // printf("tri 1=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

        int valid1;
        double sintd_area1;
        double dst_area1;
        calc_1st_order_weights_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,
                                                           3, tri,
                                                           &valid1, &sintd_area1, &dst_area1,
                                                           midmesh,
                                                           sintd_nodes,
                                                           sintd_cells, res_map, zz);

        // Set output based on validity
        if (valid1 == 1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i]=sintd_area1;
          (*dst_area_list)[i]=dst_area1;
        }


        // Tri 2
        tri[0]=dst_coords[2*tri_ind[3]];
        tri[1]=dst_coords[2*tri_ind[3]+1];
        tri[2]=dst_coords[2*tri_ind[4]];
        tri[3]=dst_coords[2*tri_ind[4]+1];
        tri[4]=dst_coords[2*tri_ind[5]];
        tri[5]=dst_coords[2*tri_ind[5]+1];

        int valid2;
        double sintd_area2;
        double dst_area2;
        calc_1st_order_weights_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,
                                                           3, tri,
                                                           &valid2, &sintd_area2, &dst_area2,
                                                           midmesh,
                                                           sintd_nodes,
                                                           sintd_cells, res_map, zz);


        // Set output based on validity
        if (valid2 == 1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i] += sintd_area2;
          (*dst_area_list)[i]   += dst_area2;
        }
      }
    }


#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
  }


  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid,
                                           std::vector<double> *wgts,
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           Mesh * midmesh,
                                           std::vector<sintd_node *> * sintd_nodes,
                                         std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {

    // Use original version if midmesh exists
    // TODO: Fei fix this
    if(midmesh || res_map) {
      calc_1st_order_weights_2D_2D_cart_orig(src_elem, src_cfield,
                                             dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                             src_elem_area,
                                             valid, wgts,
                                             sintd_areas_out, dst_areas_out,
                                             midmesh,
                                             sintd_nodes,
                                             sintd_cells, res_map, zz);

      return;
    }


// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES)

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_2D];
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];

 /* XMRKX */

    // Get src coords
    get_elem_coords_2D_ccw(src_elem, src_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);

    // Get rid of degenerate edges
    remove_0len_edges2D(&num_src_nodes, src_coords);

    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad2D(num_src_nodes, src_coords)) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // See if concave
    bool is_concave=false;
    if (num_src_nodes > 3) {
      bool left_turn=false;
      bool right_turn=false;

      rot_2D_2D_cart(num_src_nodes, src_coords, &left_turn, &right_turn);

      if (left_turn && right_turn) is_concave=true;
    }

    // If not concave then just call into the lower level
    if (!is_concave) {
      calc_1st_order_weights_2D_2D_cart_src_pnts(num_src_nodes, src_coords,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 src_elem_area,
                                                 valid,
                                                 sintd_areas_out, dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);
    } else { // else, break into two pieces...

      // Space for temporary buffers
      double td[2*4];
      int ti[4];
      int tri_ind[6];


      // This must be a quad if not complain and exit
      // IF NOT A QUAD, THEN THE ABOVE BUFFER SIZES MUST BE CHANGED!!!
      // TO EMPHASIZE THAT IT MUST BE QUAD 4 IS PASSED IN FOR THE SIZE BELOW.
      if (num_src_nodes != 4) Throw() << " This isn't a quad, but it should be!";
      int ret=triangulate_poly<GEOM_CART2D>(4, src_coords, td,
                                            ti, tri_ind);
      // Error check
      // Check return code
      if (ret != ESMCI_TP_SUCCESS) {
        if (ret == ESMCI_TP_DEGENERATE_POLY) Throw() << " - can't triangulate a polygon with less than 3 sides";
        else if (ret == ESMCI_TP_CLOCKWISE_POLY) Throw() << " - clockwise polygons not supported in triangulation routine";
        else Throw() << " - unknown error in triangulation";
      }


      // Because this is a quad it will be in 2 pieces.
      double tri[6];

      // Tri 1
      tri[0]=src_coords[2*tri_ind[0]];
      tri[1]=src_coords[2*tri_ind[0]+1];
      tri[2]=src_coords[2*tri_ind[1]];
      tri[3]=src_coords[2*tri_ind[1]+1];
      tri[4]=src_coords[2*tri_ind[2]];
      tri[5]=src_coords[2*tri_ind[2]+1];


      // printf("Concave id=%d\n",src_elem->get_id());
      // printf("tri 1=%d %d %d\n",tri_ind[0],tri_ind[1],tri_ind[2]);
      // printf("tri 1=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

      calc_1st_order_weights_2D_2D_cart_src_pnts(3, tri,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 src_elem_area,
                                                 valid,
                                                 sintd_areas_out, dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);



      // Tri 2
      tri[0]=src_coords[2*tri_ind[3]];
      tri[1]=src_coords[2*tri_ind[3]+1];
      tri[2]=src_coords[2*tri_ind[4]];
      tri[3]=src_coords[2*tri_ind[4]+1];
      tri[4]=src_coords[2*tri_ind[5]];
      tri[5]=src_coords[2*tri_ind[5]+1];


      // printf("tri 2=%d %d %d\n",tri_ind[3],tri_ind[4],tri_ind[5]);
      // printf("tri 2=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

      // Tmp variables to hold info from second triangle
      double src_elem_area2;

      // If need to expand arrays, expand
      if (dst_elems.size() > tmp_valid->size()) {
        tmp_valid->resize(dst_elems.size(),0);
        tmp_sintd_areas_out->resize(dst_elems.size(),0.0);
        tmp_dst_areas_out->resize(dst_elems.size(),0.0);
      }

      calc_1st_order_weights_2D_2D_cart_src_pnts(3, tri,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 &src_elem_area2,
                                                 tmp_valid,
                                                 tmp_sintd_areas_out, tmp_dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);

      // Merge together src area
      *src_elem_area=*src_elem_area+src_elem_area2;

      //loop through merging other things
      for (int i=0; i<dst_elems.size(); i++) {

        if ((*valid)[i]==1) {
          if ((*tmp_valid)[i]==1) {
            (*valid)[i]=1;
            (*sintd_areas_out)[i]=(*sintd_areas_out)[i]+(*tmp_sintd_areas_out)[i];
            // (*dst_areas_out)[i] already set
          } else {
            (*valid)[i]=1;
            // (*sintd_areas_out)[i] already set
            // (*dst_areas_out)[i] already set
          }
        } else {
          if ((*tmp_valid)[i]==1) {
            (*valid)[i]=1;
            (*sintd_areas_out)[i]=(*tmp_sintd_areas_out)[i];
            (*dst_areas_out)[i]=(*tmp_dst_areas_out)[i];
          } else {
            (*valid)[i]=0;
          }
        }

      }
    }

    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=(*sintd_areas_out)[i]/(*dst_areas_out)[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
  }


  //////////////// END CALC 2D 2D WEIGHTS //////////////////



///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Calculate weights for a 2D polygon on a Sphere //////////////////////////
/////////////////////////////////////     (Assumes polygon has great circle sides)   //////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////


void norm_poly3D(int num_p, double *p) {
#define NORM(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])

  // See if there are any equal points
  for (int i=0; i<num_p; i++) {
    double *pnt=p+3*i;

    double n=NORM(pnt);

    pnt[0] = pnt[0]/n;
    pnt[1] = pnt[1]/n;
    pnt[2] = pnt[2]/n;


  }

#undef NORM
}



  //////////////// BEGIN CALC 2D 3D  WEIGHTS ////////////////

  // ORIGINAL VERSION OF THIS FUNCTION

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph_orig(const MeshObj *src_elem, MEField<> *src_cfield, 
					     std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,  
					     double *src_elem_area,
					     std::vector<int> *valid, std::vector<double> *wgts, 
					     std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out, 
					     Mesh *midmesh, std::vector<sintd_node *> * sintd_nodes, 
					     std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz, 
					     MEField<> *src_side_field, MEField<> *dst_side_field) {


// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES)

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_3D];
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

    // Get src coords
    get_elem_coords_3D_ccw(src_elem, src_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);
     //  get_elem_coords(src_elem, src_cfield, 3, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);


    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_src_nodes, src_coords);


    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad3D(num_src_nodes, src_coords)) {
       *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // calculate dst area
    double src_area=great_circle_area(num_src_nodes, src_coords);


    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
         (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // Output src_elem_area
    *src_elem_area=src_area;

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Allocate something to hold areas
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
       const MeshObj *dst_elem = dst_elems[i];

      // Get dst coords
      get_elem_coords_3D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);


      // if no nodes then go to next
      if (num_dst_nodes<1) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);

      // if less than a triangle complain
      if (num_dst_nodes<3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad3D(num_dst_nodes, dst_coords)) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate dst area
     dst_areas[i]=great_circle_area(num_dst_nodes, dst_coords);

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
          ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
        printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
        for (int j=0; j<num_dst_nodes; j++) {
          printf(" [%f,%f,%f] ",dst_coords[3*j],dst_coords[3*j+1],dst_coords[3*j+2]);
        }
        printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate intersection area
      sintd_areas[i]=great_circle_area(num_sintd_nodes, sintd_coords);

#if 0
      // Get elem rotation
      bool left_turn;
      bool right_turn;
      rot_2D_3D_sph(num_sintd_nodes, sintd_coords, &left_turn, &right_turn);

      if (right_turn) {
            printf("SINTD RIGHT TURN! src:%d dst:%d left=%d right=%d area=%20.17f \n",src_elem->get_id(),dst_elem->get_id(),left_turn,right_turn,sintd_areas[i]);

            if ((src_elem->get_id()==421) && (dst_elem->get_id()==25)) {
              write_3D_poly_woid_to_vtk("concave_elem_421_25", num_sintd_nodes, sintd_coords);
            }
      }
#endif

      // Initialize to valid
      (*valid)[i]=1;

      // Invalidate masked destination elements
      if (dst_mask_field) {
         double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          (*valid)[i]=0;
          continue;
        }
      }
      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          (*valid)[i] = 0;
          continue;
        }
      }

      if(midmesh || res_map) {

        //// When we are making an XGrid if clipped piece is very
        //// small part of original cells then skip. 
#define XGRID_CLIP_TOL 1.0E-14

      // Calc ratio of intersected part to source area
      // (src_area == 0.0 is checked above)
      double src_ratio=sintd_areas[i]/src_area;

      // Calc ratio of intersected part to dst area
      // (dst_area == 0.0 is checked above)
      double dst_ratio=sintd_areas[i]/dst_areas[i];

      // if intersected part is only a small piece of the src and dst, then skip
      // (dst_areas[i] == 0.0 is checked above)
      if ((src_ratio < XGRID_CLIP_TOL) && (dst_ratio < XGRID_CLIP_TOL)) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
	continue;
      }

#undef XGRID_CLIP_TOL

	// Set side information
	int side1_ind=1; // default to 1
	if (src_side_field) {
	  double *side1_dbl=src_side_field->data(*src_elem);
	  side1_ind=(int)(*side1_dbl+0.5);
	} 
	int side2_ind=1; // default to 1
	if (dst_side_field) {
	  double *side2_dbl=dst_side_field->data(*dst_elem);
	  side2_ind=(int)(*side2_dbl+0.5);
	} 
        //printf("%d# BOB: END CC\n",Par::Rank());
	  
        compute_sintd_nodes_cells(sintd_areas[i],
				  num_sintd_nodes, sintd_coords, 2, 3, 
				  sintd_nodes, sintd_cells, zz,
#ifdef BOB_XGRID_DEBUG
                                  src_elem->get_id(),dst_elem->get_id(),
#endif
                                  side1_ind, side2_ind);


#if 0
	if ((src_elem->get_id() == 6642) && 
	    ((dst_elem->get_id() == 38262) || (dst_elem->get_id() == 38263))) {
	      
	      printf("%d# BOBXGC %d s_id=%d d_id=%d \n",Par::Rank(),sintd_cells->size(),src_elem->get_id(),dst_elem->get_id());
	      
	      write_3D_poly_to_vtk("xgc_sintdelem",dst_elem->get_id(),num_sintd_nodes, sintd_coords);
	      write_3D_poly_to_vtk("xgc_srcelem",src_elem->get_id(),num_src_nodes, src_coords);
	      write_3D_poly_to_vtk("xgc_dstelem",dst_elem->get_id(),num_dst_nodes, dst_coords);
	}
#endif


      }


      // append result to a multi-map index-ed by passive mesh element for merging optimization
      if(res_map){
        interp_map_iter it = res_map->find(src_elem);
        if(it != res_map->end()) {
          // check if this is a unique intersection
          interp_map_range range = res_map->equal_range(src_elem);
          for(interp_map_iter it = range.first; it != range.second; ++it){
            if(it->second->clip_elem == dst_elem)
              Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
          }
        }
        int sdim = 3;
        res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, sdim, src_coords, dst_coords,
          src_area, dst_areas[i], ((src_area == 0.)? 1.:sintd_areas[i]/src_area) ) ) );
      }
    }

    if(res_map) return;     // not intended for weight calculation

    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }


    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // return weight
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(int num_src_nodes, double *src_coords,
                                                          int num_dst_nodes, double *dst_coords,
                                                          int *valid, double *sintd_area, double *dst_area,
                                                          Mesh * midmesh,
                                                          std::vector<sintd_node *> * sintd_nodes,
                                                          std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {

// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES)
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Error checking of dst cell (e.g. is smashed) done above

    // calculate dst area
    *dst_area=great_circle_area(num_dst_nodes, dst_coords);

    // if destination area is 0.0, invalidate and go to next
    if (*dst_area==0.0) {
      *valid=0;
       *sintd_area=0.0;
      *dst_area=0.0;
      return;
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
        *valid=0;
        *sintd_area=0.0;
        *dst_area=0.0;
        return;
      }

      // calculate intersection area
      *sintd_area=great_circle_area(num_sintd_nodes, sintd_coords);

      // Mark this as valid
      *valid=1;

#if 0
	if (global_src_id==6488) {
	  printf("BOB: WGT CALC SINTD dst=%d src=%d area=%g\n",global_dst_id,global_src_id,*sintd_area);	
	  write_3D_poly_to_vtk("sintdelem",global_dst_id,num_sintd_nodes, sintd_coords);
	}
#endif
#if 0
      if(midmesh || res_map)
        compute_sintd_nodes_cells(sintd_areas[i],
          num_sintd_nodes, sintd_coords, 2, 2,
          sintd_nodes, sintd_cells, zz);

      // append result to a multi-map index-ed by passive mesh element for merging optimization
      if(res_map){
        interp_map_iter it = res_map->find(src_elem);
        if(it != res_map->end()) {
          // check if this is a unique intersection
          interp_map_range range = res_map->equal_range(src_elem);
          for(interp_map_iter it = range.first; it != range.second; ++it){
            if(it->second->clip_elem == dst_elem)
              Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
          }
        }
        int sdim = 2;
        res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, sdim, src_coords, dst_coords,
          src_area, dst_areas[i], ((src_area == 0.)? 1.:sintd_areas[i]/src_area) ) ) );
      }

    if(res_map) return;     // not intended for weight calculation
#endif

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph_src_pnts(int num_src_nodes, double *src_coords,
                                                  std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                                  double *src_elem_area,
                                                  std::vector<int> *valid_list,
                                                  std::vector<double> *sintd_area_list, std::vector<double> *dst_area_list,
                                                  Mesh * midmesh,
                                                  std::vector<sintd_node *> * sintd_nodes,
                                                  std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {


    // Error checking of src cell (e.g. is smashed quad) done above

    // calculate src area
    double src_area=great_circle_area(num_src_nodes, src_coords);

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid_list)[i]=0;
        (*sintd_area_list)[i]=0.0;
        (*dst_area_list)[i]=0.0;
      }
      return;
    }

    // Output src_elem_area
    *src_elem_area=src_area;

// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES)
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];


 /* XMRKX */
#ifdef BOB_XGRID_DEBUG
    double tot=0.0;
#endif

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Init everything to 0s
      (*valid_list)[i]=0;
      (*sintd_area_list)[i]=0.0;
      (*dst_area_list)[i]=0.0;


      // Invalidate masked destination elements
      if (dst_mask_field) {
        double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          // Init to 0's above
          continue;
        }
      }

      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          // Init to 0's above
          continue;
        }
      }

      // Get dst coords
      get_elem_coords_3D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);

      // if less than a triangle skip
      if (num_dst_nodes<3) {
        // Init to 0's above
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad3D(num_dst_nodes, dst_coords)) {
        // Init to 0's above
        continue;
      }

      // See if dst cell concave
      bool is_concave=false;
      if (num_src_nodes > 3) {
        bool left_turn=false;
        bool right_turn=false;

        rot_2D_3D_sph(num_dst_nodes, dst_coords, &left_turn, &right_turn);

        if (left_turn && right_turn) is_concave=true;
      }

#ifdef BOB_XGRID_DEBUG
	// BOB DEBUG
	global_dst_id=dst_elem->get_id();
#endif


      // If not concave, calculate intersection and intersection area for 1
      if (!is_concave) {
        int valid;
        double sintd_area;
        double dst_area;
        calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                           num_dst_nodes, dst_coords,
                                                           &valid, &sintd_area, &dst_area,
                                                           midmesh,
                                                           sintd_nodes,
                                                           sintd_cells, res_map, zz);

#ifdef BOB_XGRID_DEBUG
	if (valid && (global_src_id == 0)) {
	  tot += sintd_area;
          printf("BOB: WGT CALC dst=%d src=%d valid=%d darea=%g sintd_area=%g tot=%g t/s=%g\n",global_dst_id,global_src_id,valid,dst_area,sintd_area,tot,tot/src_area);	
	  write_3D_poly_to_vtk("dstelem",global_dst_id, num_dst_nodes, dst_coords);
	  write_3D_poly_to_vtk("srcelem",global_src_id, num_src_nodes, src_coords);
	}
#endif


        // Set output based on validity
        if (valid==1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i]=sintd_area;
          (*dst_area_list)[i]=dst_area;
         }
        // else {
        //  Init to 0's above
        //}

    } else { // If not concave, calculate intersection and intersection area for both and combine

        // Space for temporary buffers
        double td[3*4];
        int ti[4];
        int tri_ind[6];

        // This must be a quad if not complain and exit
        // IF NOT A QUAD, THEN THE ABOVE BUFFER SIZES MUST BE CHANGED!!!
        // TO EMPHASIZE THAT IT MUST BE QUAD 4 IS PASSED IN FOR THE SIZE BELOW.
        if (num_dst_nodes != 4) Throw() << " This isn't a quad, but it should be!";
        int ret=triangulate_poly<GEOM_SPH2D3D>(4, dst_coords, td,
                                               ti, tri_ind);
        // Error check
        // Check return code
        if (ret != ESMCI_TP_SUCCESS) {
          if (ret == ESMCI_TP_DEGENERATE_POLY) Throw() << " - can't triangulate a polygon with less than 3 sides";
          else if (ret == ESMCI_TP_CLOCKWISE_POLY) Throw() << " - clockwise polygons not supported in triangulation routine";
          else Throw() << " - unknown error in triangulation";
        }

        // Because this is a quad it will be in 2 pieces.
        double tri[9];

        // Tri 1
        tri[0]=dst_coords[3*tri_ind[0]];
        tri[1]=dst_coords[3*tri_ind[0]+1];
        tri[2]=dst_coords[3*tri_ind[0]+2];

        tri[3]=dst_coords[3*tri_ind[1]];
        tri[4]=dst_coords[3*tri_ind[1]+1];
        tri[5]=dst_coords[3*tri_ind[1]+2];

        tri[6]=dst_coords[3*tri_ind[2]];
        tri[7]=dst_coords[3*tri_ind[2]+1];
        tri[8]=dst_coords[3*tri_ind[2]+2];

        // printf("Concave id=%d\n",src_elem->get_id());
        // printf("tri 1=%d %d %d\n",tri_ind[0],tri_ind[1],tri_ind[2]);
        // printf("tri 1=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

        int valid1;
        double sintd_area1;
        double dst_area1;
        calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                           3, tri,
                                                           &valid1, &sintd_area1, &dst_area1,
                                                           midmesh,
                                                           sintd_nodes,
                                                           sintd_cells, res_map, zz);

        // Set output based on validity
        if (valid1 == 1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i]=sintd_area1;
          (*dst_area_list)[i]=dst_area1;
        }


        // Tri 2
        tri[0]=dst_coords[3*tri_ind[3]];
        tri[1]=dst_coords[3*tri_ind[3]+1];
        tri[2]=dst_coords[3*tri_ind[3]+2];

        tri[3]=dst_coords[3*tri_ind[4]];
        tri[4]=dst_coords[3*tri_ind[4]+1];
        tri[5]=dst_coords[3*tri_ind[4]+2];

        tri[6]=dst_coords[3*tri_ind[5]];
        tri[7]=dst_coords[3*tri_ind[5]+1];
        tri[8]=dst_coords[3*tri_ind[5]+2];

        int valid2;
        double sintd_area2;
        double dst_area2;
        calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                          3, tri,
                                                          &valid2, &sintd_area2, &dst_area2,
                                                          midmesh,
                                                          sintd_nodes,
                                                          sintd_cells, res_map, zz);


        // Set output based on validity
        if (valid2 == 1) {
          (*valid_list)[i]=1;
          (*sintd_area_list)[i] += sintd_area2;
          (*dst_area_list)[i]   += dst_area2;
        }
      }
    }


#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }


  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, 
                                           std::vector<double> *wgts, 
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           Mesh * midmesh, 
                                           std::vector<sintd_node *> * sintd_nodes, 
					std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz, MEField<> *src_side_field, MEField<> *dst_side_field) {

    // Use original version if midmesh exists
    // TODO: Fei fix this
    if(midmesh || res_map) {
      calc_1st_order_weights_2D_3D_sph_orig(src_elem, src_cfield, 
                                             dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                             src_elem_area,
                                             valid, wgts, 
                                             sintd_areas_out, dst_areas_out,
                                             midmesh, 
                                             sintd_nodes, 
					    sintd_cells, res_map, zz, src_side_field, dst_side_field);

      return;
    }


// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES)

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_3D];
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

 /* XMRKX */

    // Get src coords
    get_elem_coords_3D_ccw(src_elem, src_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_src_nodes, src_coords);

    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad3D(num_src_nodes, src_coords)) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // See if concave
    bool is_concave=false;
    if (num_src_nodes > 3) {
      bool left_turn=false;
      bool right_turn=false;

      rot_2D_3D_sph(num_src_nodes, src_coords, &left_turn, &right_turn);

      if (left_turn && right_turn) is_concave=true;
    }

#ifdef BOB_XGRID_DEBUG
    // BOB DEBUG
    global_src_id=src_elem->get_id();
#endif

    // If not concave then just call into the lower level
    if (!is_concave) {
      calc_1st_order_weights_2D_3D_sph_src_pnts(num_src_nodes, src_coords,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 src_elem_area,
                                                 valid,
                                                 sintd_areas_out, dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);
    } else { // else, break into two pieces...

      // Space for temporary buffers
      double td[3*4];
      int ti[4];
      int tri_ind[6];


      // This must be a quad if not complain and exit
      // IF NOT A QUAD, THEN THE ABOVE BUFFER SIZES MUST BE CHANGED!!!
      // TO EMPHASIZE THAT IT MUST BE QUAD 4 IS PASSED IN FOR THE SIZE BELOW.
      if (num_src_nodes != 4) Throw() << " This isn't a quad, but it should be!";
      int ret=triangulate_poly<GEOM_SPH2D3D>(4, src_coords, td,
                                            ti, tri_ind);
      // Error check
      // Check return code
      if (ret != ESMCI_TP_SUCCESS) {
        if (ret == ESMCI_TP_DEGENERATE_POLY) Throw() << " - can't triangulate a polygon with less than 3 sides";
        else if (ret == ESMCI_TP_CLOCKWISE_POLY) Throw() << " - clockwise polygons not supported in triangulation routine";
        else Throw() << " - unknown error in triangulation";
      }


      // Because this is a quad it will be in 2 pieces.
      double tri[9];

      // Tri 1
      tri[0]=src_coords[3*tri_ind[0]];
      tri[1]=src_coords[3*tri_ind[0]+1];
      tri[2]=src_coords[3*tri_ind[0]+2];

      tri[3]=src_coords[3*tri_ind[1]];
      tri[4]=src_coords[3*tri_ind[1]+1];
      tri[5]=src_coords[3*tri_ind[1]+2];

      tri[6]=src_coords[3*tri_ind[2]];
      tri[7]=src_coords[3*tri_ind[2]+1];
      tri[8]=src_coords[3*tri_ind[2]+2];


      // printf("Concave id=%d\n",src_elem->get_id());
      // printf("tri 1=%d %d %d\n",tri_ind[0],tri_ind[1],tri_ind[2]);
      // printf("tri 1=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

      calc_1st_order_weights_2D_3D_sph_src_pnts(3, tri,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 src_elem_area,
                                                 valid,
                                                 sintd_areas_out, dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);



      // Tri 2
      tri[0]=src_coords[3*tri_ind[3]];
      tri[1]=src_coords[3*tri_ind[3]+1];
      tri[2]=src_coords[3*tri_ind[3]+2];

      tri[3]=src_coords[3*tri_ind[4]];
      tri[4]=src_coords[3*tri_ind[4]+1];
      tri[5]=src_coords[3*tri_ind[4]+2];

      tri[6]=src_coords[3*tri_ind[5]];
      tri[7]=src_coords[3*tri_ind[5]+1];
      tri[8]=src_coords[3*tri_ind[5]+2];


      // printf("tri 2=%d %d %d\n",tri_ind[3],tri_ind[4],tri_ind[5]);
      // printf("tri 2=(%f %f) (%f %f) (%f %f)\n",tri[0],tri[1],tri[2],tri[3],tri[4],tri[5]);

      // Tmp variables to hold info from second triangle
      double src_elem_area2;

      // If need to expand arrays, expand
      if (dst_elems.size() > tmp_valid->size()) {
        tmp_valid->resize(dst_elems.size(),0);
        tmp_sintd_areas_out->resize(dst_elems.size(),0.0);
        tmp_dst_areas_out->resize(dst_elems.size(),0.0);
      }

      calc_1st_order_weights_2D_3D_sph_src_pnts(3, tri,
                                                 dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                                 &src_elem_area2,
                                                 tmp_valid,
                                                 tmp_sintd_areas_out, tmp_dst_areas_out,
                                                 midmesh,
                                                 sintd_nodes,
                                                 sintd_cells, res_map, zz);

      // Merge together src area
      *src_elem_area=*src_elem_area+src_elem_area2;

      //loop through merging valid, sintd area and dst area
      for (int i=0; i<dst_elems.size(); i++) {

        if ((*valid)[i]==1) {
          if ((*tmp_valid)[i]==1) {
            (*valid)[i]=1;
            (*sintd_areas_out)[i]=(*sintd_areas_out)[i]+(*tmp_sintd_areas_out)[i];
            // (*dst_areas_out)[i] already set
          } else {
            (*valid)[i]=1;
            // (*sintd_areas_out)[i] already set
            // (*dst_areas_out)[i] already set
          }
        } else {
          if ((*tmp_valid)[i]==1) {
            (*valid)[i]=1;
            (*sintd_areas_out)[i]=(*tmp_sintd_areas_out)[i];
            (*dst_areas_out)[i]=(*tmp_dst_areas_out)[i];
          } else {
            (*valid)[i]=0;
          }
        }

      }
    }

    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {

        // calc weight
        double weight=(*sintd_areas_out)[i]/(*dst_areas_out)[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }


  //////////////// END CALC 2D 3D WEIGHTS //////////////////


#if 0

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts,
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           Mesh *midmesh, std::vector<sintd_node *> * sintd_nodes,
                                           std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {


// Maximum size for a supported polygon
// Since the elements are of a small
// limited size. Fixed sized buffers seem
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES)

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_3D];
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

    // Get src coords
    get_elem_coords_3D_ccw(src_elem, src_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);
    //  get_elem_coords(src_elem, src_cfield, 3, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);


    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_src_nodes, src_coords);


    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad3D(num_src_nodes, src_coords)) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // calculate dst area
    double src_area=great_circle_area(num_src_nodes, src_coords);


    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }


    // Output src_elem_area
    *src_elem_area=src_area;

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Allocate something to hold areas
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Get dst coords
      get_elem_coords_3D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);


      // if no nodes then go to next
      if (num_dst_nodes<1) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);

      // if less than a triangle complain
      if (num_dst_nodes<3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad3D(num_dst_nodes, dst_coords)) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate dst area
     dst_areas[i]=great_circle_area(num_dst_nodes, dst_coords);

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
          ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
        printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
        for (int j=0; j<num_dst_nodes; j++) {
          printf(" [%f,%f,%f] ",dst_coords[3*j],dst_coords[3*j+1],dst_coords[3*j+2]);
        }
        printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate intersection area
      sintd_areas[i]=great_circle_area(num_sintd_nodes, sintd_coords);

#if 0
      // Get elem rotation
      bool left_turn;
      bool right_turn;
      rot_2D_3D_sph(num_sintd_nodes, sintd_coords, &left_turn, &right_turn);

      if (right_turn) {
            printf("SINTD RIGHT TURN! src:%d dst:%d left=%d right=%d area=%20.17f \n",src_elem->get_id(),dst_elem->get_id(),left_turn,right_turn,sintd_areas[i]);

            if ((src_elem->get_id()==421) && (dst_elem->get_id()==25)) {
              write_3D_poly_woid_to_vtk("concave_elem_421_25", num_sintd_nodes, sintd_coords);
            }
      }
#endif

      (*valid)[i]=1;

      // Invalidate masked destination elements
      if (dst_mask_field) {
        double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          (*valid)[i]=0;
          continue;
        }
      }
      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          (*valid)[i] = 0;
          continue;
        }
      }

      if(midmesh || res_map)
        compute_sintd_nodes_cells(sintd_areas[i],
          num_sintd_nodes, sintd_coords, 2, 3,
          sintd_nodes, sintd_cells, zz);

      // append result to a multi-map index-ed by passive mesh element for merging optimization
      if(res_map){
        interp_map_iter it = res_map->find(src_elem);
        if(it != res_map->end()) {
          // check if this is a unique intersection
          interp_map_range range = res_map->equal_range(src_elem);
          for(interp_map_iter it = range.first; it != range.second; ++it){
            if(it->second->clip_elem == dst_elem)
              Throw() << "Duplicate src/dst elem pair found in res_map" << std::endl;
          }
        }
        int sdim = 3;
        res_map->insert(std::make_pair(src_elem, new interp_res(dst_elem, num_sintd_nodes, num_src_nodes, num_dst_nodes, sdim, src_coords, dst_coords,
          src_area, dst_areas[i], ((src_area == 0.)? 1.:sintd_areas[i]/src_area) ) ) );
      }
    }

    if(res_map) return;     // not intended for weight calculation

    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }


    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // return weight
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }

#endif

#if 0



/*
             0-----------1-----------5
             |       1   |           |
             |   \       |   \   3   |
             |     \     |     \     |
             |  0        |   2       |
             3-----------2-----------6
             |           |
             |   \  5    |
             |     \     |
             |   4       |
             7-----------6
             |           |
             |   \  7    |
             |     \     |
             |  6        |
 7-----------4-----------5
 |           |           |
 |   \   9   |   \  11   |
 |     \     |     \     |
 |  8        |  10       |
 3-----------0-----------1

*/
// Assumes pnts contains 8 points and thus has a size of 8*3
// Assumes tris contains 12 triangles and thus has a size of 12*3*3
// Converts hexahdron point list to 12 triangles as shown above
void convert_hex(double *pnts, double *tris) {
#define COPYP2T(t,tt,tp,p,pp) (t+9*tt+3*tp)[0]=(p+3*pp)[0]; (t+9*tt+3*tp)[1]=(p+3*pp)[1]; (t+9*tt+3*tp)[2]=(p+3*pp)[2];

  COPYP2T(tris,0,0,pnts,0);
  COPYP2T(tris,0,1,pnts,3);
  COPYP2T(tris,0,2,pnts,2);

  COPYP2T(tris,1,0,pnts,0);
  COPYP2T(tris,1,1,pnts,2);
  COPYP2T(tris,1,2,pnts,1);

  COPYP2T(tris,2,0,pnts,1);
  COPYP2T(tris,2,1,pnts,2);
  COPYP2T(tris,2,2,pnts,6);

  COPYP2T(tris,3,0,pnts,1);
  COPYP2T(tris,3,1,pnts,6);
  COPYP2T(tris,3,2,pnts,5);

  COPYP2T(tris,4,0,pnts,3);
  COPYP2T(tris,4,1,pnts,7);
  COPYP2T(tris,4,2,pnts,6);

  COPYP2T(tris,5,0,pnts,3);
  COPYP2T(tris,5,1,pnts,6);
  COPYP2T(tris,5,2,pnts,2);

  COPYP2T(tris,6,0,pnts,7);
  COPYP2T(tris,6,1,pnts,4);
  COPYP2T(tris,6,2,pnts,5);

  COPYP2T(tris,7,0,pnts,7);
  COPYP2T(tris,7,1,pnts,5);
  COPYP2T(tris,7,2,pnts,6);

  COPYP2T(tris,8,0,pnts,7);
  COPYP2T(tris,8,1,pnts,3);
  COPYP2T(tris,8,2,pnts,0);

  COPYP2T(tris,9,0,pnts,7);
  COPYP2T(tris,9,1,pnts,0);
  COPYP2T(tris,9,2,pnts,4);

  COPYP2T(tris,10,0,pnts,4);
  COPYP2T(tris,10,1,pnts,0);
  COPYP2T(tris,10,2,pnts,1);

  COPYP2T(tris,11,0,pnts,4);
  COPYP2T(tris,11,1,pnts,1);
  COPYP2T(tris,11,2,pnts,5);


#undef COPYP2T
}

  // HELPER FUNCTION
  Phedra create_phedra_from_elem(const MeshObj *elem, MEField<> *cfield) {
#define  MAX_NUM_PHEDRA_NODES 40
#define  MAX_NUM_PHEDRA_COORDS_3D (3*MAX_NUM_PHEDRA_NODES)


    // Get coord info from element
    int num_nodes;
    double node_coords[MAX_NUM_PHEDRA_COORDS_3D];
    get_elem_coords(elem, cfield, 3, MAX_NUM_PHEDRA_NODES, &num_nodes, node_coords);

    // Convert to Phedra based on shape
    if (num_nodes==8) {
     double hex_tris[12*3*3];
     convert_hex(node_coords, hex_tris);

     return Phedra(12,hex_tris);
    } else {
      // Only handle Hexahedrons right now
      Throw() << " Right now 3D conservative only works with hexahedrons";
    }


#undef MAX_NUM_PHEDRA_NODES
#undef MAX_NUM_PHEDRA_COORDS_3D
  }

#endif

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_3D_3D_cart(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts,
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           Mesh *midmesh, std::vector<sintd_node *> * sintd_nodes,
                                           std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz) {



    Phedra src_phedra=create_phedra_from_elem(src_elem, src_cfield);

    // If less than a tetrahedra invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_phedra.is_degenerate()) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // calculate src volume
    double src_area=src_phedra.calc_volume();

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // Output src_elem_area
    *src_elem_area=src_area;

    // Allocate something to hold intersection and dst volumes
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing volumes of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Create Phedra
      Phedra dst_phedra=create_phedra_from_elem(dst_elem, dst_cfield);

      // If less than a tetrahedra invalidate and go to next
      if (dst_phedra.is_degenerate()) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate dst area
     dst_areas[i]=dst_phedra.calc_volume();

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
     }

#if 0
     if ((dst_elem->get_id()==2011) && (src_elem->get_id()==7246)) phedra_debug=true;
#endif

     // Intersect dst polyhedra with src polyhedra
     dst_phedra.intersect(src_phedra);

#if 0
     phedra_debug=false;
#endif

      // if intersected element isn't a complete polyhedra then go to next
     if (dst_phedra.is_degenerate()) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
     }

     // calculate intersection area
     sintd_areas[i]=dst_phedra.calc_volume();

     if (sintd_areas[i] <1.0E-20) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
     }


#if 0
      static double tot=0.0;

      if (dst_elem->get_id()==2011) {
        tot+=sintd_areas[i]/dst_areas[i];
        printf("s: %d dst_area=%f sintd_area=%f area2=%f w=%f tot=%f num_faces=%d \n",src_elem->get_id(), dst_areas[i],sintd_areas[i],sintd_area2,sintd_areas[i]/dst_areas[i],tot,dst_phedra.get_num_faces());
        char new_filename[1000];
        sprintf(new_filename,"sintd%dphedra",src_elem->get_id());
        dst_phedra.write_to_vtk(new_filename);
        if (src_elem->get_id()==7246) dst_phedra.write_faces_to_vtk("s7246_");
      }
#endif

      (*valid)[i]=1;

    }


    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }

    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // return weight
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

  }





#if 0

  // The following is some incomplete code to improve the weight calculation by making sure that
  // the sum of the intersection areas add up to the src area. The problem is you can't do this
  // if part of the source element is outside the destination mesh. Started writing code
  // to detect this. Finished code to tell which nodes were on boundary. Still need to write a method
  // to detect if a source element crosses this destination boundary. E.g. if any of the dst_elems
  // contain boundary nodes and if so if any of the edges formed from those boundary nodes cross the source elem.

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield,
                                           std:: vector<char> dst_node_on_bndry, double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas) {


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

    // calculate dst area
    double src_area=great_circle_area(num_src_nodes, src_coords);

    // if the src_area is 0 freak out
    if (src_area == 0.0) {
      Throw() << "Source Element has 0 area";
    }

    // Output src_elem_area
    *src_elem_area=src_area;

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for tmp polygon used in intersection routine
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];


    // Allocate something to hold areas
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Get dst coords
      get_elem_coords(dst_elem, dst_cfield, 3, MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);

      // if no nodes then go to next
      if (num_dst_nodes<1) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*areas)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);

      // if less than a triangle complain
      if (num_dst_nodes<3) {
        Throw() << "Source Element is degenerate";
      }

      // calculate dst area
     dst_areas[i]=great_circle_area(num_dst_nodes, dst_coords);

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       (*areas)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
          ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
        printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
        for (int j=0; j<num_dst_nodes; j++) {
          printf(" [%f,%f,%f] ",dst_coords[3*j],dst_coords[3*j+1],dst_coords[3*j+2]);
        }
        printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*areas)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // calculate intersection area
      sintd_areas[i]=great_circle_area(num_sintd_nodes, sintd_coords);

        (*valid)[i]=1;
    }

#if 0
    // See if any of the destination elements we intersected with
    // were on the boundary
    bool found_bndry_elem=false;
    for (int i=0; i<dst_elems.size(); i++){
      // If it's actually a valid intersection
      if ((*valid)[i]==1) {
        //       if (is_elem_on_bndry(dst_elems[i],dst_node_on_bndry)) {
          found_bndry_elem=true;
          //}
      }
    }

   if (found_bndry_elem) {
      // Normalize sintd areas
      double tot=0.0;
      for (int i=0; i<dst_elems.size(); i++) {
        tot+=sintd_areas[i];
      }

      // If not close to 0.0 then adjust, so pieces are the same
      // size of the original src element. This helps
      // to get rid of a bunch of little round off errors
      // resulting in them being different sizes.
      if (tot > 1.0E-15) {
        double factor=src_area/tot;
        for (int i=0; i<dst_elems.size(); i++) {
          sintd_areas[i]=factor*sintd_areas[i];
        }
      }
    }
#endif

    //// Check how close
    //    double tot2=0.0;
    //for (int i=0; i<dst_elems.size(); i++) {
    //  tot2+=sintd_areas[i];
    //}
    //    printf("%d orig diff=%e (%e) new diff=%e (%e) \n",src_elem->get_id(),std::abs(tot-src_area),std::abs(tot-src_area)/src_area,
    //   std::abs(tot2-src_area),std::abs(tot2-src_area)/src_area);



    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];

        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it.
        // if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

        if (weight > 1.0) weight = 1.0;

        // return weight
        (*wgts)[i]=weight;
      }
    }


    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // return weight
        (*areas)[i]=sintd_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }





  // If true at least one of this elements nodes lies on an open boundary
  bool is_elem_on_bndry(const MeshObj *elem, std:: vector<char> dst_node_on_bndry) {

    // Init return var
    bool is_on_bndry=false;

    // Get number of nodes in element
    const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

    // Get coords of element
    for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
      const MeshObj &node = *(elem->Relations[s].obj);
      if (dst_node_on_bndry[node.get_data_index()]) {
        is_on_bndry=true;
        printf(" %d is on boundary\n",node.get_id());
        break;
      }
    }

    return is_on_bndry;
  }



  // This is useful if we ever need to know if elements are on a boundary
  // Set flags if a node is on a bndry (this can be a proc boundary)
  // is_on_bndry is resized internally to be the correct size (i.e. the number of nodes in mesh)
  void fill_node_is_on_bndry_list(Mesh &mesh, std::vector<char> *is_on_bndry) {

    // Complain if we're not in 2D
    if (mesh.parametric_dim() != 2) Throw() << "Boundary bits only calculated for 2D meshes right now";

    // resize is_on_bndry
    is_on_bndry->resize(mesh.num_nodes(),0);

    // Node surrounding gids vector
    // Do here, so it isn't constantly being realloced
    std::vector<int> surround_gids;
    surround_gids.reserve(100);

    // Loop around nodes setting bndry flags
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      // clear out gids
      surround_gids.clear();

      // Loop around all elements around this element
        MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
        while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
          MeshObj &elem=*(el->obj);

          // Loop sides
          const MeshObjTopo *etopo = GetMeshObjTopo(elem);
          for (UInt s = 0; s < etopo->num_sides; s++) {
            const int *side_nodes = etopo->get_side_nodes(s);

            // We're just doing this for 2D right now, so number of side nodes should be 2
            if (etopo->num_side_nodes !=2) Throw()<<"Since this only works for 2D should only have 2 side nodes";
            // If one of the nodes matches the node being checked save the other
            MeshObj &node1 = *elem.Relations[side_nodes[0]].obj;
            MeshObj &node2 = *elem.Relations[side_nodes[1]].obj;

            // use & to make sure nodes are the same and just don't look the same
            if (&node1==&node) {
              surround_gids.push_back(node2.get_id());
            } else if (&node2==&node) {
              surround_gids.push_back(node1.get_id());
            }
          }
          ++el;
        }

        // Could check here for eveness

        // Sort surrounding gids
        std::sort(surround_gids.begin(),surround_gids.end());

        // Loop through and make sure that the pairs match
        // -1 on size is to make sure we don't access outside
        // memory range with +1
        bool on_bndry=false;
        for (int j=0; j<surround_gids.size()-1; j+=2){
          if (surround_gids[j] != surround_gids[j+1]) {
            on_bndry=true;
            break;
          }
        }

        // Set flag
        if (on_bndry) {
          (*is_on_bndry)[node.get_data_index()]=1;
        } else {
          (*is_on_bndry)[node.get_data_index()]=0;
        }
    }


  }

#endif

} // namespace
