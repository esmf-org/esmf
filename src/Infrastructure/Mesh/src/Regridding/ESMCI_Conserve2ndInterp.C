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
#include <Mesh/include/Regridding/ESMCI_Conserve2ndInterp.h>
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
#include <Mesh/include/Legacy/ESMCI_SM.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <algorithm>

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


  //////////////////// 2D 2D Cartesian ////////////////////////////////////
  /* XMRKX */

  void _calc_elem_centroid_2D_2D_cart(const MeshObj *elem, const MEField<>  *cfield, int sdim, double *cntr) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // init to 0.0
      for (int i=0; i<sdim; i++) {
        cntr[i]=0.0;
      }

      // Get coords of element
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
        const MeshObj &node = *(elem->Relations[s].obj);
        double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
          cntr[i] += c[i];
        }
      }

      // Compute average
      for (int i=0; i<sdim; i++) {
        cntr[i]=cntr[i]/((double)topo->num_nodes);
      }
  }

  // This is used in set creation so that things are sorted by id vs. pointer
  // sorting by pointer can result in different orders which can
  // result in different values on different runs (not bfb).
  static bool _are_gids_less_2D_2D_cart(const NBR_ELEM lhs, const NBR_ELEM rhs) {
    return (lhs.elem->get_id() < rhs.elem->get_id());
  }

  // Get the neighboring elements of a given element
  void _get_neighbor_elems_2D_2D_cart(const MeshObj *elem, const MEField<>  *cfield, const MEField<>  *mask_field,
                           std::vector<NBR_ELEM> *nbrs) {

    // Get neighboring elements
    MeshObjRelationList::const_iterator nl = MeshObjConn::find_relation(*elem, MeshObj::NODE);
    while (nl != elem->Relations.end() && nl->obj->get_type() == MeshObj::NODE){
      MeshObj &node=*(nl->obj);

      MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
      while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
        MeshObj *nbr_elem=el->obj;

        // Make sure that it's not the elem coming in
        if (nbr_elem->get_id()==elem->get_id()) {
          ++el;
          continue;
        }

        // If it's masked, then skip
        if (mask_field) {
          double *msk=mask_field->data(*nbr_elem);
          if (*msk>0.5) {
            ++el;
            continue;
          }
        }

        // Make temporary nbr elem struct.
        NBR_ELEM tmp_ne;
        tmp_ne.elem=nbr_elem;
        _calc_elem_centroid_2D_2D_cart(nbr_elem,cfield,2,tmp_ne.cntr);

        // See if it's in the list, if not add it
        std::vector<NBR_ELEM>::iterator lb =
          std::lower_bound(nbrs->begin(), nbrs->end(), tmp_ne, _are_gids_less_2D_2D_cart);

        // If it's already in the list then continue
        //if ((lb != nbrs->end()) && ((*lb).elem->get_id() == nbr_elem->get_id())) {
        if ((lb != nbrs->end()) && ((*lb).elem == nbr_elem)) {
          ++el;
          continue;
        }

        // Insert nbr_element
        nbrs->insert(lb, tmp_ne);

        // Move on to next element
        ++el;
      }
      ++nl;
    }
  }

  // Used to sort into counter clockwise order
  static bool _are_angles_less_2D_2D_cart(const NBR_ELEM lhs, const NBR_ELEM rhs) {
    return (lhs.angle < rhs.angle);
  }

  // Sort nbr elements to be counter clockwise
  void _make_nbr_elems_cntrclk_2D_2D_cart(double *src_cntr, std::vector<NBR_ELEM> *nbrs) {

    // If there are no neighbors return
    if (nbrs->empty()) return;

    // Make unit vector to src centroid
    double u_src_cntr[3];
    double len=MU_LEN_VEC2D(src_cntr);
    if (len == 0.0) Throw() << "Distance from center to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC2D(u_src_cntr,src_cntr,div_len);


    // Get cntr from nbr with max id to make things consistent
    // on different processors
    // Loop the rest of the elements
    UInt max_nbr_id=0; // Init to 0 to watch for nothing ever being selected
    double max_nbr_cntr[2];
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Get id
      int elem_id=nbr->elem->get_id();

      // Check if max id if so switch max id and coordinates
      if (elem_id > max_nbr_id) {
        double tmp_cntr[3];
        MU_ASSIGN_VEC2D(tmp_cntr,nbr->cntr);

        // If at the center, so would be a zero vector skip...
        if ((tmp_cntr[0]==src_cntr[0]) &&
            (tmp_cntr[1]==src_cntr[1])) continue;

        // Otherwise make this the new point
        max_nbr_id=elem_id;
        MU_ASSIGN_VEC2D(max_nbr_cntr,tmp_cntr);
      }
    }

     // If this is a  cell with everything at the center, then just use the center
    // this'll result in a degenerate cell which'll be handled later in the regridding with the flag.
    if (max_nbr_id==0) {
      MU_ASSIGN_VEC2D(max_nbr_cntr,src_cntr);
    }


    // Get vector to nbr chosen above
    double v1[2];
    MU_SUB_VEC2D(v1,max_nbr_cntr,src_cntr);

    // If this is a zero length vector complain
    if ((v1[0] == 0.0) &&
        (v1[1] == 0.0)) {
      Throw() << " Can't order points in conservative using a 0-vector";
    }

    // Loop over nbrs calculating angle
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Get vector to current nbr
      double vcurr[2];
      MU_SUB_VEC2D(vcurr,nbr->cntr,src_cntr);

      // Calculate angle
      double angle;
      nbr->angle=calc_angle<GEOM_CART2D>(v1, vcurr, u_src_cntr);
    }

     // Now sort by angle
    std::sort(nbrs->begin(), nbrs->end(), _are_angles_less_2D_2D_cart);
  }


  void _calc_centroid_from_sm_cells_2D_2D_cart(std::vector<SM_CELL> *sm_cells, double *cntr) {

    // Init
    cntr[0]=0.0;
    cntr[1]=0.0;

    // Loop summing coords and area
    double tot_area=0.0;
    for (int i=0; i<sm_cells->size(); i++) {
      SM_CELL *sm_cell=&((*sm_cells)[i]);

      cntr[0]+=sm_cell->cntr[0]*sm_cell->area;
      cntr[1]+=sm_cell->cntr[1]*sm_cell->area;

      tot_area += sm_cell->area;
    }

    // Divide by total area
    if (tot_area == 0.0) Throw() << "total area unexpectedly 0.0";
    double div_tot_area=1.0/tot_area;
    MU_MULT_BY_SCALAR_VEC2D(cntr,cntr,div_tot_area);
  }


  // Compute gradient across the source cell using Green's theorem
  // Assumes nbrs are in counter-clockwise order
  // returns true if successful
  bool _set_grad_info_using_greens_2D_2D_cart(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs, int se_id) {

    // Compute area of polygon
#define MAX_NUM_NBRS 100
    double nbr_coords[2*MAX_NUM_NBRS];

    // Error check
    if (nbrs->size()>MAX_NUM_NBRS) {
      Throw() << " A source cell contains more neighbors ("<<nbrs->size()<<") than is currently supported in 2nd order conservative weight calculation.";
    }

    // Load points into polygon array
    int p=0;
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      nbr_coords[p]  =nbr->cntr[0];
      nbr_coords[p+1]=nbr->cntr[1];
      p+=2;
    }

    // Compute area
    double nbr_poly_area=area_of_flat_2D_polygon(nbrs->size(), nbr_coords);
    if (nbr_poly_area == 0.0) return false;
    double div_nbr_poly_area=1.0/nbr_poly_area;

    // Set gradients to 0.0
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      MU_SET_TO_SCALAR_VEC2D(nbr->grad,0.0);
    }

    // Set src gradient term to 0.0
    MU_SET_TO_SCALAR_VEC2D(src_grad,0.0);

    // Get previous nbr index
    int prev_ind=nbrs->size()-1;

    // Loop through neighbors computing gradient
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);
      NBR_ELEM *prev_nbr=&((*nbrs)[prev_ind]);

      // Vector for edge from prev to curr center
      double edge_vec[2];
      MU_SUB_VEC2D(edge_vec,nbr->cntr,prev_nbr->cntr);

      // Compute outward normal
      double outward_norm[2];
      MU_ORTH_VEC2D(outward_norm,edge_vec);

      // Make a unit vector
      double len=MU_LEN_VEC2D(outward_norm);
      if (len == 0.0) Throw() << "Length of outward vector unexpectedly 0.0.";
      double div_len=1.0/len;
      MU_MULT_BY_SCALAR_VEC2D(outward_norm,outward_norm,div_len);

      // compute arc length
      // TODO: Should break this up to ensure vectors are 1.0;
      double edge_len=MU_LEN_VEC2D(edge_vec);
      MU_MULT_BY_SCALAR_VEC2D(outward_norm,outward_norm,edge_len);

      // Add to current nbr
      MU_ADD_VEC2D(nbr->grad,nbr->grad,outward_norm);

      // Add to prev nbr
      MU_ADD_VEC2D(prev_nbr->grad,prev_nbr->grad,outward_norm);

      // Add to source grad term
      MU_ADD_VEC2D(src_grad,src_grad,outward_norm);

      // switch to new previous nbr
      prev_ind=i;
    }

    // Further calc of gradient
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      // Divide by area
      MU_MULT_BY_SCALAR_VEC2D(nbr->grad,nbr->grad,div_nbr_poly_area);

      // Divide by 2.0
      MU_MULT_BY_SCALAR_VEC2D(nbr->grad,nbr->grad,0.5);

    }

    // Divide src_grad by area
    MU_MULT_BY_SCALAR_VEC2D(src_grad,src_grad,div_nbr_poly_area);

    // Return success
    return true;
  }

  // Set gradient info to indicate a 0 gradient across the source cell
  // This means that the value across the source cell will be treated as a constant
  void _set_grad_info_to_0_2D_2D_cart(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs) {

    // Set src gradient term to 0.0
    MU_SET_TO_SCALAR_VEC2D(src_grad,0.0);

    // Set gradients to 0.0
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      MU_SET_TO_SCALAR_VEC2D(nbr->grad,0.0);
    }
  }


#ifdef USE_NONCNCV_SM
  void _calc_centroid2D(int num_p, double *p, double *cntr) {

    // Init
    cntr[0]=0.0;
    cntr[1]=0.0;

    // Sum
    for (int i=0; i<num_p; i++) {
      double *pnt=p+2*i;

      cntr[0] += pnt[0];
      cntr[1] += pnt[1];
    }

    // Compute average
    MU_DIV_BY_SCALAR_VEC2D(cntr,cntr,((double)num_p));
  }


  void create_sm_cells_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield,
                                 std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field,
                                 double *src_elem_area,
                                 std::vector<int> *valid,
                                 std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                 std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                 std::vector<SM_CELL> *sm_cells
                                 ) {


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

    // Init output to invalid in case we return early
    *src_elem_area=0.0;
    for (int i=0; i<dst_elems.size(); i++) {
      (*valid)[i]=0;
      (*sintd_areas_out)[i]=0.0;
      (*dst_areas_out)[i]=0.0;
    }

    // if no nodes then exit
    if (num_src_nodes < 1) return;

    // Get rid of degenerate edges
    remove_0len_edges2D(&num_src_nodes, src_coords);

    // If less than a triangle leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes < 3) return;

    // If a smashed quad leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad2D(num_src_nodes, src_coords)) return;

    // calculate dst area
    double src_area=area_of_flat_2D_polygon(num_src_nodes, src_coords);

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) return;

    // Output src_elem_area
    *src_elem_area=src_area;

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      const MeshObj *dst_elem = dst_elems[i];

      // Skip masked dst elem
      if (dst_mask_field) {
        double *msk=dst_mask_field->data(*dst_elem);
        if (*msk>0.5) {
          continue;
        }
      }

      // Get dst coords
      get_elem_coords_2D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);

      // if no nodes then go to next
      if (num_dst_nodes < 1) continue;

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_dst_nodes, dst_coords);

      // if less than a triangle skip
      if (num_dst_nodes < 3) continue;

      // if a smashed quad skip
      if (is_smashed_quad2D(num_dst_nodes, dst_coords)) continue;

      // calculate dst area
      double dst_area=area_of_flat_2D_polygon(num_dst_nodes, dst_coords);

      // if destination area is 0.0, invalidate and go to next
      if (dst_area == 0.0) continue;

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
      if (num_sintd_nodes < 3) continue;

      // calculate intersection area
      double sintd_area=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords);

      // Valid, so set output variables
      (*valid)[i]=1;
      (*sintd_areas_out)[i]=sintd_area;
      (*dst_areas_out)[i]=dst_area;

      // Declare temporary supermesh cell info structure
      SM_CELL tmp_smc;

      // Add destination cell index
      tmp_smc.dst_index=i;

      // Add area to supermesh cell info
      tmp_smc.area=sintd_area;

      // Add centroid to supermesh cell info
      _calc_centroid2D(num_sintd_nodes, sintd_coords, tmp_smc.cntr);

      // Add to list
      sm_cells->push_back(tmp_smc);
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
   }
#endif

  double const_func_2D_2D_cart(double *coords) {
    return 1.0;
  }

  double xyz_func_2D_2D_cart(double *coords) {
    return coords[0]+coords[1];
  }

  void _debug_calc_gradient_2D_2D_cart(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs,
                            double (*f)(double *), double *grad) {

    // Init grad
    grad[0]=0.0;
    grad[1]=0.0;

    // loop adding nbr values
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Calc nbr value
      double src_nbr_val=f(nbr->cntr);

      // Add nbr term to gradient
      grad[0]=grad[0]+src_nbr_val*nbr->grad[0];
      grad[1]=grad[1]+src_nbr_val*nbr->grad[1];
    }

    // Calc source value
    double src_val=f(src_cntr);

    // subtract off src term
    grad[0]=grad[0]-src_val*src_grad[0];
    grad[1]=grad[1]-src_val*src_grad[1];
 }

 /* XMRKX */

  // Main Call
  void calc_2nd_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, MEField<> *src_mask_field,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid,
                                           std::vector<HC_WGHT> *wgts,
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           std::vector<SM_CELL> *sm_cells,
                                           std::vector<NBR_ELEM> *nbrs
                                           ) {


    // Create super mesh cells by intersecting src_elem and list of dst_elems
#ifndef USE_NONCNCV_SM
    create_SM_cells_2D_2D_cart(src_elem, src_cfield,
                              dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                              src_elem_area,
                              valid, sintd_areas_out, dst_areas_out,
                              tmp_valid, tmp_sintd_areas_out, tmp_dst_areas_out,
                              sm_cells);
#else
    create_sm_cells_2D_2D_cart(src_elem, src_cfield,
                              dst_elems, dst_cfield, dst_mask_field,
                              src_elem_area,
                              valid, sintd_areas_out, dst_areas_out,
                              tmp_valid, tmp_sintd_areas_out, tmp_dst_areas_out,
                              sm_cells);
#endif

    // If there are no sm cells then leave
    if (sm_cells->empty()) return;

    // Get list of source elements surrounding this one
    _get_neighbor_elems_2D_2D_cart(src_elem, src_cfield, src_mask_field, nbrs);

    // Compute src centroid
    double src_cntr[2];
    _calc_centroid_from_sm_cells_2D_2D_cart(sm_cells, src_cntr);

    // Put the nbrs into counter clockwise order
   _make_nbr_elems_cntrclk_2D_2D_cart(src_cntr, nbrs);

    // Set gradient info based on number of neighbors
   double src_grad[2];
   if (nbrs->size() < 3) {
     // Too few neighbors to use Green's, so assume constant grad
     _set_grad_info_to_0_2D_2D_cart(src_cntr, src_grad, nbrs);
   } else {
     // 3 or more neighbors so use Green's theorem
     if (!_set_grad_info_using_greens_2D_2D_cart(src_cntr, src_grad, nbrs, src_elem->get_id())) {
       // If the above doesn't suceed just default to constant
       _set_grad_info_to_0_2D_2D_cart(src_cntr, src_grad, nbrs);
     }
   }

#if 0
    // Check output
    printf("src_elem=%d nbrs= \n",src_elem->get_id());
    for (int i=0; i<nbrs.size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      printf("    %d [%g %g %g] len=%g\n",nbr->elem->get_id(),nbr->grad[0],nbr->grad[1],nbr->grad[2],MU_LEN_VEC3D(nbr->grad));
    }
    printf("\n");
#endif

    // Loop over supermesh cells calculating weights
    for (int i=0; i<sm_cells->size(); i++) {

      // get info for one supermesh cell
      SM_CELL *sm_cell=&((*sm_cells)[i]);

      // calc first part of weight
      double weight=sm_cell->area/(*dst_areas_out)[sm_cell->dst_index];

      // If weight is slightly bigger than one because of round off then push it back
      // if it's way over let it go, so we see it.
      if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

      // Subtract src centroid from subpart centroid
      double diff_cntr[2];
      MU_SUB_VEC2D(diff_cntr,sm_cell->cntr,src_cntr);

      // Calc ratio of areas
      double area_ratio=sm_cell->area/(*dst_areas_out)[sm_cell->dst_index];

      // Compute part due to source cell component of gradient
      double src_grad_wgt=MU_DOT_VEC2D(diff_cntr,src_grad)*area_ratio;

      // Compute total weight for this part
      weight = weight - src_grad_wgt;

      // Add weight to matrix
      HC_WGHT tmp_hcw;
      tmp_hcw.src_id=src_elem->get_id();
      tmp_hcw.dst_id=dst_elems[sm_cell->dst_index]->get_id();
      tmp_hcw.dst_index=sm_cell->dst_index;
      tmp_hcw.wgt=weight;
      wgts->push_back(tmp_hcw);

      // add parts due to neighbors
      for (int n=0; n<nbrs->size(); n++) {
        NBR_ELEM *nbr=&((*nbrs)[n]);

        double sintd_wgt=MU_DOT_VEC2D(diff_cntr,nbr->grad)*area_ratio;

        tmp_hcw.src_id=nbr->elem->get_id();
        tmp_hcw.dst_id=dst_elems[sm_cell->dst_index]->get_id();
        tmp_hcw.dst_index=sm_cell->dst_index;
        tmp_hcw.wgt=sintd_wgt;
        wgts->push_back(tmp_hcw);
      }
    }

#if 0
      // Debug output gradient if field is constant
      double const_grad[2]={0.0,0.0};
      _debug_calc_gradient_2D_2D_cart(src_cntr, src_grad, nbrs, const_func_2D_2D_cart, const_grad);
      printf("src_elem=%d constant field grad=%g %g\n",src_elem->get_id(),const_grad[0],const_grad[1]);

      // Debug output gradient if field is x+y+z
      double xyz_grad[2]={0.0,0.0};
      _debug_calc_gradient_2D_2D_cart(src_cntr, src_grad, nbrs, xyz_func_2D_2D_cart, xyz_grad);
      printf("src_elem=%d x+y field grad=%g %g\n",src_elem->get_id(),xyz_grad[0],xyz_grad[1]);
#endif
   }

  //////////////////// 2D 3D Spherical ////////////////////////////////////
  /* XMRKX */

  void _calc_elem_centroid_2D_3D_sph(const MeshObj *elem, const MEField<>  *cfield, int sdim, double *cntr) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // init to 0.0
      for (int i=0; i<sdim; i++) {
        cntr[i]=0.0;
      }

      // Get coords of element
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
        const MeshObj &node = *(elem->Relations[s].obj);
        double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
          cntr[i] += c[i];
        }
      }

      // Compute average
      for (int i=0; i<sdim; i++) {
        cntr[i]=cntr[i]/((double)topo->num_nodes);
      }

      // Project to sphere surface
      double len=MU_LEN_VEC3D(cntr);
      if (len == 0.0) Throw() << "Distance from center to point on sphere unexpectedly 0.0";
      for (int i=0; i<sdim; i++) {
        cntr[i]=cntr[i]/len;
      }
  }



  // This is used in set creation so that things are sorted by id vs. pointer
  // sorting by pointer can result in different orders which can
  // result in different values on different runs (not bfb).
  static bool _are_gids_less_2D_3D_sph(const NBR_ELEM lhs, const NBR_ELEM rhs) {
    return (lhs.elem->get_id() < rhs.elem->get_id());
  }

  // Get the neighboring elements of a given element
  void _get_neighbor_elems_2D_3D_sph(const MeshObj *elem, const MEField<>  *cfield, const MEField<>  *mask_field,
                           std::vector<NBR_ELEM> *nbrs) {

    // Get neighboring elements
    MeshObjRelationList::const_iterator nl = MeshObjConn::find_relation(*elem, MeshObj::NODE);
    while (nl != elem->Relations.end() && nl->obj->get_type() == MeshObj::NODE){
      MeshObj &node=*(nl->obj);

      MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
      while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
        MeshObj *nbr_elem=el->obj;

        // Make sure that it's not the elem coming in
        if (nbr_elem->get_id()==elem->get_id()) {
          ++el;
          continue;
        }

        // If it's masked, then skip
        if (mask_field) {
          double *msk=mask_field->data(*nbr_elem);
          if (*msk>0.5) {
            ++el;
            continue;
          }
        }

        // Make temporary nbr elem struct.
        NBR_ELEM tmp_ne;
        tmp_ne.elem=nbr_elem;
        _calc_elem_centroid_2D_3D_sph(nbr_elem,cfield,3,tmp_ne.cntr);


        // See if it's in the list, if not add it
        std::vector<NBR_ELEM>::iterator lb =
          std::lower_bound(nbrs->begin(), nbrs->end(), tmp_ne, _are_gids_less_2D_3D_sph);

        // If it's already in the list then continue
        //if ((lb != nbrs->end()) && ((*lb).elem->get_id() == nbr_elem->get_id())) {
        if ((lb != nbrs->end()) && ((*lb).elem == nbr_elem)) {
          ++el;
          continue;
        }

        // Insert nbr_element
        nbrs->insert(lb, tmp_ne);

        // Move on to next element
        ++el;
      }
      ++nl;
    }
  }


  // Used to sort into counter clockwise order
  static bool _are_angles_less(const NBR_ELEM lhs, const NBR_ELEM rhs) {
    return (lhs.angle < rhs.angle);
  }

  // Sort nbr elements to be counter clockwise
  void _make_nbr_elems_cntrclk(double *src_cntr, std::vector<NBR_ELEM> *nbrs) {

    // If there are no neighbors return
    if (nbrs->empty()) return;

    // Make unit vector to src centroid
    double u_src_cntr[3];
    double len=MU_LEN_VEC3D(src_cntr);
    if (len == 0.0) Throw() << "Distance from center to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC3D(u_src_cntr,src_cntr,div_len);


    // Get cntr from nbr with max id to make things consistent
    // on different processors
    // Loop the rest of the elements
    UInt max_nbr_id=0; // Init to 0 to watch for nothing ever being selected
    double max_nbr_cntr[3];
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Get id
      int elem_id=nbr->elem->get_id();

      // Check if max id if so switch max id and coordinates
      if (elem_id > max_nbr_id) {
        double tmp_cntr[3];
        MU_ASSIGN_VEC3D(tmp_cntr,nbr->cntr);

        // If at the center, so would be a zero vector skip...
        if ((tmp_cntr[0]==src_cntr[0]) &&
            (tmp_cntr[1]==src_cntr[1]) &&
            (tmp_cntr[2]==src_cntr[2])) continue;

        // Otherwise make this the new point
        max_nbr_id=elem_id;
        max_nbr_cntr[0]=tmp_cntr[0];
        max_nbr_cntr[1]=tmp_cntr[1];
        max_nbr_cntr[2]=tmp_cntr[2];
      }
    }

     // If this is a  cell with everything at the center, then just use the center
    // this'll result in a degenerate cell which'll be handled later in the regridding with the flag.
    if (max_nbr_id==0) {
      max_nbr_cntr[0]=src_cntr[0];
      max_nbr_cntr[1]=src_cntr[1];
      max_nbr_cntr[2]=src_cntr[2];
    }

    // Get vector to nbr chosen above
    double v1[3];
    MU_SUB_VEC3D(v1,max_nbr_cntr,src_cntr);

    // If this is a zero length vector complain
    if ((v1[0] == 0.0) &&
        (v1[1] == 0.0) &&
        (v1[2] == 0.0)) {
      Throw() << " Can't order points in conservative using a 0-vector";
    }

    // Loop over nbrs calculating angle
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Get vector to current nbr
      double vcurr[3];
      MU_SUB_VEC3D(vcurr,nbr->cntr,src_cntr);

      // Calculate angle
      double angle;
      nbr->angle=calc_angle<GEOM_SPH2D3D>(v1, vcurr, u_src_cntr);
    }

     // Now sort by angle
    std::sort(nbrs->begin(), nbrs->end(), _are_angles_less);
  }


  void _calc_centroid_from_sm_cells(std::vector<SM_CELL> *sm_cells, double *cntr) {

    // Init
    cntr[0]=0.0;
    cntr[1]=0.0;
    cntr[2]=0.0;

    // Loop summing
    for (int i=0; i<sm_cells->size(); i++) {
      SM_CELL *sm_cell=&((*sm_cells)[i]);

      cntr[0]+=sm_cell->cntr[0]*sm_cell->area;
      cntr[1]+=sm_cell->cntr[1]*sm_cell->area;
      cntr[2]+=sm_cell->cntr[2]*sm_cell->area;
    }

    // Project to sphere surface
    double len=MU_LEN_VEC3D(cntr);
    if (len == 0.0) Throw() << "Distance from center to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC3D(cntr,cntr,div_len);
  }


  // Compute gradient across the source cell using Green's theorem
  // Assumes nbrs are in counter-clockwise order
  // returns true if successful
  bool _set_grad_info_using_greens(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs) {

    // Compute area of polygon
#define MAX_NUM_NBRS 100
    double nbr_coords[3*MAX_NUM_NBRS];

    // Error check
    if (nbrs->size()>MAX_NUM_NBRS) {
      Throw() << " A source cell contains more neighbors ("<<nbrs->size()<<") than is currently supported in 2nd order conservative weight calculation.";
    }

    // Load points into polygon array
    int p=0;
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      nbr_coords[p]  =nbr->cntr[0];
      nbr_coords[p+1]=nbr->cntr[1];
      nbr_coords[p+2]=nbr->cntr[2];
      p+=3;
    }

    // Compute area
    double nbr_poly_area=great_circle_area(nbrs->size(), nbr_coords);
    if (nbr_poly_area == 0.0) return false;
    double div_nbr_poly_area=1.0/nbr_poly_area;

    // Set gradients to 0.0
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      MU_SET_TO_SCALAR_VEC3D(nbr->grad,0.0);
    }

    // Set src gradient term to 0.0
    MU_SET_TO_SCALAR_VEC3D(src_grad,0.0);

    // Get previous nbr index
    int prev_ind=nbrs->size()-1;

    // Loop through neighbors computing gradient
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);
      NBR_ELEM *prev_nbr=&((*nbrs)[prev_ind]);

      // Compute outward normal
      double outward_norm[3];
      MU_CROSS_PRODUCT_VEC3D(outward_norm,nbr->cntr,prev_nbr->cntr);

      // Make a unit vector
      double len=MU_LEN_VEC3D(outward_norm);
      if (len == 0.0) Throw() << "Length of outward vector unexpectedly 0.0.";
      double div_len=1.0/len;
      MU_MULT_BY_SCALAR_VEC3D(outward_norm,outward_norm,div_len);

      // compute arc length
      // TODO: Should break this up to ensure vectors are 1.0;
      double arc_len=acos(MU_DOT_VEC3D(nbr->cntr,prev_nbr->cntr));
      MU_MULT_BY_SCALAR_VEC3D(outward_norm,outward_norm,arc_len);

      // Add to current nbr
      MU_ADD_VEC3D(nbr->grad,nbr->grad,outward_norm);

      // Add to prev nbr
      MU_ADD_VEC3D(prev_nbr->grad,prev_nbr->grad,outward_norm);

      // Add to source grad term
      MU_ADD_VEC3D(src_grad,src_grad,outward_norm);

      // switch to new previous nbr
      prev_ind=i;
    }

    // Make unit vector to src centroid
    double u_src_cntr[3];
    double len=MU_LEN_VEC3D(src_cntr);
    if (len == 0.0) Throw() << "Length of vector to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC3D(u_src_cntr,src_cntr,div_len);

    // Make sure gradient is orthogonal to src_cntr
    // (because src_cntr is a constant we can use the distributive property to do each part of the sum
    //  separately)
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      // AXBXA gives a vector orthogonal to A
      double tmp[3];
      MU_CROSS_PRODUCT_VEC3D(tmp,u_src_cntr,nbr->grad);
      MU_CROSS_PRODUCT_VEC3D(nbr->grad,tmp,u_src_cntr);

      // Divide by area
      MU_MULT_BY_SCALAR_VEC3D(nbr->grad,nbr->grad,div_nbr_poly_area);

      // Divide by 2.0
      MU_MULT_BY_SCALAR_VEC3D(nbr->grad,nbr->grad,0.5);
    }

    // Make sure src_grad is orthogonal
    double tmp[3];
    MU_CROSS_PRODUCT_VEC3D(tmp,u_src_cntr,src_grad);
    MU_CROSS_PRODUCT_VEC3D(src_grad,tmp,u_src_cntr);

    // Divide by area
    MU_MULT_BY_SCALAR_VEC3D(src_grad,src_grad,div_nbr_poly_area);

    // Return success
    return true;
  }

#if 0
  // This doesn't seem to be a very good approximation. It's possible that
  // there's a bug, but I think that it's correct. It may just be that this way doesn't work
  // very well. In particular it fails if there isn't a change in one of the coordinates.
  // Go with constant grad for 1 and 2 for now. When time think if there's a better way to
  // approximate a gradient for a small number of neighbors

  // Only have 1 or 2 neighbors so approximate gradient by just taking differences and dividing by length
  void _set_grad_info_using_approx(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs) {

    // Set src gradient term to 0.0
    MU_SET_TO_SCALAR_VEC3D(src_grad,0.0);

    // Only works if we have at least one neighbor
    if (nbrs->size() == 0) {
      Throw() << "method requires at least one neighbor";
    }

    // divide by number of points to give average
    double div_num=1.0/(double)(nbrs->size());

    // Loop through neighbors computing gradient
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      // Number of nonzero components
      int num_nonzero=0;

      // Difference from here to center
      double diff[3];
      MU_SUB_VEC3D(diff,nbr->cntr,src_cntr);

      if (diff[0]!=0.0) {
        nbr->grad[0]=1.0/diff[0];
        num_nonzero++;
      } else nbr->grad[0]=0.0;

      if (diff[1]!=0.0) {
        nbr->grad[1]=1.0/diff[1];
        num_nonzero++;
      } else nbr->grad[1]=0.0;

      if (diff[2]!=0.0) {
        nbr->grad[2]=1.0/diff[2];
        num_nonzero++;
      } else nbr->grad[2]=0.0;

      // Divide by the number of nonzero components, so they are each only contributing
      // their part to the total
      if (num_nonzero > 0) {
        double div_num_nz=1.0/(double)(num_nonzero);
        MU_MULT_BY_SCALAR_VEC3D(nbr->grad,nbr->grad,div_num_nz);
      }

      // Add to source grad term
      MU_ADD_VEC3D(src_grad,src_grad,nbr->grad);
    }


    // Make unit vector to src centroid
    double u_src_cntr[3];
    double len=MU_LEN_VEC3D(src_cntr);
    if (len == 0.0) Throw() << "Length of vector to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC3D(u_src_cntr,src_cntr,div_len);

    // Make sure gradient is orthogonal to src_cntr
    // (because src_cntr is a constant we can use the distributive property to do each part of the sum
    //  separately)
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      // AXBXA gives a vector orthogonal to A
      double tmp[3];
      MU_CROSS_PRODUCT_VEC3D(tmp,u_src_cntr,nbr->grad);
      MU_CROSS_PRODUCT_VEC3D(nbr->grad,tmp,u_src_cntr);

      // Divide to give average (when all weights are summed)
      MU_MULT_BY_SCALAR_VEC3D(nbr->grad,nbr->grad,div_num);
    }

    // Make sure src_grad is orthogonal
    double tmp[3];
    MU_CROSS_PRODUCT_VEC3D(tmp,u_src_cntr,src_grad);
    MU_CROSS_PRODUCT_VEC3D(src_grad,tmp,u_src_cntr);

    // Divide to give average (when all weights are summed)
    MU_MULT_BY_SCALAR_VEC3D(src_grad,src_grad,div_num);
  }
#endif

  // Set gradient info to indicate a 0 gradient across the source cell
  // This means that the value across the source cell will be treated as a constant
  void _set_grad_info_to_0(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs) {

    // Set src gradient term to 0.0
    MU_SET_TO_SCALAR_VEC3D(src_grad,0.0);

    // Set gradients to 0.0
    for (int i=0; i<nbrs->size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      MU_SET_TO_SCALAR_VEC3D(nbr->grad,0.0);
    }
  }

 /* XMRKX */
#ifdef USE_NONCNCV_SM
  void _calc_centroid3D(int num_p, double *p, double *cntr) {

    // Init
    cntr[0]=0.0;
    cntr[1]=0.0;
    cntr[2]=0.0;

    // Sum
    for (int i=0; i<num_p; i++) {
      double *pnt=p+3*i;

      cntr[0] += pnt[0];
      cntr[1] += pnt[1];
      cntr[2] += pnt[2];
    }

    // Compute average
    MU_DIV_BY_SCALAR_VEC3D(cntr,cntr,((double)num_p));

    // Project to sphere surface
    double len=MU_LEN_VEC3D(cntr);
    if (len == 0.0) Throw() << "Distance from center to point on sphere unexpectedly 0.0";
    double div_len=1.0/len;
    MU_MULT_BY_SCALAR_VEC3D(cntr,cntr,div_len);
  }


  void create_sm_cells_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield,
                                 std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field,
                                 double *src_elem_area,
                                 std::vector<int> *valid,
                                 std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                 std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                 std::vector<SM_CELL> *sm_cells
                                 ) {

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

    // Init output to invalid in case we return early
    *src_elem_area=0.0;
    for (int i=0; i<dst_elems.size(); i++) {
      (*valid)[i]=0;
      (*sintd_areas_out)[i]=0.0;
      (*dst_areas_out)[i]=0.0;
    }

    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_src_nodes, src_coords);

    // If less than a triangle leave because it won't results in weights
     // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) return;

    // If a smashed quad leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad3D(num_src_nodes, src_coords)) return;

    // calculate src area
    double src_area=great_circle_area(num_src_nodes, src_coords);


    // If src area is 0.0 leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) return;

    // Output src_elem_area
    *src_elem_area=src_area;

    /// Loop over destination elements
    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
       const MeshObj *dst_elem = dst_elems[i];

       // Skip masked dst elem
       if (dst_mask_field) {
         double *msk=dst_mask_field->data(*dst_elem);
         if (*msk>0.5) {
           continue;
         }
       }

       // Get dst coords
       get_elem_coords_3D_ccw(dst_elem, dst_cfield, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);


      // if no nodes then go to next
      if (num_dst_nodes<1) continue;

      // Get rid of degenerate edges
      remove_0len_edges3D(&num_dst_nodes, dst_coords);

      // if less than a triangle skip
      if (num_dst_nodes<3) continue;

      // if a smashed quad skip
      if (is_smashed_quad3D(num_dst_nodes, dst_coords)) continue;

      // calculate dst area
     double dst_area=great_circle_area(num_dst_nodes, dst_coords);

     // if destination area is 0.0, skip
     if (dst_area==0.0)  continue;

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

      // if intersected element isn't a complete polygon then skip
      if (num_sintd_nodes < 3) continue;

      // calculate intersection area
      double sintd_area=great_circle_area(num_sintd_nodes, sintd_coords);

      // Valid, so set output variables
      (*valid)[i]=1;
      (*sintd_areas_out)[i]=sintd_area;
      (*dst_areas_out)[i]=dst_area;

      // Declare temporary supermesh cell info structure
      SM_CELL tmp_smc;

      // Add destination cell index
      tmp_smc.dst_index=i;

      // Add area to supermesh cell info
      tmp_smc.area=sintd_area;

      // Add centroid to supermesh cell info
      _calc_centroid3D(num_sintd_nodes, sintd_coords, tmp_smc.cntr);

      // Add to list
      sm_cells->push_back(tmp_smc);
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
   }
#endif

  double const_func(double *coords) {
    return 1.0;
  }

  double xyz_func(double *coords) {
    return coords[0]+coords[1]+coords[2];
  }

  void _debug_calc_gradient(double *src_cntr, double *src_grad, std::vector<NBR_ELEM> *nbrs,
                            double (*f)(double *), double *grad) {

    // Init grad
    grad[0]=0.0;
    grad[1]=0.0;
    grad[2]=0.0;

    // loop adding nbr values
    for (int n=0; n<nbrs->size(); n++) {
      NBR_ELEM *nbr=&((*nbrs)[n]);

      // Calc nbr value
      double src_nbr_val=f(nbr->cntr);

      // Add nbr term to gradient
      grad[0]=grad[0]+src_nbr_val*nbr->grad[0];
      grad[1]=grad[1]+src_nbr_val*nbr->grad[1];
      grad[2]=grad[2]+src_nbr_val*nbr->grad[2];
    }

    // Calc source value
    double src_val=f(src_cntr);

    // subtract off src term
    grad[0]=grad[0]-src_val*src_grad[0];
    grad[1]=grad[1]-src_val*src_grad[1];
    grad[2]=grad[2]-src_val*src_grad[2];
  }


 /* XMRKX */

  // Main Call
  void calc_2nd_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, MEField<> *src_mask_field,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid,
                                           std::vector<HC_WGHT> *wgts,
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           std::vector<SM_CELL> *sm_cells,
                                           std::vector<NBR_ELEM> *nbrs
                                        ) {

    // Create super mesh cells by intersecting src_elem and list of dst_elems
#ifndef USE_NONCNCV_SM
    create_SM_cells_2D_3D_sph(src_elem, src_cfield,
                              dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                              src_elem_area,
                              valid, sintd_areas_out, dst_areas_out,
                              tmp_valid, tmp_sintd_areas_out, tmp_dst_areas_out,
                              sm_cells);
#else
    create_sm_cells_2D_3D_sph(src_elem, src_cfield,
                              dst_elems, dst_cfield, dst_mask_field,
                              src_elem_area,
                              valid, sintd_areas_out, dst_areas_out,
                              tmp_valid, tmp_sintd_areas_out, tmp_dst_areas_out,
                              sm_cells);
#endif


    // If there are no sm cells then leave
    if (sm_cells->empty()) return;

    // Get list of source elements surrounding this one
    _get_neighbor_elems_2D_3D_sph(src_elem, src_cfield, src_mask_field, nbrs);

    // Compute src centroid
    double src_cntr[3];
    _calc_centroid_from_sm_cells(sm_cells, src_cntr);

    // Put the nbrs into counter clockwise order
    _make_nbr_elems_cntrclk(src_cntr, nbrs);

#if 0
    if (src_elem->get_id() == 137) {
      // Check output
      printf("%d# src_elem=%d is_local=%d active=%d nbrs= ",Par::Rank(),src_elem->get_id(),GetAttr(*src_elem).is_locally_owned(),GetAttr(*src_elem).GetContext().is_set(Attr::ACTIVE_ID));
      for (int i=0; i<nbrs->size(); i++) {
        NBR_ELEM *nbr=&((*nbrs)[i]);

        printf(" %d %g",nbr->elem->get_id(),nbr->angle);
      }
      printf("\n");

      printf("src_elem=%d sm_cells->size()=%d\n",src_elem->get_id(),sm_cells->size());

    }
#endif

    // Set gradient info based on number of neighbors
    double src_grad[3];
    if (nbrs->size() < 3) {
      // Too few neighbors to use Green's, so assume constant grad
      _set_grad_info_to_0(src_cntr, src_grad, nbrs);
    } else {
      // 3 or more neighbors so use Green's theorem
      if (!_set_grad_info_using_greens(src_cntr, src_grad, nbrs)) {
        // If the above doesn't suceed just default to constant
        _set_grad_info_to_0(src_cntr, src_grad, nbrs);
      }
    }

#if 0
    // Check output
    printf("src_elem=%d nbrs= \n",src_elem->get_id());
    for (int i=0; i<nbrs.size(); i++) {
      NBR_ELEM *nbr=&((*nbrs)[i]);

      printf("    %d [%g %g %g] len=%g\n",nbr->elem->get_id(),nbr->grad[0],nbr->grad[1],nbr->grad[2],MU_LEN_VEC3D(nbr->.grad));
    }
    printf("\n");
#endif

    // Loop over supermesh cells calculating weights
    for (int i=0; i<sm_cells->size(); i++) {

      // get info for one supermesh cell
      SM_CELL *sm_cell=&((*sm_cells)[i]);

      // calc first part of weight
      double weight=sm_cell->area/(*dst_areas_out)[sm_cell->dst_index];

      // If weight is slightly bigger than one because of round off then push it back
      // if it's way over let it go, so we see it.
      if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;

      // Subtract src centroid from subpart centroid
      double diff_cntr[3];
      MU_SUB_VEC3D(diff_cntr,sm_cell->cntr,src_cntr);

      // Calc ratio of areas
      double area_ratio=sm_cell->area/(*dst_areas_out)[sm_cell->dst_index];

      // Compute part due to source cell component of gradient
      double src_grad_wgt=MU_DOT_VEC3D(diff_cntr,src_grad)*area_ratio;

      // Compute total weight for this part
      weight = weight - src_grad_wgt;

      // Add weight to matrix
      HC_WGHT tmp_hcw;
      tmp_hcw.src_id=src_elem->get_id();
      tmp_hcw.dst_id=dst_elems[sm_cell->dst_index]->get_id();
      tmp_hcw.dst_index=sm_cell->dst_index;
      tmp_hcw.wgt=weight;
      wgts->push_back(tmp_hcw);

      // add parts due to neighbors
      for (int n=0; n<nbrs->size(); n++) {
        NBR_ELEM *nbr=&((*nbrs)[n]);

        double sintd_wgt=MU_DOT_VEC3D(diff_cntr,nbr->grad)*area_ratio;

        tmp_hcw.src_id=nbr->elem->get_id();
        tmp_hcw.dst_id=dst_elems[sm_cell->dst_index]->get_id();
        tmp_hcw.dst_index=sm_cell->dst_index;
        tmp_hcw.wgt=sintd_wgt;
        wgts->push_back(tmp_hcw);
      }
    }

#if 0
      // Debug output gradient if field is constant
      double const_grad[3]={0.0,0.0,0.0};
      _debug_calc_gradient(src_cntr, src_grad, nbrs, const_func, const_grad);
      printf("src_elem=%d constant field grad=%g %g %g\n",src_elem->get_id(),const_grad[0],const_grad[1],const_grad[2]);

      // Debug output gradient if field is x+y+z
      double xyz_grad[3]={0.0,0.0,0.0};
      _debug_calc_gradient(src_cntr, src_grad, nbrs, xyz_func, xyz_grad);
      printf("src_elem=%d x+y+z field grad=%g %g %g\n",src_elem->get_id(),xyz_grad[0],xyz_grad[1],xyz_grad[2]);
#endif
  }

 /* XMRKX */

} // namespace
