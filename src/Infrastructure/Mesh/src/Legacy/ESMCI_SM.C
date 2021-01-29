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
#include <Mesh/include/Legacy/ESMCI_SM.h>

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


  //////////////// BEGIN CALC 2D 2D CELLS ////////////////  

  void _calc_centroid_2D_2D_cart(int num_p, double *p, double *cntr) {

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


  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void create_SM_cells_2D_2D_cart_src_and_dst_pnts(int num_src_nodes, double *src_coords,  
                                                   int num_dst_nodes, double *dst_coords,  
                                                   int dst_index,
                                                   int *valid, double *sintd_area, double *dst_area, 
                                                   std::vector<SM_CELL> *sm_cells) {

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

      // Declare temporary supermesh cell info structure
      SM_CELL tmp_smc;

      // Add destination cell index
      tmp_smc.dst_index=dst_index;

      // Add area to supermesh cell info 
      tmp_smc.area=*sintd_area;
      
      // Add centroid to supermesh cell info 
      _calc_centroid_2D_2D_cart(num_sintd_nodes, sintd_coords, tmp_smc.cntr);

      // Add to list
      sm_cells->push_back(tmp_smc);


#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D    
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void create_SM_cells_2D_2D_cart_src_pnts(int num_src_nodes, double *src_coords,  
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid_list,
                                           std::vector<double> *sintd_area_list, std::vector<double> *dst_area_list, 
                                           std::vector<SM_CELL> *sm_cells) {


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
        create_SM_cells_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                    num_dst_nodes, dst_coords, i, 
                                                    &valid, &sintd_area, &dst_area, 
                                                    sm_cells);
      
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
        create_SM_cells_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                    3, tri, i, 
                                                    &valid1, &sintd_area1, &dst_area1,
                                                    sm_cells);

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
        create_SM_cells_2D_2D_cart_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                    3, tri, i, 
                                                    &valid2, &sintd_area2, &dst_area2,
                                                    sm_cells);
        
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
  void create_SM_cells_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                                  std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                  double *src_elem_area,
                                  std::vector<int> *valid, 
                                  std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                  std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out, 
                                  std::vector<SM_CELL> *sm_cells) {

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
      create_SM_cells_2D_2D_cart_src_pnts(num_src_nodes, src_coords,  
                                          dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                          src_elem_area,
                                          valid,  
                                          sintd_areas_out, dst_areas_out,
                                          sm_cells);

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

      create_SM_cells_2D_2D_cart_src_pnts(3, tri,  
                                          dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                          src_elem_area,
                                          valid,  
                                          sintd_areas_out, dst_areas_out, 
                                          sm_cells);

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

      create_SM_cells_2D_2D_cart_src_pnts(3, tri,  
                                          dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                          &src_elem_area2,
                                          tmp_valid, 
                                          tmp_sintd_areas_out, tmp_dst_areas_out, 
                                          sm_cells);
      
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

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D    
  }


  //////////////// END CALC 2D 2D WEIGHTS //////////////////



  //////////////// BEGIN CALC 2D 3D CELLS ////////////////
  
  void _calc_centroid_2D_3D_sph(int num_p, double *p, double *cntr) {

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

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void create_SM_cells_2D_3D_sph_src_and_dst_pnts(int num_src_nodes, double *src_coords,  
                                                  int num_dst_nodes, double *dst_coords, 
                                                  int dst_index,
                                                  int *valid, double *sintd_area, double *dst_area,
                                                   std::vector<SM_CELL> *sm_cells) {

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

      // Declare temporary supermesh cell info structure
      SM_CELL tmp_smc;

      // Add destination cell index
      tmp_smc.dst_index=dst_index;

      // Add area to supermesh cell info 
      tmp_smc.area=*sintd_area;
      
      // Add centroid to supermesh cell info 
      _calc_centroid_2D_3D_sph(num_sintd_nodes, sintd_coords, tmp_smc.cntr);

      // Add to list
      sm_cells->push_back(tmp_smc);

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D    
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void create_SM_cells_2D_3D_sph_src_pnts(int num_src_nodes, double *src_coords,  
                                          std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                          double *src_elem_area,
                                          std::vector<int> *valid_list,
                                          std::vector<double> *sintd_area_list, std::vector<double> *dst_area_list,
                                          std::vector<SM_CELL> *sm_cells) {

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

      // If not concave, calculate intersection and intersection area for 1
      if (!is_concave) {
        int valid;
        double sintd_area;
        double dst_area;
        create_SM_cells_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                   num_dst_nodes, dst_coords, i,  
                                                   &valid, &sintd_area, &dst_area,
                                                   sm_cells);
      
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
        create_SM_cells_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                   3, tri, i,  
                                                   &valid1, &sintd_area1, &dst_area1, 
                                                   sm_cells);

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
        create_SM_cells_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,  
                                                   3, tri, i, 
                                                   &valid2, &sintd_area2, &dst_area2,
                                                   sm_cells);
        
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
  void create_SM_cells_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                 std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                 double *src_elem_area,
                                 std::vector<int> *valid, 
                                 std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                 std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                 std::vector<SM_CELL> *sm_cells) {

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

    // If not concave then just call into the lower level
    if (!is_concave) {
      create_SM_cells_2D_3D_sph_src_pnts(num_src_nodes, src_coords,  
                                         dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                         src_elem_area,
                                         valid,  
                                         sintd_areas_out, dst_areas_out, 
                                         sm_cells);

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

      create_SM_cells_2D_3D_sph_src_pnts(3, tri,  
                                         dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                         src_elem_area,
                                         valid,  
                                         sintd_areas_out, dst_areas_out,
                                         sm_cells);


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


      // Tmp variables to hold info from second triangle
      double src_elem_area2;
   
      // If need to expand arrays, expand
      if (dst_elems.size() > tmp_valid->size()) {
        tmp_valid->resize(dst_elems.size(),0);
        tmp_sintd_areas_out->resize(dst_elems.size(),0.0);
        tmp_dst_areas_out->resize(dst_elems.size(),0.0);
      }

      create_SM_cells_2D_3D_sph_src_pnts(3, tri,  
                                         dst_elems, dst_cfield, dst_mask_field, dst_frac2_field,
                                         &src_elem_area2,
                                         tmp_valid, 
                                         tmp_sintd_areas_out, tmp_dst_areas_out, 
                                         sm_cells);

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

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D    
  }


  //////////////// END CALC 2D 3D WEIGHTS //////////////////


} // namespace
