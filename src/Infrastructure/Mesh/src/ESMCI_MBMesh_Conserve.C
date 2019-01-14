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
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search_EToE.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh_Rendez_Elem.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

static bool debug=false;


  //////////////// BEGIN CALC 2D 2D  WEIGHTS ////////////////
void MBMesh_calc_1st_order_weights_2D_2D_cart(MBMesh *srcmbmp, EntityHandle src_elem, MBMesh *dstmbmp,
                                                   std::vector<EntityHandle> dst_elems,
                                                   double *src_elem_area,
                                                   std::vector<int> *valid, std::vector<double> *wgts,
                                                   std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out) {



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
    MBMesh_get_elem_coords(srcmbmp, src_elem,  MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

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
      EntityHandle dstelem = dst_elems[i];


      // Get dst coords
      MBMesh_get_elem_coords(dstmbmp, dstelem,  MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);

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
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D
  }



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


  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void MBMesh_calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(int num_src_nodes, double *src_coords,
                                                                int num_dst_nodes, double *dst_coords,
                                                                int *valid, double *sintd_area, double *dst_area) {
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
        *valid=0;
        *sintd_area=0.0;
        *dst_area=0.0;
        return;
      }

      // calculate intersection area
      *sintd_area=great_circle_area(num_sintd_nodes, sintd_coords);

      // Mark this as valid
      *valid=1;

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D
  }



  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into
  // this call.
  void MBMesh_calc_1st_order_weights_2D_3D_sph_src_pnts(int num_src_nodes, double *src_coords,
                                                        MBMesh *dstmbmp, std::vector<EntityHandle> dst_elems,
                                                        double *src_elem_area,
                                                        std::vector<int> *valid_list,
                                                        std::vector<double> *sintd_area_list, std::vector<double> *dst_area_list) {

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

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      EntityHandle dst_elem = dst_elems[i];

      // Init everything to 0s
      (*valid_list)[i]=0;
      (*sintd_area_list)[i]=0.0;
      (*dst_area_list)[i]=0.0;


    // Invalidate masked destination elements
    if (dstmbmp->has_elem_mask) {
      // Get dst elem mask value
      int masked;
      int merr=dstmbmp->mesh->tag_get_data(dstmbmp->elem_mask_tag, &dst_elem, 1, &masked);
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }

      // If masked go on to next
      if (masked) {
        // Init to 0's above
        continue;
      }
    }


#if 0
      // Invalidate creeped out dst element
      if(dst_frac2_field){
        double *dst_frac2=dst_frac2_field->data(*dst_elem);
        if (*dst_frac2 == 0.0){
          // Init to 0's above
          continue;
        }
      }
#endif

      // Get dst coords
      MBMesh_get_elem_coords_3D_ccw(dstmbmp, dst_elem, MAX_NUM_POLY_NODES, tmp_coords, &num_dst_nodes, dst_coords);

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
        MBMesh_calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                                 num_dst_nodes, dst_coords,
                                                                 &valid, &sintd_area, &dst_area);

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
        MBMesh_calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                                 3, tri,
                                                                 &valid1, &sintd_area1, &dst_area1);

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
        MBMesh_calc_1st_order_weights_2D_3D_sph_src_and_dst_pnts(num_src_nodes, src_coords,
                                                                 3, tri,
                                                                 &valid2, &sintd_area2, &dst_area2);

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
  //////////////// BEGIN CALC 2D 3D  WEIGHTS ////////////////
void MBMesh_calc_1st_order_weights_2D_3D_sph(MBMesh *srcmbmp, EntityHandle src_elem, MBMesh *dstmbmp,
                                             std::vector<EntityHandle> dst_elems,
                                             double *src_elem_area,
                                             std::vector<int> *valid, std::vector<double> *wgts,
                                              std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                             std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out) {

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
    MBMesh_get_elem_coords_3D_ccw(srcmbmp, src_elem, MAX_NUM_POLY_NODES, tmp_coords, &num_src_nodes, src_coords);

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
     MBMesh_calc_1st_order_weights_2D_3D_sph_src_pnts(num_src_nodes, src_coords,
                                                      dstmbmp, dst_elems,
                                                      src_elem_area,
                                                      valid,
                                                      sintd_areas_out, dst_areas_out);
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

      MBMesh_calc_1st_order_weights_2D_3D_sph_src_pnts(3, tri,
                                                       dstmbmp, dst_elems,
                                                       src_elem_area,
                                                       valid,
                                                       sintd_areas_out, dst_areas_out);

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

      MBMesh_calc_1st_order_weights_2D_3D_sph_src_pnts(3, tri,
                                                       dstmbmp, dst_elems,
                                                       src_elem_area,
                                                       tmp_valid,
                                                       tmp_sintd_areas_out, tmp_dst_areas_out);

      // Merge together src area
      *src_elem_area=*src_elem_area+src_elem_area2;

      //loop through merging other things
      for (int i=0; i<dst_elems.size(); i++) {

        if (((*valid)[i]==1) || ((*tmp_valid)[i]==1)) (*valid)[i]=1;
        else (*valid)[i]=0;

        (*sintd_areas_out)[i]=(*sintd_areas_out)[i]+(*tmp_sintd_areas_out)[i];
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


void calc_conserve_mat_serial_2D_2D_cart(MBMesh *srcmbmp, MBMesh *dstmbmp, MBMesh_Search_EToE_Result_List &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");


  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;

  // MOAB error
   int merr;

  // Get MOAB Meshes
  Interface *src_moab_mesh=srcmbmp->mesh;
  Interface *dst_moab_mesh=dstmbmp->mesh;

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<double> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;
  std::vector<int> dst_gids;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  MBMesh_Search_EToE_Result_List::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    MBMesh_Search_EToE_Result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.dst_elems.size() > max_num_dst_elems) max_num_dst_elems=sr.dst_elems.size();
  }


  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  wgts.resize(max_num_dst_elems,0.0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);
  dst_gids.resize(max_num_dst_elems,0.0);

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
      MBMesh_Search_EToE_Result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.dst_elems.size() == 0) continue;

    // Get src elem gid
    int src_gid;
    merr=src_moab_mesh->tag_get_data(srcmbmp->gid_tag, &sr.src_elem, 1, &src_gid);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Get dst elem gids
    for (int i=0; i<sr.dst_elems.size(); i++) {
      merr=dst_moab_mesh->tag_get_data(dstmbmp->gid_tag, &(sr.dst_elems[i]), 1, &(dst_gids[i]));
       if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
    }


    // If this source element is masked then skip it
    if (srcmbmp->has_elem_mask) {
        // Get src elem mask value
        int masked;
        merr=src_moab_mesh->tag_get_data(srcmbmp->elem_mask_tag, &sr.src_elem, 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }

        // Skip if masked
        if (masked) {
          continue; // if this is masked, then go to next search result
        }
    }

#if 0
    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }
#endif


    // Declare src_elem_area
    double src_elem_area;

    // Calculate weights
    std::vector<sintd_node *> tmp_nodes;
    std::vector<sintd_cell *> tmp_cells;
    MBMesh_calc_1st_order_weights_2D_2D_cart(srcmbmp, sr.src_elem, dstmbmp, sr.dst_elems,
                                             &src_elem_area, &valid, &wgts, &areas, &dst_areas);



    // Invalidate masked destination elements
    if (dstmbmp->has_elem_mask) {
      for (int i=0; i<sr.dst_elems.size(); i++) {
        // Get dst elem mask value
        int masked;
        merr=dst_moab_mesh->tag_get_data(dstmbmp->elem_mask_tag, &(sr.dst_elems[i]), 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }

         // Invalidate masked elems
        if (masked) {
          valid[i]=0;
        }
      }
    }


#if 0
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }
#endif

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.dst_elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;


      // Temporary empty col with negatives so unset values
      // can be detected if they sneak through
      IWeights::Entry col_empty(-1, 0, -1.0, 0);

      // Insert fracs into src_frac
      {
         // Allocate column of empty entries
        std::vector<IWeights::Entry> col;
        col.resize(num_valid,col_empty);

        // Put weights into column
        int j=0;
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {
            col[j].id=dst_gids[i];
            col[j].value=areas[i]/src_elem_area;
            j++;
          }
        }

        // Set row info
        IWeights::Entry row(src_gid, 0, 0.0, 0);

        // Put weights into weight matrix
        src_frac.InsertRowMerge(row, col);
      }


      // Put weights into dst_frac and then add
      // Don't do this if there are no user areas
      if (use_dst_frac) {
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {

            // Get src_gid
            int src_gid;
             MBMesh_get_gid(srcmbmp, sr.src_elem, &src_gid);

            // Set col info
            IWeights::Entry col(src_gid, 0, wgts[i], 0);

            // Get dst_gid
            int dst_gid;
            MBMesh_get_gid(srcmbmp, sr.dst_elems[i], &dst_gid);

            // Set row info
            IWeights::Entry row(dst_gid, 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);
          }
        }
      }


      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
#if 0
      if (src_area_field) {
          const MeshObj &src_elem = *sr.elem;
          double *area=src_area_field->data(src_elem);
          src_user_area_adj=*area/src_elem_area;
      }
#endif

      // Put weights into row column and then add
      for (int i=0; i<sr.dst_elems.size(); i++) {
        if (valid[i]==1) {

          // Calculate dest user area adjustment
          double dst_user_area_adj=1.0;
#if 0
           if (dst_area_field) {
            const MeshObj &dst_elem = *(sr.dst_elems[i]);
            double *area=dst_area_field->data(dst_elem);
            if (*area==0.0) Throw() << "0.0 user area in destination grid";
            dst_user_area_adj=dst_areas[i]/(*area);
          }
#endif

          // Set col info
          IWeights::Entry col(src_gid, 0,
                              src_user_area_adj*dst_user_area_adj*wgts[i], 0);
          //                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(dst_gids[i], 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);
        }
      }
} // for searchresult

}



void calc_conserve_mat_serial_2D_3D_sph(MBMesh *srcmbmp, MBMesh *dstmbmp, MBMesh_Search_EToE_Result_List &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");


  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;

  // MOAB error
  int merr;

  // Get MOAB Meshes
  Interface *src_moab_mesh=srcmbmp->mesh;
  Interface *dst_moab_mesh=dstmbmp->mesh;

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<double> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;
  std::vector<int> dst_gids;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

   // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  MBMesh_Search_EToE_Result_List::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
     // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    MBMesh_Search_EToE_Result &sr = **sb;

    // If there are no associated dst elements then skip it
     if (sr.dst_elems.size() > max_num_dst_elems) max_num_dst_elems=sr.dst_elems.size();
  }


  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  wgts.resize(max_num_dst_elems,0.0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);
  dst_gids.resize(max_num_dst_elems,0.0);

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    MBMesh_Search_EToE_Result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.dst_elems.size() == 0) continue;

    // Get src elem gid
    int src_gid;
    merr=src_moab_mesh->tag_get_data(srcmbmp->gid_tag, &sr.src_elem, 1, &src_gid);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Get dst elem gids
    for (int i=0; i<sr.dst_elems.size(); i++) {
      merr=dst_moab_mesh->tag_get_data(dstmbmp->gid_tag, &(sr.dst_elems[i]), 1, &(dst_gids[i]));
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
    }


    // If this source element is masked then skip it
    if (srcmbmp->has_elem_mask) {
        // Get src elem mask value
        int masked;
        merr=src_moab_mesh->tag_get_data(srcmbmp->elem_mask_tag, &sr.src_elem, 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }

        // Skip if masked
        if (masked) {
          continue; // if this is masked, then go to next search result
        }
    }

#if 0
    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
       const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }
#endif


    // Declare src_elem_area
    double src_elem_area;

    // Calculate weights
    MBMesh_calc_1st_order_weights_2D_3D_sph(srcmbmp, sr.src_elem, dstmbmp, sr.dst_elems,
                                            &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                            &tmp_valid, &tmp_areas, &tmp_dst_areas);



    // Invalidate masked destination elements
    if (dstmbmp->has_elem_mask) {
      for (int i=0; i<sr.dst_elems.size(); i++) {
        // Get dst elem mask value
        int masked;
        merr=dst_moab_mesh->tag_get_data(dstmbmp->elem_mask_tag, &(sr.dst_elems[i]), 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
         }

        // Invalidate masked elems
        if (masked) {
          valid[i]=0;
        }
      }
    }


#if 0
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }
#endif

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.dst_elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
     if (num_valid < 1) continue;


      // Temporary empty col with negatives so unset values
      // can be detected if they sneak through
      IWeights::Entry col_empty(-1, 0, -1.0, 0);

      // Insert fracs into src_frac
      {
        // Allocate column of empty entries
         std::vector<IWeights::Entry> col;
        col.resize(num_valid,col_empty);

        // Put weights into column
        int j=0;
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {
            col[j].id=dst_gids[i];
            col[j].value=areas[i]/src_elem_area;
            j++;
          }
        }

        // Set row info
        IWeights::Entry row(src_gid, 0, 0.0, 0);

        // Put weights into weight matrix
        src_frac.InsertRowMerge(row, col);
      }


      // Put weights into dst_frac and then add
      // Don't do this if there are no user areas
      if (use_dst_frac) {
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {

             // Set col info
            IWeights::Entry col(src_gid, 0, wgts[i], 0);

            // Set row info
            IWeights::Entry row(dst_gids[i], 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);
          }
        }
      }


      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
#if 0
      if (src_area_field) {
          const MeshObj &src_elem = *sr.elem;
          double *area=src_area_field->data(src_elem);
          src_user_area_adj=*area/src_elem_area;
      }
#endif

      // Put weights into row column and then add
      for (int i=0; i<sr.dst_elems.size(); i++) {
        if (valid[i]==1) {

          // Calculate dest user area adjustment
          double dst_user_area_adj=1.0;
#if 0
          if (dst_area_field) {
            const MeshObj &dst_elem = *(sr.dst_elems[i]);
            double *area=dst_area_field->data(dst_elem);
            if (*area==0.0) Throw() << "0.0 user area in destination grid";
            dst_user_area_adj=dst_areas[i]/(*area);
          }
#endif

           // Set col info
          IWeights::Entry col(src_gid, 0,
                              src_user_area_adj*dst_user_area_adj*wgts[i], 0);
          //                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(dst_gids[i], 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);
        }
      }
} // for searchresult

}



void calc_conserve_mat(MBMesh *srcmbmp, MBMesh *dstmbmp, MBMesh_Search_EToE_Result_List &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // both meshes have to have the same dimensions
  if (srcmbmp->pdim != dstmbmp->pdim) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmbmp->sdim != dstmbmp->sdim) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

  // Get dimension, because they're the same can just get one
  int sdim=srcmbmp->sdim;
  int pdim=srcmbmp->pdim;

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_conserve_mat_serial_2D_2D_cart(srcmbmp, dstmbmp, sres, iw, src_frac, dst_frac);
    } else if (sdim==3) {
      calc_conserve_mat_serial_2D_3D_sph(srcmbmp, dstmbmp, sres, iw, src_frac, dst_frac);
    }
  } else if (pdim==3) {
    if (sdim==3) {
      // calc_conserve_mat_serial_3D_3D_cart(srcmesh, dstmesh, midmesh, sres, iw, src_frac, dst_frac, zz);
    } else {
      Throw() << "Meshes with parametric dim == 3, but spatial dim !=3 not supported for conservative regridding";
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 or 3 not supported for conservative regridding";
  }

}

// Copy fractions to mesh
void set_frac_in_mesh(MBMesh *mesh, IWeights &frac) {
#undef  ESMC_METHOD
#define ESMC_METHOD "set_frac()"

  // Error return codes
  int merr,localrc;

  // Get a range containing all elements
  Range range_elem;
  merr=mesh->mesh->get_entities_by_dimension(0,mesh->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }


  // Loop the elements in the mesh and set to 0
  std::map<int,EntityHandle> id_to_elem;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;

    // Init to 0.0
    double frac=0;
    merr=mesh->mesh->tag_set_data(mesh->elem_frac_tag, &elem, 1, &frac);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // Get gid
    int gid;
    MBMesh_get_gid(mesh, elem, &gid);

    // Get id to build map
    id_to_elem[gid]=elem;
  }


   // Go through weights calculating and setting frac
   WMat::WeightMap::iterator wi = frac.begin_row(), we = frac.end_row();
   for (; wi != we; ++wi) {
     const WMat::Entry &w = wi->first;
      std::vector<WMat::Entry> &wcol = wi->second;

     // total frac
     double tot=0.0;
     for (UInt j = 0; j < wcol.size(); ++j) {
       WMat::Entry &wc = wcol[j];
       tot += wc.value;
     } // for j

     // Get entity handle from gid
     std::map<int,EntityHandle>::iterator itoei =  id_to_elem.find(w.id);
     if (itoei == id_to_elem.end()) {
       Throw() << " Gid not found in map!";
     }

     // Get EntityHandle
     EntityHandle elem=itoei->second;

     // Set to total
     merr=mesh->mesh->tag_set_data(mesh->elem_frac_tag, &elem, 1, &tot);
     if (merr != MB_SUCCESS) {
       if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
     }
   } // for wi
}


 /* XMRKX */
void calc_cnsrv_regrid_wgts(MBMesh *srcmesh, MBMesh *dstmesh, IWeights &wts) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_cnsrc_regrid_wgts()"

  // Get Parallel Information
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception


  // Set meshes to use for regrid weight calculations
  MBMesh *srcmesh_regrid=srcmesh;
  MBMesh *dstmesh_regrid=dstmesh;


  // If parallel then generate rendezvous meshes...and use them instead
  MBMesh *srcmesh_rend=NULL;
  MBMesh *dstmesh_rend=NULL;
  if (petCount > 1) {

    // Create rendez meshes
    create_rendez_mbmesh_elem(srcmesh, dstmesh, &srcmesh_rend, &dstmesh_rend);

    // Use rendezvous meshes instead
    srcmesh_regrid=srcmesh_rend;
    dstmesh_regrid=dstmesh_rend;
  }


  // Do search
  MBMesh_Search_EToE_Result_List result;
  MBMesh_Search_EToE(srcmesh_regrid, ESMCI_UNMAPPEDACTION_IGNORE, dstmesh_regrid, ESMCI_UNMAPPEDACTION_IGNORE,
                 1.0E-8, result);


  IWeights src_frac;
  IWeights dst_frac;
  calc_conserve_mat(srcmesh_regrid, dstmesh_regrid, result, wts, src_frac, dst_frac);


  // If parallel then migrate weights and fracs
  // back to decompostion of original destination mesh
  if (petCount > 1) {
     wts.MigrateToElem(*dstmesh);
     dst_frac.MigrateToElem(*dstmesh);

     // Migrate and set fracs
     src_frac.MigrateToElem(*srcmesh);
  }

  // Copy dst fractions to mesh
  // TODO: If users areas are used, then use dst_frac instead
  set_frac_in_mesh(dstmesh, wts);

  // Copy src fractions to mesh
  set_frac_in_mesh(srcmesh, src_frac);

  // If parallel then get rid of rendezvous meshes.
  if (petCount > 1) {
    if (srcmesh_rend != NULL) delete srcmesh_rend;
    if (dstmesh_rend != NULL) delete dstmesh_rend;
  }
}

#endif // ESMF_MOAB
