// $Id: ESMCI_ConserveInterp.C,v 1.3 2010/09/17 03:13:32 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_ConserveInterp.h>
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

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_ConserveInterp.C,v 1.3 2010/09/17 03:13:32 oehmke Exp $";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

          
namespace ESMCI {


  //  bool debug=false;
  
  
  // Intersects between the line a and the seqment s
  // returns true if we should add point p.
  // sin is the end of the segment inside the polygon
  // sout is the end of the segment outside the polygon
  // This subroutine is set up to be used with the poly intersect 
  // code below, and has a number of tweaks for special cases
  // which might make it odd to be used as a general intesect code.
  bool line_with_seg2D(double *a1, double *a2, double *sin, double *sout,
		       double *p) {
    
    // Calculate thing to divide both line equations by
    double ttdb= 
      a1[0]*(sout[1] - sin[1]) +
      a2[0]*(sin[1] - sout[1]) +
      sin[0]*(a1[1] - a2[1]) + 
      sout[0]*(a2[1] - a1[1]);
    
    
    // if ttdb is 0.0 then the lines are parallel, this
    // shouldn't happen, but if it does it makes the
    // most sense to add the out point
    if (ttdb == 0.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }
    
    // Calculate t
    double t=
      -(a1[0]*(sin[1]-a2[1]) +
	a2[0]*(a1[1]-sin[1]) +
	sin[0]*(a2[1]-a1[1]))/ttdb;
    
    // We shouldn't be off the ends, but
    // if we are because of rounding then
    // do what makes sense
    
    // if we're off the in end, then don't add
    if (t<0.0) return false;
    
    // if we're off the out end, then just add the
    // out point
    if (t>=1.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }
    
    // Otherwise calculate point of intersection
    // and add that
    p[0]=sin[0] + t*(sout[0]-sin[0]);
    p[1]=sin[1] + t*(sout[1]-sin[1]);
    return true;
    
  }
  
  // intersects 2D convex polygons whose vertices are stored in counter clockwise
  // order.
  // p should be of size 2*num_p
  // q should be of size 2*num_q
  // tmp and out should both be allocated to be at least of size 2*(num_p+num_q)
  void intersect_convex_poly2D(int num_p, double *p,
			       int num_q, double *q,
			       double *tmp,
			       int *num_out, double *out) 
  {
    
#define CLIP_EQUAL_TOL 1.0e-15
    
    // If p is empty then leave
    if (num_p==0) {
      *num_out=0;
    }
    
    // If q is empty then leave
    if (num_q==0) {
      *num_out=0;
    }
    
    // INSTEAD OF TMP USE T???
    
    // Copy q into tmp
    double *end_q=q+2*num_q;
    for (double *q_i=q, *tmp_i=tmp; q_i<end_q; q_i++, tmp_i++) {
      *tmp_i=*q_i;
    }
    int num_tmp=num_q;
    
    // Setup alias for output array
    int num_o=0;
    double *o=out;
    
    // Loop through p
    for (int ip=0; ip<num_p; ip++) {
      // Get points of current edge of p
      double *p1=p+2*ip;
      double *p2=p+2*((ip+1)%num_p);
      
    // calc p_vec (vector along the current edge of p)
      double p_vec[2];
      p_vec[0]=p2[0]-p1[0];
      p_vec[1]=p2[1]-p1[1];
      
      // Set initial t1
      double *t1=tmp+2*(num_tmp-1);
      double inout1=p_vec[0]*(t1[1]-p1[1]) - p_vec[1]*(t1[0]-p1[0]);
      
      // Make sure we don't have a degenerate polygon after clipping
      bool in_but_not_on_p_vec=false;
      
      // Init output poly
      num_o=0;
      
      // Loop through other polygon
      for (int it=0; it<num_tmp; it++) {
	double *t2=tmp+2*it;
	
	// calculate variable which says if t2 is in or out of polygon p
	// (the cross product of p_vec with the vector from p1 to t2
	double inout2=p_vec[0]*(t2[1]-p1[1]) - p_vec[1]*(t2[0]-p1[0]);
	
	// process point
	if (inout2 > CLIP_EQUAL_TOL) { // t2 inside 
	  if (inout1 < 0.0) { //  t1 outside
	    double intersect_pnt[2];        
	    
	    // Do intersection and add that point
	    if (line_with_seg2D(p1, p2, t2, t1, intersect_pnt)) {
	      o[2*num_o]=intersect_pnt[0];
	      o[2*num_o+1]=intersect_pnt[1];
	      num_o++;
	    }
	  }
	  
	  // Add t2 point because it's inside
	  o[2*num_o]=t2[0];
	  o[2*num_o+1]=t2[1];
	  num_o++;
	  
	  // record the fact that a point isn't on p_vec, but is in
	  in_but_not_on_p_vec=true;
	  
	} else if (inout2 < 0.0) { // t2 outside
	  
	  if (inout1 > CLIP_EQUAL_TOL) {  //  t1 inside (this excludes the EQUAL region, because 
	    double intersect_pnt[2];      //             if a point was added in there we don't
	    //             want to add another one right next to it)
	    
	    // Do intersection and add that point
	    if (line_with_seg2D(p1, p2, t1, t2, intersect_pnt)) {
	      o[2*num_o]=intersect_pnt[0];
	      o[2*num_o+1]=intersect_pnt[1];
	      num_o++;
	    }
	  }
	  
	} else {  // t2 on edge
	  // Just add point because it's on the edge
	  o[2*num_o]=t2[0];
	  o[2*num_o+1]=t2[1];
	  num_o++;
	}
	
	// old t2 becomes the new t1
	t1=t2;
	inout1=inout2;  
      }
      
      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;
      
      // if poly is empty then leave
      if (num_o==0) break;

      // if not on the last cycle then copy out poly back to tmp
      if (ip != num_p-1) {
	double *end_o=o+2*num_o;
	for (double *o_i=o, *tmp_i=tmp; o_i<end_o; o_i++, tmp_i++) {
	  *tmp_i=*o_i;
	}
	num_tmp=num_o;
      }
    }
    
  // Do output
    *num_out=num_o;
    // o is an alias for out so don't need to copy
    
#undef CLIP_EQUAL_TOL
  }
  
  
  
  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void calc_1st_order_weights_2D_2D_cart(const MeshObj *dst_elem, MEField<> *dst_cfield, 
                             std::vector<const MeshObj *> src_elems, MEField<> *src_cfield, 
			      std::vector<int> *valid, std::vector<double> *wgts) {


// Maximum size for a supported polygon
// Since the elements are of a small 
// limited size. Fixed sized buffers seem 
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES) 


    // Declaration for src polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Get dst coords
    get_elem_coords(dst_elem, dst_cfield, 2, MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);

    // if no nodes then exit
    if (num_dst_nodes<1) return;

    // calculate dst area
    double dst_area=area_of_flat_2D_polygon(num_dst_nodes, dst_coords); 

    // if the dst_area is 0 freak out
    if (dst_area == 0.0) {
      Throw() << "Destination Element has 0 area";
    }
    
#if 0
    printf("coords  "); 
    for (int i=0; i<dst_coords.size(); i++) {
      if (i%2==0) printf("[");
      printf("  %f ",dst_coords[i]);
      if (i%2==1) printf("]");
    }
    printf("\n");
#endif


    // Declaration for dst polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for tmp polygon used in intersection routine
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];

    // Loop intersecting and calculating weights
    for (int i=0; i<src_elems.size(); i++) {
      const MeshObj *src_elem = src_elems[i];
      
      // Get src coords
      get_elem_coords(src_elem, src_cfield, 2, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

      // if no nodes then go to next
      if (num_src_nodes<1) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
	continue;
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
#if 0
      printf("sintd: "); 
      for (int i=0; i<num_sintd_nodes; i++) {
	printf(" [%f,%f] ",dst_coords[2*i],dst_coords[2*i+1]);
      }
      printf("\n");
#endif

      // if no sintd_nodes then go to next
      if (num_sintd_nodes < 1) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
	continue;
      }

      // calculate intersection area
      double sintd_area=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords); 

      // calc weight
      (*valid)[i]=1;
      (*wgts)[i]=sintd_area/dst_area;
    }
    

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D

  }




///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Calculate weights for a 2D polygon on a Sphere //////////////////////////
/////////////////////////////////////     (Assumes polygon has great circle sides)   //////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
                           



  // Intersects between the line a and the seqment s
  // where both line and segment are great circle lines on the sphere represented by
  // 3D cartesian points.
  // Subroutine eturns true if we should add point p.
  // sin is the end of the segment inside the polygon
  // sout is the end of the segment outside the polygon
  // This subroutine is set up to be used with the poly intersect 
  // code below, and has a number of tweaks for special cases
  // which might make it odd to be used as a general intesect code.
  bool line_with_gc_seg3D(double *a1, double *a2, double *sin, double *sout,
		       double *p) {
    
    // Do this intersection by reprsenting the line a1 to a2 as a plane through the
    // two line points and the origin of the sphere (0,0,0). This is the 
    // definition of a great circle arc. 
    double plane[9];
    double plane_p[2];
    double t;

    // Load points defining plane into variable (these are supposed to be in counterclockwise order)
    plane[0]=a1[0];
    plane[1]=a1[1];
    plane[2]=a1[2];
    plane[3]=a2[0];
    plane[4]=a2[1];
    plane[5]=a2[2];
    plane[6]=0.0;
    plane[7]=0.0;
    plane[8]=0.0;
    
    
    // Intersect the segment with the plane
    if(!intersect_tri_with_line(plane, sin, sout, plane_p, &t)) {
      // If can't intesect then the lines and plane are parallel, this
      // shouldn't happen, but if it does it makes the
      // most sense to add the out point
      p[0]=sout[0];
      p[1]=sout[1];
      p[2]=sout[2];
       return true;
    }
    
    

    //    if (debug) printf("     t=%f \n",t);
    
    // We shouldn't be off the ends, but
    // if we are because of rounding then
    // do what makes sense
    
    // if we're off the in end, then don't add
    if (t<0.0) return false;
    
    // if we're off the out end, then just add the
    // out point
    if (t>=1.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      p[2]=sout[2];
      return true;
    }
    
    // Otherwise calculate point of intersection
    // and add that
    p[0]=sin[0] + t*(sout[0]-sin[0]);
    p[1]=sin[1] + t*(sout[1]-sin[1]);
    p[2]=sin[2] + t*(sout[2]-sin[2]);
    return true;
    
  }



 
  // intersects convex polygons whose vertices are stored in counter clockwise
  // order and which lie on a sphere. Note that the polygons have 3D coordinates
  // but are only 2D in the parameter space of the sphere. This subroutine
  // assumes that the polygons have great circle edges
  // p should be of size 3*num_p
  // q should be of size 3*num_q
  // tmp and out should both be allocated to be at least of size 3*(num_p+num_q)
  void intersect_convex_2D_3D_sph_gc_poly(int num_p, double *p,
			       int num_q, double *q,
			       double *tmp,
			       int *num_out, double *out) 
  {

#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define NORM(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])
#define CLIP_EQUAL_TOL 1.0e-15
#define CLOSE_TO_ZERO(vec) ((std::abs(vec[0])<CLIP_EQUAL_TOL) && (std::abs(vec[1])<CLIP_EQUAL_TOL) && (std::abs(vec[2])<CLIP_EQUAL_TOL))
    
    // If p is empty then leave
    if (num_p==0) {
      *num_out=0;
    }
    
    // If q is empty then leave
    if (num_q==0) {
      *num_out=0;
    }
    
    // INSTEAD OF TMP USE T???
    
    // Copy q into tmp
    double *end_q=q+3*num_q;
    for (double *q_i=q, *tmp_i=tmp; q_i<end_q; q_i++, tmp_i++) {
      *tmp_i=*q_i;
    }
    int num_tmp=num_q;
    
    // Setup alias for output array
    int num_o=0;
    double *o=out;
    
    // Loop through p
   for (int ip=0; ip<num_p; ip++) {

      // Get points of current edge of p
      double *p1=p+3*ip;
      double *p2=p+3*((ip+1)%num_p);

      //      if (debug) printf("ip=%d p1=[%f %f %f] p2=[%f %f %f] -------------------- \n ",ip,p1[0],p1[1],p1[2],p2[0],p2[1],p2[2]);
      
      // calc p_vec (vector along the current edge of p)
      double p_vec[3];
      p_vec[0]=p2[0]-p1[0]; p_vec[1]=p2[1]-p1[1]; p_vec[2]=p2[2]-p1[2];
      
      // Set initial t1 (last point in tmp polygon)
      double *t1=tmp+3*(num_tmp-1);

      // Vector from p to t1     
      double pt1_vec[3];
      pt1_vec[0]=t1[0]-p1[0]; pt1_vec[1]=t1[1]-p1[1]; pt1_vec[2]=t1[2]-p1[2];


      // Make sure that we're not dealing with a zero length vector
      bool inout1_same=false;
      double inout1;
      if (CLOSE_TO_ZERO(pt1_vec)) {
         inout1_same=true;     
         inout1=1000.0; // Set to a value so we know where to catch in point processing ifs
      } else {
	// Normal vector which is the length of |p_vec||pt_vec|*the sin between them
	double n_vec[3];
	CROSS_PRODUCT3D(n_vec,p_vec,pt1_vec);
	
	// Get magnitude which is distance out * |p_vec| without sign to indicate direction
	inout1=NORM(n_vec);
	
	// Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0)) 
	// This gives angle with respect to surface of the sphere and hence allows us to assign
	// a direction (i.e. a sign) to inout1
	if (DOT_PRODUCT3D(n_vec,p1)<0.0) inout1=-inout1; 
      }
      
      // Make sure we don't have a degenerate polygon after clipping
      bool in_but_not_on_p_vec=false;
      
      // Init output poly
      num_o=0;
      
      // Loop through other polygon
      for (int it=0; it<num_tmp; it++) {
	double *t2=tmp+3*it;
	
	// calculate variable which says if t2 is in or out of polygon p
	// (the cross product of p_vec with the vector from p1 to t2
	//// Vector from p to t2    
	double pt2_vec[3];
	pt2_vec[0]=t2[0]-p1[0]; pt2_vec[1]=t2[1]-p1[1]; pt2_vec[2]=t2[2]-p1[2];
	
	// Make sure that we're not dealing with a zero length vector
	bool inout2_same=false;
	double inout2;
	if (CLOSE_TO_ZERO(pt2_vec)) {
	  inout2_same=true;     
          inout2=1000.0; // Set to a value so we know where to catch in point processing ifs
	}
        else {
	  //// Normal vector which is the length of |p_vec||pt2_vec|*the sin between them
	  double n2_vec[3];
	  CROSS_PRODUCT3D(n2_vec,p_vec,pt2_vec);
	  
	  //// Get magnitude which is distance out * |p_vec| without sign to indicate direction
	  inout2=NORM(n2_vec);      
	  	  
	  //// Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0)) 
	  //// This gives angle with respect to surface of the sphere and hence allows us to assign
	  //// a direction (i.e. a sign) to inout1
	  if (DOT_PRODUCT3D(n2_vec,p1)<0.0) inout2=-inout2; 
	}

	//	if (debug) printf("   it=%d t1=[%f %f %f] t2=[%f %f %f] inout1=%20.17f inout2=%20.17f \n ",it,t1[0],t1[1],t1[2],t2[0],t2[1],t2[2],inout1,inout2);
	
      
	// process point
	if (inout2 > CLIP_EQUAL_TOL) { // t2 inside  
	  if ((inout1 < 0.0) && !inout2_same && !inout1_same) { //  t1 outside
	    double intersect_pnt[3];        
	    
	    // Do intersection and add that point
	    if (line_with_gc_seg3D(p1, p2, t2, t1, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
	      o[3*num_o]=intersect_pnt[0]/ipnorm;
	      o[3*num_o+1]=intersect_pnt[1]/ipnorm;
	      o[3*num_o+2]=intersect_pnt[2]/ipnorm;
	      //  	      if (debug) printf("    it=%d t1-out t2-in inter=[%f %f %f] \n ",it,o[3*num_o],o[3*num_o+1],o[3*num_o+2]);
	      num_o++;
	    }
	  }
	  
	  // Add t2 point because it's inside
	  o[3*num_o]=t2[0];
	  o[3*num_o+1]=t2[1];
	  o[3*num_o+2]=t2[2];
	  num_o++;
	  
	  // record the fact that a point isn't on p_vec, but is in
	  in_but_not_on_p_vec=true;
	  
	} else if (inout2 < 0.0) { // t2 outside
	  
	  if (!inout1_same && (inout1 > CLIP_EQUAL_TOL)) {  //  t1 inside (this excludes the EQUAL region, because 
	    double intersect_pnt[3];      //             if a point was added in there we don't
  	                                 //              want to add another one right next to it)
	    
	    // Do intersection and add that point
	    if (line_with_gc_seg3D(p1, p2, t1, t2, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
	      o[3*num_o]=intersect_pnt[0]/ipnorm;
	      o[3*num_o+1]=intersect_pnt[1]/ipnorm;
	      o[3*num_o+2]=intersect_pnt[2]/ipnorm;

	      // if (debug) printf("    it=%d t1-in t2-out inter=[%f %f %f] \n ",it,o[3*num_o],o[3*num_o+1],o[3*num_o+2]);
	      num_o++;
	    }
	  }
	  
	} else {  // t2 on edge
	  // Just add point because it's on the edge
	  o[3*num_o]=t2[0];
	  o[3*num_o+1]=t2[1];
	  o[3*num_o+2]=t2[2];
	  num_o++;
	}
	
	// old t2 becomes the new t1
	t1=t2;
	inout1=inout2;  
	inout1_same=inout2_same;  
      }
      
      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;
      
      // if poly is empty then leave
      if (num_o==0) break;

	remove_0len_edges3D(&num_o, o);

      // if poly is empty then leave
      if (num_o==0) break;

      // if not on the last cycle then copy out poly back to tmp
      if (ip != num_p-1) {
	double *end_o=o+3*num_o;
	for (double *o_i=o, *tmp_i=tmp; o_i<end_o; o_i++, tmp_i++) {
	  *tmp_i=*o_i;
	}
	num_tmp=num_o;

      }
    }
    
  // Do output
    *num_out=num_o;
    // o is an alias for out so don't need to copy

#undef CROSS_PRODUCT3D
#undef DOT_PRODUCT3D
#undef NORM
#undef CLIP_EQUAL_TOL
  }

  // Here valid and wghts need to be resized to the same size as dst_elems before being passed into 
  // this call. 
  void calc_1st_order_weights_2D_3D_sph(const MeshObj *dst_elem, MEField<> *dst_cfield, 
                             std::vector<const MeshObj *> src_elems, MEField<> *src_cfield, 
			      std::vector<int> *valid, std::vector<double> *wgts) {


// Maximum size for a supported polygon
// Since the elements are of a small 
// limited size. Fixed sized buffers seem 
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_3D (3*MAX_NUM_POLY_NODES) 

    // Declaration for src polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_3D];

    // Get dst coords
    get_elem_coords(dst_elem, dst_cfield, 3, MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);

    // if no nodes then exit
    if (num_dst_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges3D(&num_dst_nodes, dst_coords);

    // if less than a triangle complain
    if (num_dst_nodes<3) {
      Throw() << "Destination Element is degenerate";
    }

    // calculate dst area
    double dst_area=great_circle_area(num_dst_nodes, dst_coords); 

    // if the dst_area is 0 freak out
    if (dst_area == 0.0) {
      Throw() << "Destination Element has 0 area";
    }
    
#if 0
    printf("coords  "); 
    for (int i=0; i<dst_coords.size(); i++) {
      if (i%2==0) printf("[");
      printf("  %f ",dst_coords[i]);
      if (i%2==1) printf("]");
    }
    printf("\n");
#endif


    // Declaration for dst polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_3D];

    // Declaration for tmp polygon used in intersection routine
    double tmp_coords[MAX_NUM_POLY_COORDS_3D];

    // Loop intersecting and calculating weights
    for (int i=0; i<src_elems.size(); i++) {
      const MeshObj *src_elem = src_elems[i];
      
      // Get src coords
      get_elem_coords(src_elem, src_cfield, 3, MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

      // if no nodes then go to next
      if (num_src_nodes<1) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
	continue;
      }


      // Get rid of degenerate edges
      remove_0len_edges3D(&num_src_nodes, src_coords);
      
      // if less than a triangle complain
      if (num_src_nodes<3) {
	Throw() << "Source Element is degenerate";
      }


      // Make sure that we aren't going to go over size of tmp buffers
      if ((num_src_nodes + num_dst_nodes) > MAX_NUM_POLY_NODES) {
	Throw() << " src and dst poly size too big for temp buffer";
      }


#if 0
      if ((dst_elem->get_id()==173) && (src_elem->get_id()==409)) {
	debug=true;
      }
#endif


      // Intersect src with dst element
      intersect_convex_2D_3D_sph_gc_poly(num_dst_nodes, dst_coords,
			      num_src_nodes, src_coords,
			      tmp_coords,
			      &num_sintd_nodes, sintd_coords); 

      //      debug=false;

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

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
	  ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
	printf("%d %d src: ",dst_elem->get_id(),src_elem->get_id());
	for (int j=0; j<num_src_nodes; j++) {
	  printf(" [%f,%f,%f] ",src_coords[3*j],src_coords[3*j+1],src_coords[3*j+2]);
	}
	printf("\n");
      }
#endif

#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
	  ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
	printf("%d %d sintd: ",dst_elem->get_id(),src_elem->get_id());
	for (int j=0; j<num_sintd_nodes; j++) {
	  printf(" [%f,%f,%f] ",sintd_coords[3*j],sintd_coords[3*j+1],sintd_coords[3*j+2]);
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
	continue;
      }

      // calculate intersection area
      double sintd_area=great_circle_area(num_sintd_nodes, sintd_coords); 
   
      //      debug=false;
      //      printf("sintd_area=%f %d dst_area=%f \n",sintd_area,num_sintd_nodes,dst_area);

      // calc weight
      (*valid)[i]=1;
      (*wgts)[i]=sintd_area/dst_area;
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_3D    
  }


} // namespace
