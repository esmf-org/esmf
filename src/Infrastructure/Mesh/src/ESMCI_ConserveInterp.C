// $Id: ESMCI_ConserveInterp.C,v 1.2 2010/09/09 20:26:11 oehmke Exp $
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
static const char *const version = "$Id: ESMCI_ConserveInterp.C,v 1.2 2010/09/09 20:26:11 oehmke Exp $";
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
  
  
  
  // Num is the number of vertices in the polygon
  // coords is of size 2*num
  double area_of_flat_2D_polygon(int num, double *coords) {

    double area=0;
    for(int i=0; i<num; i++) {
      int ip1=(i+1)%num; // i plus 1 mod the size to wrap at end

      area += (coords[2*i]*coords[2*ip1+1]-coords[2*ip1]*coords[2*i+1]);
    }
    
    return 0.5*area;

  }

  void get_elem_coords(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // make sure that we're not bigger than max size
      if (topo->num_nodes > max_num_nodes) {
	Throw() << "Element exceeds maximum poly size";
      }

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem->Relations[s].obj);
	double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
	  coords[k]=c[i];
          k++;
	}
      }

      // Get number of nodes
      *num_nodes=topo->num_nodes;
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
                           


//    w
//    | \
//    |   \
//    |     \
//    u------v
//    ^
//    | --- angle being calculated
//      
  double angle(double *u, double *v, double *w) {
  
  double cosa=u[0]*v[0]+u[1]*v[1]+u[2]*v[2];

  double cosb=u[0]*w[0]+u[1]*w[1]+u[2]*w[2];

  double cosc=v[0]*w[0]+v[1]*w[1]+v[2]*w[2];

  //  printf("a=%f b=%f c=%f \n",cosa,cosb,cosc);

  double sina=sqrt(1.0-cosa*cosa);

  double sinb=sqrt(1.0-cosb*cosb);

#if 0
    if (debug) {
        printf("u=[%f %f %f] ",u[0],u[1],u[2]);
        printf("v=[%f %f %f] ",v[0],v[1],v[2]);
        printf("w=[%f %f %f] ",w[0],w[1],w[2]);
	//        printf("sina=%f sinb=%f ",sina,sinb);
	printf("sina*sinb=%f ",sina*sinb);
	printf("cosc-(cosa*cosb)=%f ",cosc-(cosa*cosb));
	printf("cosc-(cosa*cosb)/sina*sinb=%30.27f ",(cosc-(cosa*cosb))/(sina*sinb));
        printf("angle=%f \n",acos((cosc-(cosa*cosb))/(sina*sinb)));


    }
#endif

    // Calculate the cosine of the angle we're calculating
    // using spherical trigonometry formula
   double cos_angle=(cosc-(cosa*cosb))/(sina*sinb);

   // If we've gone a little out of bounds due to round off, shift back
   if (cos_angle > 1.0) cos_angle=1.0;
   if (cos_angle < -1.0) cos_angle=-1.0;

   // return angle
   return acos(cos_angle);

}






// Compute the great circle area of a polygon on a sphere
double great_circle_area(int n, double *pnts) {

  // sum angles around polygon
  double sum=0.0;
  for (int i=0; i<n; i++) {
    // points that make up a side of polygon
    double *pnt0=pnts+3*i;
    double *pnt1=pnts+3*((i+1)%n);
    double *pnt2=pnts+3*((i+2)%n);

    // compute angle for pnt1
    sum += angle(pnt1, pnt2, pnt0);
  }

  // return area
  return sum-(((double)(n-2))*((double)M_PI));
}


void remove_0len_edges3D(int *num_p, double *p) {

#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < 1E-15) &&	\
                           (std::abs(p1[1]-p2[1]) < 1E-15) &&	\
                           (std::abs(p1[2]-p2[2]) < 1E-15))
    
  // Get old value of num_p
  int old_num_p=*num_p;

  // See if there are any equal points
  int j=-1;
  double *last_pnt=p+3*(old_num_p-1);
  for (int i=0; i<old_num_p; i++) {
    double *pnti=p+3*i;
    
    if (PNTS_EQUAL(pnti,last_pnt)) {
      j=i;
      break;
    }

    // advance last point
    last_pnt=pnti;
  } 

  // We found an equal point so start trimming them out
  if (j>-1) {
    for (int i=j; i<old_num_p; i++) {
      double *pnti=p+3*i;
      if (!PNTS_EQUAL(pnti,last_pnt)) {
	double *pntj=p+3*j;

	// copy the non-equal point to j
	pntj[0]=pnti[0];
	pntj[1]=pnti[1];
	pntj[2]=pnti[2];

	// move j
	j++;

	// reset the last pointer to the last non-repeating value
	last_pnt=pntj;
      }
    }

    // reset num_p to the new number of points
    *num_p=j;
  } else {
    // Leave num_p as it is
  }

#undef PNTS_EQUAL
}



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
      
      // calc p_vec (vector along the current edge of p)
      double p_vec[3];
      p_vec[0]=p2[0]-p1[0]; p_vec[1]=p2[1]-p1[1]; p_vec[2]=p2[2]-p1[2];
      
      // Set initial t1 (last point in tmp polygon)
      double *t1=tmp+3*(num_tmp-1);

      // Vector from p to t1     
      double pt1_vec[3];
      pt1_vec[0]=t1[0]-p1[0]; pt1_vec[1]=t1[1]-p1[1]; pt1_vec[2]=t1[2]-p1[2];

      // Normal vector which is the length of |p_vec||pt_vec|*the sin between them
      double n_vec[3];
      CROSS_PRODUCT3D(n_vec,p_vec,pt1_vec);

#if 0
      printf("ip=%d p_vec=%f %f %f \n ",ip,p_vec[0],p_vec[1],p_vec[2]);
      printf("ip=%d pt1_vec=%f %f %f \n ",ip,pt1_vec[0],pt1_vec[1],pt1_vec[2]);
      printf("ip=%d it before loop  nvec=%f %f %f \n ",ip,n_vec[0],n_vec[1],n_vec[2]);
#endif     
 
      // Get magnitude which is distance out * |p_vec| without sign to indicate direction
      double inout1=NORM(n_vec);

      // Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0)) 
      // This gives angle with respect to surface of the sphere and hence allows us to assign
      // a direction (i.e. a sign) to inout1
      if (DOT_PRODUCT3D(n_vec,p1)<0.0) inout1=-inout1; 

      
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
	
	//// Normal vector which is the length of |p_vec||pt2_vec|*the sin between them
	double n2_vec[3];
	CROSS_PRODUCT3D(n2_vec,p_vec,pt2_vec);
      
#if 0
	printf("ip=%d it=%dp_vec=%f %f %f \n ",ip,it,p_vec[0],p_vec[1],p_vec[2]);
	printf("ip=%d it=%d pt2_vec=%f %f %f \n ",ip,it,pt2_vec[0],pt2_vec[1],pt2_vec[2]);
	printf("ip=%d it=%d  p1=%f %f %f \n ",ip,it,p1[0],p1[1],p1[2]);
	printf("ip=%d it=%d  n2vec=%f %f %f \n ",ip,it,n2_vec[0],n2_vec[1],n2_vec[2]);
	double dp=DOT_PRODUCT3D(n2_vec,p1);
	printf("p=%d t=%d dp=%f \n ",ip,it,dp);
        printf("p=%d t=%d inout1=%f inout2=%f \n ",ip,it,inout1,inout2);
#endif     

	//// Get magnitude which is distance out * |p_vec| without sign to indicate direction
	double inout2=NORM(n2_vec);

	//// Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0)) 
	//// This gives angle with respect to surface of the sphere and hence allows us to assign
	//// a direction (i.e. a sign) to inout1
	if (DOT_PRODUCT3D(n2_vec,p1)<0.0) inout2=-inout2; 
	
	// process point
	if (inout2 > CLIP_EQUAL_TOL) { // t2 inside 
	  if (inout1 < 0.0) { //  t1 outside
	    double intersect_pnt[3];        
	    
	    // Do intersection and add that point
	    if (line_with_gc_seg3D(p1, p2, t2, t1, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
	      o[3*num_o]=intersect_pnt[0]/ipnorm;
	      o[3*num_o+1]=intersect_pnt[1]/ipnorm;
	      o[3*num_o+2]=intersect_pnt[2]/ipnorm;
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
	  
	  if (inout1 > CLIP_EQUAL_TOL) {  //  t1 inside (this excludes the EQUAL region, because 
	    double intersect_pnt[3];      //             if a point was added in there we don't
  	                                 //              want to add another one right next to it)
	    
	    // Do intersection and add that point
	    if (line_with_gc_seg3D(p1, p2, t1, t2, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
	      o[3*num_o]=intersect_pnt[0]/ipnorm;
	      o[3*num_o+1]=intersect_pnt[1]/ipnorm;
	      o[3*num_o+2]=intersect_pnt[2]/ipnorm;
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
      }
      
      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;
      
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

      // Intersect src with dst element
      intersect_convex_2D_3D_sph_gc_poly(num_dst_nodes, dst_coords,
			      num_src_nodes, src_coords,
			      tmp_coords,
			      &num_sintd_nodes, sintd_coords); 

#if 0
      printf("dst: "); 
      for (int j=0; j<num_dst_nodes; j++) {
	printf(" [%f,%f,%f] ",dst_coords[3*j],dst_coords[3*j+1],dst_coords[3*j+2]);
      }
      printf("\n");
#endif

#if 0
      printf("src: "); 
      for (int j=0; j<num_src_nodes; j++) {
	printf(" [%f,%f,%f] ",src_coords[3*j],src_coords[3*j+1],src_coords[3*j+2]);
      }
      printf("\n");
#endif

#if 0
      if ((dst_elem->get_id()==135) && (src_elem->get_id()==322)) {
	printf("sintd: "); 
	for (int j=0; j<num_sintd_nodes; j++) {
	  printf(" [%f,%f,%f] ",sintd_coords[3*j],sintd_coords[3*j+1],sintd_coords[3*j+2]);
	}
	printf("\n");
      debug=true;
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
