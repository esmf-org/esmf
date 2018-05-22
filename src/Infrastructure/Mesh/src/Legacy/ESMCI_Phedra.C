#include <stdio.h>
#include <string.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <fstream>
#include <Mesh/include/Legacy/ESMCI_Phedra.h>


namespace ESMCI {

  bool phedra_debug=false;


//// Intersect a line and a plane
// Intersects between the plane determined by pln_pnt and pln_normal
// and the line determined by the endpoints l1 and l2 (t=0.0 at l1 and t=1.0 at l2)
// returns true if the two intersect and the output variables are valid, otherwise line and plane are parallel.
// outputs t the coordinate on the line of the intersection.
// NOTE: the intersection doesn't have to be inside the line for this to return true
bool intersect_plane_with_line(const double *pln_pnt, const double *pln_normal, const double *l1, const double *l2, double *t) {
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])

  // compute vector along line
  double l1to2_vec[3];
  l1to2_vec[0]=l2[0]-l1[0];
  l1to2_vec[1]=l2[1]-l1[1];
  l1to2_vec[2]=l2[2]-l1[2];

  // compute vector from start of line to plane point
  double ltop_vec[3];
  ltop_vec[0]=pln_pnt[0]-l1[0];
  ltop_vec[1]=pln_pnt[1]-l1[1];
  ltop_vec[2]=pln_pnt[2]-l1[2];

  // compute top of equation
  double top=DOT_PRODUCT3D(ltop_vec, pln_normal);
  //  if (top == 0.0) return false;

  // compute bottom of equation
  double btm=DOT_PRODUCT3D(l1to2_vec, pln_normal);
  if (btm == 0.0) return false; // The line is parallel to the plane

  // compute position on line
  *t=top/btm;

  return true;

#undef DOT_PRODUCT3D
}

  // Do intersection between plane and line, output point of intersection. Return true if output is valid, false if not (e.g. line and plane are parallel). 
bool intersect_plane_with_line_output_pnt(const double *pln_pnt, const double *pln_normal, const double *l1, const double *l2, double *o_pnt) {
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
  double t12, t21;  
  double vec_diff[3];

  // Intersect frontways 
  if (!intersect_plane_with_line(pln_pnt, pln_normal, l1, l2, &t12)) return false;
  
  // Calc intersection point
  double pnt12[3];
  pnt12[0]=t12*(l2[0]-l1[0])+l1[0];
  pnt12[1]=t12*(l2[1]-l1[1])+l1[1];
  pnt12[2]=t12*(l2[2]-l1[2])+l1[2];  
  
  // calculate dot product to plane normal
  double dot12;
  vec_diff[0]=pnt12[0]-pln_pnt[0];      
  vec_diff[1]=pnt12[1]-pln_pnt[1];      
  vec_diff[2]=pnt12[2]-pln_pnt[2];      
  
  dot12=std::abs(DOT_PRODUCT3D(pln_normal,vec_diff));

  // Intersect backwards
  if (!intersect_plane_with_line(pln_pnt, pln_normal, l2, l1, &t21)) return false;
  
  // Calc intersection point
  double pnt21[3];
  pnt21[0]=t21*(l1[0]-l2[0])+l2[0];
  pnt21[1]=t21*(l1[1]-l2[1])+l2[1];
  pnt21[2]=t21*(l1[2]-l2[2])+l2[2];  
  
  // calculate dot product to plane normal
  double dot21;
  vec_diff[0]=pnt21[0]-pln_pnt[0];      
  vec_diff[1]=pnt21[1]-pln_pnt[1];      
  vec_diff[2]=pnt21[2]-pln_pnt[2];      
  
  dot21=std::abs(DOT_PRODUCT3D(pln_normal,vec_diff));

  // Output point based on which is closer to plane
  // (e.g. closer to 0.0)
  if (dot12 < dot21) {
    o_pnt[0]=pnt12[0];
    o_pnt[1]=pnt12[1];
    o_pnt[2]=pnt12[2];
  } else if (dot12 > dot21) {
    o_pnt[0]=pnt21[0];
    o_pnt[1]=pnt21[1];
    o_pnt[2]=pnt21[2];
  } else {
    if (pnt12[0] < pnt21[0]) {
      o_pnt[0]=pnt12[0];
      o_pnt[1]=pnt12[1];
      o_pnt[2]=pnt12[2];
    } else if (pnt12[0] > pnt21[0]) {
      o_pnt[0]=pnt21[0];
      o_pnt[1]=pnt21[1];
      o_pnt[2]=pnt21[2];   
    } else {
      if (pnt12[1] < pnt21[1]) {
        o_pnt[0]=pnt12[0];
        o_pnt[1]=pnt12[1];
        o_pnt[2]=pnt12[2];
      } else if (pnt12[1] > pnt21[1]) {
          o_pnt[0]=pnt21[0];
          o_pnt[1]=pnt21[1];
          o_pnt[2]=pnt21[2];   
      } else {
        if (pnt12[2] < pnt21[2]) {
          o_pnt[0]=pnt12[0];
          o_pnt[1]=pnt12[1];
          o_pnt[2]=pnt12[2];
        } else {
          o_pnt[0]=pnt21[0];
          o_pnt[1]=pnt21[1];
          o_pnt[2]=pnt21[2];   
        } 
      }
    }
  }
  
  return true;
#undef DOT_PRODUCT3D
}



// Intersects convex 3D polygon whose vertices are stored in counter clockwise
// order and a plane defined by a normal and a point. Note that the polygons have 3D coordinates
// The part of the polygon which is kept is the part which is behind the plane defined by pln_normal and pln_pnt, e.g. 
// the part on the opposite side from which the normal is pointing. 
// but are only 2D. 
// p should be of size 3*num_p
// pln_pnt and pln_normal should both be of size 3
// ioo is a temporary buffer that should be of size num_p
// out should both be allocated to be at least of size 3*(num_p+num_q)
// num_ipnts,ipnts will contain the list of intersection points of polygon tmp with the plane tri. 
// ipnts should be allocated to be at least of size 2? num_tmp?
// NOTE: num_ipnts isn't initialized in this method, so if used repeatedly it'll just add to the list

// NOTES:
// If this function changes the input polynomial (num_tmp,tmp), then it MUST return points in ipnt, so that a new face can be constructed.
// One exception to this is if p lies completely outside of the plane and is completely clipped away (e.g. num_out=0), then
// no intersection points should be returned because any necessary new face will be constructed, from the intersection points
// from other polygons which are only partially clipped away. 
void intersect_convex_2D_3D_cart_poly_w_plane(int num_p, double *p,
                                              double *pln_pnt, double *pln_normal, int *ioo,
                                              int *num_out, double *out, int *num_ipnt, double *ipnt, bool *has_in) 
{
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define LEN(a) std::sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])
  //#define CLIP_EQUAL_TOL 1.0e-17
#define CLIP_EQUAL_TOL 1.0e-19
#define CLOSE_TO_ZERO(vec) ((std::abs(vec[0])<CLIP_EQUAL_TOL) && (std::abs(vec[1])<CLIP_EQUAL_TOL) && (std::abs(vec[2])<CLIP_EQUAL_TOL))

#define ADD_PNT_TO_O_LIST(pnt) o[3*num_o]=pnt[0]; o[3*num_o+1]=pnt[1]; o[3*num_o+2]=pnt[2]; num_o++;
#define ADD_PNT_TO_I_LIST(pnt) i[3*num_i]=pnt[0]; i[3*num_i+1]=pnt[1]; i[3*num_i+2]=pnt[2]; num_i++;

#define IOO_IN 1
#define IOO_ON 0
#define IOO_OUT -1

    
    
  // Setup alias for output polygon array
  int num_o=0;
  double *o=out;

  // Setup alias for output intersected point array
  int num_i=*num_ipnt;
  double *i=ipnt;


  // If tmp is empty then leave
  if (num_p==0) {
    // num_ipnt remains the same
    *has_in=false;
    return;
  }
    
  // Set t as a point on the plane
  double *t=pln_pnt;



  ///// First loop setting in,out,on criteria ////
  bool exists_in=false;
  bool exists_on=false;
  for (int ip=0; ip<num_p; ip++) {

    // Point in polygon
    double *p1=p+3*ip;

    // Vector from t to p1     
    double tp1_vec[3];
    tp1_vec[0]=p1[0]-t[0]; tp1_vec[1]=p1[1]-t[1]; tp1_vec[2]=p1[2]-t[2];

    // Set criteria
    if (CLOSE_TO_ZERO(tp1_vec)) {
      ioo[ip]=IOO_ON;
  //                          if (phedra_debug) printf("on ");
    } else {
      double cos_w_normal=DOT_PRODUCT3D(pln_normal,tp1_vec);
      cos_w_normal =cos_w_normal/LEN(tp1_vec); // checking for 0 length above

      if (cos_w_normal < -CLIP_EQUAL_TOL) { 
        ioo[ip]=IOO_IN;
  //                            if (phedra_debug) printf("in ");
        exists_in=true;
      } else if (cos_w_normal > 0.0) {
        //      } else if (cos_w_normal > CLIP_EQUAL_TOL) {
        ioo[ip]=IOO_OUT;
  //                            if (phedra_debug) printf("out ");
      } else {
        ioo[ip]=IOO_ON;
        exists_on=true;
  //                            if (phedra_debug) printf("on ");
      }
    }
  }



  // Set has_in
  if (exists_in) {
    *has_in=true;
  } else {
    *has_in=false;
  }



  // If there are no "in" or "on" points then leave
  if (!exists_in && !exists_on) {
    *num_out=0;
    // *num_ipnt remains the same
  //                      if (phedra_debug) printf("num_i pnts added=0\n");
    return;
  } 


  // Clip polygon using the in, on, out information generated above
  //// Set initial p1 (last point in polygon)
  double *p1=p+3*(num_p-1);
  int     ioo1=ioo[num_p-1];

  //// Loop through clipping based on ioo
  for (int ip=0; ip<num_p; ip++) {
    double *p2=p+3*ip;
    int     ioo2=ioo[ip];
    
    // Do something based on ioo status of p1 and p2
    switch (ioo1) {
    //// ioo1 is out ////
    case IOO_OUT: 
      switch (ioo2) {
      case IOO_OUT:
        // Add nothing
        break;
        
      case IOO_ON:        
        ADD_PNT_TO_O_LIST(p2);
        ADD_PNT_TO_I_LIST(p2);
        break;

      case IOO_IN:
        // Calc intersection point and add
        double pnt[3];        
        if (intersect_plane_with_line_output_pnt(pln_pnt, pln_normal, p1, p2, pnt)) {
          // Add to lists
          ADD_PNT_TO_O_LIST(pnt);
          ADD_PNT_TO_I_LIST(pnt);
        }

        ADD_PNT_TO_O_LIST(p2);
        break;
      } 
      break;
      
    //// ioo1 is on ////
    case IOO_ON:
      switch (ioo2) {
      case IOO_OUT:
        // Add p1 to intersection list because an intersection 
        // is occurring with p1 the intersection point
        // p1 was added to the out (_O_) list when it was p2
        ADD_PNT_TO_I_LIST(p1);
        break;
        
      case IOO_ON:
        ADD_PNT_TO_O_LIST(p2);
        break;
        
      case IOO_IN:
        ADD_PNT_TO_O_LIST(p2);
        break;
      } 
      break;
      
    //// ioo1 is in ////
    case IOO_IN:
      switch (ioo2) {
      case IOO_OUT:
        // Calc intersection point and add
        double pnt[3];        
        if (intersect_plane_with_line_output_pnt(pln_pnt, pln_normal, p1, p2, pnt)) {
          // Add to lists
          ADD_PNT_TO_O_LIST(pnt);
          ADD_PNT_TO_I_LIST(pnt);
        }
        break;
        
      case IOO_ON:
        ADD_PNT_TO_O_LIST(p2);
        break;
        
      case IOO_IN:
        ADD_PNT_TO_O_LIST(p2);
        break;
      } 
      break;
    }


    // p1 now moves to p2
    p1=p2;
    ioo1=ioo2;
  }

  // Remove redundant points
  remove_0len_edges3D(&num_o, o);

  // if poly is empty then leave
  if (num_o==0) {
    *num_out=0;
    // *num_ipnt remains the same
  //                      if (phedra_debug) printf("num_i pnts added=0\n");
    return;
  }
    
  // Do output
  *num_out=num_o;
  // o is an alias for out so don't need to copy

#if 0
  if (phedra_debug) {
    int num_added=num_i-*num_ipnt;
    printf("num_i pnts added=%d :: ",num_added);
    for (int j=0; j<num_added; j++) {
      double *pnt=i+3*(*num_ipnt+j);

      double vec_diff[3];
      vec_diff[0]=pnt[0]-pln_pnt[0];      
      vec_diff[1]=pnt[1]-pln_pnt[1];      
      vec_diff[2]=pnt[2]-pln_pnt[2];      

      double dot=DOT_PRODUCT3D(pln_normal,vec_diff);

      printf("(%20.17f %20.17f %20.17f N%30.27f) ",pnt[0],pnt[1],pnt[2],dot);
    } 
    printf("\n");
  }
#endif

  *num_ipnt=num_i;
  // i is an alias for ipnt so don't need to copy


#undef IOO_IN
#undef IOO_ON
#undef IOO_OUT
#undef ADD_PNT_TO_O_LIST
#undef ADD_PNT_TO_I_LIST
#undef DOT_PRODUCT3D
#undef LEN
#undef CLIP_EQUAL_TOL
}


// Takes in a list of points (in pnts) and outputs a polygon in clockwise order (in out).
// Here clockwise is looking down normal
// The pnts array should be of size 3*num_pnts
// The output array (out) should be of size 3*num_pnts
// The tmp array should be of size num_pnts
void make_polygon_3Dcart(int num_pnts, double *pnts, double *normal, PntAndAngle *tmp, double *out) {
  double center[3]; 
#define LEN(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define CLIP_EQUAL_TOL 1.0e-20
#define CLOSE(vec1,vec2) ((std::abs(vec1[0]-vec2[0])<CLIP_EQUAL_TOL) && (std::abs(vec1[1]-vec2[1])<CLIP_EQUAL_TOL) && (std::abs(vec1[2]-vec2[2])<CLIP_EQUAL_TOL))

  // If there are no points then leave
  if (num_pnts<1) {
    return;
  }

  // Compute point in the center (average)
  center[0]=0.0;   center[1]=0.0;   center[2]=0.0;
  for (int i=0; i<num_pnts; i++){
    double *pnt=pnts+3*i;

    center[0] += pnt[0];
    center[1] += pnt[1];
    center[2] += pnt[2];
  }
  center[0]=center[0]/((double)num_pnts);
  center[1]=center[1]/((double)num_pnts);
  center[2]=center[2]/((double)num_pnts);


  // Loop through to find a point which is not zero distance to center
  int first=-1;
  for (int i=0; i<num_pnts; i++) {
    if (!(CLOSE((pnts+3*first),center))) {
      first=i;
      break;
    }
  }

  // Everything is on top of the center so leave 
  if (first<0) {
    std::copy(pnts,pnts+3*num_pnts,out);
    return;
  }

  // vector from center to first point
  double vec_c_to_f[3]; 
  vec_c_to_f[0]=pnts[3*first+0]-center[0];
  vec_c_to_f[1]=pnts[3*first+1]-center[1];
  vec_c_to_f[2]=pnts[3*first+2]-center[2];
  double len_vec_c_to_f=LEN(vec_c_to_f); 

  // Set angles for values up to first to 0.0 
  // since they are all at center
  for (int i=0; i<first; i++) {
    tmp[i].set(pnts+3*i,0.0);
  }

  // Set first angle to 0.0, because it's the start
  tmp[first].set(pnts+3*first,0.0);

  // Loop through the rest of the points and set angles
  for (int i=first+1; i<num_pnts; i++) {
    double *pnt=pnts+3*i;

    // vector from the center to pnt
    double vec_c_to_p[3];
    vec_c_to_p[0]=pnt[0]-center[0];
    vec_c_to_p[1]=pnt[1]-center[1];
    vec_c_to_p[2]=pnt[2]-center[2];
    double len_vec_c_to_p=LEN(vec_c_to_p); 

    // Calc Cos using dot product
    double dot=DOT_PRODUCT3D(vec_c_to_p,vec_c_to_f);
    double cs=dot/(len_vec_c_to_p*len_vec_c_to_f);

    // Calc Sin using cross product
    double cross[3];
    CROSS_PRODUCT3D(cross,vec_c_to_f,vec_c_to_p);
    double len_cross=LEN(cross);
    double sn=len_cross/(len_vec_c_to_p*len_vec_c_to_f);
    if (DOT_PRODUCT3D(normal,cross) <0.0) sn=-sn; // assign a sign based on how cross lines up with normal
    
    // get angle using atan2
    double angle=std::atan2(sn,cs);

    // Put data into structure
    tmp[i].set(pnt,angle);
  }

  // Sort in order of angle
  std::sort(tmp,tmp+num_pnts);  


  // Output
  for (int i=0; i<num_pnts; i++) {
    tmp[i].getPnt(out+3*i);
  }

#undef LEN
#undef CROSS_PRODUCT3D
#undef DOT_PRODUCT3D
#undef CLIP_EQUAL_TOL
#undef CLOSE
}



PhedraFace::PhedraFace() {
  num_pnts=0;

  normal[0]=0.0;
  normal[1]=0.0;
  normal[2]=0.0;
}

void calc_unit_normal(double *p1, double *p2, double *p3, double *out_normal) {
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define LEN(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])

  // calc outward normal to triangle
  double vec12[3];
  vec12[0]=p2[0]-p1[0];
  vec12[1]=p2[1]-p1[1];
  vec12[2]=p2[2]-p1[2];

  double vec13[3];
  vec13[0]=p3[0]-p1[0];
  vec13[1]=p3[1]-p1[1];
  vec13[2]=p3[2]-p1[2];

  // normal to plane
  CROSS_PRODUCT3D(out_normal,vec12,vec13);

  // normalize
  double len=LEN(out_normal);
  if (len > 0.0) {
    out_normal[0] /= len;
    out_normal[1] /= len;
    out_normal[2] /= len;
  }

#undef CROSS_PRODUCT3D
#undef LEN
}

void PhedraFace::set_tri(double *p1, double *p2, double *p3) {

  // set number of points
  num_pnts=3;
  
  // Fill point list
  pnts[0]=p1[0];   pnts[1]=p1[1];   pnts[2]=p1[2];
  pnts[3]=p2[0];   pnts[4]=p2[1];   pnts[5]=p2[2];
  pnts[6]=p3[0];   pnts[7]=p3[1];   pnts[8]=p3[2];

  // calc unit normal and put into face structure
  calc_unit_normal(p1, p2, p3, normal);
}


void Phedra::clip_against_plane(double *pln_pnt, double *pln_normal) {
  double tri[9];
  int num_tmp;
  double tmp[3*(PhedraFace::max_num_pnts+1)]; //I think the only time it can be more than the number going in is clipping off a point
  int tmp_ioo[PhedraFace::max_num_pnts]; 

  PntAndAngle tmp_paa[PhedraFace::max_num_pnts]; 

  int num_ipnt;
  double ipnt[2*3*Phedra::max_num_faces]; // 2 possible points per face intersection. 


  // If no faces, then leave
  if (num_faces<1) {
    return;
  }

  // Clip against faces 
  num_ipnt=0;
  int face_to_fill=0;
  bool phedra_has_in=false;
  for (int i=0; i<num_faces; i++) {
    
    // clip against plane in tri
    bool face_has_in;
    intersect_convex_2D_3D_cart_poly_w_plane(faces[i].num_pnts, faces[i].pnts,
                                             pln_pnt, pln_normal, tmp_ioo,
                                             &num_tmp, tmp,
                                             &num_ipnt, ipnt,
                                             &face_has_in); 

    // track if there are any ins
    if (face_has_in) phedra_has_in=true;

    // If clipped face is empty, don't need to copy back into face, so skip
    if (num_tmp == 0) continue;

    // Make sure not going over number of faces
    if (face_to_fill >= Phedra::max_num_faces) {
      Throw()<<"ERROR: Adding too many faces during clipping! \n";
    }

    // If number of points is bigger than the max number complain
    if (num_tmp > PhedraFace::max_num_pnts) {
      Throw()<<"ERROR: Adding too many points to a face! \n";
    }


    // copy back to face
    // TODO: HERE MAKE SURE POINTS ARE SMALLER THAN PhedraFace::max_num_pnts
    std::copy(tmp,tmp+3*num_tmp,faces[face_to_fill].pnts);
    faces[face_to_fill].num_pnts=num_tmp;
    faces[face_to_fill].normal[0]=faces[i].normal[0];
    faces[face_to_fill].normal[1]=faces[i].normal[1];
    faces[face_to_fill].normal[2]=faces[i].normal[2];

    // filled a face, so move to next
    face_to_fill++;
  }

  // If there's nothing left after clipping then set to 0 and exit
  if (!phedra_has_in) {
    num_faces=0;
    return;
  }

  // Set new number of faces after clipping
  // (Here face_to_fill is +1 the last filled face, but because C is 0-based that's the number of faces)
  num_faces=face_to_fill;

#if 0
  if (phedra_debug) {
    // print out intersection points 
    for (int i=0; i<num_ipnt; i++) {
      printf("%d :: %f %f %f \n",i,ipnt[3*i],ipnt[3*i+1],ipnt[3*i+2]); 
    }
  }
#endif


  // If there are less than 3 intersected points then leave, because we don't 
  // have enough points to make a face
  if (num_ipnt<3) return;

  // Make a polygon of the face, by putting them in the correct order
  make_polygon_3Dcart(num_ipnt, ipnt, pln_normal, tmp_paa, tmp);

  // Get rid of extra points
  num_tmp=num_ipnt;
  remove_0len_edges3D(&num_tmp, tmp);    
  
#if 0
  // print out intersection points 
  for (int i=0; i<num_tmp; i++) {
    printf("%d :: %f %f %f \n",i,tmp[3*i],tmp[3*i+1],tmp[3*i+2]); 
  }

  if (phedra_debug) {
    printf("creating new face=%d with num_pnts=%d\n",num_faces,num_tmp);
  }
#endif

  // If there are less than 3 intersected points then leave, because we don't 
  // have enough points to make a face
  if (num_tmp<3) return;

  // If number of points is bigger than the max number
  if (num_tmp > PhedraFace::max_num_pnts) {
    Throw()<<"ERROR: Adding too many points to a face! \n";
  }  


  // Add new face
  PhedraFace *new_face=faces+num_faces;
  std::copy(tmp,tmp+3*num_tmp,new_face->pnts);
  new_face->num_pnts=num_tmp;
  new_face->normal[0]=pln_normal[0];
  new_face->normal[1]=pln_normal[1];
  new_face->normal[2]=pln_normal[2];
  num_faces++;
}


// Build a tetrahedra 
// Here the order of p1...4 is the same as that in the Mesh
Phedra::Phedra(double *p1, double *p2, double *p3, double *p4) {

  num_faces=4;
  faces[0].set_tri(p1,p2,p3);
  faces[1].set_tri(p1,p3,p4);
  faces[2].set_tri(p2,p4,p3);
  faces[3].set_tri(p1,p4,p2);
}


// Construct a polyhedra from a list of triangle faces
// tri should be of size 9*num_tri. 
// Each tri should be counter-clockwise looking down outward normal 
Phedra::Phedra(int num_tri, double *tri) {

    // Make sure not going over number of faces
  if (num_tri > Phedra::max_num_faces) {
    Throw() <<"ERROR: Can't create a Phedra with this many faces. \n";
  }

  for (int i=0; i<num_tri; i++) {
    double *one_tri=tri+9*i;

    faces[i].set_tri(one_tri,one_tri+3,one_tri+6);
  }

  num_faces=num_tri;
}



void Phedra::write_to_vtk(const char *filename) {
#define MAX_WTV_STR_LEN 1000
    char new_filename[MAX_WTV_STR_LEN];
    std::ofstream myfile;
    
    if (strlen(filename)+4 > MAX_WTV_STR_LEN) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s.vtk",filename);

    myfile.open (new_filename);

    // sum number of points
    int tot_num_pnts=0;
    for (int i=0; i<num_faces; i++) {
      tot_num_pnts += faces[i].num_pnts;
    }

    myfile << "# vtk DataFile Version 3.0\n";
    myfile << "This file generated by ESMF\n";
    myfile << "ASCII\n";
    myfile << "DATASET UNSTRUCTURED_GRID\n";
    myfile << "POINTS "<<tot_num_pnts<<" double\n";
    for (int i=0; i<num_faces; i++) {
      double *p=faces[i].pnts;
      for (int j=0; j<faces[i].num_pnts ; j++){
        myfile << p[3*j]<<" "<< " "<<p[3*j+1] <<" "<< p[3*j+2]<<"\n";
      }
    }
    myfile << "CELLS "<<num_faces<<" "<<tot_num_pnts+num_faces<<"\n";
    int k=0;
    for (int i=0; i<num_faces; i++) {
      myfile << faces[i].num_pnts <<" ";
      for (int j=0; j<faces[i].num_pnts ; j++){
        myfile << k <<" ";
        k++;
      }
      myfile << "\n";
    }
    myfile << "CELL_TYPES "<<num_faces<<"\n";
    for(int i=0; i<num_faces; i++) {
      myfile << "7 \n";
    }
    myfile.close();
#undef MAX_WTV_STR_LEN 
  }


  void Phedra::write_faces_to_vtk(char *filename) {
#define MAX_PW3PTVID_STR_LEN 1000
    char new_filename[MAX_PW3PTVID_STR_LEN];
    
    if (((double)strlen(filename))+log10((double)num_faces)+((double)strlen("face"))+1.0 > ((double)MAX_PW3PTVID_STR_LEN)) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    for (int i=0; i<num_faces; i++) {
      sprintf(new_filename,"%s%dface",filename,i);
      write_3D_poly_woid_to_vtk(new_filename, faces[i].num_pnts, faces[i].pnts);
    }

#undef MAX_PW3PTVID_STR_LEN
  }



void Phedra::intersect(Phedra &p) {

  // Clip against faces 
  for (int i=0; i<p.num_faces; i++) {
#if 0
    if (phedra_debug && (i==p.num_faces-1)) {
      write_faces_to_vtk("before_last");
      write_3D_poly_woid_to_vtk("before_last_plane", p.faces[i].num_pnts, p.faces[i].pnts);
    }
#endif

    clip_against_plane(p.faces[i].pnts,p.faces[i].normal);
  }
}

double Phedra::calc_volume() {
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
  const double one_sixth=1.0/6.0;

  // Iterate through faces
  double volume=0.0; 
  for (int i=0; i<num_faces; i++) {
    // Get pointer to current face
    PhedraFace *face=faces+i;

    // Skip 0 sized faces
    if (face->num_pnts==0) continue;

    // Calc Area of a face
    double vec_sum[3]={0.0,0.0,0.0};
    for (int j=0; j<face->num_pnts; j++) {
      double *pnt_j=face->pnts+3*j;
      double *pnt_jp1=face->pnts+3*((j+1)%face->num_pnts);

      // Calcuate cross product
      double cross[3];
      CROSS_PRODUCT3D(cross,pnt_j,pnt_jp1);

      // Sum into vector
      vec_sum[0]+=cross[0];
      vec_sum[1]+=cross[1];
      vec_sum[2]+=cross[2];
    }

    double face_areax2=DOT_PRODUCT3D(face->normal,vec_sum); // Took 1/2 off here and made below 1/6 for efficiency

    // A point in face (the first point)
    double *pnt0=face->pnts;
       
    // Add to volume
    volume += one_sixth*face_areax2*DOT_PRODUCT3D(face->normal,pnt0);
  }



  // output 
  return volume;

#undef CROSS_PRODUCT3D
#undef DOT_PRODUCT3D
}

  void Phedra::calc_min(double *min) {
    
    min[0]=1.0E10;
    min[1]=1.0E10;
    min[2]=1.0E10;
    for (int i=0; i<num_faces; i++) {
      for (int j=0; j<faces[i].num_pnts; j++) {
        double *pnt=faces[i].pnts+3*j;

        if (pnt[0]<min[0]) min[0]=pnt[0];
        if (pnt[1]<min[1]) min[1]=pnt[1];
        if (pnt[2]<min[2]) min[2]=pnt[2];
       
      }
    }

  }


  void Phedra::calc_max(double *max) {
    
    max[0]=-1.0E10;
    max[1]=-1.0E10;
    max[2]=-1.0E10;
    for (int i=0; i<num_faces; i++) {
      for (int j=0; j<faces[i].num_pnts; j++) {
        double *pnt=faces[i].pnts+3*j;

        if (pnt[0]>max[0]) max[0]=pnt[0];
        if (pnt[1]>max[1]) max[1]=pnt[1];
        if (pnt[2]>max[2]) max[2]=pnt[2];
       
      }
    }

  }


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
#endif


  // Get tris for a side of a hexahedron.  
  // The ids are used to make triangulation consistent for neighboring faces. 
  // side_ids and side_ind are both of size 4
  // tri1_ind and tri2_ind are both of size 3
  void get_tris_for_side(int *side_ids, int *side_inds, int *tri1_inds, int *tri2_inds) {
    
    // Find the max id on the side
    int max_id=-1;
    int max_i=-1;
    for (int i=0; i<4; i++) {
      if (side_ids[i] > max_id) {
        max_id=side_ids[i];
        max_i=i;
      } 
    }

    // Get 1st triangle
    tri1_inds[0]=side_inds[max_i];
    max_i=(max_i+1)%4;
    tri1_inds[1]=side_inds[max_i];
    max_i=(max_i+1)%4;
    tri1_inds[2]=side_inds[max_i];

    // Get 2nd triangle
    tri2_inds[0]=side_inds[max_i];
    max_i=(max_i+1)%4;
    tri2_inds[1]=side_inds[max_i];
    max_i=(max_i+1)%4;
    tri2_inds[2]=side_inds[max_i];    
  }
  

/*
             0-----------1-----------5
             |       1   |       3   |
             |   \ /     |   \ /     |
             |   / \     |   / \     |
             |  0        |  2        |
             3-----------2-----------6
             |       5   |
             |   \ /     |
             |   / \     |
             |  4        |
             7-----------6
             |      7    |
             |   \ /     |
             |   / \     |
             |  6        |
 7-----------4-----------5
 |       9   |      11   |
 |   \ /     |   \ /     |
 |   / \     |   / \     |
 |  8        |  10       |
 3-----------0-----------1

*/
// Assumes pnts contains 8 points and thus has a size of 8*3
// Assumes tris contains 12 triangles and thus has a size of 12*3*3
// Converts hexahdron point list to 12 triangles. Here the ids are used to
// decide which way to split the sides into triangles. This is to 
// make sure neighboring hexs have touching faces split into triangles
// in the same way.  
  void convert_hex_using_ids(double *pnts, int *ids, double *tris) {
#define COPYP2T(t,tt,tp,p,pp) (t+9*tt+3*tp)[0]=(p+3*pp)[0]; (t+9*tt+3*tp)[1]=(p+3*pp)[1]; (t+9*tt+3*tp)[2]=(p+3*pp)[2];
    int side_ids[4];
    int side_inds[4];
    int tri1_inds[3];
    int tri2_inds[3];

    // Triangulate side 1
    side_ids[0]=ids[0]; side_inds[0]=0;
    side_ids[1]=ids[3]; side_inds[1]=3;
    side_ids[2]=ids[2]; side_inds[2]=2;
    side_ids[3]=ids[1]; side_inds[3]=1;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,0,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,0,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,0,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,1,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,1,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,1,2,pnts,tri2_inds[2]);  


    // Triangulate side 2
    side_ids[0]=ids[1]; side_inds[0]=1;
    side_ids[1]=ids[2]; side_inds[1]=2;
    side_ids[2]=ids[6]; side_inds[2]=6;
    side_ids[3]=ids[5]; side_inds[3]=5;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,2,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,2,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,2,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,3,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,3,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,3,2,pnts,tri2_inds[2]);  


    // Triangulate side 3
    side_ids[0]=ids[3]; side_inds[0]=3;
    side_ids[1]=ids[7]; side_inds[1]=7;
    side_ids[2]=ids[6]; side_inds[2]=6;
    side_ids[3]=ids[2]; side_inds[3]=2;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,4,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,4,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,4,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,5,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,5,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,5,2,pnts,tri2_inds[2]);  


    // Triangulate side 4
    side_ids[0]=ids[7]; side_inds[0]=7;
    side_ids[1]=ids[4]; side_inds[1]=4;
    side_ids[2]=ids[5]; side_inds[2]=5;
    side_ids[3]=ids[6]; side_inds[3]=6;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,6,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,6,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,6,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,7,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,7,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,7,2,pnts,tri2_inds[2]);  


    // Triangulate side 5
    side_ids[0]=ids[7]; side_inds[0]=7;
    side_ids[1]=ids[3]; side_inds[1]=3;
    side_ids[2]=ids[0]; side_inds[2]=0;
    side_ids[3]=ids[4]; side_inds[3]=4;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,8,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,8,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,8,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,9,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,9,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,9,2,pnts,tri2_inds[2]);  


    // Triangulate side 6
    side_ids[0]=ids[4]; side_inds[0]=4;
    side_ids[1]=ids[0]; side_inds[1]=0;
    side_ids[2]=ids[1]; side_inds[2]=1;
    side_ids[3]=ids[5]; side_inds[3]=5;
    
    get_tris_for_side(side_ids, side_inds, tri1_inds, tri2_inds);

    COPYP2T(tris,10,0,pnts,tri1_inds[0]);  
    COPYP2T(tris,10,1,pnts,tri1_inds[1]);  
    COPYP2T(tris,10,2,pnts,tri1_inds[2]);  

    COPYP2T(tris,11,0,pnts,tri2_inds[0]);  
    COPYP2T(tris,11,1,pnts,tri2_inds[1]);  
    COPYP2T(tris,11,2,pnts,tri2_inds[2]);  

#undef COPYP2T
}


 // HELPER FUNCTION
 // Didn't put this in class for now, so that it's easy to remove if we want to move this class. 

 Phedra create_phedra_from_elem(const MeshObj *elem, MEField<> *cfield) {
#define  MAX_NUM_PHEDRA_NODES 40
#define  MAX_NUM_PHEDRA_COORDS_3D (3*MAX_NUM_PHEDRA_NODES) 


    // Get coord info from element
    int num_nodes;
    double node_coords[MAX_NUM_PHEDRA_COORDS_3D];
    int    node_ids[MAX_NUM_PHEDRA_NODES];
    get_elem_coords_and_ids(elem, cfield, 3, MAX_NUM_PHEDRA_NODES, &num_nodes, node_coords, node_ids);    

    // Convert to Phedra based on shape
    if (num_nodes==8) {    
     double hex_tris[12*3*3];
     convert_hex_using_ids(node_coords, node_ids, hex_tris);

     return Phedra(12,hex_tris);
    } else if (num_nodes==4) {

      return Phedra(node_coords,
                    node_coords+3, 
                    node_coords+6,
                    node_coords+9);  
    } else {
      // Only handle Hexahedrons right now
      Throw() << " Right now 3D conservative only works with hexahedrons";
    }


#undef MAX_NUM_PHEDRA_NODES
#undef MAX_NUM_PHEDRA_COORDS_3D
  }




}
