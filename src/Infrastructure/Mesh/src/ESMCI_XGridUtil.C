// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_PatchRecovery.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/ESMCI_MeshRegrid.h>
#include <Mesh/include/ESMCI_MathUtil.h>

#include <cmath>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <algorithm>

#include <ESMCI_VM.h>
#include "ESMCI_Macros.h"

namespace ESMCI{

//
// Most of the algorithm comes from: http://paulbourke.net/geometry/
// except the Weiler algorithm motivated by the intersection version.
// These algorithms are slightly treaked for XGrid implementation.
//

inline double dot(const xvector & v1, const xvector & v2){
  return v1.c[0]*v2.c[0]+v1.c[1]*v2.c[1]+v1.c[2]*v2.c[2];
}
inline xvector cross(const xvector & v1, const xvector & v2){
  // cross(i) = epsilon(i,j,k)*v1,j*v2,k     i,j,k=1..3
  return xvector(v1.c[1]*v2.c[2]-v1.c[2]*v2.c[1], 
                 v1.c[2]*v2.c[0]-v1.c[0]*v2.c[2],
                 v1.c[0]*v2.c[1]-v1.c[1]*v2.c[0]);
}

inline double metric(const xvector & v){
  return v.metric();
}

inline xvector operator -(const xvector & v1, const xvector & v2){
  return xvector(v1.c[0]-v2.c[0], v1.c[1]-v2.c[1], v1.c[2]-v2.c[2]);
}
inline xvector operator +(const xvector & v1, const xvector & v2){
  return xvector(v1.c[0]+v2.c[0], v1.c[1]+v2.c[1], v1.c[2]+v2.c[2]);
}
inline xvector operator *(const double ratio, const xvector & v){
  return xvector(v.c[0]*ratio, v.c[1]*ratio, v.c[2]*ratio);
}

inline xvector operator /(const xvector & v, const double ratio){
  return xvector(v.c[0]/ratio, v.c[1]/ratio, v.c[2]/ratio);
}

double polygon::area(int sdim) const {
  double split_area = 0.;
  if(sdim == 2){
    double * coords = new double[sdim * points.size()];
    polygon_to_coords(*this, sdim, coords);
    split_area = area_of_flat_2D_polygon(points.size(), coords);
    delete[] coords;
    return split_area;
  }else if(sdim == 3){
    double * coords = new double[sdim * points.size()];
    polygon_to_coords(*this, sdim, coords);
    split_area = great_circle_area(points.size(), coords);
    delete[] coords;

    // great_circle_area does not take CCW sense into account
    // dot of the radial normal with polygon normal gives a sense if the points are CCW arranged
    int np = points.size(); double ccw_sense = 0.;
    for(int i = 0; i < np-2; i++)
      ccw_sense += dot(cross(points[(i+1)%np]-points[i], points[(i+2)%np]-points[(i+1)%np]), points[i]);
    return (ccw_sense > 0)? split_area: (-split_area);
  }
  return split_area;
}

xpoint polygon::centroid(int sdim) const {
  double area = std::abs(this->area(sdim));
  int n = this->size();
  double sum[3]; for(int i = 0; i < 3; i ++) sum[i] = 0.;

  if(sdim == 2){
    double tmp;
    for(int i = 0; i < n; i ++){
      tmp = points[i].c[0]*points[(i+1)%n].c[1] - points[(i+1)%n].c[0]*points[i].c[1];
      sum[0] += (points[i].c[0]+points[(i+1)%n].c[0])*tmp;
      sum[1] += (points[i].c[1]+points[(i+1)%n].c[1])*tmp;
    }
    for(int i = 0; i < 3; i ++) sum[i] /= 6.*area;
    return xpoint(sum, sdim);
  }else if(sdim == 3){
    //translate the center of the coordinate system to points[0]
    double tmp;
    for(int i = 0; i < n-2; i ++){
      tmp = tri_area(points[i].c, points[(i+1)%n].c, points[(i+2)%n].c);
      sum[0] += (points[i].c[0]+points[(i+1)%n].c[0]+points[(i+2)%n].c[0])*tmp;
      sum[1] += (points[i].c[1]+points[(i+1)%n].c[1]+points[(i+2)%n].c[1])*tmp;
      sum[2] += (points[i].c[2]+points[(i+1)%n].c[2]+points[(i+2)%n].c[2])*tmp;
    }
    for(int i = 0; i < 3; i ++) {
      sum[i] /= 3.*area;
    } 
    return xpoint(sum, sdim);
  }else
    Throw() << "Cannot handle sdim > 3\n";
}

void sintd_cell::get_centroid(double * centroid, int sdim, int pdim){
  int n = nodes.size();
  double * points = new double[sdim*n];
  if(n <= 2) Throw() << "sintd_cell: get_centroid(): number of nodes must be greater than 2.\n";

  std::vector<sintd_node *>::iterator it = nodes.begin();
  for(int i=0; it != nodes.end(); it++, i++)
    std::memcpy(points+i*sdim, (*it)->get_coord(), sdim*sizeof(double));

  polygon res_poly;
  coords_to_polygon(n, points, sdim, res_poly);
  std::memcpy(centroid, res_poly.centroid(sdim).c, sdim*sizeof(double));

  delete[] points;

}
/**
 *\brief check if two line segments intersect
 * @param[in] p1            start point of first line segment
 * @param[in] p2            end   point of first line segment
 * @param[in] q1            start point of first line segment
 * @param[in] q2            end   point of first line segment
 * @param[out] intersect    intersection coord
 * @param[out] inbound      whether the intersection is outbound or inbound
 * @param[out] on_p_seg     whether the intersection is on p line segment
 * @param[out] on_q_seg     whether the intersection is on q line segment
 * @return                  if the p and q intersects
 */
bool line_intersect_2D_2D(double *p1, double *p2, double *q1, double *q2, double *intersect, 
  int & inbound, bool & on_p_seg, bool & on_q_seg){

  double mua,mub;
  double denom,numera,numerb;
  double epsilon = 1.e-20;
  inbound = -1;
  on_p_seg = true;
  on_q_seg = true;

  denom  = (q2[1]-q1[1]) * (p2[0]-p1[0]) - (q2[0]-q1[0]) * (p2[1]-p1[1]);
  numera = (q2[0]-q1[0]) * (p1[1] - q1[1]) - (q2[1]-q1[1]) * (p1[0]-q1[0]);
  numerb = (p2[0]-p1[0]) * (p1[1]-q1[1]) - (p2[1]-p1[1]) * (p1[0]-q1[0]);

  /* Are the line coincident? Do not consider the lines intersecting in this case. */
  if (std::abs(numera) < epsilon && std::abs(numerb) < epsilon && std::abs(denom) < epsilon) {
    //intersect[0] = (p1[0] + p2[0]) / 2;
    //intersect[1] = (p1[1] + p2[1]) / 2;
    return false;
  }

  /* Are the line parallel */
  if (std::abs(denom) < epsilon) {
    intersect[0] = 0.;
    intersect[1] = 0.;
    return false;
  }

  /* Is the intersection outside of the the segments */
  mua = numera / denom;
  mub = numerb / denom;
  if (mua < 0 || mua > 1 || mub < 0 || mub > 1) {
    intersect[0] = 0.;
    intersect[1] = 0.;
    return false;
  }

  // if intersection is one of the end points
  if(std::abs(mua) < epsilon || std::abs(mua-1.) < epsilon)
    on_p_seg = false; 

  if(std::abs(mub) < epsilon || std::abs(mub-1.) < epsilon) 
    on_q_seg = false; 

  intersect[0] = p1[0] + mua * (p2[0] - p1[0]);
  intersect[1] = p1[1] + mua * (p2[1] - p1[1]);

  xvector v1 = xvector(p2[0]-p1[0], p2[1]-p1[1], 0.);
  xvector v2 = xvector(q2[0]-q1[0], q2[1]-q1[1], 0.);

  inbound = 1;
  if(dot(v1, v2.normal2D()) < 0.) inbound = 2; // v1 going into v2 in CCW sense

  return true;
}


// Intersects between the line a and the seqment s
// where both line and segment are great circle lines on the sphere represented by
// 3D cartesian points.
//  [sin sout] are the ends of a line segment
// returns true if the lines could be intersected, false otherwise.
//
// Note: this test does not work well for line segments on sphere because the underlying
// theory computes the shortest distance between 2 3D line segments, the minima of this
// shortest distance for intersection is inaccurate for line segments with end points on sphere.
bool line_intersect_2D_3D(double *a1, double *a2, double *q1, double *q2, double *q3,
                     double *intersect, 
  int & inbound, bool & on_p_seg, bool & on_q_seg){

  // Do this intersection by reprsenting the line a1 to a2 as a plane through the
  // two line points and the origin of the sphere (0,0,0). This is the
  // definition of a great circle arc.
  double plane[9];
  double plane_p[2];
  double t,u;
  double epsilon = 1.e-15;

  // Load points defining plane into variable (these are supposed to be in counterclockwise order)
  plane[0]=q1[0];
  plane[1]=q1[1];
  plane[2]=q1[2];
  plane[3]=q2[0];
  plane[4]=q2[1];
  plane[5]=q2[2];
  plane[6]=0.0;
  plane[7]=0.0;
  plane[8]=0.0;

  // Intersect the segment with the plane
  if(!intersect_tri_with_line(plane, a1, a2, plane_p, &t))
     return false;

  //if( (t < 0) || (t > 1) )
  if( (t < -epsilon) || (t > (1+epsilon)) )
    return false;

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
  if(!intersect_tri_with_line(plane, q1, q2, plane_p, &u))
     return false;

  if( (u < -epsilon) || (u > (1+epsilon)) )
    return false;

  xvector p1 = xvector(a2[0]-a1[0], a2[1]-a1[1], a2[2]-a1[2]);
  xvector v1 = xvector(q2[0]-q1[0], q2[1]-q1[1], q2[2]-q1[2]);
  xvector v2 = xvector(q3[0]-q2[0], q3[1]-q2[1], q3[2]-q2[2]);

  // The two planes are coincidental
  double coincident = metric(cross(cross(xvector(a1,3), xvector(a2,3)), cross(xvector(q1,3), xvector(q2,3))));
  if(coincident < 1.e-15) return false;

  double sense = dot(cross(v1,v2), cross(v1,p1));
  //if(std::abs(sense) < epsilon) return false;

  // Calculate point of intersection
  intersect[0]=q1[0] + u*(q2[0]-q1[0]);
  intersect[1]=q1[1] + u*(q2[1]-q1[1]);
  intersect[2]=q1[2] + u*(q2[2]-q1[2]);
  double norm = xvector(intersect[0], intersect[1], intersect[2]).metric();
  for(int i = 0; i < 3; i ++) intersect[i] /= norm;

  inbound = 1;
  // sense of inbound: (v1 x v2).(v1 x p1) > 0
  //if(sense > 0. && (t>0 && t<1)) inbound = 2; // v1 going into v2 in CCW sense
  if(sense > 0) inbound = 2; // v1 going into v2 in CCW sense

  return true;
}

/**
 *\brief check if two line segments intersect
 * @param[in] p1            start point of first line segment
 * @param[in] p2            end   point of first line segment
 * @param[in] q1            start point of first line segment
 * @param[in] q2            end   point of first line segment
 * @param[out] intersect    intersection coord
 * @param[out] inbound      whether the intersection is outbound or inbound
 * @param[out] on_p_seg     whether the intersection is on p line segment
 * @param[out] on_q_seg     whether the intersection is on q line segment
 * @return                  if the p and q intersects
 */
bool line_intersect_2D_3Da(double *p1, double *p2, double *q1, double *q2, double *q3, double *intersect, 
  int & inbound, bool & on_p_seg, bool & on_q_seg){

  double mua,mub;
  double denom,numer;
  double epsilon = 1.e-20;
  inbound = -1;
  on_p_seg = true;
  on_q_seg = true;
  double d1343,d4321,d1321,d4343,d2121;

  xvector a1 = xvector(p1[0],p1[1],p1[2]);
  xvector a2 = xvector(p2[0],p2[1],p2[2]);
  xvector b1 = xvector(q1[0],q1[1],q1[2]);
  xvector b2 = xvector(q2[0],q2[1],q2[2]);
  xvector v1 = xvector(p2[0]-p1[0], p2[1]-p1[1], p2[2]-p1[2]);
  xvector v2 = xvector(q2[0]-q1[0], q2[1]-q1[1], q2[2]-q1[2]);
  xvector v3 = xvector(q3[0]-q2[0], q3[1]-q2[1], q3[2]-q2[2]);

  // /*
  //  nA = dot(cross(B2-B1,A1-B1),cross(A2-A1,B2-B1));
  //  nB = dot(cross(A2-A1,A1-B1),cross(A2-A1,B2-B1));
  //  d = dot(cross(A2-A1,B2-B1),cross(A2-A1,B2-B1));
  //  A0 = A1 + (nA/d)*(A2-A1);
  //  B0 = B1 + (nB/d)*(B2-B1);
  // */
  // denom  = dot(cross(a2-a1,b2-b1), cross(a2-a1,b2-b1));
  // numera = dot(cross(b2-b1,a1-b1), cross(a2-a1,b2-b1));
  // numerb = dot(cross(a2-a1,a1-b1), cross(a2-a1,b2-b1));

  if(v1.metric() < epsilon)
    return false;
  
  if(v2.metric() < epsilon)
    return false;

  d1343 = dot(a1-b1, b2-b1);
  d4321 = dot(b2-b1, a2-a1);
  d1321 = dot(a1-b1, a2-a1);
  d4343 = dot(v2,v2);
  d2121 = dot(v1,v1);

  denom = d2121 * d4343 - d4321 * d4321;
  /* Are the line parallel */
  if (std::abs(denom) < epsilon)
    return false;

  numer = d1343 * d4321 - d1321 * d4343;

  mua = numer / denom;
  mub = (d1343 + d4321 * mua) / d4343;

  /* Is the intersection outside of the the segments */
  if (mua < 0 || mua > 1 || mub < 0 || mub > 1)
    return false;

  // if intersection is one of the end points
  if(std::abs(mua) < epsilon || std::abs(mua-1.) < epsilon)
    on_p_seg = false; 

  if(std::abs(mub) < epsilon || std::abs(mub-1.) < epsilon) 
    on_q_seg = false; 

  xvector a0 = a1 + mua*v1;
  xvector b0 = b1 + mub*v2;
  xvector an = a0.normalize();
  xvector bn = b0.normalize();

  // if an and bn does not coincide, then a and b does not intersect.
  double diff = std::abs(metric(bn-an));
  if(diff > 1.e-5) return false;
  std::memcpy(intersect, bn.c, 3*sizeof(double));

  inbound = 1;
  double sense = dot(cross(v2,v3), cross(v2,v1));
  // sense of inbound: (v1 x v2).(v1 x p1) > 0
  if(sense > 0.) inbound = 2; // v1 going into v2,v3 in CCW sense

  return true;
}

/**
 *\brief                    insert intersection point to a self-closed polgon point list
 * @param[in,out] final_nodes self-closed polygon
 * @param[in] nodes         original polygon
 * @param[in] i             index of starting point of the line segment in original polygon
 * @param[in] intersect     intersection coord
 * @param[in] n_inter       rough estimate of number of intersection point for labeling
 * @param[in] inbound       the intersection point is outbound or inbound
 * @return                  success or failure
 */
int insert_intersect(int pdim, int sdim, std::list<xpoint> & final_nodes, const std::vector<xpoint> & nodes, int i, 
  double * intersect, int n_inter, int inbound){

  double epsilon = 1.e-10;
  xpoint xp;
  if(sdim == 2)
    xp = xpoint(intersect[0], intersect[1], '1'+n_inter, true, inbound);
  else
    xp = xpoint(intersect[0], intersect[1], intersect[2], '1'+n_inter, true, inbound);

  // shortcut to see if intersect is already in final_nodes list
  std::list<xpoint>::iterator sit = std::find(final_nodes.begin(), final_nodes.end(), xp);
  if(sit != final_nodes.end()){
    if(inbound > sit->inbound) sit->inbound = inbound;
    sit->intersection = true;
    return 0;
  }

  // locate the begining and ending point in final node list that are also in original polygonal list
  // Since the final node list are updated with intersection points, it will have more segments.
  // Our job is to find the segment that contains the intersection point and insert it if necessary.
  std::list<xpoint>::iterator it = std::find(final_nodes.begin(), final_nodes.end(), nodes[i]);
  if(it == final_nodes.end()) Throw() << "Failed to locate p(subject) vertex.\n";

  xpoint end_point;
  if((i+1) == nodes.size()) end_point = nodes[0];
  else end_point = nodes[i+1];

  std::list<xpoint>::iterator it_end = std::find(final_nodes.begin(), final_nodes.end(), end_point);

  // vector of intersection point, find the begin node and end node that holds it.
  xvector vp = xvector(xp);
  int success = -1;
  for(; it != it_end;){
    std::list<xpoint>::iterator start_point = it;
    std::list<xpoint>::iterator end_point = ++it;
    if(end_point == final_nodes.end()) end_point = final_nodes.begin(); // circular
    xvector v1=xvector(*start_point);
    xvector v2=xvector(*end_point);
    double d1 = metric(vp-v1);
    double d2 = metric(v2-v1);
    if(d2 == 0.) 
      Throw() << "Cannot have degenerate polygon (2+ nodes have identical coords)\n";
    double ratio = d1/d2;
    if((ratio > epsilon) && (ratio < (1-epsilon)) ){
      final_nodes.insert(end_point, xp);
      success = 0;
      break;
    }else if(std::abs(ratio) < epsilon || std::abs(d1-d2) < epsilon){
      // Don't insert a new point, simply mark start_point as an intersection point and compute new inbound value
      //final_nodes.insert(start_point, xpoint(intersect[0], intersect[1], '1'+n_inter, true, inbound));
      if(inbound > start_point->inbound) start_point->inbound=inbound;
      start_point->intersection=true;
      success = 0;
      break;
    }else if( std::abs(ratio-1.) < epsilon ) {
      if(inbound > end_point->inbound) end_point->inbound=inbound;
      end_point->intersection=true;
      success = 0;
      break;
    }
    it = end_point;
  }

  return success;
}

/**
 *\brief                    test if a point is inside of a polygon, *on edge point* is considered not inside.
 * @param[in] nvert         number of vertices of polygon
 * @param[in] p             polygon coordinates
 * @param[in] point         point coordinate
 * @return                  point inside or outside of polygon
 * This method does not work well for points on a sphere when the total angel will always be slightly less than 2Pi
 * Use the walk_polygon method instead which is less sensitive to FPN
 */
bool check_angle_sum(int sdim, int n, const double * const p, const double * const point)
{
  int i;
  double m1,m2;
  double anglesum=0,costheta;
  double epsilon = 1.e-10;
  double twopi = 4*std::asin(1.);
  xpoint p1,p2;

  for (i=0;i<n;i++) {

    for(int j = 0; j < sdim; j ++){
      p1.c[j] = p[sdim*i+j] - point[j];
      p2.c[j] = p[sdim*((i+1)%n)+j] - point[j];
    }

    m1 = metric(p1);
    m2 = metric(p2);
    if (m1*m2 <= epsilon)
      return false;   /* We are on a node, consider this outside: 0 instead of 2pi */
    else
      costheta = dot(p1,p2) / (m1*m2);

    anglesum += acos(costheta);
  }
  if( std::abs(anglesum - twopi) < epsilon ) return true;
  else return false;
}

// return value: 0 -> outside, 1 -> inside, 2 -> on edge
unsigned int walk_polygon(int sdim, int n, const double * const p, const double * const point)
{
  // if point is always on left hand side (sense > 0) of polygon during CCW walk, then it's inside
  unsigned int r = 2;
  for(int i = 0; i < n; i ++){
    //xvector midpoint = (xvector(p+i*sdim, sdim)+xvector(p+((i+1)%n)*sdim, sdim))/2.;
    //double sense = dot(cross(xvector(p+i*sdim, sdim), xvector(p+((i+1)%n)*sdim, sdim)-xvector(p+i*sdim, sdim)), xvector(point, sdim)-midpoint.normalize());
    xvector midpoint = xvector(p+i*sdim, sdim);
    double sense = dot(cross(xvector(p+i*sdim, sdim), xvector(p+((i+1)%n)*sdim, sdim)-xvector(p+i*sdim, sdim)), xvector(point, sdim)-midpoint);
    // [(p(i+1)-p(i))x(p(i+2)-p(i+1))].[(p(i+1)-p(i))x(pp-p(i))]
    //double sense = dot( cross((xvector(p+((i+1)%n)*sdim, sdim)-xvector(p+i*sdim, sdim)),
    //                          (xvector(p+((i+2)%n)*sdim, sdim)-xvector(p+((i+1)%n)*sdim, sdim))), 
    //                    cross((xvector(p+((i+1)%n)*sdim, sdim)-xvector(p+i*sdim, sdim)),
    //                          (xvector(point, sdim)-xvector(p+i*sdim, sdim)))); 
    //double sense = dot(xvector(p+i*sdim, sdim), 
    //                  cross((xvector(p+((i+1)%n)*sdim, sdim)-xvector(p+i*sdim, sdim)),
    //                        (xvector(point, sdim)-xvector(p+i*sdim, sdim)))); 
                       
    if(std::abs(sense) < 1.e-15)      // consider the point on edge if sense is really small, use the other point to determine if it's truely inside or not.
      return 2;
    if(sense < 0.) 
      return 0;
  }
  return 1;
}

/**
 *\brief                    test if a point is inside of a polygon, *on edge point* is considered not inside.
 * @param[in] nvert         number of vertices of polygon
 * @param[in] poly_cd       polygon coordinates
 * @param[in] point         point coordinate
 * @return                  point inside or outside of polygon
 */
unsigned int point_in_poly(int pdim, int sdim, int nvert, const double * const poly_cd, const double * const point){
  if(sdim == 2){
    int i, j; unsigned int c = 0;
    double testx = point[0], testy = point[1];
    xpoint xp(point[0], point[1], 'p');
    for (i = 0, j = nvert-1; i < nvert; j = i++) {

      // Pathological case: treat a point lying on a vertex as outside
      if(xp == xpoint(poly_cd+i*sdim,2) ||
         xp == xpoint(poly_cd+j*sdim,2) ) return 0;

      if ( ((poly_cd[i*sdim+1] > testy) != (poly_cd[j*sdim+1]>testy)) &&
     (testx < (poly_cd[j*sdim]-poly_cd[i*sdim]) * (testy - poly_cd[i*sdim+1]) / 
       (poly_cd[j*sdim+1] - poly_cd[i*sdim+1]) + poly_cd[i*sdim]) )
     // if ( ((verty[i]>testy) != (verty[j]>testy)) &&
     //(testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
         c = !c;
    }
    return c;
  }else if(sdim == 3){
    //return check_angle_sum(sdim, nvert, poly_cd, point);
    return walk_polygon(sdim, nvert, poly_cd, point);
  }else
    Throw() << "cannot handle spatial dimension over 3\n";
}

/**
 *\brief                    test if a point is inside of a polygon, *on edge point* is considered not inside.
 * @param[in] sdim          number of spatial    dimension
 * @param[in] poly          polygon
 * @param[in] xpoint        point
 * @return                  point inside or outside of polygon
 */
unsigned int point_in_poly(int pdim, int sdim, const polygon & poly, const xpoint & point){
  double * coords = new double[sdim*poly.size()];
  polygon_to_coords(poly, sdim, coords);
  unsigned int res = point_in_poly(pdim, sdim, poly.size(), coords, point.c);
  delete [] coords;
  return res;
}

/**
 *\brief                    construct a concave polygon from an enclosing polygon and internal polygon
 * @param[in] subject       enclosing polygon
 * @param[in] clip          internal polygon
 * @return                  a concave polygon with a hold inside defined by the clip polygon
 */
polygon make_concave_polygon(const int pdim, const int sdim, const std::vector<xpoint> & subject, 
  const std::vector<xpoint> & clip){
  double * cd = new double [sdim*(subject.size()+clip.size())];
  polygon_to_coords(polygon(subject), sdim, cd);
  // change to clock wise direction for inner polygon which contains a hole.
  polygon_to_coords(polygon(clip), sdim, cd+sdim*subject.size(), false);
  polygon result;
  coords_to_polygon(subject.size()+clip.size(), cd, sdim, result);

  return result;
}

/**
//
// Test 3 spatial relationship between 2 polygons with this api
// input argument: subject, clip polygons
// return: if subject and clip are disjont
// output argument: s_contains_c, if s contains c
// output argument: c_contains_s, if c contains s
*/
bool disjoint(int pdim, int sdim, const std::vector<xpoint> & subject, const std::vector<xpoint> & clip, 
  bool & s_contains_c, bool & c_contains_s){

  int i; int num_clip_p=clip.size(), num_subject_p=subject.size();
  bool disjoint = true;
  s_contains_c = true;
  c_contains_s = true;

  double * subject_cd = new double[sdim*subject.size()];
  polygon_to_coords(polygon(subject), sdim, subject_cd);
  double * clip_cd = new double[sdim*clip.size()];
  polygon_to_coords(polygon(clip), sdim, clip_cd);

  // check if any p point is in q
  // if any s point is in c, disjoint is false
  // if any s point is outside c, c contains s is false
  // if all s point are on c, disjoint is false, also implies c contains s
  int np = 0;
  for(int i = 0; i < subject.size(); i ++){
    unsigned int r = point_in_poly(pdim, sdim, num_clip_p, clip_cd, subject_cd+i*sdim);
    if(r == 1)
      // if a point is found contained
      disjoint = false;
    else if(r == 0)
      c_contains_s = false;
    else if(r == 2)
      np ++;
  }
  if(np == subject.size()) disjoint = false;

  // check if any q point is in p
  np = 0;
  for(int i = 0; i < clip.size(); i ++){
    unsigned int r = point_in_poly(pdim, sdim, num_subject_p, subject_cd, clip_cd+i*sdim);
    if(r == 1)
      // if a point is found contained
      disjoint = false;
    else if(r == 0)
      s_contains_c = false;
    else if(r == 2)
      np ++;
  }
  if(np == clip.size()) disjoint = false;

  // tally the results, only one scenario can happen unless c and s are identical
  int n_cond = 0;
  if(disjoint) n_cond ++;
  if(s_contains_c) n_cond ++;
  if(c_contains_s) n_cond ++;
  if(n_cond > 1 && !(c_contains_s && s_contains_c)) Throw() << "Invalid spatial relation between subject and clip\n";

  delete [] subject_cd, clip_cd;
  return disjoint;
}

// Assume a counter clock wise order of points in p and q
int weiler_clip_difference(int pdim, int sdim, int num_p, double *p, int num_q, double *q, 
  std::vector<polygon> & difference){

  // return if the subject polygon is empty
  if(num_p < 3) return 0;

  // prepare subject and clip vertex lists
  std::vector<xpoint> pnodes, qnodes;
  if(sdim == 2){
    for(int i = 0; i < num_p; i ++)
      pnodes.push_back(xpoint(*(p+sdim*i), *(p+sdim*i+1), 'A'+i)); 
    for(int i = 0; i < num_q; i ++)
      qnodes.push_back(xpoint(*(q+sdim*i), *(q+sdim*i+1), 'a'+i)); 
  }else if(sdim == 3){
    for(int i = 0; i < num_p; i ++)
      pnodes.push_back(xpoint(*(p+sdim*i), *(p+sdim*i+1), *(p+sdim*i+2), 'A'+i)); 
    for(int i = 0; i < num_q; i ++)
      qnodes.push_back(xpoint(*(q+sdim*i), *(q+sdim*i+1), *(q+sdim*i+2), 'a'+i)); 
  }

  // return the subject polygon if the clip polygon is empty
  if(num_p >= 3 && num_q < 3) {
    difference.push_back(polygon(pnodes));
    return 0;
  }
  
  //double * sintd_coords = new double[120]; int num_sintd_nodes; 
  //double * tmp_coords=new double[120];
  //if(sdim == 3)
  //  intersect_convex_2D_3D_sph_gc_poly(num_p, p,
  //                                   num_q, q,
  //                                   tmp_coords,
  //                                   &num_sintd_nodes, sintd_coords); 
  //delete[] tmp_coords;
  
  // phase 1, find all intersection points, note degenerated points too
  std::list<xpoint> final_pnodes, final_qnodes, degenerated;

  final_pnodes.resize(num_p);
  final_qnodes.resize(num_q);
  std::copy(pnodes.begin(), pnodes.end(), final_pnodes.begin());
  std::copy(qnodes.begin(), qnodes.end(), final_qnodes.begin());

  double * intersect = new double[sdim];
  int inbound = -1; bool on_p_seg, on_q_seg;

  unsigned int n_inter = 0;
  unsigned int num_inbinter = 0;
  for(int i = 0; i < num_p; i ++){
    double *p1 = (pnodes[i].c);
    double *p2 = (pnodes[(i+1)%num_p].c);

    for(int j = 0; j < num_q; j ++){

      double *q1 = (qnodes[j].c);
      double *q2 = (qnodes[(j+1)%num_q].c); 
      double *q3 = (qnodes[(j+2)%num_q].c);

      bool result = false;
      if(sdim == 2)
        result = line_intersect_2D_2D(p1, p2, q1, q2, intersect, inbound, on_p_seg, on_q_seg);
      else{
        //result = line_intersect_2D_3Da(p1, p2, q1, q2, q3, intersect, inbound, on_p_seg, on_q_seg);
        result = line_intersect_2D_3D(p1, p2, q1, q2, q3, intersect, inbound, on_p_seg, on_q_seg);
      }

      if(result){

        insert_intersect(pdim, sdim, final_pnodes, pnodes, i, intersect, n_inter, inbound);
        insert_intersect(pdim, sdim, final_qnodes, qnodes, j, intersect, n_inter, inbound);

        n_inter++;
        if(inbound==2) num_inbinter++;
      }
    }
  }
  delete [] intersect;

  // reTally the number of num_inbinter, because intersection points can be coincidental
  n_inter = 0;
  num_inbinter = 0;
  {
    std::list<xpoint>::const_iterator it = final_pnodes.begin(), eit=final_pnodes.end();
    for(;it != eit; ++it){
      if(it->intersection){
        n_inter ++;
        if(it->inbound == 2) num_inbinter ++;
      }
    }
  }

  // First, handle the corner cases, no intersect, no inbound point or odd number of inter/inbound point
  // p: subject, q: clip
  // No intersection points => a) p and q are disjoint, return p 
  //                           b) q contains p, return nothing
  //                           c) p contains q, return concave polygon
  if(n_inter == 0 || num_inbinter == 0 || (n_inter == num_inbinter && num_inbinter%2)) {
    xpoint p_centroid = polygon(pnodes).centroid(sdim);
    xpoint q_centroid = polygon(qnodes).centroid(sdim);
    bool s_contains_c = false, c_contains_s = false;
    if(disjoint(pdim, sdim, pnodes, qnodes, s_contains_c, c_contains_s)){
      difference.push_back(polygon(pnodes));
      return 0;
    }
    if(c_contains_s || point_in_poly(pdim, sdim, qnodes, p_centroid) ) return 0; 
    if(s_contains_c || point_in_poly(pdim, sdim, pnodes, q_centroid) ){
      difference.push_back(make_concave_polygon(pdim, sdim, pnodes, qnodes));
      return 0;
    }
  }


  // At this point, we know we have a 'cliping' scenario to deal with
  if(false){ // debug: dump final lists

    std::list<xpoint>::const_iterator it = final_pnodes.begin(), eit=final_pnodes.end();
    for(;it != eit; ++it)
      std::cout << it->c[0] << ',' << it->c[1] << ',' << it->c[2] << ',' << it->label << std::endl;

    it = final_qnodes.begin(), eit=final_qnodes.end();
    for(;it != eit; ++it)
      std::cout << it->c[0] << ',' << it->c[1] << ',' << it->c[2] << ',' << it->label << std::endl;
  }

  // Phase 2, compute difference
  {

    unsigned int npts = 0;  // number of inbound points used from p(subject) list
    bool done = false;

    // Start with the subject polygon
    std::list<xpoint>::iterator it = final_pnodes.begin();
    std::list<xpoint> nodes;
    std::vector<xpoint> visited_inbnodes;
    bool on_subject = true;
    bool start = false;
    while(! done){
      
      // completed a loop, save this polygon
      if(it->visited && on_subject) {
        std::list<xpoint>::iterator tmpit = it;
        // clear visited attributes
        for(it = final_pnodes.begin(); it != final_pnodes.end(); ++it) it->visited = false;
        for(it = final_qnodes.begin(); it != final_qnodes.end(); ++it) it->visited = false;
        it = tmpit;

        polygon nodal_poly = polygon(nodes);

        // adjust degenerated polygons
        int num_nodes = nodal_poly.size();
        double * coords = new double[num_nodes*sdim];
        polygon_to_coords(nodal_poly, sdim, coords);
        if(sdim == 2) remove_0len_edges2D(&num_nodes, coords);
        if(sdim == 3) remove_0len_edges3D(&num_nodes, coords);
    
        if(num_nodes >=3){
          polygon res_poly;
          coords_to_polygon(num_nodes, coords, sdim, res_poly);
          if(res_poly.area(sdim) > 0.)
            difference.push_back(res_poly);
        }
        delete[] coords;

        nodes.clear();
        on_subject = true;
        start = false;
        it = std::find(final_pnodes.begin(), final_pnodes.end(), *it);
        if(it == final_pnodes.end()) Throw() << "it must be on final pnodes list\n";
        ++it;
      }

      // if after a traverse, all inbound intersection points are used, then exit
      if(!start && npts == num_inbinter){
        done = true;
        continue;
      }

      // if p is not an inbound intersection, 
      //   save p, mark it visited, switch to clip list and move to previous clip polygon node. 
      // else
      //   if(star loop) save, mark
      //   move to the next subject polygon node;
      if(on_subject){
        if(it->intersection && it->inbound==2){
          if(!start){ // if this inbound node has been visited in previous loop, continue on subject polygon
            std::vector<xpoint>::iterator it_tmp = std::find(visited_inbnodes.begin(), visited_inbnodes.end(), *it);
            if(it_tmp != visited_inbnodes.end()) {
              if(it == --final_pnodes.end()) it = final_pnodes.begin();
              else ++it;
              continue;
            }
          }
          visited_inbnodes.push_back(*it);
          start = true;
          it->visited = true;
          nodes.push_back(*it);
          it = std::find(final_qnodes.begin(), final_qnodes.end(), *it);
          if(it == final_qnodes.end()) Throw() << "it must be on final qnodes list\n";
          it->visited = true; // mark intersection point on q list at the same time
          if(it == final_qnodes.begin()) it = --final_qnodes.end();
          else --it;
          npts ++;
          on_subject = false;
        }
        else{
          if(start) {
            it->visited = true;
            nodes.push_back(*it);
          }
          if(it == --final_pnodes.end()) it = final_pnodes.begin();
          else ++it;
        }
      }else{
        // if q is an intersection, save q, mark it visited, switch to p(subject) list
        // and move to next subject polygon node
        // else move to the previous clip polygon node, save, mark visited, 
        if(it->intersection){
          it->visited = true;
          nodes.push_back(*it);
          it = std::find(final_pnodes.begin(), final_pnodes.end(), *it);
          if(it == final_pnodes.end()) Throw() << "it must be on final pnodes list\n";
          it->visited = true; // mark intersection point on p list at the same time
          if(it == --final_pnodes.end()) it = final_pnodes.begin();
          else ++it;
          on_subject = true;
        }else{
          if(start) {
            it->visited = true;
            nodes.push_back(*it);
          }
          if(it == final_qnodes.begin()) it = --final_qnodes.end();
          else --it;
        }   
      }
    }
  }

  return 0;

}


void compute_midmesh(std::vector<sintd_node *> & sintd_nodes, std::vector<sintd_cell *> & sintd_cells, int pdim, int sdim, Mesh *midmesh){

  // Debug
  //if(false){
  //  std::vector<sintd_node *>::iterator ib = sintd_nodes.begin();
  //  std::vector<sintd_node *>::iterator ie = sintd_nodes.end();
  //  
  //  for(; ib != ie; ib++){
  //    if((*ib)->get_dim() != 2 && (*ib)->get_dim() != 3) (*ib)->print();
  //  }
  //}
  //if(false){
  //  std::vector<sintd_cell *>::iterator ib = sintd_cells.begin();
  //  std::vector<sintd_cell *>::iterator ie = sintd_cells.end();
  //  
  //  for(; ib != ie; ib++){
  //    for(int i = 0; i < (*ib)->num_edges(); i ++)
  //      if((*ib)->operator[](i)->get_dim() != 2 && (*ib)->operator[](i)->get_dim() != 3) 
  //        (*ib)->operator[](i)->print();
  //  }
  //}
  // collect all the unique intersection points
  std::vector<sintd_node *> tmpnodes;
  std::stable_sort(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_less());
  { // make sure the genesis cells in the duplicates are pointing 
    // to the retained node. The two loops take linear time.
    std::vector<sintd_node *>::iterator it = sintd_nodes.begin();
    while(it != sintd_nodes.end()){

      tmpnodes.push_back(*it);
      std::vector<sintd_node *>::iterator itt = it+1;
      while(itt != sintd_nodes.end() && (**itt == **it)) {
        (*itt)->get_cell()->replace_node(*it);
        // delete the node pointed to; reset the pointer value
        delete (*itt); *itt = 0;
        itt++;
      }

      it = itt;
    }
  }
  sintd_nodes.clear(); sintd_nodes.resize(tmpnodes.size());
  std::copy(tmpnodes.begin(), tmpnodes.end(), sintd_nodes.begin());
  //sintd_nodes.erase(std::unique(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_equal()), sintd_nodes.end());

  Mesh & meshmid = *midmesh;
  meshmid.set_parametric_dimension(pdim);
  meshmid.set_spatial_dimension(sdim);
  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();

  int offset=0;
  { //reduction to compute the global ids
    int nnodes = sintd_nodes.size();
    int lrc = MPI_Scan((void *)&nnodes, (void*)&offset, 1, MPI_INT, MPI_SUM, 
      VM::getCurrent(&rc)->getMpi_c()); 
    if(lrc != 0) Throw() << "MPI_Scan failed to reduce the local offset.\n";
    offset -= nnodes;
  }

  // Add nodes to mesh for each intersection point
  std::vector<MeshObj *> node_list;
  int num_int = sintd_nodes.size();
  node_list.resize(num_int);

  for (int i=0; i<num_int; i++) {
    // Create new node in mesh object       
    MeshObj *node = new MeshObj(MeshObj::NODE,i+offset+1,i);
    node->set_owner(me); 

    // Add to mesh    
    meshmid.add_node(node, 0);

    // Add to local list
    node_list[i]=node;

    // cross reference
    sintd_nodes[i]->set_node(node);
  }

  // Register the nodal coordinate field.
  IOField<NodalField> *node_coord = meshmid.RegisterNodalField(meshmid, "coordinates", meshmid.spatial_dim());
  
  // Add coordinates to Nodes
  for (int i=0; i<num_int; i++) {
    double *c = node_coord->data(*(node_list[i]));

    for (int d=0; d<sdim; d++) {
      c[d]=sintd_nodes[i]->operator[](d);
    }
  }

  // collect all the unique intersection cells ? shouldn't need to do this
  //std::stable_sort(sintd_cells.begin(), sintd_cells.end());
  //sintd_cells.erase(unique(sintd_cells.begin(), sintd_cells.end()), sintd_cells.end());
  // Add Elements
  offset = 0;
  { //reduction to compute the global ids
    int ncells = sintd_cells.size();
    int lrc = MPI_Scan((void *)&ncells, (void*)&offset, 1, MPI_INT, MPI_SUM, 
      VM::getCurrent(&rc)->getMpi_c()); 
    if(lrc != 0) Throw() << "MPI_Scan failed to reduce the local offset.\n";
    offset -= ncells;
  }
  int num_cells = sintd_cells.size();
  std::vector<MeshObj *> elem_list;
  elem_list.resize(num_cells);
  for (int i=0; i<num_cells; i++) {
    // dump Cells
    // sintd_cells[i]->print(me, i+offset+1, i);

    // only add those that are cells, these should all be valid cells coming out of the valid check
    int num_edges = sintd_cells[i]->num_edges();
    if(num_edges < 3) continue;
    MeshObj *cell = new MeshObj(MeshObj::ELEMENT, i+offset+1, i);    // Mesh equivalent of Cell

    // Set Owner
    cell->set_owner(me);
    std::vector<MeshObj *> nodes;
    for(int in = 0; in < num_edges; in ++){
      nodes.push_back(sintd_cells[i]->operator[](in)->get_node());
    }

    meshmid.add_element(cell, nodes, num_edges, sintd_cells[i]->get_topo(sdim, pdim));
    elem_list[i] = cell;
  }

  // set up the fraction Field
  // Intersection cells should not be masked, fraction should all be 1.0
  Context ctxt; ctxt.flip();
  MEField<> *elem_frac = meshmid.RegisterField("elem_frac",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  MEField<> *elem_frac2 = meshmid.RegisterField("elem_frac2",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  // turn on elem_area so that user supplied area can be used during weight calculation
  MEField<> *elem_area = meshmid.RegisterField("elem_area",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  MEField<> *elem_centroid = meshmid.RegisterField("elem_centroid",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, sdim, true);

  // Finalize mesh
  meshmid.build_sym_comm_rel(MeshObj::NODE);
  //meshmid.build_sym_comm_rel(MeshObj::ELEMENT);

  meshmid.Commit();

  elem_frac = meshmid.GetField("elem_frac");
  elem_frac2 = meshmid.GetField("elem_frac2");
  elem_area = meshmid.GetField("elem_area");
  elem_centroid = meshmid.GetField("elem_centroid");
  for (int i=0; i<num_cells; i++) {
    double *frac = elem_frac->data(*(elem_list[i]));
    *frac = 1.0;
    double *frac2 = elem_frac2->data(*(elem_list[i]));
    *frac2 = 1.0;
    double *area = elem_area->data(*(elem_list[i]));
    *area = sintd_cells[i]->get_area();
    //std::cout << i << "th cell area: " << *area << "\n";
    double *centroid = elem_centroid->data(*(elem_list[i]));
    sintd_cells[i]->get_centroid(centroid, sdim, pdim);
  }

  //char str[64]; memset(str, 0, 64);
  //sprintf(str, "midMesh.vtk.%d", me);
  //WriteVTKMesh(meshmid, str);
  //WriteVTKMesh(srcmesh, "srcMesh.vtk");
  //WriteVTKMesh(dstmesh, "dstMesh.vtk");

  // free all memory
  std::vector<sintd_node *>::iterator it1=sintd_nodes.begin();
  for(;it1 != sintd_nodes.end(); it1++)
    delete *it1;
  std::vector<sintd_cell *>::iterator it2=sintd_cells.begin();
  for(;it2 != sintd_cells.end(); it2++)
    delete *it2;

}

// Compute cells of middle mesh based on clipping results
void compute_sintd_nodes_cells(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, struct Zoltan_Struct * zz){

  // bubble up the nodes and cells
  // cross reference the nodes and cells
  int rc;
  int proc, part;
  int me = VM::getCurrent(&rc)->getLocalPet();
  int npet = VM::getCurrent(&rc)->getNpets();
  
  if(npet != 1 && zz){
    // determine if we want to build the nodes/cells on this proc
    int num_owned = 0;
    std::map<int,int> proc_owned;
    for(int in = 0; in < num_sintd_nodes; in ++){
      Zoltan_LB_Point_PP_Assign(zz, sintd_coords+sdim*in, &proc, &part);
      if(proc == me) num_owned ++;
      proc_owned[proc]++; 
    }
    // if none of the nodes is owned by this proc, return
    if(num_owned == 0) return;
    // partially owned
    if(num_owned > 0 && num_owned < num_sintd_nodes){
      std::vector<proc_count> proc_counts;
      std::map<int,int>::const_iterator it=proc_owned.begin(),
        ie=proc_owned.end();
      for(;it != ie; it++) proc_counts.push_back(proc_count(it->first, it->second));
      std::vector<proc_count>::iterator it1 = std::find_if(proc_counts.begin(),
        proc_counts.end(), proc_count_equal(me));
      if(it1 == proc_counts.end()) return; // this shouldn't happen
      int my_count = it1->count;
      std::sort(proc_counts.begin(), proc_counts.end(), proc_count_less());
      // I don't own the most nodes
      if(proc_counts[0].proc != me && proc_counts[0].count > my_count) return;

      // almost there...
      std::vector<sintd_node *> sorted_nodes;
      for(int in = 0; in < num_sintd_nodes; in ++){
        sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*in);
        sorted_nodes.push_back(node);
      }
      std::sort(sorted_nodes.begin(), sorted_nodes.end(), sintd_node_less());
      Zoltan_LB_Point_PP_Assign(zz, sorted_nodes[0]->get_coord(), &proc, &part);
      if(proc != me){
        std::vector<sintd_node *>::iterator it=sorted_nodes.begin();
        for(;it != sorted_nodes.end(); it++)
          delete *it;
        return;
      }
    }
  }

  construct_sintd(area, num_sintd_nodes, sintd_coords, pdim, sdim, 
    sintd_nodes, sintd_cells);

}

void construct_sintd(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells){

  // Get rid of degenerate edges
  if(sdim == 2)
    remove_0len_edges2D(&num_sintd_nodes, sintd_coords);
  else
    remove_0len_edges3D(&num_sintd_nodes, sintd_coords);

  if(num_sintd_nodes < 3) return;

  // Ready to create nodes etc..
  // Break up the cell into triangles if it's got more than 4 nodes
  int break_threshold = 4;
  if(num_sintd_nodes > break_threshold){
    double * coords = new double[3*sdim];
    for(int i = 0; i < sdim; i ++)
      coords[i] = sintd_coords[i];

    for(int start_node = 1; start_node < num_sintd_nodes-1; start_node ++){
      std::vector<sintd_node *> cell_nodes;
      sintd_node * root_node = new sintd_node(sdim, sintd_coords);
      cell_nodes.push_back(root_node);

      for(int i = 0; i < 2; i ++){
        sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*(i+start_node));
        cell_nodes.push_back(node);
      }
      for(int i = 0; i < 2; i ++)
        for(int j = 0; j < sdim; j ++)
        coords[(i+1)*sdim+j] = cell_nodes[i+1]->operator[](j);

      double split_area;
      if(sdim == 2)
        split_area = area_of_flat_2D_polygon(3, coords);
      if(sdim == 3)
        split_area = great_circle_area(3, coords);
      if(split_area == 0.){
        // release the nodes of this cell and continue split process
        for(int i = 0; i < 3; i ++) delete cell_nodes[i];
        continue;
      }

      // Ready to add the split cell
      for(int i = 0; i < 3; i ++) sintd_nodes->push_back(cell_nodes[i]);

      // every cell keeps track of the nodes enclosing it
      sintd_cell * cell = new sintd_cell(split_area, cell_nodes);
      sintd_cells->push_back(cell);

      for(int in = 0; in < 3; in ++)
        (*(cell_nodes[in])).set_cell(cell);
    }
    delete[] coords; 

//    // Init variables for polygon triangulation
//    int num_tri=num_sintd_nodes-2;
//    int *tri_ind=new int[3*num_tri];
//    double *td=new double[sdim*num_sintd_nodes];
//    int *ti=new int[num_sintd_nodes];
//
//    // Triangulate polygon       
//    int rc;
//    if(sdim == 2) {
//      rc=triangulate_poly<GEOM_CART2D>(num_sintd_nodes, sintd_coords, td, ti, tri_ind);
//    } else if(sdim == 3) {
//      rc=triangulate_poly<GEOM_SPH2D3D>(num_sintd_nodes, sintd_coords, td, ti, tri_ind);
//    }
//
//    // handle return code
//    if (rc != ESMCI_TP_SUCCESS) {
//      if (rc==ESMCI_TP_DEGENERATE_POLY) return;
//      if (rc==ESMCI_TP_CLOCKWISE_POLY) Throw()<<"Clockwise Polygon in XGrid create\n";
//    }
//
//    //  space for triangle coords
//    double tri_coords[9];
//
//    // Make triangles. If too small, ignore
//    for(int i=0; i<num_tri; i++) {
//      int *tri=tri_ind+3*i;
//
//      // fill triangle coords
//      int t=0;
//      for(int j = 0; j < 3; j ++){
//        double *pnt=sintd_coords+sdim*tri[j];
//
//        for (int k=0; k<sdim; k++) {
//          tri_coords[t]=pnt[k];
//          t++;
//        }
//      }
//
//      // Calc area
//      double split_area;
//      if(sdim == 2) {
//        split_area = area_of_flat_2D_polygon(3, tri_coords);
//      } else if(sdim == 3) {
//        split_area = great_circle_area(3, tri_coords);
//      }
//
//      // If 0.0 then don't make triangle
//      if(split_area==0.0) continue;
//
//      // List of cell nodes
//      std::vector<sintd_node *> cell_nodes;
//      
//      // Add nodes to list
//      for (int j=0; j<3; j++) {
//        sintd_node * node = new sintd_node(sdim, tri_coords+sdim*j);
//        sintd_nodes->push_back(node);
//        cell_nodes.push_back(node);
//      }
//
//      // every cell keeps track of the nodes enclosing it
//      sintd_cell * cell = new sintd_cell(split_area, cell_nodes);
//      sintd_cells->push_back(cell);
//      
//      // nodes refer to their cell    
//      for(int in = 0; in < 3; in ++) {
//        (*(cell_nodes[in])).set_cell(cell);
//      }
//    }
//
//    delete[] tri_ind;
//    delete[] td;
//    delete[] ti;
      
  }else{
    // Make sure cell area is non-zero
    double split_area;
    if(sdim == 2)
      split_area = area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords);
    if(sdim == 3)
      split_area = great_circle_area(num_sintd_nodes, sintd_coords);
    if(split_area == 0.) return;

    // Ready to create nodes etc in one shot
    std::vector<sintd_node *> cell_nodes;
    for(int in = 0; in < num_sintd_nodes; in ++){
      sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*in);
      sintd_nodes->push_back(node);
      cell_nodes.push_back(node);
    }

    // every cell keeps track of the nodes enclosing it
    sintd_cell * cell = new sintd_cell(area, cell_nodes);
    sintd_cells->push_back(cell);

    // every node associated with this genesis cell refers to it
    for(int in = 0; in < num_sintd_nodes; in ++)
      (*(cell_nodes[in])).set_cell(cell);
  }
}

// A different path for online regrid
int online_regrid_xgrid(Mesh &srcmesh, Mesh &dstmesh, Mesh * midmesh, IWeights &wts,
                  int *regridConserve, int *regridMethod, 
                  int *unmappedaction) {

  // Conservative regridding
  // pole type and points, and scheme are not needed for cons. regridding
  // This is the current layer cut off subroutine
  int regridPoleType = 0;
  int regridPoleNPnts = 1;
  int regridScheme = 0;
  int map_type=0;
//WriteVTKMesh(srcmesh, "srcmesh");
//WriteVTKMesh(dstmesh, "dstmesh");
  if (!regrid(&srcmesh, NULL, &dstmesh, NULL, midmesh, wts, regridMethod, &regridScheme, 
              &regridPoleType, &regridPoleNPnts, &map_type, unmappedaction))
    Throw() << "Regridding error" << std::endl;

  return 1;
}

// p must be allocated before entering this sub with room to hold number of polygon nodes * sdim
int polygon_to_coords(const struct polygon & poly, const int sdim, double *p, bool ccw_dir){
  if(ccw_dir){
    polygon::const_iterator cit = poly.points.begin(), cie = poly.points.end();
    int np = 0;
    for(; cit != cie; ++ cit, np++){
      for(int j = 0; j < sdim; j ++)
        p[np*sdim+j] = cit->c[j];
    }
  }else{
    polygon::const_reverse_iterator cit = poly.points.rbegin(), cie = poly.points.rend();
    int np = 0;
    for(; cit != cie; ++ cit, np++){
      for(int j = 0; j < sdim; j ++)
        p[np*sdim+j] = cit->c[j];
    }
  }
  return 0;
}
int coords_to_polygon(const int num_p, const double * const p, const int sdim, struct polygon & poly){
  for(int i = 0; i < num_p; i ++){
    if(sdim == 2){ 
      poly.points.push_back(xpoint(p[i*sdim], p[i*sdim+1], 'p'));
    }else if(sdim == 3){
      poly.points.push_back(xpoint(p[i*sdim], p[i*sdim+1], p[i*sdim+2], 'p'));
    }
    //if(poly.area(sdim) < 0.) // if the result polygon is in CW sense, reverse it.
    //  std::reverse(poly.points.begin(), poly.points.end());
  }
  return 0;
}

void reverse_coord(int sdim, int num_point, double * cd){

  double * tmp = new double[sdim];
  for(int i = 0; i < num_point/2; i ++){
    std::memcpy(tmp, cd+i, sdim*sizeof(double));
    std::memcpy(cd+i, cd+(num_point-1-i)*sdim, sdim*sizeof(double));
    std::memcpy(cd+(num_point-1-i)*sdim, tmp, sdim*sizeof(double));
  }
  delete [] tmp;
}

void cart2sph(int num_p, double *coord, double *lonlat){
  const double DEG2RAD = M_PI/180.0;
  const double RAD2DEG = 180.0/M_PI;
  const double ninety = 90.0;

  for(int i = 0; i < num_p; i ++){
    lonlat[i*2]   = asin(coord[i*3+1]/sin(acos(coord[i*3+2])) ) * RAD2DEG;
    lonlat[i*2+1] = ninety - acos(coord[i*3+2]) * RAD2DEG;
  }
}

void cart2sph(const polygon & cart, polygon & sph){
  double * pts =  new double[3*cart.size()];
  polygon_to_coords(cart, 3, pts);
  double * sph_cd = new double[2*cart.size()];
  cart2sph(cart.size(), pts, sph_cd);
  coords_to_polygon(cart.size(), sph_cd, 2, sph);
  delete[] pts;
  delete[] sph_cd;
}

void cart2sph(const std::vector<polygon> & cart, std::vector<polygon> & sph){
  sph.clear(); sph.resize(cart.size());
  for(int i = 0; i < cart.size(); i ++)
    cart2sph(cart[i], sph[i]);
}

void sph2cart(int num_p, double *lonlat, double *coord){
  const double DEG2RAD = M_PI/180.0;
  const double RAD2DEG = 180.0/M_PI;
  const double ninety = 90.0;

  for(int i = 0; i < num_p; i ++){
    double theta = DEG2RAD*lonlat[i*2], phi = DEG2RAD*(ninety-lonlat[i*2+1]);
    coord[i*3]   = std::cos(theta)*std::sin(phi);
    coord[i*3+1] = std::sin(theta)*std::sin(phi);
    coord[i*3+2] = std::cos(phi);
  }
}

void sph2cart(const polygon & sph, polygon & cart){
  double * pts =  new double[2*sph.size()];
  polygon_to_coords(sph, 2, pts);
  double * cart_cd = new double[3*sph.size()];
  sph2cart(sph.size(), pts, cart_cd);
  coords_to_polygon(sph.size(), cart_cd, 3, cart);
  delete[] pts;
  delete[] cart_cd;
}

void sph2cart(const std::vector<polygon> & sph, std::vector<polygon> & cart){
  cart.clear(); cart.resize(sph.size());
  for(int i = 0; i < sph.size(); i ++)
    sph2cart(sph[i], cart[i]);
}

} //namespace
