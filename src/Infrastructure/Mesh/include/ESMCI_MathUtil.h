// $Id: ESMCI_MathUtil.h,v 1.15 2012/11/15 20:50:42 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MathUtil_h
#define  ESMCI_MathUtil_h


#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MCoord.h>

#include <vector>

namespace ESMCI {


  bool invert_matrix_3x3(double m[], double m_inv[]);

  bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
				double *t);

  bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
			       double *t);

  double area_of_flat_2D_polygon(int num, double *coords);

  double great_circle_area(int n, double *pnts);

  double tri_area(const double * const u, const double * const v, const double * const w);

  void get_elem_coords(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords);

  void get_elem_coords_2D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords);

  void get_elem_coords_3D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords);

  void get_elem_coords_and_ids(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords, int *ids);

  void remove_0len_edges3D(int *num_p, double *p);

  void remove_0len_edges2D(int *num_p, double *p);

  void write_3D_poly_to_vtk(const char *filename, int id, int num_p, double *p);
  void write_3D_poly_woid_to_vtk(const char *filename, int num_p, double *p);

  void rot_2D_2D_cart(int num_p, double *p, bool *left_turn, bool *right_turn);

  void rot_2D_3D_sph(int num_p, double *p, bool *left_turn, bool *right_turn);

  void convert_cart_to_sph(double x, double y, double z,
                         double *lon, double *lat, double *r);

  void convert_cart_to_sph_deg(double x, double y, double z,
                               double *lon, double *lat, double *r);


  bool is_smashed_quad2D(int num_p, double *p);

  bool is_smashed_quad3D(int num_p, double *p);

  

  // STUFF FOR TRIANGULATION

struct GEOM_CART2D {

  static const int pnt_size=2;

  static double *getPntAt(double *a, int i) {return a+2*i;}

  // Direction of turn between vectors a and b, starting both starting from point p
  // based on cross product
  static double turn(double *a, double *b, double *p) {return a[0]*b[1]-a[1]*b[0];}

  // Used as an approximation of sharpness of angle between two vectors
  static double dot(double *a, double *b) {return a[0]*b[0]+a[1]*b[1];}

  static void copy(double *a, double *b) {a[0]=b[0]; a[1]=b[1];}

  static void sub(double *out, double *a, double *b) {out[0]=a[0]-b[0]; out[1]=a[1]-b[1];}

};


struct GEOM_SPH2D3D {

  static const int pnt_size=3;

  static double *getPntAt(double *a, int i) {return a+3*i;}

  // direction of turn between vectors a and b, starting both starting from point p
  // based on cross product
  static double turn(double *a, double *b, double *p) {return p[0]*(a[1]*b[2]-a[2]*b[1])+p[1]*(a[2]*b[0]-a[0]*b[2])+p[2]*(a[0]*b[1]-a[1]*b[0]);}

  // Used as an approximation of sharpness of angle between two vectors
  static double dot(double *a, double *b) {return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];}

  static void copy(double *a, double *b) {a[0]=b[0]; a[1]=b[1]; a[2]=b[2];}

  static void sub(double *out, double *a, double *b) {out[0]=a[0]-b[0]; out[1]=a[1]-b[1]; out[2]=a[2]-b[2];}

};


#define ESMCI_TP_SUCCESS 0 
#define ESMCI_TP_DEGENERATE_POLY 1 
#define ESMCI_TP_CLOCKWISE_POLY 2 
template <class TYPE>
int triangulate_poly(int num_p, double *p, double *td, int *ti, int *tri_ind);

template <class TYPE>
bool is_pnt_in_poly(int num_p, double *p, double *pnt);

} // namespace

#endif
