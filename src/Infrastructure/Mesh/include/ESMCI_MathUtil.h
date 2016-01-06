// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
#include <limits>

namespace ESMCI {

  bool is_outside_hex_sph3D_xyz(const double *hex_xyz, const double *pnt_xyz);

  bool calc_p_hex_sph3D_xyz(const double *hex_xyz, const double *pnt_xyz, double *p);

  bool invert_matrix_3x3(double m[], double m_inv[]);

  bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
				double *t);

  bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
			       double *t);

  double area_of_flat_2D_polygon(int num, double *coords);

  double great_circle_area(int n, double *pnts);

  double tri_area(const double * const u, const double * const v, const double * const w);

  void get_elem_coords(const MeshObj *elem, const MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords);

  void get_elem_coords_2D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords);

  void get_elem_coords_3D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords);

  void get_elem_coords_and_ids(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords, int *ids);


  void remove_0len_edges3D(int *num_p, double *p, int *_first_remove_ind=NULL);

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

  void calc_sph_mmbox(double *pnt1, double *pnt2, double *pnt3, double *min, double *max);


int calc_gc_parameters_quad(const double *pnt, double *pnt1, double *pnt2, double *pnt3, double *pnt4, 
                            double *p1, double *p2);

int calc_gc_parameters_tri(const double *pnt, double *t1, double *t2, double *t3,
                           double *p1, double *p2);

//// Handy macros ////

// Do it this way because some compilers don't support isfinite (e.g. pgi)
#define MU_IS_FINITE(n) ((n) <= std::numeric_limits<double>::max() && (n) >= -std::numeric_limits<double>::max())
  

#define MU_SET_MIN_VEC3D(min,vec)       \
  if (vec[0]<min[0]) min[0]=vec[0];\
  if (vec[1]<min[1]) min[1]=vec[1];\
  if (vec[2]<min[2]) min[2]=vec[2];

#define MU_SET_MAX_VEC3D(max,vec)       \
  if (vec[0]>max[0]) max[0]=vec[0];\
  if (vec[1]>max[1]) max[1]=vec[1];\
  if (vec[2]>max[2]) max[2]=vec[2];

#define MU_ADD_VEC3D(out,a,b) \
  out[0]=a[0]+b[0]; \
  out[1]=a[1]+b[1]; \
  out[2]=a[2]+b[2];

#define MU_SUB_VEC3D(out,a,b) \
  out[0]=a[0]-b[0]; \
  out[1]=a[1]-b[1]; \
  out[2]=a[2]-b[2];

// multiply 3x3 MAT BY 3D VEC
#define MU_MAT_X_VEC3D(out_v, m, v) \
  out_v[0]=m[0]*v[0]+m[1]*v[1]+m[2]*v[2]; \
  out_v[1]=m[3]*v[0]+m[4]*v[1]+m[5]*v[2]; \
  out_v[2]=m[6]*v[0]+m[7]*v[1]+m[8]*v[2];


#define MU_DIV_BY_SCALAR_VEC3D(out,a,s) \
  out[0]=a[0]/(s);                      \
  out[1]=a[1]/(s);                      \
  out[2]=a[2]/(s);


#define MU_EQUAL_PNT3D(p1,p2,tol) ((std::abs(p1[0]-p2[0]) < tol) && \
                                  (std::abs(p1[1]-p2[1]) < tol) && \
                                  (std::abs(p1[2]-p2[2]) < tol))

#define MU_CROSS_PRODUCT_VEC3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];

#define MU_LEN_VEC3D(a) std::sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])

#define MU_LENSQ_VEC3D(a) (a[0]*a[0]+a[1]*a[1]+a[2]*a[2])

#define MU_DOT_VEC3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])


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

template <class GEOM>
double calc_angle(double *v1, double *v2, double *norm);

} // namespace

#endif
