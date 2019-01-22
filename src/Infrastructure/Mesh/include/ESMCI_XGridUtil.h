// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_XGridUtil_h
#define ESMCI_XGridUtil_h

#include <vector>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_MathUtil.h>

namespace ESMCI {

/**
 *\brief generate a mesh based on the nodes and cells vector. The cells are unique and refer to the nodes.
 * @param[in] sintd_nodes vector to allocated intersecting nodal points
 * @param[in] sintd_cells vector to allocated intersecting cells
 * @param[in] pdim number of paramatric dimension
 * @param[in] sdim number of spatial    dimension
 * @param[out] midmesh mesh in the middle
 */
void compute_midmesh(std::vector<sintd_node *> & sintd_nodes, std::vector<sintd_cell *> & sintd_cells, 
  int pdim, int sdim, Mesh *midmesh);

/**
 *\brief compute intersecting cell and nodes in parallel using zoltan structure to help spatial analysis
 * @param[in] area        area of polygon described sintd_coords
 * @param[in] num_sintd_nodes    number of nodes for intersection cell
 * @param[in] sintd_coords       coordinate values of nodes of the intersection
 * @param[in] pdim number of paramatric dimension
 * @param[in] sdim number of spatial    dimension
 * @param[out] sintd_nodes vector to allocated intersecting nodal points
 * @param[out] sintd_cells vector to allocated intersecting cells
 * @param[in] zz Zoltan structure
 */
void compute_sintd_nodes_cells(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, struct Zoltan_Struct * zz);

/**
 *\brief compute intersecting cell and nodes from a polygon
 * @param[in] area        area of polygon described sintd_coords
 * @param[in] num_sintd_nodes    number of nodes for intersection cell
 * @param[in] sintd_coords       coordinate values of nodes of the intersection
 * @param[in] pdim number of paramatric dimension
 * @param[in] sdim number of spatial    dimension
 * @param[out] sintd_nodes vector to allocated intersecting nodal points
 * @param[out] sintd_cells vector to allocated intersecting cells
 */
void construct_sintd(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells);

/**
 *\brief compute mesh in the middle from meshes on side A and B
 * @param[in] srcmesh            mesh on side A
 * @param[in] dstmesh            mesh on side B
 * @param[out] midmesh           mesh in the middle
 * @param[out] wts               weight matrices (?)
 * @param[in] regridConserve     use new conservative scheme
 * @param[in] regridMethod       regrid method
 * @param[in] unmappedaction     unmapped action
 */
int online_regrid_xgrid(Mesh &srcmesh, Mesh &dstmesh, Mesh * midmesh, IWeights &wts,
  int *regridConserve, int *regridMethod, 
  int *unmappedaction);

/**
 *\brief in-place reversal of coordinate points of a polygon in CW or CCW sense
 * @param[in] sdim number of spatial    dimension
 * @param[in] num_point          number of polygon points
 * @param[in,out] cd             polygon coordinate
 */
void reverse_coord(int sdim, int num_point, double * cd);

// Weiler Atherton Algorithm O(nlogn), Sutherland–Hodgman O(n**2)

/**
  *\class xpoint
  *\brief a point used for polygon representation used in XGrid algorithms
  */

struct xpoint{
  double c[3];              /*! coordinate */
  char label;               /*! character label: for debugging */
  bool intersection;        /*! if this point is an intersection */
  int inbound;  // -1 uninitialized, 0 coincident, 1 outbound, 2 inbound
  bool visited;             /*! if this point is visited */
  xpoint() : intersection(false), inbound(-1), visited(false) {
    c[0] = 0.; c[1] = 0.; c[2] = 0.;
  }
  xpoint(double x_, double y_, char label_, bool intersection_=false, int inbound_=-1) : 
    label(label_), intersection(intersection_), inbound(inbound_), visited(false) {
    c[0] = x_; c[1] = y_; c[2] = 0.;
  }
  xpoint(double x_, double y_, double z_, char label_, bool intersection_=false, int inbound_=-1) : 
    label(label_), intersection(intersection_), inbound(inbound_), visited(false) {
    c[0] = x_; c[1] = y_; c[2] = z_;
  }
  xpoint(const double * const c_, int sdim) : label('p'), intersection(false), inbound(-1), visited(false) {
    if(sdim == 2) { c[0] = c_[0]; c[1] = c_[1]; c[2] = 0.; }
    if(sdim == 3) { c[0] = c_[0]; c[1] = c_[1]; c[2] = c_[2]; }
  }

  // metric epsilon is 1.e-20
  bool operator == (const xpoint & that) const{
    double epsilon = 1.e-10;
    return (std::sqrt( (this->c[0]-that.c[0])*((this->c[0]-that.c[0])) +
                       (this->c[1]-that.c[1])*((this->c[1]-that.c[1])) +
                       (this->c[2]-that.c[2])*((this->c[2]-that.c[2])) ) <= epsilon);
  }

  bool operator != (const xpoint & that) const{
    double epsilon = 1.e-10;
    return (std::sqrt( (this->c[0]-that.c[0])*((this->c[0]-that.c[0])) +
                       (this->c[1]-that.c[1])*((this->c[1]-that.c[1])) +
                       (this->c[2]-that.c[2])*((this->c[2]-that.c[2])) ) > epsilon);
  }

};

/**
  *\class xvector
  *\brief a vector representation used in XGrid algorithms
  */
struct xvector{
  double c[3];

  xvector(double x, double y, double z){
    c[0] = x; c[1] = y; c[2] = z;
  }

  xvector(const double * const c_, int sdim){
    c[2] = 0.;
    std::memcpy(c, c_, sdim*sizeof(double));
  }

  xvector(const xpoint & p){
    c[0] = p.c[0];
    c[1] = p.c[1];
    c[2] = p.c[2];
  }

  double metric() const {
    double d = std::sqrt(c[0]*c[0] + c[1]*c[1]+c[2]*c[2]);
    return d;
  }

  xvector normalize() const{
    double metric = std::sqrt(c[0]*c[0] + c[1]*c[1]+c[2]*c[2]);
    if(metric == 0.) return xvector(0.,0.,0.);
    return xvector(c[0]/metric, c[1]/metric, c[2]/metric);
  }

  // compute the unit vector normal to this vector in 2D
  // in counter clock wise sense: *exp(-i*pi/2)
  xvector normal2D() const{
    xvector p = xvector(c[1], -c[0], c[2]);
    return p.normalize(); 
  }

  xvector normal3D(xvector p2) const{
    // normal3D = (p1 x p2) x p1 = n x p1, p1 = this
    xvector n = xvector(c[1]*p2.c[2]-c[2]*p2.c[1], 
                        c[2]*p2.c[0]-c[0]*p2.c[2],
                        c[0]*p2.c[1]-c[1]*p2.c[0]);
    xvector p = xvector(n.c[1]*c[2]-n.c[2]*c[1], 
                        n.c[2]*c[0]-n.c[0]*c[2],
                        n.c[0]*c[1]-n.c[1]*c[0]);
    return p.normalize(); 
  }

};

xvector operator -(const xvector & v1, const xvector & v2);
xvector operator +(const xvector & v1, const xvector & v2);
xvector operator *(const double ratio, const xvector & v);
double dot(const xvector & v1, const xvector & v2);

struct xpoint_equal{

  bool operator () (const xpoint & rhs, const xpoint & lhs){
    return (std::sqrt( (rhs.c[0]-lhs.c[0])*((rhs.c[0]-lhs.c[0])) +
                       (rhs.c[1]-lhs.c[1])*((rhs.c[1]-lhs.c[1])) +
                       (rhs.c[2]-lhs.c[2])*((rhs.c[2]-lhs.c[2])) ) < 1.e-10);
  }
};

struct xedge{
  xpoint *s,*e;
  xvector norm; // in CCW sense
};

struct polygon;

/**
 *\brief convert polygon to a double * representation of the nodal points coordinate
 * @param[in] poly polygon
 * @param[in] sdim number of spatial    dimension
 * @param[out] p   output coordinate, this must be preallocated to be at least sdim*poly.size()
 * @param[in]  ccw_dir by default, the output polygon coordinate is arranged in CCW sense
 */
int polygon_to_coords(const struct polygon & poly, const int sdim, double *p, bool ccw_dir=true);

/**
 *\brief convert a double * representation of the nodal points coordinate to a polygon
 * @param[in] num_p number of polygon points
 * @param[in] p     polygon coordinates
 * @param[in] sdim number of spatial    dimension
 * @param[out] poly polygon, always in CCW sense.
 */
int coords_to_polygon(const int num_p, const double * const p, const int sdim, struct polygon & poly);

/**
 * \class polygon
 * \brief a representation of self-closed polygon
 */
struct polygon{
  typedef std::vector<xpoint>::const_iterator const_iterator;
  typedef std::vector<xpoint>::iterator iterator;
  typedef std::vector<xpoint>::const_reverse_iterator const_reverse_iterator;
  typedef std::vector<xpoint>::reverse_iterator reverse_iterator;

  std::vector<xpoint> points;
  polygon(){}
  polygon(const std::list<xpoint> & that) {
    points.resize(that.size());
    std::copy(that.begin(), that.end(), points.begin());
  }
  polygon(const std::vector<xpoint> & that) {
    points.resize(that.size());
    std::copy(that.begin(), that.end(), points.begin());
  }

  unsigned int size() const { return points.size(); }

  double area(int sdim) const;

  xpoint centroid(int sdim) const;
};

// Compute the difference polygons: p-q
// p: subject
// q: clip
/**
 *\brief compute the difference polygon
 * @param[in] num_p number of subject polygon points
 * @param[in] p     subject polygon coordinates
 * @param[in] num_q number of clip polygon points
 * @param[in] q     clip polygon coordinates
 * @param[out] difference the difference polygons stored in a vector
 */
int weiler_clip_difference(int pdim, int sdim, int num_p, double *p, int num_q, double *q, std::vector<polygon> & difference);
bool same_point(const double * const p1, const double * const p2, const double epsilon=1.e-15);
bool intersect_line_with_line(const double *p1, const double *p2, const double *q1, const double *q2, double * result, bool * coincident, 
  double * pidx, double *qidx);
double gcdistance(double l1, double g1, double l2, double g2);
double gcdistance(double * v1, double * v2);

// Debugging apis
void cart2sph(int num_p, const double *p, double *lonlat);
void cart2sph(const polygon & cart, polygon & sph);
void cart2sph(const std::vector<polygon> & cart, std::vector<polygon> & sph);
void sph2cart(int num_p, const double *lonlat, double *p);
void sph2cart(const polygon & sph, polygon & cart);
void sph2cart(const std::vector<polygon> & sph, std::vector<polygon> & cart);
void test_clip2D(int pdim, int sdim, int num_s, double * s_coord, int num_c, double * c_coord);
void test_clip3D(int pdim, int sdim, int num_s, double * s_coord, int num_c, double * c_coord);
void dump_sph_coords(int num, const double * coord);
void dump_cart_coords(int num, const double * coord, bool only_sph=false);
void dump_polygon(const polygon & poly, bool only_sph=false);

} // namespace

#endif
