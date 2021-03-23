#ifndef SMOOTH_FACE_EVAL_HPP
#define SMOOTH_FACE_EVAL_HPP

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CartVect.hpp"
#include "MBTagConventions.hpp"
#include "moab/Types.hpp"

#include <cmath>
#include <vector>
#include <map>

#define determ3(p1,q1,p2,q2,p3,q3) ((q3)*((p2)-(p1)) + (q2)*((p1)-(p3)) + (q1)*((p3)-(p2)))
#define blend(x) (-2.0*(x)*(x)*(x) + 3.0*(x)*(x))

// work only with CAMAL > = 500
// #if CAMAL_VERSION < 500
// #else

#include "moab/GeomTopoTool.hpp"

namespace moab {
class SmoothCurve;// it is derived from SmoothBase, maybe just need

//! Implement CAMAL geometry callbacks using smooth iMesh
class SmoothFace // public CMLSurfEval, public SmoothBase
{
public:
  SmoothFace(Interface * mb, EntityHandle surface_set, GeomTopoTool * gTool); // entity or entity set

  virtual ~SmoothFace();

  virtual double area();

  virtual void bounding_box(double box_min[3], double box_max[3]);

  virtual void move_to_surface(double& x, double& y, double& z);
  /*
   virtual void move_to_surface(double& x, double& y, double& z,
   double& u_guess, double& v_guess);*/

  virtual bool normal_at(double x, double y, double z, double& nx, double& ny,
      double& nz);

  // initialize normals// they will be stored as tags on nodes
  int init_gradient();

  // some functions for edge evaluations
  ErrorCode evaluate_smooth_edge(EntityHandle eh, double &t, CartVect & outv);

  ErrorCode eval_bezier_patch(EntityHandle tri, CartVect &areacoord,
      CartVect &pt);

  void project_to_facet_plane(EntityHandle tri, CartVect &pt,
      CartVect &point_on_plane, double &dist_to_plane);

  void facet_area_coordinate(EntityHandle facet, CartVect & pt_on_plane,
      CartVect & areacoord);

  ErrorCode project_to_facets_main(CartVect &this_point, bool trim,
      bool & outside, CartVect * closest_point_ptr = NULL, // interested only in normal
      CartVect * normal_ptr = NULL); // interested only in closest point

  ErrorCode project_to_facets(std::vector<EntityHandle> & facet_list,
      EntityHandle & lastFacet, int interpOrder, double compareTol,
      CartVect &this_point, bool trim, bool & outside,
      CartVect *closest_point_ptr, CartVect * normal_ptr);

  ErrorCode project_to_facet(EntityHandle facet, CartVect &pt,
      CartVect &areacoord, CartVect &close_point, bool &outside_facet,
      double compare_tol);

  bool is_at_vertex(EntityHandle facet, // (IN) facet we are evaluating
      CartVect &pt, // (IN) the point
      CartVect &ac, // (IN) the ac of the point on the facet plane
      double compare_tol, // (IN) return TRUE of closer than this
      CartVect &eval_pt, // (OUT) location at vertex if TRUE
      CartVect *eval_norm_ptr); // (OUT) normal at vertex if TRUE

  ErrorCode project_to_patch(EntityHandle facet, // (IN) the facet where the patch is defined
      CartVect &ac, // (IN) area coordinate initial guess (from linear facet)
      CartVect &pt, // (IN) point we are projecting to patch
      CartVect &eval_pt, // (OUT) The projected point
      CartVect *eval_norm, // (OUT) normal at evaluated point
      bool &outside, // (OUT) the closest point on patch to pt is on an edge
      double compare_tol, // (IN) comparison tolerance
      int edge_id); // (IN) only used if this is to be projected to one
  //      of the edges.  Otherwise, should be -1

  ErrorCode eval_bezier_patch_normal(EntityHandle facet, CartVect &areacoord,
      CartVect &normal);

  // this will be called now from driver...
  ErrorCode compute_tangents_for_each_edge();// they will be used for control points

  ErrorCode get_normals_for_vertices(const EntityHandle * conn2, CartVect N[2]);// here we need the gradient tag

  // make this one public, will be called during initialization !!!
  ErrorCode init_edge_control_points(CartVect &P0, CartVect &P3, CartVect &N0,
      CartVect &N3, CartVect &T0, CartVect &T3, CartVect * Pi);

  // moved from private, because will be called from PaveDriver
  ErrorCode compute_control_points_on_edges(double min_dot, Tag edgeCtrlTag,
      Tag markTag);

  ErrorCode compute_internal_control_points_on_facets(double min_dot,
      Tag facetCtrlTag, Tag facetEdgeCtrlTag);

  // move from private too
  void DumpModelControlPoints();

  int eval_counter()
  {
    return _evaluationsCounter;
  }

  // new method for ray intersection correction
  ErrorCode ray_intersection_correct(EntityHandle facet, // (IN) the facet where the patch is defined
      CartVect &pt, // (IN) shoot from
      CartVect &ray, // (IN) ray direction
      CartVect &eval_pt, // (INOUT) The intersection point
      double & distance, // (IN OUT) the new distance
      bool &outside); // (OUT) the closest point on patch to pt is on an edge

  void append_smooth_tags(std::vector<Tag> & smoothTags);
  //      of the edges.  Otherwise, should be -1)
private:

  //===========================================================================
  //Function Name: move_ac_inside
  //
  //Member Type:  PRIVATE
  //Description:  find the closest area coordinate to the boundary of the
  //              patch if any of its components are < 0
  //              Return if the ac was modified.
  //===========================================================================
  bool move_ac_inside(CartVect &ac, double tol);

  //===========================================================================
  //Function Name: ac_at_edge
  //
  //Member Type:  PRIVATE
  //Description:  determine the area coordinate of the facet at the edge
  //===========================================================================
  void ac_at_edge(CartVect &fac, // facet area coordinate
      CartVect &eac, // edge area coordinate
      int edge_id); // id of edge

  // some local functions that do not need to be public
  ErrorCode init_bezier_edge(EntityHandle edge, double min_dot);
  //

  ErrorCode find_edges_orientations(EntityHandle edges[3],
      const EntityHandle * conn3, int orient[3]); // maybe we will set it?

  ErrorCode init_facet_control_points(CartVect N[6], // vertex normals (per edge)
      CartVect P[3][5], // edge control points
      CartVect G[6]); // return internal control points

  // those are the bounding box limits;
  // they are adjusted for the control points too in each triangle
  void adjust_bounding_box(CartVect & vect);
  CartVect _minim;
  CartVect _maxim;

  Range _triangles;
  Range _edges;
  Range _nodes;

  //std::vector<double> _fractions;// they are increasing from 0. to 1., do we need these?
  //std::vector<double> _loopLengths;

  // number of loops is decided by the size of _loopEnds.size()
  // this ref face will be gone, we will replace it with a new call
  //RefFace * _smooth_face;
  //int myDimension;
  //double meshSize;

  // this tag is on edges
  // rval = _mb->tag_create("MARKER", 1, MB_TAG_BIT, _markTag, &value);
  Tag _markTag; // this is a tag used to mark edges when we look for loops

  // this tag is on nodes
  //ErrorCode rval = _mb->tag_create("GRADIENT", 3 * sizeof(double),
  // MB_TAG_DENSE, _gradientTag, &defNormal);
  Tag _gradientTag; // this will be used for normal at nodes

  // this tag is on edges
  //ErrorCode rval = _mb->tag_create("TANGENTS", 6 * sizeof(double),
  // MB_TAG_DENSE, _tangentsTag, &defTangents);
  Tag _tangentsTag; // each edge will have exactly 2 tangents, because there is only
  // one feature edge, and it is periodic
  // the feature edge is exactly on the boundary

  // this tag is on edges
  //ErrorCode rval = _mb->tag_create("CONTROLEDGE", 9 * sizeof(double),
  // MB_TAG_DENSE, _edgeCtrlTag, &defControls);
  Tag _edgeCtrlTag;

  // this tag is on facets (triangles), 6 control points on each facet
  // there are also some 15 points used in evaluation; how to store them?
  //ErrorCode rval = _mb->tag_create("CONTROLFACE", 18 * sizeof(double),
  // MB_TAG_DENSE, _facetCtrlTag, &defControls);
  Tag _facetCtrlTag;

  // these are the 12 points stored for each edge, again
  // it is cheaper this way compared to retrieve the edges every time, determine their orientation, order
  // in triangle, and retrieve the control points from the edge
  // the control points are stored as 12 points, in order : edge 0, 1, and 2, in that order
  //ErrorCode rval = _mb->tag_create("CONTROLEDGEFACE", 36 * sizeof(double),
  // MB_TAG_DENSE, _facetEdgeCtrlTag, &defControls);
  Tag _facetEdgeCtrlTag; //
  // plane of the facet stores as a normal a, b, c and d, distance, for
  // ax+by+cz+d=0
  //ErrorCode rval = _mb->tag_create("PLANE", 4 * sizeof(double),
  // MB_TAG_DENSE, _planeTag, &defPlane);
  Tag _planeTag;

  Interface * _mb;
  EntityHandle _set;
  GeomTopoTool * _my_geomTopoTool;
  EntityHandle _obb_root;
  // counter for calls
  long _evaluationsCounter;
};
// #endif
} // namespace moab
#endif /* SMOOTH_FACE_EVAL_HPP*/
