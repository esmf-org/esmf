#include <iostream>
#include <fstream>
#include "SmoothFace.hpp"

#include <algorithm>
#include <iomanip>

#include "assert.h"
// included in the header now
// #include "Range.hpp"
// #include "CartVect.hpp"

// some defines from CUBIT
#define GEOMETRY_RESABS 1.e-6
#define CUBIT_DBL_MAX 1.e+30
//#define DBL_EPSILON  1.e-8
#include <float.h>

namespace moab {

bool within_tolerance(CartVect & p1, CartVect & p2, const double & tolerance)
{
  if ((fabs(p1[0] - p2[0]) < tolerance) && (fabs(p1[1] - p2[1]) < tolerance)
      && (fabs(p1[2] - p2[2]) < tolerance))
    return true;
  return false;
}
int numAdjTriInSet(Interface * mb, EntityHandle startEdge, EntityHandle set)
{
  std::vector<EntityHandle> adjTri;
  mb->get_adjacencies(&startEdge, 1, 2, false, adjTri, Interface::UNION);
  // count how many are in the set
  int nbInSet = 0;
  for (size_t i = 0; i < adjTri.size(); i++)
  {
    EntityHandle tri = adjTri[i];
    if (mb->contains_entities(set, &tri, 1))
      nbInSet++;
  }
  return nbInSet;
}

bool debug_surf_eval1 = false;

SmoothFace::SmoothFace(Interface * mb, EntityHandle surface_set,
    GeomTopoTool * gTool) :
  _markTag(0), _gradientTag(0), _tangentsTag(0), _edgeCtrlTag(0),
  _facetCtrlTag(0), _facetEdgeCtrlTag(0), _planeTag(0),
  _mb(mb), _set(surface_set), _my_geomTopoTool(gTool), _obb_root(0), _evaluationsCounter(0)
{
  //_smooth_face = NULL;
  //_mbOut->create_meshset(MESHSET_SET, _oSet); //will contain the
  // get also the obb_root
  if (_my_geomTopoTool)
  {
    _my_geomTopoTool->get_root(this->_set, _obb_root);
    if (debug_surf_eval1)
      _my_geomTopoTool->obb_tree()->stats(_obb_root, std::cout);
  }
}

SmoothFace::~SmoothFace()
{
}

double SmoothFace::area()
{
  // find the area of this entity
  //assert(_smooth_face);
  //double area1 = _smooth_face->area();
  double totArea = 0.;
  for (Range::iterator it = _triangles.begin(); it != _triangles.end(); ++it)
  {
    EntityHandle tria = *it;
    const EntityHandle * conn3;
    int nnodes;
    _mb->get_connectivity(tria, conn3, nnodes);
    //
    //double coords[9]; // store the coordinates for the nodes
    //_mb->get_coords(conn3, 3, coords);
    CartVect p[3];
    _mb->get_coords(conn3, 3, (double*) &p[0]);
    // need to compute the angles
    // compute angles and the normal
    //CartVect p1(&coords[0]), p2(&coords[3]), p3(&coords[6]);

    CartVect AB(p[1] - p[0]);//(p2 - p1);
    CartVect BC(p[2] - p[1]);//(p3 - p2);
    CartVect normal = AB * BC;
    totArea += normal.length() / 2;
  }
  return totArea;
}
// these tags will be collected for deletion
void SmoothFace::append_smooth_tags(std::vector<Tag> & smoothTags)
{
  // these are created locally, for each smooth face
  smoothTags.push_back(_gradientTag);
  smoothTags.push_back(_planeTag);

}
void SmoothFace::bounding_box(double box_min[3], double box_max[3])
{

  for (int i = 0; i < 3; i++)
  {
    box_min[i] = _minim[i];
    box_max[i] = _maxim[i];
  }
  // _minim, _maxim
}

void SmoothFace::move_to_surface(double& x, double& y, double& z)
{

  CartVect loc2(x, y, z);
  bool trim = false;// is it needed?
  bool outside = true;
  CartVect closestPoint;

  ErrorCode rval = project_to_facets_main(loc2, trim, outside, &closestPoint,
      NULL);
  if (MB_SUCCESS != rval) return;
  assert(rval==MB_SUCCESS);
  x = closestPoint[0];
  y = closestPoint[1];
  z = closestPoint[2];

}
/*
 void SmoothFace::move_to_surface(double& x, double& y, double& z,
 double& u_guess, double& v_guess) {
 if (debug_surf_eval1) {
 std::cout << "move_to_surface called." << std::endl;
 }
 }*/

bool SmoothFace::normal_at(double x, double y, double z, double& nx,
    double& ny, double& nz)
{

  CartVect loc2(x, y, z);

  bool trim = false;// is it needed?
  bool outside = true;
  //CartVect closestPoint;// not needed
  // not interested in normal
  CartVect normal;
  ErrorCode rval = project_to_facets_main(loc2, trim, outside, NULL, &normal);
  if (MB_SUCCESS != rval) return false;
  assert(rval==MB_SUCCESS);
  nx = normal[0];
  ny = normal[1];
  nz = normal[2];

  return true;
}

ErrorCode SmoothFace::compute_control_points_on_edges(double min_dot,
    Tag edgeCtrlTag, Tag markTag)
{

  _edgeCtrlTag = edgeCtrlTag;
  _markTag = markTag;

  // now, compute control points for all edges that are not marked already (they are no on the boundary!)
  for (Range::iterator it = _edges.begin(); it != _edges.end(); ++it)
  {
    EntityHandle edg = *it;
    // is the edge marked? already computed
    unsigned char tagVal = 0;
    _mb->tag_get_data(_markTag, &edg, 1, &tagVal);
    if (tagVal)
      continue;
    //double min_dot;
    init_bezier_edge(edg, min_dot);
    tagVal = 1;
    _mb->tag_set_data(_markTag, &edg, 1, &tagVal);
  }
  return MB_SUCCESS;
}

int SmoothFace::init_gradient()
{
  // first, create a Tag for gradient (or normal)
  // loop over all triangles in set, and modify the normals according to the angle as weight
  // get all the edges from this subset
  if (NULL == _mb)
    return 1; //fail
  _triangles.clear();
  ErrorCode rval = _mb->get_entities_by_type(_set, MBTRI, _triangles);
  if (MB_SUCCESS != rval)
    return 1;
  // get a new range of edges, and decide the loops from here
  _edges.clear();
  rval = _mb-> get_adjacencies(_triangles, 1, true, _edges, Interface::UNION);
  assert(rval == MB_SUCCESS);

  rval = _mb->get_adjacencies(_triangles, 0, false, _nodes, Interface::UNION);
  assert(rval == MB_SUCCESS);

  // initialize bounding box limits
  CartVect vert1;
  EntityHandle v1 = _nodes[0];// first vertex
  _mb->get_coords(&v1, 1, (double*) &vert1);
  _minim = vert1;
  _maxim = vert1;

  double defNormal[] = { 0., 0., 0. };
  // look for a tag name here that is definitely unique. We do not want the tags to interfere with each other
  // this normal will be for each node in a face
  // some nodes have multiple normals, if they are at the feature edges
  unsigned long setId = _mb->id_from_handle(_set);
  char name[50] = { 0 };
  sprintf(name, "GRADIENT%lu", setId);// name should be something like GRADIENT29, where 29 is the set ID of the face
  rval = _mb->tag_get_handle(name, 3, MB_TYPE_DOUBLE, _gradientTag,
      MB_TAG_DENSE|MB_TAG_CREAT, &defNormal);
  assert(rval == MB_SUCCESS);

  double defPlane[4] = { 0., 0., 1., 0. };
  // also define a plane tag ; this will be for each triangle
  char namePlaneTag[50] = { 0 };
  sprintf(namePlaneTag, "PLANE%lu", setId);
  rval = _mb->tag_get_handle("PLANE", 4, MB_TYPE_DOUBLE, _planeTag,
      MB_TAG_DENSE|MB_TAG_CREAT, &defPlane);
  assert(rval == MB_SUCCESS);
  // the fourth double is for weight, accumulated at each vertex so far
  // maybe not needed in the end
  for (Range::iterator it = _triangles.begin(); it != _triangles.end(); ++it)
  {
    EntityHandle tria = *it;
    const EntityHandle * conn3;
    int nnodes;
    _mb->get_connectivity(tria, conn3, nnodes);
    if (nnodes != 3)
      return 1; // error
    //double coords[9]; // store the coordinates for the nodes
    //_mb->get_coords(conn3, 3, coords);
    CartVect p[3];
    _mb->get_coords(conn3, 3, (double*) &p[0]);
    // need to compute the angles
    // compute angles and the normal
    //CartVect p1(&coords[0]), p2(&coords[3]), p3(&coords[6]);

    CartVect AB(p[1] - p[0]);//(p2 - p1);
    CartVect BC(p[2] - p[1]);//(p3 - p2);
    CartVect CA(p[0] - p[2]);//(p1 - p3);
    double a[3];
    a[1] = angle(AB, -BC); // angle at B (p2), etc.
    a[2] = angle(BC, -CA);
    a[0] = angle(CA, -AB);
    CartVect normal = -AB * CA;
    normal.normalize();
    double plane[4];

    const double * coordNormal = normal.array();

    plane[0] = coordNormal[0];
    plane[1] = coordNormal[1];
    plane[2] = coordNormal[2];
    plane[3] = -normal % p[0]; // dot product
    //set the plane
    rval = _mb->tag_set_data(_planeTag, &tria, 1, plane);
    assert(rval == MB_SUCCESS);

    // add to each vertex the tag value the normal multiplied by the angle
    double values[9];

    _mb->tag_get_data(_gradientTag, conn3, 3, values);
    for (int i = 0; i < 3; i++)
    {
      //values[4*i]+=a[i]; // first is the weight, which we do not really need
      values[3 * i + 0] += a[i] * coordNormal[0];
      values[3 * i + 1] += a[i] * coordNormal[1];
      values[3 * i + 2] += a[i] * coordNormal[2];
    }

    // reset those values
    _mb->tag_set_data(_gradientTag, conn3, 3, values);

  }
  // normalize the gradients at each node; maybe not needed here?
  // no, we will do it, it is important
  int numNodes = _nodes.size();
  double * normalVal = new double[3 * numNodes];
  _mb->tag_get_data(_gradientTag, _nodes, normalVal);// get all the normal values at the _nodes
  for (int i = 0; i < numNodes; i++)
  {
    CartVect p1(&normalVal[3 * i]);
    p1.normalize();
    p1.get(&normalVal[3 * i]);
  }

  // reset the normal values after normalization
  _mb->tag_set_data(_gradientTag, _nodes, normalVal);
  // print the loops size and some other stuff
  if (debug_surf_eval1)
  {
    std::cout << " normals at  " << numNodes << " nodes" << std::endl;
    int i = 0;
    for (Range::iterator it = _nodes.begin(); it != _nodes.end(); ++it, i++)
    {
      EntityHandle node = *it;
      std::cout << " Node id " << _mb->id_from_handle(node) << "  "
          << normalVal[3 * i] << " " << normalVal[3 * i + 1] << " "
          << normalVal[3 * i + 2] << std::endl;
    }
  }

  delete[] normalVal;

  return 0;
}

// init bezier edges
// start copy
//===========================================================================
//Function Name: init_bezier_edge
//
//Member Type:  PRIVATE
//Description:  compute the control points for an edge
//===========================================================================
ErrorCode SmoothFace::init_bezier_edge(EntityHandle edge, double )
{
  // min dot was used for angle here
  //int stat = 0; // CUBIT_SUCCESS;
  // all boundaries will be simple, initially
  // we may complicate them afterwards

  CartVect ctrl_pts[3];
  int nnodes = 0;
  const EntityHandle * conn2 = NULL;
  ErrorCode rval = _mb->get_connectivity(edge, conn2, nnodes);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;
  
  assert(2 == nnodes);
  //double coords[6]; // store the coordinates for the nodes
  CartVect P[2];
  //ErrorCode rval = _mb->get_coords(conn2, 2, coords);
  rval = _mb->get_coords(conn2, 2, (double*) &P[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  //CartVect P0(&coords[0]);
  //CartVect P3(&coords[3]);

  //double normalVec[6];
  CartVect N[2];
  //_mb->tag_get_data(_gradientTag, conn2, 2, normalVec);
  rval = _mb->tag_get_data(_gradientTag, conn2, 2, (double*) &N[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  CartVect T[2]; // T0, T3

  rval = _mb->tag_get_data(_tangentsTag, &edge, 1, &T[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  rval = init_edge_control_points(P[0], P[1], N[0], N[1], T[0], T[1], ctrl_pts);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  rval = _mb->tag_set_data(_edgeCtrlTag, &edge, 1, &ctrl_pts[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  if (debug_surf_eval1)
  {
    std::cout << "edge: " << _mb-> id_from_handle(edge) << " tangents: "
        << T[0] << T[1] << std::endl;
    std::cout << "  points: " << P[0] << " " << P[1] << std::endl;
    std::cout << "  normals: " << N[0] << " " << N[1] << std::endl;
    std::cout << "  Control points  " << ctrl_pts[0] << " " << ctrl_pts[1]
        << " " << ctrl_pts[2] << std::endl;
  }

  return MB_SUCCESS;
}

ErrorCode SmoothFace::compute_tangents_for_each_edge()
// they will be used for control points
{
  double defTangents[6] = { 0., 0., 0., 0., 0., 0. };
  ErrorCode rval = _mb->tag_get_handle("TANGENTS", 6, MB_TYPE_DOUBLE,
      _tangentsTag, MB_TAG_DENSE|MB_TAG_CREAT, &defTangents);
  if (MB_SUCCESS != rval)
    return MB_FAILURE;

  // now, compute Tangents for all edges that are not on boundary, so they are not marked
  for (Range::iterator it = _edges.begin(); it != _edges.end(); ++it)
  {
    EntityHandle edg = *it;

    int nnodes;
    const EntityHandle * conn2;//
    _mb->get_connectivity(edg, conn2, nnodes);
    assert(nnodes == 2);
    CartVect P[2]; // store the coordinates for the nodes
    rval = _mb->get_coords(conn2, 2, (double *) &P[0]);
    if (MB_SUCCESS != rval) return rval;
    assert(rval==MB_SUCCESS);
    CartVect T[2];
    T[0] = P[1] - P[0];
    T[0].normalize();
    T[1] = T[0]; //
    _mb->tag_set_data(_tangentsTag, &edg, 1, (double*) &T[0]);// set the tangents computed at every edge
  }
  return MB_SUCCESS;
}
// start copy
//===========================================================================
//Function Name: init_edge_control_points
//
//Member Type:  PRIVATE
//Description:  compute the control points for an edge
//===========================================================================
ErrorCode SmoothFace::init_edge_control_points(CartVect &P0, CartVect &P3,
    CartVect &N0, CartVect &N3, CartVect &T0, CartVect &T3, CartVect * Pi)
{
  CartVect Vi[4];
  Vi[0] = P0;
  Vi[3] = P3;
  CartVect P03(P3 - P0);
  double di = P03.length();
  double ai = N0 % N3;// this is the dot operator, the same as in cgm for CubitVector
  double ai0 = N0 % T0;
  double ai3 = N3 % T3;
  double denom = 4 - ai * ai;
  if (fabs(denom) < 1e-20)
  {
    return MB_FAILURE; // CUBIT_FAILURE;
  }
  double row = 6.0e0 * (2.0e0 * ai0 + ai * ai3) / denom;
  double omega = 6.0e0 * (2.0e0 * ai3 + ai * ai0) / denom;
  Vi[1] = Vi[0] + (di * (((6.0e0 * T0) - ((2.0e0 * row) * N0) + (omega * N3))
      / 18.0e0));
  Vi[2] = Vi[3] - (di * (((6.0e0 * T3) + (row * N0) - ((2.0e0 * omega) * N3))
      / 18.0e0));
  //CartVect Wi[3];
  //Wi[0] = Vi[1] - Vi[0];
  //Wi[1] = Vi[2] - Vi[1];
  //Wi[2] = Vi[3] - Vi[2];

  Pi[0] = 0.25 * Vi[0] + 0.75 * Vi[1];
  Pi[1] = 0.50 * Vi[1] + 0.50 * Vi[2];
  Pi[2] = 0.75 * Vi[2] + 0.25 * Vi[3];

  return MB_SUCCESS;
}

ErrorCode SmoothFace::find_edges_orientations(EntityHandle edges[3],
    const EntityHandle * conn3, int orient[3])// maybe we will set it?
{
  // find the edge that is adjacent to 2 vertices at a time
  for (int i = 0; i < 3; i++)
  {
    // edge 0 is 1-2, 1 is 3-1, 2 is 0-1
    EntityHandle v[2];
    v[0] = conn3[(i + 1) % 3];
    v[1] = conn3[(i + 2) % 3];
    std::vector<EntityHandle> adjacencies;
    // generate all edges for these two hexes
    ErrorCode rval = _mb->get_adjacencies(v, 2, 1, false, adjacencies,
                                          Interface::INTERSECT);
    if (MB_SUCCESS != rval) return rval;

    // find the edge connected to both vertices, and then see its orientation
    assert(adjacencies.size() == 1);
    const EntityHandle * conn2 = NULL;
    int nnodes = 0;
    rval = _mb->get_connectivity(adjacencies[0], conn2, nnodes);
    assert(rval == MB_SUCCESS);
    assert(2 == nnodes);
    edges[i] = adjacencies[0];
    // what is the story morning glory?
    if (conn2[0] == v[0] && conn2[1] == v[1])
      orient[i] = 1;
    else if (conn2[0] == v[1] && conn2[1] == v[0])
      orient[i] = -1;
    else
      return MB_FAILURE;
  }
  return MB_SUCCESS;
}
ErrorCode SmoothFace::compute_internal_control_points_on_facets(double ,
    Tag facetCtrlTag, Tag facetEdgeCtrlTag)
{
  // collect from each triangle the control points in order
  //

  _facetCtrlTag = facetCtrlTag;
  _facetEdgeCtrlTag = facetEdgeCtrlTag;

  for (Range::iterator it = _triangles.begin(); it != _triangles.end(); ++it)
  {
    EntityHandle tri = *it;
    // first get connectivity, and the edges
    // we need a fast method to retrieve the adjacent edges to each triangle
    const EntityHandle * conn3;
    int nnodes;
    ErrorCode rval = _mb->get_connectivity(tri, conn3, nnodes);
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;
    assert(3 == nnodes);

    // would it be easier to do
    CartVect vNode[3];// position at nodes
    rval = _mb->get_coords(conn3, 3, (double*) &vNode[0]);
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;

    // get gradients (normal) at each node of triangle
    CartVect NN[3];
    rval = _mb->tag_get_data(_gradientTag, conn3, 3, &NN[0]);
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;

    EntityHandle edges[3];
    int orient[3]; // + 1 or -1, if the edge is positive or negative within the face
    rval = find_edges_orientations(edges, conn3, orient);// maybe we will set it?
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;
    // maybe we will store some tags with edges and their orientation with respect to
    // a triangle;
    CartVect P[3][5];
    CartVect N[6], G[6];
    // create the linear array for control points on edges, for storage (expensive !!!)
    CartVect CP[9];
    int index = 0;
    // maybe store a tag / entity handle for edges?
    for (int i = 0; i < 3; i++)
    {
      // populate P and N with the right vectors
      int i1 = (i + 1) % 3; // the first node of the edge
      int i2 = (i + 2) % 3; // the second node of the edge
      N[2 * i] = NN[i1];
      N[2 * i + 1] = NN[i2];
      P[i][0] = vNode[i1];
      rval = _mb->tag_get_data(_edgeCtrlTag, &edges[i], 1, &(P[i][1]));
      // if sense is -1, swap 1 and 3 control points
      if (orient[i] == -1)
      {
        CartVect tmp;
        tmp = P[i][1];
        P[i][1] = P[i][3];
        P[i][3] = tmp;
      }
      P[i][4] = vNode[i2];
      for (int j = 1; j < 4; j++)
        CP[index++] = P[i][j];

      // the first edge control points
    }

    //  stat = facet->get_edge_control_points( P );
    init_facet_control_points(N, P, G);
    // what do we need to store in the tag control points?
    rval = _mb->tag_set_data(_facetCtrlTag, &tri, 1, &G[0]);
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;

    // store here again the 9 control points on the edges
    rval = _mb->tag_set_data(_facetEdgeCtrlTag, &tri, 1, &CP[0]);
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval) return rval;
    // look at what we retrieve later

    // adjust the bounding box
    int j = 0;
    for (j = 0; j < 3; j++)
      adjust_bounding_box(vNode[j]);
    // edge control points
    for (j = 0; j < 9; j++)
      adjust_bounding_box(CP[j]);
    // internal facet control points
    for (j = 0; j < 6; j++)
      adjust_bounding_box(G[j]);

  }
  return MB_SUCCESS;
}
void SmoothFace::adjust_bounding_box(CartVect & vect)
{
  // _minim, _maxim
  for (int j = 0; j < 3; j++)
  {
    if (_minim[j] > vect[j])
      _minim[j] = vect[j];
    if (_maxim[j] < vect[j])
      _maxim[j] = vect[j];
  }
}
//===============================================================
////Function Name: init_facet_control_points
////
////Member Type:  PRIVATE
////Description:  compute the control points for a facet
////===============================================================
ErrorCode SmoothFace::init_facet_control_points(CartVect N[6], // vertex normals (per edge)
    CartVect P[3][5], // edge control points
    CartVect G[6]) // return internal control points
{
  CartVect Di[4], Ai[3], N0, N3, Vi[4], Wi[3];
  double denom;
  double lambda[2], mu[2];

  ErrorCode rval = MB_SUCCESS;

  for (int i = 0; i < 3; i++)
  {
    N0 = N[i * 2];
    N3 = N[i * 2 + 1];
    Vi[0] = P[i][0];
    Vi[1] = (P[i][1] - 0.25 * P[i][0]) / 0.75;
    Vi[2] = (P[i][3] - 0.25 * P[i][4]) / 0.75;
    Vi[3] = P[i][4];
    Wi[0] = Vi[1] - Vi[0];
    Wi[1] = Vi[2] - Vi[1];
    Wi[2] = Vi[3] - Vi[2];
    Di[0] = P[(i + 2) % 3][3] - 0.5 * (P[i][1] + P[i][0]);
    Di[3] = P[(i + 1) % 3][1] - 0.5 * (P[i][4] + P[i][3]);
    Ai[0] = (N0 * Wi[0]) / Wi[0].length();
    Ai[2] = (N3 * Wi[2]) / Wi[2].length();
    Ai[1] = Ai[0] + Ai[2];
    denom = Ai[1].length();
    Ai[1] /= denom;
    lambda[0] = (Di[0] % Wi[0]) / (Wi[0] % Wi[0]);
    lambda[1] = (Di[3] % Wi[2]) / (Wi[2] % Wi[2]);
    mu[0] = (Di[0] % Ai[0]);
    mu[1] = (Di[3] % Ai[2]);
    G[i * 2] = 0.5 * (P[i][1] + P[i][2]) + 0.66666666666666 * lambda[0] * Wi[1]
        + 0.33333333333333 * lambda[1] * Wi[0] + 0.66666666666666 * mu[0]
        * Ai[1] + 0.33333333333333 * mu[1] * Ai[0];
    G[i * 2 + 1] = 0.5 * (P[i][2] + P[i][3]) + 0.33333333333333 * lambda[0]
        * Wi[2] + 0.66666666666666 * lambda[1] * Wi[1] + 0.33333333333333
        * mu[0] * Ai[2] + 0.66666666666666 * mu[1] * Ai[1];
  }
  return rval;
}

void SmoothFace::DumpModelControlPoints()
{
  // here, we will dump all control points from edges and facets (6 control points for each facet)
  // we may also create some edges; maybe later...
  // create a point3D file
  // output a Point3D file (special visit format)
  unsigned long setId = _mb->id_from_handle(_set);
  char name[50] = { 0 };
  sprintf(name, "%lucontrol.Point3D", setId);// name should be something 2control.Point3D
  std::ofstream point3DFile;
  point3DFile.open(name);//("control.Point3D");
  point3DFile << "# x y z \n";
  std::ofstream point3DEdgeFile;
  sprintf(name, "%lucontrolEdge.Point3D", setId);//
  point3DEdgeFile.open(name);//("controlEdge.Point3D");
  point3DEdgeFile << "# x y z \n";
  std::ofstream smoothPoints;
  sprintf(name, "%lusmooth.Point3D", setId);//
  smoothPoints.open(name);//("smooth.Point3D");
  smoothPoints << "# x y z \n";
  CartVect controlPoints[3]; // edge control points
  for (Range::iterator it = _edges.begin(); it != _edges.end(); ++it)
  {
    EntityHandle edge = *it;

    _mb->tag_get_data(_edgeCtrlTag, &edge, 1, (double*) &controlPoints[0]);
    for (int i = 0; i < 3; i++)
    {
      CartVect & c = controlPoints[i];
      point3DEdgeFile << std::setprecision(11) << c[0] << " " << c[1] << " "
          << c[2] << " \n";
    }
  }
  CartVect controlTriPoints[6]; // triangle control points
  CartVect P_facet[3];// result in 3 "mid" control points
  for (Range::iterator it2 = _triangles.begin(); it2 != _triangles.end(); ++it2)
  {
    EntityHandle tri = *it2;

    _mb->tag_get_data(_facetCtrlTag, &tri, 1, (double*) &controlTriPoints[0]);

    // draw a line of points between pairs of control points
    int numPoints = 7;
    for (int n = 0; n < numPoints; n++)
    {
      double a = 1. * n / (numPoints - 1);
      double b = 1.0 - a;

      P_facet[0] = a * controlTriPoints[3] + b * controlTriPoints[4];
      //1,2,1
      P_facet[1] = a * controlTriPoints[0] + b * controlTriPoints[5];
      //1,1,2
      P_facet[2] = a * controlTriPoints[1] + b * controlTriPoints[2];
      for (int i = 0; i < 3; i++)
      {
        CartVect & c = P_facet[i];
        point3DFile << std::setprecision(11) << c[0] << " " << c[1] << " "
            << c[2] << " \n";
      }
    }

    // evaluate for each triangle a lattice of points
    int N = 40;
    for (int k = 0; k <= N; k++)
    {
      for (int m = 0; m <= N - k; m++)
      {
        int n = N - m - k;
        CartVect areacoord(1. * k / N, 1. * m / N, 1. * n / N);
        CartVect pt;
        eval_bezier_patch(tri, areacoord, pt);
        smoothPoints << std::setprecision(11) << pt[0] << " " << pt[1] << " "
            << pt[2] << " \n";

      }
    }
  }
  point3DFile.close();
  smoothPoints.close();
  point3DEdgeFile.close();
  return;
}
//===========================================================================
//Function Name: evaluate_single
//
//Member Type:  PUBLIC
//Description:  evaluate edge not associated with a facet (this is used
// by camal edge mesher!!!)
//Note:         t is a value from 0 to 1, for us
//===========================================================================
ErrorCode SmoothFace::evaluate_smooth_edge(EntityHandle eh, double &tt,
    CartVect & outv)
{
  CartVect P[2]; // P0 and P1
  CartVect controlPoints[3]; // edge control points
  double t4, t3, t2, one_minus_t, one_minus_t2, one_minus_t3, one_minus_t4;

  // project the position to the linear edge
  // t is from 0 to 1 only!!
  //double tt = (t + 1) * 0.5;
  if (tt <= 0.0)
    tt = 0.0;
  if (tt >= 1.0)
    tt = 1.0;

  int nnodes = 0;
  const EntityHandle * conn2 = NULL;
  ErrorCode rval = _mb->get_connectivity(eh, conn2, nnodes);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  rval = _mb->get_coords(conn2, 2, (double*) &P[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  rval = _mb->tag_get_data(_edgeCtrlTag, &eh, 1, (double*) &controlPoints[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  t2 = tt * tt;
  t3 = t2 * tt;
  t4 = t3 * tt;
  one_minus_t = 1. - tt;
  one_minus_t2 = one_minus_t * one_minus_t;
  one_minus_t3 = one_minus_t2 * one_minus_t;
  one_minus_t4 = one_minus_t3 * one_minus_t;

  outv = one_minus_t4 * P[0] + 4. * one_minus_t3 * tt * controlPoints[0] + 6.
      * one_minus_t2 * t2 * controlPoints[1] + 4. * one_minus_t * t3
      * controlPoints[2] + t4 * P[1];

  return MB_SUCCESS;
}

ErrorCode SmoothFace::eval_bezier_patch(EntityHandle tri, CartVect &areacoord,
    CartVect &pt)
{
  //
  // interpolate internal control points

  CartVect gctrl_pts[6];
  // get the control points  facet->get_control_points( gctrl_pts );
  //init_facet_control_points( N, P, G) ;
  // what do we need to store in the tag control points?
  ErrorCode rval = _mb->tag_get_data(_facetCtrlTag, &tri, 1, &gctrl_pts[0]);// get all 6 control points
  assert(MB_SUCCESS == rval);
  if (MB_SUCCESS != rval) return rval;
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  rval = _mb->get_connectivity(tri, conn3, nnodes);
  assert(MB_SUCCESS == rval);

  CartVect vN[3];
  _mb->get_coords(conn3, 3, (double*) &vN[0]); // fill the coordinates of the vertices

  if (fabs(areacoord[1] + areacoord[2]) < 1.0e-6)
  {
    pt = vN[0];
    return MB_SUCCESS;
  }
  if (fabs(areacoord[0] + areacoord[2]) < 1.0e-6)
  {
    pt = vN[0];
    return MB_SUCCESS;
  }
  if (fabs(areacoord[0] + areacoord[1]) < 1.0e-6)
  {
    pt = vN[0];
    return MB_SUCCESS;
  }

  CartVect P_facet[3];
  //2,1,1
  P_facet[0] = (1.0e0 / (areacoord[1] + areacoord[2])) * (areacoord[1]
      * gctrl_pts[3] + areacoord[2] * gctrl_pts[4]);
  //1,2,1
  P_facet[1] = (1.0e0 / (areacoord[0] + areacoord[2])) * (areacoord[0]
      * gctrl_pts[0] + areacoord[2] * gctrl_pts[5]);
  //1,1,2
  P_facet[2] = (1.0e0 / (areacoord[0] + areacoord[1])) * (areacoord[0]
      * gctrl_pts[1] + areacoord[1] * gctrl_pts[2]);

  // sum the contribution from each of the control points

  pt = CartVect(0.);// set all to 0, we start adding / accumulating different parts
  // first edge is from node 0 to 1, index 2 in

  // retrieve the points, in order, and the control points on edges

  // store here again the 9 control points on the edges
  CartVect CP[9];
  rval = _mb->tag_get_data(_facetEdgeCtrlTag, &tri, 1, &CP[0]);
  assert(MB_SUCCESS == rval);

  //CubitFacetEdge *edge;
  //edge = facet->edge(2);! start with edge 2, from 0-1
  int k = 0;
  CartVect ctrl_pts[5];
  //edge->control_points(facet, ctrl_pts);
  ctrl_pts[0] = vN[0]; //
  for (k = 1; k < 4; k++)
    ctrl_pts[k] = CP[k + 5]; // for edge index 2
  ctrl_pts[4] = vN[1]; //

  //i=4; j=0; k=0;
  double B = quart(areacoord[0]);
  pt += B * ctrl_pts[0];

  //i=3; j=1; k=0;
  B = 4.0 * cube(areacoord[0]) * areacoord[1];
  pt += B * ctrl_pts[1];

  //i=2; j=2; k=0;
  B = 6.0 * sqr(areacoord[0]) * sqr(areacoord[1]);
  pt += B * ctrl_pts[2];

  //i=1; j=3; k=0;
  B = 4.0 * areacoord[0] * cube(areacoord[1]);
  pt += B * ctrl_pts[3];

  //edge = facet->edge(0);
  //edge->control_points(facet, ctrl_pts);
  // edge index 0, from 1 to 2
  ctrl_pts[0] = vN[1]; //
  for (k = 1; k < 4; k++)
    ctrl_pts[k] = CP[k - 1]; // for edge index 0
  ctrl_pts[4] = vN[2]; //

  //i=0; j=4; k=0;
  B = quart(areacoord[1]);
  pt += B * ctrl_pts[0];

  //i=0; j=3; k=1;
  B = 4.0 * cube(areacoord[1]) * areacoord[2];
  pt += B * ctrl_pts[1];

  //i=0; j=2; k=2;
  B = 6.0 * sqr(areacoord[1]) * sqr(areacoord[2]);
  pt += B * ctrl_pts[2];

  //i=0; j=1; k=3;
  B = 4.0 * areacoord[1] * cube(areacoord[2]);
  pt += B * ctrl_pts[3];

  //edge = facet->edge(1);
  //edge->control_points(facet, ctrl_pts);
  // edge index 1, from 2 to 0
  ctrl_pts[0] = vN[2]; //
  for (k = 1; k < 4; k++)
    ctrl_pts[k] = CP[k + 2]; // for edge index 0
  ctrl_pts[4] = vN[0]; //

  //i=0; j=0; k=4;
  B = quart(areacoord[2]);
  pt += B * ctrl_pts[0];

  //i=1; j=0; k=3;
  B = 4.0 * areacoord[0] * cube(areacoord[2]);
  pt += B * ctrl_pts[1];

  //i=2; j=0; k=2;
  B = 6.0 * sqr(areacoord[0]) * sqr(areacoord[2]);
  pt += B * ctrl_pts[2];

  //i=3; j=0; k=1;
  B = 4.0 * cube(areacoord[0]) * areacoord[2];
  pt += B * ctrl_pts[3];

  //i=2; j=1; k=1;
  B = 12.0 * sqr(areacoord[0]) * areacoord[1] * areacoord[2];
  pt += B * P_facet[0];

  //i=1; j=2; k=1;
  B = 12.0 * areacoord[0] * sqr(areacoord[1]) * areacoord[2];
  pt += B * P_facet[1];

  //i=1; j=1; k=2;
  B = 12.0 * areacoord[0] * areacoord[1] * sqr(areacoord[2]);
  pt += B * P_facet[2];

  return MB_SUCCESS;

}

//===========================================================================
//Function Name: project_to_facet_plane
//
//Member Type:  PUBLIC
//Descriptoin:  Project a point to the plane of a facet
//===========================================================================
void SmoothFace::project_to_facet_plane(EntityHandle tri, CartVect &pt,
    CartVect &point_on_plane, double &dist_to_plane)
{
  double plane[4];

  ErrorCode rval = _mb->tag_get_data(_planeTag, &tri, 1, plane);
  if (MB_SUCCESS != rval) return;
  assert(rval == MB_SUCCESS);
  // _planeTag
  CartVect normal(&plane[0]);// just first 3 components are used

  double dist = normal % pt + plane[3]; // coeff d is saved!!!
  dist_to_plane = fabs(dist);
  point_on_plane = pt - dist * normal;

  return;
}

//===========================================================================
//Function Name: facet_area_coordinate
//
//Member Type:  PUBLIC
//Descriptoin:  Determine the area coordinates of a point on the plane
//              of a facet
//===========================================================================
void SmoothFace::facet_area_coordinate(EntityHandle facet,
    CartVect & pt_on_plane, CartVect & areacoord)
{

  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  ErrorCode rval = _mb->get_connectivity(facet, conn3, nnodes);
  assert(MB_SUCCESS == rval);
  if (rval) {} // empty statement to prevent compiler warning

  //double coords[9]; // store the coordinates for the nodes
  //_mb->get_coords(conn3, 3, coords);
  CartVect p[3];
  rval = _mb->get_coords(conn3, 3, (double*) &p[0]);
  assert(MB_SUCCESS == rval);
  if (rval) {} // empty statement to prevent compiler warning
  double plane[4];

  rval = _mb->tag_get_data(_planeTag, &facet, 1, plane);
  assert(rval == MB_SUCCESS);
  if (rval) {} // empty statement to prevent compiler warning
  CartVect normal(&plane[0]);// just first 3 components are used

  double area2;

  double tol = GEOMETRY_RESABS * 1.e-5;// 1.e-11;

  CartVect v1(p[1] - p[0]);
  CartVect v2(p[2] - p[0]);

  area2 = (v1 * v2).length_squared();// the same for CartVect
  if (area2 < 100 * tol)
  {
    tol = .01 * area2;
  }
  CartVect absnorm(fabs(normal[0]), fabs(normal[1]), fabs(normal[2]));

  // project to the closest coordinate plane so we only have to do this in 2D

  if (absnorm[0] >= absnorm[1] && absnorm[0] >= absnorm[2])
  {
    area2 = determ3( p[0][1], p[0][2],
        p[1][1], p[1][2],
        p[2][1], p[2][2]);
    if (fabs(area2) < tol)
    {
      areacoord = CartVect(-CUBIT_DBL_MAX);// .set( -CUBIT_DBL_MAX, -CUBIT_DBL_MAX, -CUBIT_DBL_MAX );
    }
    else if (within_tolerance(p[0], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(1., 0., 0.);//.set( 1.0, 0.0, 0.0 );
    }
    else if (within_tolerance(p[1], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 1., 0.);//.set( 0.0, 1.0, 0.0 );
    }
    else if (within_tolerance(p[2], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 0., 1.);//.set( 0.0, 0.0, 1.0 );
    }
    else
    {

      areacoord[0] = determ3(pt_on_plane[1], pt_on_plane[2],
          p[1][1], p[1][2], p[2][1], p[2][2] ) / area2;

      areacoord[1] = determ3( p[0][1], p[0][2],
          pt_on_plane[1], pt_on_plane[2],
          p[2][1], p[2][2]) / area2;

      areacoord[2] = determ3( p[0][1], p[0][2], p[1][1], p[1][2],
          pt_on_plane[1], pt_on_plane[2]) / area2;
    }
  }
  else if (absnorm[1] >= absnorm[0] && absnorm[1] >= absnorm[2])
  {

    area2 = determ3(p[0][0], p[0][2],
        p[1][0], p[1][2],
        p[2][0], p[2][2]);
    if (fabs(area2) < tol)
    {
      areacoord = CartVect(-CUBIT_DBL_MAX);//.set( -CUBIT_DBL_MAX, -CUBIT_DBL_MAX, -CUBIT_DBL_MAX );
    }
    else if (within_tolerance(p[0], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(1., 0., 0.);//.set( 1.0, 0.0, 0.0 );
    }
    else if (within_tolerance(p[1], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 1., 0.);//.set( 0.0, 1.0, 0.0 );
    }
    else if (within_tolerance(p[2], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 0., 1.);//.set( 0.0, 0.0, 1.0 );
    }
    else
    {

      areacoord[0] = determ3(pt_on_plane[0], pt_on_plane[2],
          p[1][0], p[1][2], p[2][0], p[2][2] ) / area2;

      areacoord[1] = determ3( p[0][0], p[0][2],
          pt_on_plane[0], pt_on_plane[2],
          p[2][0], p[2][2]) / area2;

      areacoord[2] = determ3( p[0][0], p[0][2], p[1][0], p[1][2],
          pt_on_plane[0], pt_on_plane[2]) / area2;

    }
  }
  else
  {
    /*area2 = determ3(pt0->x(), pt0->y(),
     pt1->x(), pt1->y(),
     pt2->x(), pt2->y());*/
    area2 = determ3( p[0][0], p[0][1],
        p[1][0], p[1][1],
        p[2][0], p[2][1]);
    if (fabs(area2) < tol)
    {
      areacoord = CartVect(-CUBIT_DBL_MAX);//.set( -CUBIT_DBL_MAX, -CUBIT_DBL_MAX, -CUBIT_DBL_MAX );
    }
    else if (within_tolerance(p[0], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(1., 0., 0.);//.set( 1.0, 0.0, 0.0 );
    }
    else if (within_tolerance(p[1], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 1., 0.);//.set( 0.0, 1.0, 0.0 );
    }
    else if (within_tolerance(p[2], pt_on_plane, GEOMETRY_RESABS))
    {
      areacoord = CartVect(0., 0., 1.);//.set( 0.0, 0.0, 1.0 );
    }
    else
    {

      areacoord[0] = determ3(pt_on_plane[0], pt_on_plane[1],
          p[1][0], p[1][1], p[2][0], p[2][1] ) / area2;

      areacoord[1] = determ3( p[0][0], p[0][1],
          pt_on_plane[0], pt_on_plane[1],
          p[2][0], p[2][1]) / area2;

      areacoord[2] = determ3( p[0][0], p[0][1], p[1][0], p[1][1],
          pt_on_plane[0], pt_on_plane[1]) / area2;
    }
  }
}

ErrorCode SmoothFace::project_to_facets_main(CartVect &this_point, bool trim,
    bool & outside, CartVect * closest_point_ptr, CartVect * normal_ptr)
{

  // if there are a lot of facets on this surface - use the OBB search first
  // to narrow the selection

  _evaluationsCounter++;
  double tolerance = 1.e-5;
  std::vector<EntityHandle> facets_out;
  // we will start with a list of facets anyway, the best among them wins
  ErrorCode rval = _my_geomTopoTool->obb_tree()->closest_to_location(
      (double*) &this_point, _obb_root, tolerance, facets_out);
  if (MB_SUCCESS != rval) return rval;

  int interpOrder = 4;
  double compareTol = 1.e-5;
  EntityHandle lastFacet = facets_out.front();
  rval = project_to_facets(facets_out, lastFacet, interpOrder, compareTol,
      this_point, trim, outside, closest_point_ptr, normal_ptr);

  return rval;
}
ErrorCode SmoothFace::project_to_facets(std::vector<EntityHandle> & facet_list,
    EntityHandle & lastFacet, int interpOrder, double compareTol,
    CartVect &this_point, bool , bool & outside,
    CartVect *closest_point_ptr, CartVect * normal_ptr)
{

  bool outside_facet = false;
  bool best_outside_facet = true;
  double mindist = 1.e20;
  CartVect close_point, best_point(mindist, mindist, mindist), best_areacoord;
  EntityHandle best_facet = 0L;// no best facet found yet
  EntityHandle facet;
  assert(facet_list.size() > 0);

  double big_dist = compareTol * 1.0e3;

  // from the list of close facets, determine the closest point
  for (size_t i = 0; i < facet_list.size(); i++)
  {
    facet = facet_list[i];
    CartVect pt_on_plane;
    double dist_to_plane;
    project_to_facet_plane(facet, this_point, pt_on_plane, dist_to_plane);

    CartVect areacoord;
    //CartVect close_point;
    facet_area_coordinate(facet, pt_on_plane, areacoord);
    if (interpOrder != 0)
    {

      // modify the areacoord - project to the bezier patch- snaps to the
      // edge of the patch if necessary


      if (project_to_facet(facet, this_point, areacoord, close_point,
          outside_facet, compareTol) != MB_SUCCESS)
      {
        return MB_FAILURE;
      }
      //if (closest_point_ptr)
      //*closest_point_ptr = close_point;
    }
    // keep track of the minimum distance

    double dist = (close_point - this_point).length();//close_point.distance_between(this_point);
    if ((best_outside_facet == outside_facet && dist < mindist)
        || (best_outside_facet && !outside_facet && (dist < big_dist
            || best_facet == 0L/*!best_facet*/)))
    {
      mindist = dist;
      best_point = close_point;
      best_facet = facet;
      best_areacoord = areacoord;
      best_outside_facet = outside_facet;

      if (dist < compareTol)
      {
        break;
      }
      big_dist = 10.0 * mindist;
    }
    //facet->marked(1);
    //used_facet_list.append(facet);

  }

  if (normal_ptr)
  {
    CartVect normal;
    if (eval_bezier_patch_normal(best_facet, best_areacoord, normal)
        != MB_SUCCESS)
    {
      return MB_FAILURE;
    }
    *normal_ptr = normal;
  }

  if (closest_point_ptr)
  {
    *closest_point_ptr = best_point;
  }

  outside = best_outside_facet;
  lastFacet = best_facet;

  return MB_SUCCESS;
  //end copy
}

//===========================================================================
//Function Name: project_to_patch
//
//Member Type:  PUBLIC
//Description:  Project a point to a bezier patch. Pass in the areacoord
//              of the point projected to the linear facet.  Function
//              assumes that the point is contained within the patch -
//              if not, it will project to one of its edges.
//===========================================================================
ErrorCode SmoothFace::project_to_patch(EntityHandle facet, // (IN) the facet where the patch is defined
    CartVect &ac, // (IN) area coordinate initial guess (from linear facet)
    CartVect &pt, // (IN) point we are projecting to patch
    CartVect &eval_pt, // (OUT) The projected point
    CartVect *eval_norm, // (OUT) normal at evaluated point
    bool &outside, // (OUT) the closest point on patch to pt is on an edge
    double compare_tol, // (IN) comparison tolerance
    int edge_id) // (IN) only used if this is to be projected to one
//      of the edges.  Otherwise, should be -1
{
  ErrorCode status = MB_SUCCESS;

  // see if we are at a vertex

#define INCR 0.01
  const double tol = compare_tol;

  if (is_at_vertex(facet, pt, ac, compare_tol, eval_pt, eval_norm))
  {
    outside = false;
    return MB_SUCCESS;
  }

  // check if the start ac is inside the patch -if not, then move it there

  int nout = 0;
  const double atol = 0.001;
  if (move_ac_inside(ac, atol))
    nout++;

  int diverge = 0;
  int iter = 0;
  CartVect newpt;
  eval_bezier_patch(facet, ac, newpt);
  CartVect move = pt - newpt;
  double lastdist = move.length();
  double bestdist = lastdist;
  CartVect bestac = ac;
  CartVect bestpt = newpt;
  CartVect bestnorm(0, 0, 0);

  // If we are already close enough, then return now

  if (lastdist <= tol && !eval_norm && nout == 0)
  {
    eval_pt = pt;
    outside = false;
    return status;
  }

  double ratio, mag, umove, vmove, det, distnew, movedist;
  CartVect lastpt = newpt;
  CartVect lastac = ac;
  CartVect norm;
  CartVect xpt, ypt, zpt, xac, yac, zac, xvec, yvec, zvec;
  CartVect du, dv, newac;
  bool done = false;
  while (!done)
  {

    // We will be locating the projected point within the u,v,w coordinate
    // system of the triangular bezier patch.  Since u+v+w=1, the components
    // are not linearly independent.  We will choose only two of the
    // coordinates to use and compute the third.

    int system;
    if (lastac[0] >= lastac[1] && lastac[0] >= lastac[2])
    {
      system = 0;
    }
    else if (lastac[1] >= lastac[2])
    {
      system = 1;
    }
    else
    {
      system = 2;
    }

    // compute the surface derivatives with respect to each
    // of the barycentric coordinates


    if (system == 1 || system == 2)
    {
      xac[0] = lastac[0] + INCR; // xac.x( lastac.x() + INCR );
      if (lastac[1] + lastac[2] == 0.0)
        return MB_FAILURE;
      ratio = lastac[2] / (lastac[1] + lastac[2]);//ratio = lastac.z() / (lastac.y() + lastac.z());
      xac[1] = (1.0 - xac[0]) * (1.0 - ratio);//xac.y( (1.0 - xac.x()) * (1.0 - ratio) );
      xac[2] = 1.0 - xac[0] - xac[1]; // xac.z( 1.0 - xac.x() - xac.y() );
      eval_bezier_patch(facet, xac, xpt);
      xvec = xpt - lastpt;
      xvec /= INCR;
    }
    if (system == 0 || system == 2)
    {
      yac[1] = (lastac[1] + INCR);//yac.y( lastac.y() + INCR );
      if (lastac[0] + lastac[2] == 0.0)//if (lastac.x() + lastac.z() == 0.0)
        return MB_FAILURE;
      ratio = lastac[2] / (lastac[0] + lastac[2]);//ratio = lastac.z() / (lastac.x() + lastac.z());
      yac[0] = ((1.0 - yac[1]) * (1.0 - ratio));//yac.x( (1.0 - yac.y()) * (1.0 - ratio) );
      yac[2] = (1.0 - yac[0] - yac[1]);//yac.z( 1.0 - yac.x() - yac.y() );
      eval_bezier_patch(facet, yac, ypt);
      yvec = ypt - lastpt;
      yvec /= INCR;
    }
    if (system == 0 || system == 1)
    {
      zac[2] = (lastac[2] + INCR);//zac.z( lastac.z() + INCR );
      if (lastac[0] + lastac[1] == 0.0)//if (lastac.x() + lastac.y() == 0.0)
        return MB_FAILURE;
      ratio = lastac[1] / (lastac[0] + lastac[1]);//ratio = lastac.y() / (lastac.x() + lastac.y());
      zac[0] = ((1.0 - zac[2]) * (1.0 - ratio));//zac.x( (1.0 - zac.z()) * (1.0 - ratio) );
      zac[1] = (1.0 - zac[0] - zac[2]);//zac.y( 1.0 - zac.x() - zac.z() );
      eval_bezier_patch(facet, zac, zpt);
      zvec = zpt - lastpt;
      zvec /= INCR;
    }

    // compute the surface normal

    switch (system) {
    case 0:
      du = yvec;
      dv = zvec;
      break;
    case 1:
      du = zvec;
      dv = xvec;
      break;
    case 2:
      du = xvec;
      dv = yvec;
      break;
    }
    norm = du * dv;
    mag = norm.length();
    if (mag < DBL_EPSILON)
    {
      return MB_FAILURE;
      // do something else here (it is likely a flat triangle -
      // so try evaluating just an edge of the bezier patch)
    }
    norm /= mag;
    if (iter == 0)
      bestnorm = norm;

    // project the move vector to the tangent plane

    move = (norm * move) * norm;

    // compute an equivalent u-v-w vector

    CartVect absnorm(fabs(norm[0]), fabs(norm[1]), fabs(norm[2]));
    if (absnorm[2] >= absnorm[1] && absnorm[2] >= absnorm[0])
    {
      det = du[0] * dv[1] - dv[0] * du[1];
      if (fabs(det) <= DBL_EPSILON)
      {
        return MB_FAILURE; // do something else here
      }
      umove = (move[0] * dv[1] - dv[0] * move[1]) / det;
      vmove = (du[0] * move[1] - move[0] * du[1]) / det;
    }
    else if (absnorm[1] >= absnorm[2] && absnorm[1] >= absnorm[0])
    {
      det = du[0] * dv[2] - dv[0] * du[2];
      if (fabs(det) <= DBL_EPSILON)
      {
        return MB_FAILURE;
      }
      umove = (move[0] * dv[2] - dv[0] * move[2]) / det;
      vmove = (du[0] * move[2] - move[0] * du[2]) / det;
    }
    else
    {
      det = du[1] * dv[2] - dv[1] * du[2];
      if (fabs(det) <= DBL_EPSILON)
      {
        return MB_FAILURE;
      }
      umove = (move[1] * dv[2] - dv[1] * move[2]) / det;
      vmove = (du[1] * move[2] - move[1] * du[2]) / det;
    }

    /* === compute the new u-v coords and evaluate surface at new location */

    switch (system) {
    case 0:
      newac[1] = (lastac[1] + umove);//newac.y( lastac.y() + umove );
      newac[2] = (lastac[2] + vmove);//newac.z( lastac.z() + vmove );
      newac[0] = (1.0 - newac[1] - newac[2]);//newac.x( 1.0 - newac.y() - newac.z() );
      break;
    case 1:
      newac[2] = (lastac[2] + umove);//newac.z( lastac.z() + umove );
      newac[0] = (lastac[0] + vmove);//newac.x( lastac.x() + vmove );
      newac[1] = (1.0 - newac[2] - newac[0]);//newac.y( 1.0 - newac.z() - newac.x() );
      break;
    case 2:
      newac[0] = (lastac[0] + umove);//newac.x( lastac.x() + umove );
      newac[1] = (lastac[1] + vmove);//newac.y( lastac.y() + vmove );
      newac[2] = (1.0 - newac[0] - newac[1]);//newac.z( 1.0 - newac.x() - newac.y() );
      break;
    }

    // Keep it inside the patch

    if (newac[0] >= -atol && newac[1] >= -atol && newac[2] >= -atol)
    {
      nout = 0;
    }
    else
    {
      if (move_ac_inside(newac, atol))
        nout++;
    }

    // Evaluate at the new location

    if (edge_id != -1)
      ac_at_edge(newac, newac, edge_id); // move to edge first
    eval_bezier_patch(facet, newac, newpt);

    // Check for convergence

    distnew = (pt - newpt).length();//pt.distance_between(newpt);
    move = newpt - lastpt;
    movedist = move.length();
    if (movedist < tol || distnew < tol)
    {
      done = true;
      if (distnew < bestdist)
      {
        bestdist = distnew;
        bestac = newac;
        bestpt = newpt;
        bestnorm = norm;
      }
    }
    else
    {

      // don't allow more than 30 iterations

      iter++;
      if (iter > 30)
      {
        //if (movedist > tol * 100.0) nout=1;
        done = true;
      }

      // Check for divergence - don't allow more than 5 divergent
      // iterations

      if (distnew > lastdist)
      {
        diverge++;
        if (diverge > 10)
        {
          done = true;
          //if (movedist > tol * 100.0) nout=1;
        }
      }

      // Check if we are continuing to project outside the facet.
      // If so, then stop now

      if (nout > 3)
      {
        done = true;
      }

      // set up for next iteration

      if (!done)
      {
        if (distnew < bestdist)
        {
          bestdist = distnew;
          bestac = newac;
          bestpt = newpt;
          bestnorm = norm;
        }
        lastdist = distnew;
        lastpt = newpt;
        lastac = newac;
        move = pt - lastpt;
      }
    }
  }

  eval_pt = bestpt;
  if (eval_norm)
  {
    *eval_norm = bestnorm;
  }
  outside = (nout > 0) ? true : false;
  ac = bestac;

  return status;
}

//===========================================================================
//Function Name: ac_at_edge
//
//Member Type:  PRIVATE
//Description:  determine the area coordinate of the facet at the edge
//===========================================================================
void SmoothFace::ac_at_edge(CartVect &fac, // facet area coordinate
    CartVect &eac, // edge area coordinate
    int edge_id) // id of edge
{
  double u, v, w;
  switch (edge_id) {
  case 0:
    u = 0.0;
    v = fac[1] / (fac[1] + fac[2]);//v = fac.y() / (fac.y() + fac.z());
    w = 1.0 - v;
    break;
  case 1:
    u = fac[0] / (fac[0] + fac[2]);// u = fac.x() / (fac.x() + fac.z());
    v = 0.0;
    w = 1.0 - u;
    break;
  case 2:
    u = fac[0] / (fac[0] + fac[1]);//u = fac.x() / (fac.x() + fac.y());
    v = 1.0 - u;
    w = 0.0;
    break;
  default:
    assert(0);
    u = -1; // needed to eliminate warnings about used before set
    v = -1; // needed to eliminate warnings about used before set
    w = -1; // needed to eliminate warnings about used before set
    break;
  }
  eac[0] = u;
  eac[1] = v;
  eac[2] = w; //= CartVect(u, v, w);
}

//===========================================================================
//Function Name: project_to_facet
//
//Member Type:  PUBLIC
//Description:  project to a single facet.  Uses the input areacoord as
//              a starting guess.
//===========================================================================
ErrorCode SmoothFace::project_to_facet(EntityHandle facet, CartVect &pt,
    CartVect &areacoord, CartVect &close_point, bool &outside_facet,
    double compare_tol)
{
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  _mb->get_connectivity(facet, conn3, nnodes);
  //
  //double coords[9]; // store the coordinates for the nodes
  //_mb->get_coords(conn3, 3, coords);
  CartVect p[3];
  _mb->get_coords(conn3, 3, (double*) &p[0]);

  int edge_id = -1;
  ErrorCode stat = project_to_patch(facet, areacoord, pt, close_point, NULL,
      outside_facet, compare_tol, edge_id);
  /* }
   break;
   }*/

  return stat;
}

//===========================================================================
//Function Name: is_at_vertex
//
//Member Type:  PRIVATE
//Description:  determine if the point is at one of the facet's vertices
//===========================================================================
bool SmoothFace::is_at_vertex(EntityHandle facet, // (IN) facet we are evaluating
    CartVect &pt, // (IN) the point
    CartVect &ac, // (IN) the ac of the point on the facet plane
    double compare_tol, // (IN) return TRUE of closer than this
    CartVect &eval_pt, // (OUT) location at vertex if TRUE
    CartVect *eval_norm_ptr) // (OUT) normal at vertex if TRUE
{
  double dist;
  CartVect vert_loc;
  const double actol = 0.1;
  // get coordinates get_coords
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  _mb->get_connectivity(facet, conn3, nnodes);
  //
  //double coords[9]; // store the coordinates for the nodes
  //_mb->get_coords(conn3, 3, coords);
  CartVect p[3];
  _mb->get_coords(conn3, 3, (double*) &p[0]);
  // also get the normals at nodes
  CartVect NN[3];
  _mb->tag_get_data(_gradientTag, conn3, 3, (double*) &NN[0]);
  if (fabs(ac[0]) < actol && fabs(ac[1]) < actol)
  {
    vert_loc = p[2];
    dist = (pt - vert_loc).length(); //pt.distance_between( vert_loc );
    if (dist <= compare_tol)
    {
      eval_pt = vert_loc;
      if (eval_norm_ptr)
      {
        *eval_norm_ptr = NN[2];
      }
      return true;
    }
  }

  if (fabs(ac[0]) < actol && fabs(ac[2]) < actol)
  {
    vert_loc = p[1];
    dist = (pt - vert_loc).length();//pt.distance_between( vert_loc );
    if (dist <= compare_tol)
    {
      eval_pt = vert_loc;
      if (eval_norm_ptr)
      {
        *eval_norm_ptr = NN[1];//facet->point(1)->normal( facet );
      }
      return true;
    }
  }

  if (fabs(ac[1]) < actol && fabs(ac[2]) < actol)
  {
    vert_loc = p[0];
    dist = (pt - vert_loc).length();//pt.distance_between( vert_loc );
    if (dist <= compare_tol)
    {
      eval_pt = vert_loc;
      if (eval_norm_ptr)
      {
        *eval_norm_ptr = NN[0];
      }
      return true;
    }
  }

  return false;
}

//===========================================================================
//Function Name: move_ac_inside
//
//Member Type:  PRIVATE
//Description:  find the closest area coordinate to the boundary of the
//              patch if any of its components are < 0
//              Return if the ac was modified.
//===========================================================================
bool SmoothFace::move_ac_inside(CartVect &ac, double tol)
{
  int nout = 0;
  if (ac[0] < -tol)
  {
    ac[0] = 0.0;
    ac[1] = ac[1] / (ac[1] + ac[2]); //( ac.y() / (ac.y() + ac.z()) ;
    ac[2] = 1. - ac[1]; //ac.z( 1.0 - ac.y() );
    nout++;
  }
  if (ac[1] < -tol)
  {
    ac[1] = 0.;//ac.y( 0.0 );
    ac[0] = ac[0] / (ac[0] + ac[2]);//ac.x( ac.x() / (ac.x() + ac.z()) );
    ac[2] = 1. - ac[0];//ac.z( 1.0 - ac.x() );
    nout++;
  }
  if (ac[2] < -tol)
  {
    ac[2] = 0.;// ac.z( 0.0 );
    ac[0] = ac[0] / (ac[0] + ac[1]);//ac.x( ac.x() / (ac.x() + ac.y()) );
    ac[1] = 1. - ac[0]; // ac.y( 1.0 - ac.x() );
    nout++;
  }
  return (nout > 0) ? true : false;
}

//===========================================================================
//Function Name: hodograph
//
//Member Type:  PUBLIC
//Description:  get the hodograph control points for the facet
//Note:  This is a triangle cubic patch that is defined by the
//       normals of quartic facet control point lattice.  Returned coordinates
//       in Nijk are defined by the following diagram
//
//
//                         *9               index  polar
//                        / \                 0     300    point(0)
//                       /   \                1     210
//                     7*-----*8              2     120
//                     / \   / \              3     030    point(1)
//                    /   \ /   \             4     201
//                  4*----5*-----*6           5     111
//                  / \   / \   / \           6     021
//                 /   \ /   \ /   \          7     102
//                *-----*-----*-----*         8     012
//                0     1     2     3         9     003    point(2)
//

//===========================================================================
//Function Name: eval_bezier_patch_normal
//
//Member Type:  PRIVATE
//Description:  evaluate the Bezier patch defined at a facet
//===========================================================================
ErrorCode SmoothFace::eval_bezier_patch_normal(EntityHandle facet,
    CartVect &areacoord, CartVect &normal)
{
  // interpolate internal control points

  CartVect gctrl_pts[6];
  //facet->get_control_points( gctrl_pts );
  ErrorCode rval = _mb->tag_get_data(_facetCtrlTag, &facet, 1, &gctrl_pts[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;
  // _gradientTag
  // get normals at points
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  rval = _mb->get_connectivity(facet, conn3, nnodes);
  if (MB_SUCCESS != rval) return rval;

  CartVect NN[3];
  rval = _mb->tag_get_data(_gradientTag, conn3, 3, &NN[0]);
  assert(rval == MB_SUCCESS);
  if (MB_SUCCESS != rval) return rval;

  if (fabs(areacoord[1] + areacoord[2]) < 1.0e-6)
  {
    normal = NN[0];
    return MB_SUCCESS;
  }
  if (fabs(areacoord[0] + areacoord[2]) < 1.0e-6)
  {
    normal = NN[1];//facet->point(1)->normal(facet);
    return MB_SUCCESS;
  }
  if (fabs(areacoord[0] + areacoord[1]) < 1.0e-6)
  {
    normal = NN[2]; //facet->point(2)->normal(facet);
    return MB_SUCCESS;
  }

  // compute the hodograph of the quartic Gregory patch

  CartVect Nijk[10];
  //hodograph(facet,areacoord,Nijk);
  // start copy from hodograph
  //CubitVector gctrl_pts[6];
  // facet->get_control_points( gctrl_pts );
  CartVect P_facet[3];

  //2,1,1
  /*P_facet[0] = (1.0e0 / (areacoord.y() + areacoord.z())) *
   (areacoord.y() * gctrl_pts[3] +
   areacoord.z() * gctrl_pts[4]);*/
  P_facet[0] = (1.0e0 / (areacoord[1] + areacoord[2])) * (areacoord[1]
      * gctrl_pts[3] + areacoord[2] * gctrl_pts[4]);
  //1,2,1
  /*P_facet[1] = (1.0e0 / (areacoord.x() + areacoord.z())) *
   (areacoord.x() * gctrl_pts[0] +
   areacoord.z() * gctrl_pts[5]);*/
  P_facet[1] = (1.0e0 / (areacoord[0] + areacoord[2])) * (areacoord[0]
      * gctrl_pts[0] + areacoord[2] * gctrl_pts[5]);
  //1,1,2
  /*P_facet[2] = (1.0e0 / (areacoord.x() + areacoord.y())) *
   (areacoord.x() * gctrl_pts[1] +
   areacoord.y() * gctrl_pts[2]);*/
  P_facet[2] = (1.0e0 / (areacoord[0] + areacoord[1])) * (areacoord[0]
      * gctrl_pts[1] + areacoord[1] * gctrl_pts[2]);

  // corner control points are just the normals at the points

  //3, 0, 0
  Nijk[0] = NN[0];
  //0, 3, 0
  Nijk[3] = NN[1];
  //0, 0, 3
  Nijk[9] = NN[2];//facet->point(2)->normal(facet);

  // fill in the boundary control points.  Define as the normal to the local
  // triangle formed by the quartic control point lattice

  // store here again the 9 control points on the edges
  CartVect CP[9]; // 9 control points on the edges,
  rval = _mb->tag_get_data(_facetEdgeCtrlTag, &facet, 1, &CP[0]);
  if (MB_SUCCESS != rval) return rval;
  // there are 3 CP for each edge, 0, 1, 2; first edge is 1-2
  //CubitFacetEdge *edge;
  //edge = facet->edge( 2 );
  //CubitVector ctrl_pts[5];
  //edge->control_points(facet, ctrl_pts);

  //2, 1, 0
  //Nijk[1] = (ctrl_pts[2] - ctrl_pts[1]) * (P_facet[0] - ctrl_pts[1]);
  Nijk[1] = (CP[7] - CP[6]) * (P_facet[0] - CP[6]);
  Nijk[1].normalize();

  //1, 2, 0
  //Nijk[2] = (ctrl_pts[3] - ctrl_pts[2]) * (P_facet[1] - ctrl_pts[2]);
  Nijk[2] = (CP[8] - CP[7]) * (P_facet[1] - CP[7]);
  Nijk[2].normalize();

  //edge = facet->edge( 0 );
  //edge->control_points(facet, ctrl_pts);

  //0, 2, 1
  //Nijk[6] = (ctrl_pts[1] - P_facet[1]) * (ctrl_pts[2] - P_facet[1]);
  Nijk[6] = (CP[0] - P_facet[1]) * (CP[1] - P_facet[1]);
  Nijk[6].normalize();

  //0, 1, 2
  //Nijk[8] = (ctrl_pts[2] - P_facet[2]) * (ctrl_pts[3] - P_facet[2]);
  Nijk[8] = (CP[1] - P_facet[2]) * (CP[2] - P_facet[2]);
  Nijk[8].normalize();

  //edge = facet->edge( 1 );
  //edge->control_points(facet, ctrl_pts);

  //1, 0, 2
  //Nijk[7] = (P_facet[2] - ctrl_pts[2]) * (ctrl_pts[1] - ctrl_pts[2]);
  Nijk[7] = (P_facet[2] - CP[4]) * (CP[3] - CP[4]);
  Nijk[7].normalize();

  //2, 0, 1
  //Nijk[4] = (P_facet[0] - ctrl_pts[3]) * (ctrl_pts[2] - ctrl_pts[3]);
  Nijk[4] = (P_facet[0] - CP[5]) * (CP[4] - CP[5]);
  Nijk[4].normalize();

  //1, 1, 1
  Nijk[5] = (P_facet[1] - P_facet[0]) * (P_facet[2] - P_facet[0]);
  Nijk[5].normalize();
  // end copy from hodograph

  // sum the contribution from each of the control points

  normal = CartVect(0.0e0, 0.0e0, 0.0e0);

  //i=3; j=0; k=0;
  //double Bsum = 0.0;
  double B = cube(areacoord[0]);
  //Bsum += B;
  normal += B * Nijk[0];

  //i=2; j=1; k=0;
  B = 3.0 * sqr(areacoord[0]) * areacoord[1];
  //Bsum += B;
  normal += B * Nijk[1];

  //i=1; j=2; k=0;
  B = 3.0 * areacoord[0] * sqr(areacoord[1]);
  //Bsum += B;
  normal += B * Nijk[2];

  //i=0; j=3; k=0;
  B = cube(areacoord[1]);
  //Bsum += B;
  normal += B * Nijk[3];

  //i=2; j=0; k=1;
  B = 3.0 * sqr(areacoord[0]) * areacoord[2];
  //Bsum += B;
  normal += B * Nijk[4];

  //i=1; j=1; k=1;
  B = 6.0 * areacoord[0] * areacoord[1] * areacoord[2];
  //Bsum += B;
  normal += B * Nijk[5];

  //i=0; j=2; k=1;
  B = 3.0 * sqr(areacoord[1]) * areacoord[2];
  //Bsum += B;
  normal += B * Nijk[6];

  //i=1; j=0; k=2;
  B = 3.0 * areacoord[0] * sqr(areacoord[2]);
  //Bsum += B;
  normal += B * Nijk[7];

  //i=0; j=1; k=2;
  B = 3.0 * areacoord[1] * sqr(areacoord[2]);
  //Bsum += B;
  normal += B * Nijk[8];

  //i=0; j=0; k=3;
  B = cube(areacoord[2]);
  //Bsum += B;
  normal += B * Nijk[9];

  //assert(fabs(Bsum - 1.0) < 1e-9);

  normal.normalize();

  return MB_SUCCESS;
}

ErrorCode SmoothFace::get_normals_for_vertices(const EntityHandle * conn2,
    CartVect N[2])
// this method will be called to retrieve the normals needed in the calculation of control edge points..
{
  //CartVect N[2];
  //_mb->tag_get_data(_gradientTag, conn2, 2, normalVec);
  ErrorCode rval = _mb->tag_get_data(_gradientTag, conn2, 2, (double*) &N[0]);
  return rval;
}

ErrorCode SmoothFace::ray_intersection_correct(EntityHandle , // (IN) the facet where the patch is defined
    CartVect &pt, // (IN) shoot from
    CartVect &ray, // (IN) ray direction
    CartVect &eval_pt, // (INOUT) The intersection point
    double & distance, // (IN OUT) the new distance
    bool &outside)
{
  // find a point on the smooth surface
  CartVect currentPoint = eval_pt;
  int numIter = 0;
  double improvement = 1.e20;
  CartVect diff;
  while (numIter++ < 5 && improvement > 0.01)
  {
    CartVect newPos;

    bool trim = false;// is it needed?
    outside = true;
    CartVect closestPoint;
    CartVect normal;

    ErrorCode rval = project_to_facets_main(currentPoint, trim, outside,
        &newPos, &normal);
    if (MB_SUCCESS != rval) return rval;
    assert(rval==MB_SUCCESS);
    diff = newPos - currentPoint;
    improvement = diff.length();
    // ( pt + t * ray - closest ) % normal = 0;
    // intersect tangent plane that goes through closest point with the direction
    // t = normal%(closest-pt) / normal%ray;
    double dot = normal % ray; // if it is 0, get out while we can
    if (dot < 0.00001)
    {
      // bad convergence, get out, do not modify anything
      return MB_SUCCESS;
    }
    double t = ((newPos - pt) % normal) / (dot);
    currentPoint = pt + t * ray;

  }
  eval_pt = currentPoint;
  diff = currentPoint - pt;
  distance = diff.length();
  return MB_SUCCESS;
}
}// namespace moab
