/*
 * SmoothCurve.cpp
 *
 */

#include "SmoothCurve.hpp"
//#include "SmoothVertex.hpp"
#include "SmoothFace.hpp"
#include "assert.h"
#include <iostream>
#include "moab/GeomTopoTool.hpp"

namespace moab {

SmoothCurve::SmoothCurve(Interface * mb, EntityHandle curve, GeomTopoTool * gTool ):
 _mb(mb), _set(curve), _gtt(gTool)
{
  //_mbOut->create_meshset(MESHSET_ORDERED, _oSet);
  /*_cmlEdgeMesher = new CMLEdgeMesher (this, CML::STANDARD);
   _cmlEdgeMesher->set_sizing_function(CML::LINEAR_SIZING);*/
  _leng = 0; // not initialized
  _edgeTag = 0; // not initialized
}
SmoothCurve::~SmoothCurve()
{
  // TODO Auto-generated destructor stub
}

double SmoothCurve::arc_length()
{

  return _leng;
}

//! \brief Get the parametric status of the curve.
//!
//! \return \a true if curve is parametric, \a false otherwise.
bool SmoothCurve::is_parametric()
{
  return true;
}

//! \brief Get the periodic status of the curve.
//!
//! \param period The period of the curve if periodic.
//!
//! \return \a true if curve is periodic, \a false otherwise.
bool SmoothCurve::is_periodic(double& period)
{
  //assert(_ref_edge);
  //return _ref_edge->is_periodic(   period);
  Range vsets;
  _mb->get_child_meshsets(_set, vsets);// num_hops =1
  if (vsets.size() == 1)
  {
    period = _leng;
    return true;//  true , especially for ice sheet data
  }
  return false;
}

//! \brief Get the parameter range of the curve.
//!
//! \param u_start The beginning curve parameter
//! \param u_end The ending curve parameter
//!
//! \note The numerical value of \a u_start may be greater
//! than the numerical value of \a u_end.
void SmoothCurve::get_param_range(double& u_start, double& u_end)
{
  //assert(_ref_edge);
  u_start = 0;
  u_end = 1.;

  return;
}

//! Compute the parameter value at a specified distance along the curve.
//!
//! \param u_root The start parameter from which to compute the distance
//! along the curve.
//! \param arc_length The distance to move along the curve.
//!
//! \note For positive values of \a arc_length the distance will be
//! computed in the direction of increasing parameter value along the
//! curve.  For negative values of \a arc_length the distance will be
//! computed in the direction of decreasing parameter value along the
//! curve.
//!
//! \return The parametric coordinate u along the curve
double SmoothCurve::u_from_arc_length(double u_root, double arc_leng)
{

  if (_leng <= 0)
    return 0;
  return u_root + arc_leng / _leng;
}

//! \brief Evaluate the curve at a specified parameter value.
//!
//! \param u The parameter at which to evaluate the curve
//! \param x The x coordinate of the evaluated point
//! \param y The y coordinate of the evaluated point
//! \param z The z coordinate of the evaluated point
bool SmoothCurve::position_from_u(double u, double& x, double& y, double& z, double * tg)
{

  // _fractions are increasing, so find the
  double * ptr = std::lower_bound(&_fractions[0],
      &_fractions[_fractions.size()], u);
  int index = ptr - &_fractions[0];
  double nextFraction = _fractions[index];
  double prevFraction = 0;
  if (index > 0)
  {
    prevFraction = _fractions[index - 1];
  }
  double t = (u - prevFraction) / (nextFraction - prevFraction);

  EntityHandle edge = _entities[index];

  CartVect position, tangent;
  ErrorCode rval = evaluate_smooth_edge(edge, t, position, tangent);
  if (MB_SUCCESS != rval) return false;
  assert(rval==MB_SUCCESS);
  x = position[0];
  y = position[1];
  z = position[2];
  if (tg)
  {
    // we need to do some scaling,
    double dtdu = 1/(nextFraction - prevFraction);
    tg[0] = tangent[0] * dtdu;
    tg[1] = tangent[1] * dtdu;
    tg[2] = tangent[2] * dtdu;
  }

  return true;

}
//! \brief Move a point near the curve to the closest point on the curve.
//!
//! \param x The x coordinate of the point
//! \param y The y coordinate of the point
//! \param z The z coordinate of the point
void SmoothCurve::move_to_curve(double& x, double& y, double& z)
{

  // find closest point to the curve, and the parametric position
  // must be close by, but how close ???
  EntityHandle v;
  int edgeIndex;
  double u = u_from_position( x,  y,   z, v, edgeIndex);
  position_from_u(u, x, y, z);

  return;
}

//! Get the u parameter value on the curve closest to x,y,z
//! and the point on the curve.
//!
//! \param x The x coordinate of the point
//! \param y The y coordinate of the point
//! \param z The z coordinate of the point
//!
//! \return The parametric coordinate u on the curve
double SmoothCurve::u_from_position(double x, double y, double z, EntityHandle & v,
    int & edgeIndex)
{
  // this is an iterative process, expensive usually
  // get first all nodes , and their positions
  // find the closest node (and edge), and from there do some
  // iterations up to a point
  // do not exaggerate with convergence criteria

  v= 0; // we do not have a close by vertex yet
  CartVect initialPos(x, y, z);
  double u=0;
  int nbNodes = (int)_entities.size()*2;// the mesh edges are stored
  std::vector<EntityHandle> nodesConnec;
  nodesConnec.resize(nbNodes);
  ErrorCode rval = this->_mb->get_connectivity(&(_entities[0]), nbNodes/2, nodesConnec);
  if (MB_SUCCESS!=rval)
  {
    std::cout<<"error in getting connectivity\n";
    return 0;
  }
  // collapse nodesConnec, nodes should be in order
  for (int k=0; k<nbNodes/2; k++)
  {
    nodesConnec[k+1] = nodesConnec[2*k+1];
  }
  int numNodes = nbNodes/2+1;
  std::vector<CartVect> coordNodes;
  coordNodes.resize(numNodes);

  rval = _mb->get_coords(&(nodesConnec[0]), numNodes, (double*)&(coordNodes[0]));
  if (MB_SUCCESS!=rval)
  {
    std::cout<<"error in getting node positions\n";
    return 0;
  }
  // find the closest node, then find the closest edge, based on closest node

  int indexNode = 0;
  double minDist = 1.e30;
  // expensive linear search
  for (int i=0; i<numNodes; i++)
  {
    double d1 = (initialPos-coordNodes[i]).length();
    if (d1<minDist)
    {
      indexNode = i;
      minDist = d1;
    }
  }
  double tolerance = 0.00001; // what is the unit?
  // something reasonable
  if (minDist<tolerance)
  {
    v = nodesConnec[indexNode];
    // we are done, just return the proper u (from fractions)
    if (indexNode==0)
    {
      return 0; // first node has u = 0
    }
    else
      return _fractions[indexNode-1]; // fractions[0] > 0!!)
  }
  // find the mesh edge; could be previous or next edge
  edgeIndex = indexNode;// could be the previous one, though!!
  if (edgeIndex == numNodes-1)
    edgeIndex--;// we have one less edge, and do not worry about next edge
  else
  {
    if (edgeIndex>0)
    {
      // could be the previous; decide based on distance to the other
      // nodes of the 2 connected edges
      CartVect  prevNodePos = coordNodes[edgeIndex-1];
      CartVect  nextNodePos = coordNodes[edgeIndex+1];
      if ( (prevNodePos-initialPos).length_squared() <
           (nextNodePos-initialPos).length_squared() )
      {
        edgeIndex --;
      }
    }
  }
  // now, we know for sure that the closest point is somewhere on edgeIndex edge
  //

  // do newton iteration for local t between 0 and 1

  // copy from evaluation method
  CartVect P[2]; // P0 and P1
  CartVect controlPoints[3]; // edge control points
  double t4, t3, t2, one_minus_t, one_minus_t2, one_minus_t3, one_minus_t4;

  P[0] = coordNodes[edgeIndex];
  P[1] = coordNodes[edgeIndex+1];

  if (0 == _edgeTag)
  {
    rval = _mb->tag_get_handle("CONTROLEDGE", 9, MB_TYPE_DOUBLE, _edgeTag);
    if (rval != MB_SUCCESS)
      return 0;
  }
  rval = _mb->tag_get_data(_edgeTag, &(_entities[edgeIndex]), 1, (double*) &controlPoints[0]);
  if (rval != MB_SUCCESS)
    return rval;

  // starting point
  double tt = 0.5;// between the 2 ends of the edge
  int iterations=0;
  // find iteratively a better point
  int maxIterations = 10; // not too many
  CartVect outv;
  // we will solve minimize F = 0.5 * ( ini - r(t) )^2
  // so solve  F'(t) = 0
  // Iteration:  t_ -> t - F'(t)/F"(t)
  // F'(t) = r'(t) (ini-r(t) )
  // F"(t) = r"(t) (ini-r(t) ) - (r'(t))^2
  while (iterations<maxIterations)
  //
  {
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

    CartVect out_tangent = -4.*one_minus_t3*P[0] +
              4.*(one_minus_t3 -3.*tt*one_minus_t2)*controlPoints[0] +
              12.*(tt*one_minus_t2 - t2*one_minus_t)*controlPoints[1] +
              4.*(3.*t2*one_minus_t - t3)*controlPoints[2] +
              4.*t3*P[1];

    CartVect second_deriv = 12.*one_minus_t2 * P[0] +
        4.*( -3.*one_minus_t2 -3.*one_minus_t2 + 6.*tt * one_minus_t)*controlPoints[0] +
        12.*(one_minus_t2 - 4*tt*one_minus_t + t2)*controlPoints[1] +
        4.*(6.*tt - 12*t2)*controlPoints[2] +
        12.*t2*P[1];
    CartVect diff = outv - initialPos;
    double F_d =  out_tangent % diff;
    double F_dd = second_deriv % diff + out_tangent.length_squared();

    if (0==F_dd)
      break;// get out, we found minimum?

    double delta_t = - F_d/F_dd;

    if (fabs(delta_t) < 0.000001)
      break;
    tt = tt+delta_t;
    if (tt<0)
    {
      tt = 0.;
      v = nodesConnec[edgeIndex];// we are at end of mesh edge
      break;
    }
    if (tt>1)
    {
      tt = 1;
      v = nodesConnec[edgeIndex+1];// we are at one end
      break;
    }
    iterations++;
  }
  // so we have t on the segment, convert to u, which should
  // be between _fractions[edgeIndex] numbers
  double prevFraction = 0;
  if (edgeIndex>0)
    prevFraction = _fractions[edgeIndex-1];

  u =prevFraction + tt*(_fractions[edgeIndex] - prevFraction);
  return u;
}

//! \brief Get the starting point of the curve.
//!
//! \param x The x coordinate of the start point
//! \param y The y coordinate of the start point
//! \param z The z coordinate of the start point
void SmoothCurve::start_coordinates(double& x, double& y, double& z)
{

  int nnodes;
  const EntityHandle * conn2;
  _mb->get_connectivity(_entities[0], conn2, nnodes);
  double c[3];
  _mb->get_coords(conn2, 1, c);

  x = c[0];
  y = c[1];
  z = c[2];

  return;
}

//! \brief Get the ending point of the curve.
//!
//! \param x The x coordinate of the start point
//! \param y The y coordinate of the start point
//! \param z The z coordinate of the start point
void SmoothCurve::end_coordinates(double& x, double& y, double& z)
{

  int nnodes;
  const EntityHandle * conn2;
  _mb->get_connectivity(_entities[_entities.size() - 1], conn2, nnodes);
  double c[3];
  // careful, the second node here
  _mb->get_coords(&conn2[1], 1, c);

  x = c[0];
  y = c[1];
  z = c[2];
  return;
}

// this will recompute the 2 tangents for each edge, considering the geo edge they are into
void SmoothCurve::compute_tangents_for_each_edge()
{
  // will retrieve the edges in each set; they are retrieved in order they were put into, because
  // these sets are "MESHSET_ORDERED"
  // retrieve the tag handle for the tangents; it should have been created already
  // this tangents are computed for the chain of edges that form a geometric edge
  // some checks should be performed on the vertices, but we trust the correctness of the model completely
  // (like the vertices should match in the chain...)
  Tag tangentsTag;
  ErrorCode rval = _mb->tag_get_handle("TANGENTS", 6, MB_TYPE_DOUBLE, tangentsTag);
  if (rval != MB_SUCCESS)
    return; // some error should be thrown
  std::vector<EntityHandle> entities;
  _mb->get_entities_by_type(_set, MBEDGE, entities); // no recursion!!
  // basically, each tangent at a node will depend on previous tangent
  int nbEdges = entities.size();
  // now, we can advance in the loop
  // the only special problem is if the first node coincides with the last node, then we should
  // consider the closed loop; or maybe we should look at angles in that case too?
  // also, should we look at the 2 semi-circles case? How to decide if we need to continue the "tangents"
  // maybe we can do that later, and we can alter the tangents at the feature nodes, in the directions of the loops
  // again, do we need to decide the "closed" loop or not? Not yet...
  EntityHandle previousEdge = entities[0]; // this is the first edge in the chain
  CartVect TP[2]; // tangents for the previous edge
  rval = _mb->tag_get_data(tangentsTag, &previousEdge, 1, &TP[0]);// tangents for previous edge
  if (rval != MB_SUCCESS)
    return; // some error should be thrown
  CartVect TC[2]; // tangents for the current edge
  EntityHandle currentEdge;
  for (int i = 1; i < nbEdges; i++)
  {
    // current edge will start after first one
    currentEdge = entities[i];
    rval = _mb->tag_get_data(tangentsTag, &currentEdge, 1, &TC[0]);//
    // now compute the new tangent at common vertex; reset tangents for previous edge and current edge
    // a little bit of CPU and memory waste, but this is life
    CartVect T = 0.5 * TC[0] + 0.5 * TP[1]; //
    T.normalize();
    TP[1] = T;
    rval = _mb->tag_set_data(tangentsTag, &previousEdge, 1, &TP[0]);//
    TC[0] = T;
    rval = _mb->tag_set_data(tangentsTag, &currentEdge, 1, &TC[0]);//
    // now set the next edge
    previousEdge = currentEdge;
    TP[0] = TC[0];
    TP[1] = TC[1];
  }
  return;
}

void SmoothCurve::compute_control_points_on_boundary_edges(double ,
    std::map<EntityHandle, SmoothFace*> & mapSurfaces, Tag controlPointsTag,
    Tag markTag)
{
  // these points really need the surfaces they belong to, because the control points on edges
  // depend on the normals on surfaces
  // the control points are averaged from different surfaces, by simple mean average
  // the surfaces have
  // do we really need min_dot here?
  // first of all, find out the SmoothFace for each surface set that is adjacent here
  //GeomTopoTool gTopoTool(_mb);
  std::vector<EntityHandle> faces;
  std::vector<int> senses;
  ErrorCode rval = _gtt->get_senses(_set, faces, senses);
  if (MB_SUCCESS != rval)
    return;

  // need to find the smooth face attached
  unsigned int numSurfacesAdjacent = faces.size();
  // get the edges, and then get the
  //std::vector<EntityHandle> entities;
  _mb->get_entities_by_type(_set, MBEDGE, _entities); // no recursion!!
  // each edge has the tangent computed already
  Tag tangentsTag;
  rval = _mb->tag_get_handle("TANGENTS", 6, MB_TYPE_DOUBLE, tangentsTag);
  if (rval != MB_SUCCESS)
    return; // some error should be thrown

  // we do not want to search every time
  std::vector<SmoothFace*> smoothFaceArray;
  unsigned int i = 0;
  for (i = 0; i < numSurfacesAdjacent; i++)
  {
    SmoothFace * sms = mapSurfaces[faces[i]];
    smoothFaceArray.push_back(sms);
  }

  unsigned int e = 0;
  for (e = 0; e < _entities.size(); e++)
  {
    CartVect zero(0.);
    CartVect ctrlP[3] = { zero, zero, zero };// null positions initially
    // the control points are averaged from connected faces
    EntityHandle edge = _entities[e]; // the edge in the chain

    int nnodes;
    const EntityHandle * conn2;
    rval = _mb->get_connectivity(edge, conn2, nnodes);
    if (rval != MB_SUCCESS || 2 != nnodes)
      return;// or continue or return error

    //double coords[6]; // store the coordinates for the nodes
    CartVect P[2];
    //ErrorCode rval = _mb->get_coords(conn2, 2, coords);
    rval = _mb->get_coords(conn2, 2, (double*) &P[0]);
    if (rval != MB_SUCCESS)
      return;

    CartVect chord = P[1] - P[0];
    _leng += chord.length();
    _fractions.push_back(_leng);
    CartVect N[2];

    //MBCartVect N0(&normalVec[0]);
    //MBCartVect N3(&normalVec[3]);
    CartVect T[2]; // T0, T3
    //if (edge->num_adj_facets() <= 1) {
    //stat = compute_curve_tangent(edge, min_dot, T0, T3);
    //  if (stat != CUBIT_SUCCESS)
    //	return stat;
    //} else {
    //}
    rval = _mb->tag_get_data(tangentsTag, &edge, 1, &T[0]);
    if (rval != MB_SUCCESS)
      return;

    for (i = 0; i < numSurfacesAdjacent; i++)
    {
      CartVect controlForEdge[3];
      rval = smoothFaceArray[i]->get_normals_for_vertices(conn2, N);
      if (rval != MB_SUCCESS)
        return;

      rval = smoothFaceArray[i]->init_edge_control_points(P[0], P[1], N[0],
          N[1], T[0], T[1], controlForEdge);
      if (rval != MB_SUCCESS)
        return;

      // accumulate those over faces!!!
      for (int j = 0; j < 3; j++)
      {
        ctrlP[j] += controlForEdge[j];
      }
    }
    // now divide them for the average position!
    for (int j = 0; j < 3; j++)
    {
      ctrlP[j] /= numSurfacesAdjacent;
    }
    // we are done, set the control points now!
    //edge->control_points(ctrl_pts, 4);
    rval = _mb->tag_set_data(controlPointsTag, &edge, 1, &ctrlP[0]);
    if (rval != MB_SUCCESS)
      return;

    this->_edgeTag = controlPointsTag;// this is a tag that will be stored with the edge
    // is that a waste of memory or not...
    // also mark the edge for later on
    unsigned char used = 1;
    _mb->tag_set_data(markTag, &edge, 1, &used);
  }
  // now divide fractions, to make them vary from 0 to 1
  assert(_leng>0.);
  for (e = 0; e < _entities.size(); e++)
    _fractions[e] /= _leng;

}

ErrorCode SmoothCurve::evaluate_smooth_edge(EntityHandle eh, double &tt,
    CartVect & outv, CartVect & out_tangent)
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

  int nnodes;
  const EntityHandle * conn2;
  ErrorCode rval = _mb->get_connectivity(eh, conn2, nnodes);
  if (rval != MB_SUCCESS)
    return rval;

  rval = _mb->get_coords(conn2, 2, (double*) &P[0]);
  if (rval != MB_SUCCESS)
    return rval;

  if (0 == _edgeTag)
  {
    rval = _mb->tag_get_handle("CONTROLEDGE", 9, MB_TYPE_DOUBLE, _edgeTag);
    if (rval != MB_SUCCESS)
      return rval;
  }
  rval = _mb->tag_get_data(_edgeTag, &eh, 1, (double*) &controlPoints[0]);
  if (rval != MB_SUCCESS)
    return rval;

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

  out_tangent = -4.*one_minus_t3*P[0] +
            4.*(one_minus_t3 -3.*tt*one_minus_t2)*controlPoints[0] +
            12.*(tt*one_minus_t2 - t2*one_minus_t)*controlPoints[1] +
            4.*(3.*t2*one_minus_t - t3)*controlPoints[2] +
            4.*t3*P[1];
  return MB_SUCCESS;
}
} // namespace moab
