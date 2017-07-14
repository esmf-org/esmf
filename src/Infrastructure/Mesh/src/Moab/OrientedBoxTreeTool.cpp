/*
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**\file OrientedBox.hpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2006-07-18
 */

#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/OrientedBoxTreeTool.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "moab/GeomUtil.hpp"
#include "MBTagConventions.hpp"
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <limits>
#include <assert.h>
#include <math.h>

//#define MB_OBB_USE_VECTOR_QUERIES
//#define MB_OBB_USE_TYPE_QUERIES

namespace moab {

/**\brief Determine if a ray-edge/node intersection is glancing or piercing.
 *        This function avoids asking for upward adjacencies to prevent their
 *        creation.
 *\param tri           The intersected triangle
 *\param ray_direction The direction of the ray
 *\param int_type      The type of intersection (EDGE0, EDGE1, NODE2, ...)
 *\param close_tris    Vector of triangles in the proximity of the intersection
 *\param close_senses  Vector of surface senses for tris in the proximity of
 *                     the intersection
 *\param neighborhood  Vector of triangles in the topological neighborhood of the intersection
 *\return              True if piercing, false otherwise.
 */
static
bool edge_node_intersect(const EntityHandle                tri,
                         const CartVect&                   ray_direction,
                         const GeomUtil::intersection_type int_type,
                         const std::vector<EntityHandle>&  close_tris,
                         const std::vector<int>&           close_senses,
                         const Interface*                  MBI,
                         std::vector<EntityHandle>*        neighborhood_tris = 0 ) {

  // get the node of the triangle
  const EntityHandle* conn = NULL;
  int len = 0;
  ErrorCode rval = MBI->get_connectivity( tri, conn, len );
  if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;

  // get adjacent tris (and keep their corresponding senses)
  std::vector<EntityHandle> adj_tris;
  std::vector<int>          adj_senses;

  // node intersection
  if(GeomUtil::NODE0==int_type || GeomUtil::NODE1==int_type || GeomUtil::NODE2==int_type) {
 
    // get the intersected node
    EntityHandle node;
    if     (GeomUtil::NODE0==int_type) node = conn[0];
    else if(GeomUtil::NODE1==int_type) node = conn[1];
    else                               node = conn[2];

    // get tris adjacent to node
    for(unsigned i=0; i<close_tris.size(); ++i) {
      const EntityHandle* con = NULL;
      rval = MBI->get_connectivity( close_tris[i], con, len );
      if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;

      if(node==con[0] || node==con[1] || node==con[2]) {
        adj_tris.push_back(   close_tris[i]   );
        adj_senses.push_back( close_senses[i] );
      }
    }
    if( adj_tris.empty() ) {
      std::cerr << "error: no tris are adjacent to the node" << std::endl;
      return MB_FAILURE;
    }
  // edge intersection
  } else if(GeomUtil::EDGE0==int_type || GeomUtil::EDGE1==int_type || GeomUtil::EDGE2==int_type) {

    // get the endpoints of the edge
    EntityHandle endpts[2];
    if       (GeomUtil::EDGE0==int_type) {
      endpts[0] = conn[0];
      endpts[1] = conn[1]; 
    } else if(GeomUtil::EDGE1==int_type) {
      endpts[0] = conn[1];
      endpts[1] = conn[2];
    } else {
      endpts[0] = conn[2];
      endpts[1] = conn[0];
    }

    // get tris adjacent to edge
    for(unsigned i=0; i<close_tris.size(); ++i) {
      const EntityHandle* con = NULL;
      rval = MBI->get_connectivity( close_tris[i], con, len );
      if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;

      // check both orientations in case close_tris are not on the same surface
      if( (endpts[0]==con[0] && endpts[1]==con[1]) ||
          (endpts[0]==con[1] && endpts[1]==con[0]) ||
          (endpts[0]==con[1] && endpts[1]==con[2]) ||
          (endpts[0]==con[2] && endpts[1]==con[1]) ||
          (endpts[0]==con[2] && endpts[1]==con[0]) ||
          (endpts[0]==con[0] && endpts[1]==con[2]) ) {
        adj_tris.push_back(   close_tris[i]   );
        adj_senses.push_back( close_senses[i] );
      }
    }   
    // In a 2-manifold each edge is adjacent to exactly 2 tris
    if(2 != adj_tris.size() ) {
      std::cerr << "error: edge of a manifold must be topologically adjacent to exactly 2 tris" 
                << std::endl;
      MBI->list_entities( endpts, 2 );
      return true;
    }
  } else {
    std::cerr << "error: special case not an node/edge intersection" << std::endl;
    return MB_FAILURE;
  }

  // The close tris were in proximity to the intersection. The adj_tris are
  // topologically adjacent to the intersection (the neighborhood).
  if(neighborhood_tris) (*neighborhood_tris).assign( adj_tris.begin(), adj_tris.end() );

  // determine glancing/piercing
  // If a desired_orientation was used in this call to ray_intersect_sets, 
  // the plucker_ray_tri_intersect will have already used it. For a piercing
  // intersection, the normal of all tris must have the same orientation.
  int sign = 0;
  for(unsigned i=0; i<adj_tris.size(); ++i) {
    const EntityHandle* con = NULL;
    rval = MBI->get_connectivity( adj_tris[i], con, len );
    if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;
    CartVect coords[3];
    rval = MBI->get_coords( con, len, coords[0].array() );      
    if(MB_SUCCESS != rval) return MB_FAILURE;

    // get normal of triangle
    CartVect v0 = coords[1] - coords[0];
    CartVect v1 = coords[2] - coords[0];
    CartVect norm = adj_senses[i]*(v0*v1);
    double dot_prod = norm%ray_direction;

    // if the sign has not yet been decided, choose it
    if(0==sign && 0!=dot_prod) {
      if(0<dot_prod) sign = 1;
      else           sign = -1;
    }

    // intersection is glancing if tri and ray do not point in same direction
    // for every triangle
    if(0!=sign && 0>sign*dot_prod) return false;

  }
  return true;

}

#if defined(MB_OBB_USE_VECTOR_QUERIES) && defined(MB_OBB_USE_TYPE_QUERIES)
# undef MB_OBB_USE_TYPE_QUERIES
#endif

const char DEFAULT_TAG_NAME[] = "OBB";

OrientedBoxTreeTool::Op::~Op() {}

OrientedBoxTreeTool::OrientedBoxTreeTool( Interface* i,
                                              const char* tag_name,
                                              bool destroy_created_trees )
  : instance( i ), cleanUpTrees(destroy_created_trees)
{
  if (!tag_name)
    tag_name = DEFAULT_TAG_NAME;
  ErrorCode rval = OrientedBox::tag_handle( tagHandle, instance, tag_name);
  if (MB_SUCCESS != rval)
    tagHandle = 0;
}

OrientedBoxTreeTool::~OrientedBoxTreeTool()
{
  if (!cleanUpTrees)
    return;
    
  while (!createdTrees.empty()) {
    EntityHandle tree = createdTrees.back();
      // make sure this is a tree (rather than some other, stale handle)
    const void* data_ptr = 0;
    ErrorCode rval = instance->tag_get_by_ptr( tagHandle, &tree, 1, &data_ptr );
    if (MB_SUCCESS == rval)
      rval = delete_tree( tree );
    if (MB_SUCCESS != rval)
      createdTrees.pop_back();
  }
}

OrientedBoxTreeTool::Settings::Settings() 
  : max_leaf_entities( 8 ),
    max_depth( 0 ),
    worst_split_ratio( 0.95 ),
    best_split_ratio( 0.4 ),
    set_options( MESHSET_SET )
  {}

bool OrientedBoxTreeTool::Settings::valid() const
{
  return max_leaf_entities > 0 
      && max_depth >= 0
      && worst_split_ratio <= 1.0
      && best_split_ratio >= 0.0
      && worst_split_ratio >= best_split_ratio
      ;
}


/********************** Simple Tree Access Methods ****************************/


ErrorCode OrientedBoxTreeTool::box( EntityHandle set, OrientedBox& obb )
{
  return instance->tag_get_data( tagHandle, &set, 1, &obb );
}

ErrorCode OrientedBoxTreeTool::box( EntityHandle set,
                                        double center[3],
                                        double axis1[3],
                                        double axis2[3],
                                        double axis3[3] )
{
  OrientedBox obb;
  ErrorCode rval = this->box( set, obb );
  obb.center.get( center );
  obb.scaled_axis(0).get( axis1 );
  obb.scaled_axis(1).get( axis2 );
  obb.scaled_axis(2).get( axis3 );
  return rval;
}


/********************** Tree Construction Code ****************************/


struct OrientedBoxTreeTool::SetData {
  EntityHandle handle;
  OrientedBox::CovarienceData box_data;
  //Range vertices;
};


ErrorCode OrientedBoxTreeTool::build( const Range& entities,
                                          EntityHandle& set_handle_out,
                                          const Settings* settings )
{
  if (!entities.all_of_dimension(2))
    return MB_TYPE_OUT_OF_RANGE;
  if (settings && !settings->valid())
    return MB_FAILURE;
    
  return build_tree( entities, set_handle_out, 0, 
                     settings ? *settings : Settings() );
}

ErrorCode OrientedBoxTreeTool::join_trees( const Range& sets,
                                               EntityHandle& set_handle_out,
                                               const Settings* settings )
{
  if (!sets.all_of_type(MBENTITYSET))
    return MB_TYPE_OUT_OF_RANGE;
  if (settings && !settings->valid())
    return MB_FAILURE;
  
    // Build initial set data list.
  std::list<SetData> data;
  for (Range::const_iterator i = sets.begin(); i != sets.end(); ++i) {
    Range elements;
    ErrorCode rval = instance->get_entities_by_dimension( *i, 2, elements, true );
    if (MB_SUCCESS != rval)
      return rval;
    if (elements.empty())
      continue;
    
    data.push_back( SetData() );
    SetData& set_data = data.back();
    set_data.handle = *i;
    rval = OrientedBox::covariance_data_from_tris( set_data.box_data, instance, elements );
    if (MB_SUCCESS != rval)
      return rval;
  }

  ErrorCode result = build_sets( data, set_handle_out, 0, 
                          settings ? *settings : Settings() );
  if (MB_SUCCESS != result)
    return result;
  
  for (Range::reverse_iterator i = sets.rbegin(); i != sets.rend(); ++i) {
    createdTrees.erase(
      std::remove( createdTrees.begin(), createdTrees.end(), *i ), 
      createdTrees.end() );
  }
  createdTrees.push_back( set_handle_out );
  return MB_SUCCESS;
}
  

/**\brief Split triangles by which side of a plane they are on
 *
 * Given a plane specified as a bisecting plane normal to one
 * of the axes of a box, split triangles based on which side
 * of the plane they are on.
 *\param instance   MOAB instance
 *\param box        The oriented box containing all the entities
 *\param axis       The axis for which the split plane is orthogonal
 *\param left_list  Output, entities to the left of the plane
 *\param right_list Output, entities to the right of the plane
 *\param num_intersecting Output, number entities intersecting plane
 */
static ErrorCode split_box( Interface* instance, 
                              const OrientedBox& box, 
                              int axis, 
                              const Range& entities, 
                              Range& left_list, 
                              Range& right_list )
{
  ErrorCode rval;
  left_list.clear();
  right_list.clear();

  std::vector<CartVect> coords;
  for (Range::reverse_iterator i = entities.rbegin(); i != entities.rend(); ++i) {
    const EntityHandle *conn = NULL;
    int conn_len = 0;
    rval = instance->get_connectivity( *i, conn, conn_len );
    if (MB_SUCCESS != rval)
      return rval;
    
    coords.resize( conn_len );
    rval = instance->get_coords( conn, conn_len, coords[0].array() );
    if (MB_SUCCESS != rval)
      return rval;
    
    CartVect centroid(0.0);
    for (int j = 0; j < conn_len; ++j)
      centroid += coords[j];
    centroid /= conn_len;
    
    if ((box.axis[axis] % (centroid - box.center)) < 0.0)
      left_list.insert( *i );
    else
      right_list.insert( *i );
  }
  
  return MB_SUCCESS;
}


ErrorCode OrientedBoxTreeTool::build_tree( const Range& entities,
                                               EntityHandle& set,
                                               int depth,
                                               const Settings& settings )
{
  OrientedBox tmp_box;
  ErrorCode rval;
  
  if (entities.empty()) {
    CartVect axis[3] = { CartVect(0.), CartVect(0.), CartVect(0.) };
    tmp_box = OrientedBox( axis, CartVect(0.) );
  }
  else {
    rval = OrientedBox::compute_from_2d_cells( tmp_box, instance, entities );
    if (MB_SUCCESS != rval)
      return rval;
  }
  
    // create an entity set for the tree node
  rval = instance->create_meshset( settings.set_options, set );
  if (MB_SUCCESS != rval)
    return rval;
  
  rval = instance->tag_set_data( tagHandle, &set, 1, &tmp_box );
  if (MB_SUCCESS != rval) 
    { delete_tree( set ); return rval; }
  
    // check if should create children
  bool leaf = true;
  ++depth;
  if ((!settings.max_depth || depth < settings.max_depth) && 
      entities.size() > (unsigned)settings.max_leaf_entities) {
      // try splitting with planes normal to each axis of the box
      // until we find an acceptable split
    double best_ratio = settings.worst_split_ratio; // worst case ratio
    Range best_left_list, best_right_list;
      // Axes are sorted from shortest to longest, so search backwards
    for (int axis = 2; best_ratio > settings.best_split_ratio && axis >= 0; --axis) {
      Range left_list, right_list;

      rval = split_box( instance, tmp_box, axis, entities, left_list, right_list );
      if (MB_SUCCESS != rval) 
        { delete_tree( set ); return rval; }
        
      double ratio = fabs((double)right_list.size() - left_list.size()) / entities.size();
      
      if (ratio < best_ratio) {
        best_ratio = ratio;
        best_left_list.swap( left_list );
        best_right_list.swap( right_list );
      }
    }
    
      // create children
    if (!best_left_list.empty())
    {
      EntityHandle child = 0;
      
      rval = build_tree( best_left_list, child, depth, settings );
      if (MB_SUCCESS != rval)
        { delete_tree( set ); return rval; }
      rval = instance->add_child_meshset( set, child );
      if (MB_SUCCESS != rval)
        { delete_tree( set ); delete_tree( child ); return rval; }
      
      rval = build_tree( best_right_list, child, depth, settings );
      if (MB_SUCCESS != rval)
        { delete_tree( set ); return rval; }
      rval = instance->add_child_meshset( set, child );
      if (MB_SUCCESS != rval)
        { delete_tree( set ); delete_tree( child ); return rval; }
      
      leaf = false;
    }
  }
  
  if (leaf)
  {
    rval = instance->add_entities( set, entities );
    if (MB_SUCCESS != rval) 
      { delete_tree( set ); return rval; }
  }
  
  createdTrees.push_back( set );
  return MB_SUCCESS;
}


static ErrorCode split_sets( Interface* , 
                               const OrientedBox& box, 
                               int axis, 
                               const std::list<OrientedBoxTreeTool::SetData>& sets,
                               std::list<OrientedBoxTreeTool::SetData>& left,
                               std::list<OrientedBoxTreeTool::SetData>& right )
{
  left.clear();
  right.clear();

  std::list<OrientedBoxTreeTool::SetData>::const_iterator i;
  for (i = sets.begin(); i != sets.end(); ++i) {
    CartVect centroid(i->box_data.center / i->box_data.area);
    if ((box.axis[axis] % (centroid - box.center)) < 0.0)
      left.push_back( *i );
    else
      right.push_back( *i );
  }
  
  return MB_SUCCESS;
}


ErrorCode OrientedBoxTreeTool::build_sets( std::list<SetData>& sets,
                                               EntityHandle& node_set,
                                               int depth,
                                               const Settings& settings )
{
  ErrorCode rval;
  int count = sets.size();
  if (0 == count)
    return MB_FAILURE;
  
    // calculate box
  OrientedBox obox;

  // make vector go out of scope when done, so memory is released
  { 
    Range elems;
    std::vector<OrientedBox::CovarienceData> data(sets.size());
    data.clear();
    for (std::list<SetData>::iterator i = sets.begin(); i != sets.end(); ++i) {
      data.push_back( i->box_data );
      rval = instance->get_entities_by_dimension( i->handle, 2, elems, true );
      if (MB_SUCCESS != rval)
        return rval;
    }
    
    Range points;
    rval = instance->get_adjacencies( elems, 0, false, points, Interface::UNION );
    if (MB_SUCCESS != rval)
      return rval;
    
    rval = OrientedBox::compute_from_covariance_data( obox, instance, &data[0], data.size(), points );
    if (MB_SUCCESS != rval)
      return rval;
  }
  
    // If only one set in list...
  if (count == 1) {
    node_set = sets.front().handle;
    return instance->tag_set_data( tagHandle, &node_set, 1, &obox );
  }
  
    // create an entity set for the tree node
  rval = instance->create_meshset( settings.set_options, node_set );
  if (MB_SUCCESS != rval)
    return rval;
  
  rval = instance->tag_set_data( tagHandle, &node_set, 1, &obox );
  if (MB_SUCCESS != rval) 
    { delete_tree( node_set ); return rval; }
  
  double best_ratio = 2.0; 
  std::list<SetData> best_left_list, best_right_list;
  for (int axis = 0; axis < 2; ++axis) {
    std::list<SetData> left_list, right_list;
    rval = split_sets( instance, obox, axis, sets, left_list, right_list );
    if (MB_SUCCESS != rval) 
      { delete_tree( node_set ); return rval; }

    double ratio = fabs((double)right_list.size() - left_list.size()) / sets.size();

    if (ratio < best_ratio) {
      best_ratio = ratio;
      best_left_list.swap( left_list );
      best_right_list.swap( right_list );
    }
  }
  
    // We must subdivide the list of sets, because we want to guarantee that
    // there is a node in the tree corresponding to each of the sets.  So if
    // we couldn't find a usable split plane, just split them in an arbitrary
    // manner.
  if (best_left_list.empty() || best_right_list.empty()) {
    best_left_list.clear();
    best_right_list.clear();
    std::list<SetData>* lists[2] = {&best_left_list,&best_right_list};
    int i = 0;
    while (!sets.empty()) {
      lists[i]->push_back( sets.front() );
      sets.pop_front();
      i = 1 - i;
    }
  }
  else {
    sets.clear(); // release memory before recursion
  }
  
    // Create child sets
    
  EntityHandle child = 0;
      
  rval = build_sets( best_left_list, child, depth+1, settings );
  if (MB_SUCCESS != rval)
    { delete_tree( node_set ); return rval; }
  rval = instance->add_child_meshset( node_set, child );
  if (MB_SUCCESS != rval)
    { delete_tree( node_set ); delete_tree( child ); return rval; }

  rval = build_sets( best_right_list, child, depth+1, settings );
  if (MB_SUCCESS != rval)
    { delete_tree( node_set ); return rval; }
  rval = instance->add_child_meshset( node_set, child );
  if (MB_SUCCESS != rval)
    { delete_tree( node_set ); delete_tree( child ); return rval; }
  
  return MB_SUCCESS;
}

ErrorCode OrientedBoxTreeTool::delete_tree( EntityHandle set )
{
  std::vector<EntityHandle> children;
  ErrorCode rval = instance->get_child_meshsets( set, children, 0 );
  if (MB_SUCCESS != rval)
    return rval;
  
  createdTrees.erase( 
    std::remove( createdTrees.begin(), createdTrees.end(), set ),
    createdTrees.end() );
  children.insert( children.begin(), set );
  return instance->delete_entities( &children[0], children.size() );
}


/********************** Generic Tree Traversal ****************************/
struct Data { EntityHandle set; int depth; };
ErrorCode OrientedBoxTreeTool::preorder_traverse( EntityHandle set,
                                                      Op& operation, 
                                                      TrvStats* accum )
{
  ErrorCode rval;
  std::vector<EntityHandle> children;
  std::vector<Data> the_stack;
  Data data = { set, 0 };
  the_stack.push_back( data );
  int max_depth = -1;
  
  while (!the_stack.empty())
  {
    data = the_stack.back();
    the_stack.pop_back();
    
    // increment traversal statistics
    if( accum ){
      accum->increment( data.depth );
      max_depth = std::max( max_depth, data.depth );
    }

    bool descend = true;
    rval = operation.visit( data.set, data.depth, descend );
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval)
      return rval;
    
    if (!descend)
      continue;
    
    children.clear();
    rval = instance->get_child_meshsets( data.set, children );
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval)
      return rval;
    if (children.empty()) {
      if( accum ){ accum->increment_leaf( data.depth ); }
      rval = operation.leaf( data.set );
      assert(MB_SUCCESS == rval);
      if (MB_SUCCESS != rval)
        return rval;
    }
    else if (children.size() == 2) {
      data.depth++;
      data.set = children[0];
      the_stack.push_back( data );
      data.set = children[1];
      the_stack.push_back( data );
    }
    else
      return MB_MULTIPLE_ENTITIES_FOUND;
  }
  
  if( accum ){
    accum->end_traversal( max_depth );
  }

  return MB_SUCCESS;
}

/********************** General Sphere/Triangle Intersection ***************/

struct OBBTreeSITFrame { 
  OBBTreeSITFrame( EntityHandle n, EntityHandle s, int dp )
    : node(n), surf(s), depth(dp) {}
  EntityHandle node;
  EntityHandle surf;
  int depth;
};

ErrorCode OrientedBoxTreeTool::sphere_intersect_triangles( 
                                        const double* center_v,
                                        double radius,
                                        EntityHandle tree_root,
                                        std::vector<EntityHandle>& facets_out,
                                        std::vector<EntityHandle>* sets_out, 
                                        TrvStats* accum )
{
  const double radsqr = radius * radius;
  OrientedBox b;
  ErrorCode rval;
  Range sets;
  const CartVect center(center_v);
  CartVect closest, coords[3];
  const EntityHandle* conn;
  int num_conn;
#ifndef MB_OBB_USE_VECTOR_QUERIES
  Range tris;
  Range::const_iterator t;
#else
  std::vector<EntityHandle> tris;
  std::vector<EntityHandle>::const_iterator t;
#endif
  
  std::vector<OBBTreeSITFrame> stack;
  std::vector<EntityHandle> children;
  stack.reserve(30);
  stack.push_back( OBBTreeSITFrame( tree_root, 0, 0 ) );

  int max_depth = -1;

  while (!stack.empty()) {
    EntityHandle surf = stack.back().surf; 
    EntityHandle node = stack.back().node;
    int current_depth   = stack.back().depth;
    stack.pop_back();
    
      // increment traversal statistics.  
    if( accum ){
      accum->increment( current_depth );
      max_depth = std::max( max_depth, current_depth );
    }

    if (!surf && sets_out) {
      rval = get_moab_instance()->get_entities_by_type( node, MBENTITYSET, sets );
      if (!sets.empty())
        surf = sets.front();
      sets.clear();
    }
    
      // check if sphere intersects box
    rval = box( node, b );
    if (MB_SUCCESS != rval)
      return rval;
    b.closest_location_in_box( center, closest );
    closest -= center;
    if ((closest % closest) > radsqr)
      continue;
    
      // push child boxes on stack
    children.clear();
    rval = instance->get_child_meshsets( node, children );
    if (MB_SUCCESS != rval)
      return rval;
    if (!children.empty()) {
      assert(children.size() == 2);
      stack.push_back( OBBTreeSITFrame( children[0], surf, current_depth + 1 ) );
      stack.push_back( OBBTreeSITFrame( children[1], surf, current_depth + 1 ) );
      continue;
    }
    
    if(accum){ accum->increment_leaf( current_depth ); }
      // if leaf, intersect sphere with triangles
#ifndef MB_OBB_USE_VECTOR_QUERIES
# ifdef MB_OBB_USE_TYPE_QUERIES
    rval = get_moab_instance()->get_entities_by_type( node, MBTRI, tris );
# else
    rval = get_moab_instance()->get_entities_by_handle( node, tris );
# endif
    t = tris.begin();
#else
    rval = get_moab_instance()->get_entities_by_handle( node, tris );
    t = tris.lower_bound( MBTRI );
#endif
    if (MB_SUCCESS != rval)
      return rval;
    
    for (t = tris.begin(); t != tris.end(); ++t) {
#ifndef MB_OBB_USE_VECTOR_QUERIES
      if (TYPE_FROM_HANDLE(*t) != MBTRI)
        break;
#elif !defined(MB_OBB_USE_TYPE_QUERIES)
      if (TYPE_FROM_HANDLE(*t) != MBTRI)
        continue;
#endif      
      rval = get_moab_instance()->get_connectivity( *t, conn, num_conn, true );
      if (MB_SUCCESS != rval)
        return rval;
      if (num_conn != 3)
        continue;
      
      rval = get_moab_instance()->get_coords( conn, num_conn, coords[0].array() );
      if (MB_SUCCESS != rval)
        return rval;
      
      GeomUtil::closest_location_on_tri( center, coords, closest );
      closest -= center;
      if ((closest % closest) <= radsqr &&
          std::find(facets_out.begin(),facets_out.end(),*t) == facets_out.end()) {
        facets_out.push_back( *t );
        if (sets_out)
          sets_out->push_back( surf );
      }
    }
  }

  if( accum ){
    accum->end_traversal( max_depth );
  }
  
  return MB_SUCCESS;
}
      


/********************** General Ray/Tree and Ray/Triangle Intersection ***************/


class RayIntersector : public OrientedBoxTreeTool::Op
{
  private:
    OrientedBoxTreeTool* tool;
    const CartVect b, m;
    const double* len;
    const double tol;
    Range& boxes;
    

  public:
    RayIntersector( OrientedBoxTreeTool* tool_ptr,
                    const double* ray_point,
                    const double* unit_ray_dir,
                    const double *ray_length,
                    double tolerance,
                    Range& leaf_boxes )
      : tool(tool_ptr),
        b(ray_point), m(unit_ray_dir),
        len(ray_length), tol(tolerance),
        boxes(leaf_boxes) 
      { }
  
    virtual ErrorCode visit( EntityHandle node,
                             int          depth,
                             bool&        descend );
    virtual ErrorCode leaf( EntityHandle node );
};

//#include <stdio.h>
//inline void dump_fragmentation( const Range& range ) {
//  static FILE* file = fopen( "fragmentation", "w" );
//  unsigned ranges = 0, entities = 0;
//  for (Range::const_pair_iterator i = range.const_pair_begin(); i != range.const_pair_end(); ++i)
//  {
//    ++ranges;
//    entities += i->second - i->first + 1;
//  }
//  fprintf( file, "%u %u\n", ranges, entities );
//}

ErrorCode OrientedBoxTreeTool::ray_intersect_triangles( 
                          std::vector<double>& intersection_distances_out,
                          std::vector<EntityHandle>& intersection_facets_out,
                          const Range& boxes,
                          double tolerance,
                          const double ray_point[3],
                          const double unit_ray_dir[3],
                          const double* ray_length, 
                          unsigned int* raytri_test_count )
{
  ErrorCode rval;
  intersection_distances_out.clear();
#ifdef MB_OBB_USE_VECTOR_QUERIES
  std::vector<EntityHandle> tris;
#endif
    
  const CartVect point( ray_point );
  const CartVect dir( unit_ray_dir );
  
  for (Range::iterator b = boxes.begin(); b != boxes.end(); ++b)
  {
#ifndef MB_OBB_USE_VECTOR_QUERIES
    Range tris;
# ifdef MB_OBB_USE_TYPE_QUERIES
    rval = instance->get_entities_by_type( *b, MBTRI, tris );
# else
    rval = instance->get_entities_by_handle( *b, tris );
# endif
#else
    tris.clear();
    rval = instance->get_entities_by_handle( *b, tris );
#endif
    if (MB_SUCCESS != rval)
      return rval;
//dump_fragmentation( tris );
    
#ifndef MB_OBB_USE_VECTOR_QUERIES
    for (Range::iterator t = tris.begin(); t != tris.end(); ++t)
#else
    for (std::vector<EntityHandle>::iterator t = tris.begin(); t != tris.end(); ++t)
#endif
    {
#ifndef MB_OBB_USE_TYPE_QUERIES
      if (TYPE_FROM_HANDLE(*t) != MBTRI)
        continue;
#endif
    
      const EntityHandle* conn = NULL;
      int len = 0;
      rval = instance->get_connectivity( *t, conn, len, true );
      if (MB_SUCCESS != rval)
        return rval;
      
      CartVect coords[3];
      rval = instance->get_coords( conn, 3, coords[0].array() );
      if (MB_SUCCESS != rval)
        return rval;
      
      if( raytri_test_count ) *raytri_test_count += 1; 

      double td;
      if (GeomUtil::ray_tri_intersect( coords, point, dir, tolerance, td, ray_length )){
        intersection_distances_out.push_back(td);
        intersection_facets_out.push_back( *t );
      }
    }
  }
  
  return MB_SUCCESS;
}                    

ErrorCode OrientedBoxTreeTool::ray_intersect_triangles( 
                          std::vector<double>& intersection_distances_out,
                          std::vector<EntityHandle>& intersection_facets_out,
                          EntityHandle root_set,
                          double tolerance,
                          const double ray_point[3],
                          const double unit_ray_dir[3],
                          const double* ray_length, 
                          TrvStats* accum )
{
  Range boxes;
  ErrorCode rval;
  
  rval = ray_intersect_boxes( boxes, root_set, tolerance, ray_point, unit_ray_dir, ray_length, accum );
  if (MB_SUCCESS != rval)
    return rval;
    
  return ray_intersect_triangles( intersection_distances_out, intersection_facets_out, boxes, 
                                  tolerance, ray_point, unit_ray_dir, ray_length, 
                                  accum ? &(accum->ray_tri_tests_count) : NULL );
}

ErrorCode OrientedBoxTreeTool::ray_intersect_boxes( 
                          Range& boxes_out,
                          EntityHandle root_set,
                          double tolerance,
                          const double ray_point[3],
                          const double unit_ray_dir[3],
                          const double* ray_length, 
                          TrvStats* accum )
{
  RayIntersector op( this, ray_point, unit_ray_dir, ray_length, tolerance, boxes_out );
  return preorder_traverse( root_set, op, accum );
}

ErrorCode RayIntersector::visit( EntityHandle node,
                                 int ,
                                 bool&        descend ) 
{
  OrientedBox box;
  ErrorCode rval = tool->box( node, box );
  if (MB_SUCCESS != rval)
    return rval;
  
  descend = box.intersect_ray( b, m, tol, len);
  return MB_SUCCESS;
}


ErrorCode RayIntersector::leaf( EntityHandle node )
{
  boxes.insert(node);
  return MB_SUCCESS;
}


/********************** Ray/Set Intersection ****************************/


class RayIntersectSets : public OrientedBoxTreeTool::Op
{
  private:
    // Input
    OrientedBoxTreeTool* tool;
    const CartVect       ray_origin;
    const CartVect       ray_direction;
    const double*        nonneg_ray_len;  /* length to search ahead of ray origin */
    const double*        neg_ray_len;     /* length to search behind ray origin */
    const double         tol;             /* used for box.intersect_ray, radius of
                                             neighborhood for adjacent triangles,
                                             and old mode of add_intersection */
    const int            minTolInt;       /* used for old mode of add_intersection */

    // Output
    std::vector<double>&       intersections;
    std::vector<EntityHandle>& sets;
    std::vector<EntityHandle>& facets;

    // Optional Input - to screen RTIs by orientation and edge/node intersection
    const EntityHandle*  rootSet;         /* used for sphere_intersect */
    const EntityHandle*  geomVol;         /* used for determining surface sense */
    const Tag*           senseTag;        /* allows screening by triangle orientation.
                                             both geomVol and senseTag must be used together. */
    const int*           desiredOrient;   /* points to desired orientation of ray with
                                             respect to surf normal, if this feature is used.
                                             Must point to -1 (reverse) or 1 (forward).
                                             geomVol and senseTag are needed for this feature */
    int*                 surfTriOrient;   /* holds desired orientation of tri wrt surface */
    int                  surfTriOrient_val;

    // Optional Input - to avoid returning these as RTIs
    const std::vector<EntityHandle>* prevFacets; /* intersections on these triangles 
                                                    will not be returned */

    // Other Variables
    unsigned int*        raytri_test_count;
    EntityHandle         lastSet;
    int                  lastSetDepth;
    std::vector< std::vector<EntityHandle> > neighborhoods;
    std::vector<EntityHandle> neighborhood;

    void add_intersection( double t, EntityHandle facet );
    
  public:
    RayIntersectSets( OrientedBoxTreeTool*       tool_ptr,
                      const double*              ray_point,
                      const double*              unit_ray_dir,
                      const double*              nonneg_ray_length,
		      const double*              neg_ray_length,
                      double                     tolerance,
                      int                        min_tol_intersections,
                      std::vector<double>&       inters,
                      std::vector<EntityHandle>& surfaces,
                      std::vector<EntityHandle>& facts,
	              EntityHandle*              root_set,
		      const EntityHandle*        geom_volume,
		      const Tag*                 sense_tag,
		      const int*                 desired_orient,
	              const std::vector<EntityHandle>* prev_facets,
                      unsigned int*              tmp_count   )
      : tool(tool_ptr),
        ray_origin(ray_point), ray_direction(unit_ray_dir),
        nonneg_ray_len(nonneg_ray_length), neg_ray_len(neg_ray_length), 
        tol(tolerance), minTolInt(min_tol_intersections),
        intersections(inters), sets(surfaces), facets(facts), 
        rootSet(root_set), geomVol(geom_volume), senseTag(sense_tag),
        desiredOrient(desired_orient), surfTriOrient_val(0), prevFacets(prev_facets),
        raytri_test_count(tmp_count), lastSet(0), lastSetDepth(0)
      {
	// specified orientation should be 1 or -1, indicating ray and surface
        // normal in the same or opposite directions, respectively.
        if(desiredOrient) {
          assert(1==*desiredOrient || -1==*desiredOrient);
          surfTriOrient = &surfTriOrient_val;
        } else {
          surfTriOrient = NULL;
        }

        // check the limits  
        if(nonneg_ray_len) {
          assert(0 <= *nonneg_ray_len);
        } 
        if(neg_ray_len) {
          assert(0 > *neg_ray_len);
        }

      }
   

    virtual ErrorCode visit( EntityHandle node,
                             int          depth,
			     bool&        descend );
    virtual ErrorCode leaf( EntityHandle node );
};

ErrorCode RayIntersectSets::visit( EntityHandle node,
                                   int          depth,
				   bool&        descend ) 
{
  OrientedBox box;
  ErrorCode rval = tool->box( node, box );
  assert(MB_SUCCESS == rval);
  if (MB_SUCCESS != rval)
    return rval;
  
  descend = box.intersect_ray( ray_origin, ray_direction, tol, nonneg_ray_len, 
                               neg_ray_len );

  if (lastSet && depth <= lastSetDepth)
    lastSet = 0;

  if (descend && !lastSet) {
    Range tmp_sets;
    rval = tool->get_moab_instance()->get_entities_by_type( node, MBENTITYSET, tmp_sets );
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval)
      return rval;
 
    if (!tmp_sets.empty()) {
      if (tmp_sets.size() > 1)
        return MB_FAILURE;
      lastSet = *tmp_sets.begin();
      lastSetDepth = depth;
      // Get desired orientation of surface wrt volume. Use this to return only 
      // exit or entrance intersections.
      if(geomVol && senseTag && desiredOrient && surfTriOrient) {
        if(1!=*desiredOrient && -1!=*desiredOrient) {
	  std::cerr << "error: desired orientation must be 1 (forward) or -1 (reverse)" 
                    << std::endl;
        }
	EntityHandle vols[2];
	rval = tool->get_moab_instance()->tag_get_data( *senseTag, &lastSet, 1, vols );
	assert(MB_SUCCESS == rval);
	if(MB_SUCCESS != rval) return rval;
	if(vols[0] == vols[1]) {
	  std::cerr << "error: surface has positive and negative sense wrt same volume" 
                    << std::endl;
	  return MB_FAILURE;
	}
        // surfTriOrient will be used by plucker_ray_tri_intersect to avoid
        // intersections with wrong orientation.
	if       (*geomVol == vols[0]) {
	  *surfTriOrient = *desiredOrient*1;
	} else if(*geomVol == vols[1]) {
	  *surfTriOrient = *desiredOrient*(-1);
	} else {
	  assert(false);
	  return MB_FAILURE;
	}
      }
    }
  }
    
  return MB_SUCCESS;
}

ErrorCode RayIntersectSets::leaf( EntityHandle node )
{
  assert(lastSet);
  if (!lastSet) // if no surface has been visited yet, something's messed up.
    return MB_FAILURE;
  
#ifndef MB_OBB_USE_VECTOR_QUERIES
  Range tris;
# ifdef MB_OBB_USE_TYPE_QUERIES
  ErrorCode rval = tool->get_moab_instance()->get_entities_by_type( node, MBTRI, tris );
# else
  ErrorCode rval = tool->get_moab_instance()->get_entities_by_handle( node, tris );
# endif
#else
  std::vector<EntityHandle> tris;
  ErrorCode rval = tool->get_moab_instance()->get_entities_by_handle( node, tris );
#endif
  assert(MB_SUCCESS == rval);
  if (MB_SUCCESS != rval)
    return rval;

#ifndef MB_OBB_USE_VECTOR_QUERIES
  for (Range::iterator t = tris.begin(); t != tris.end(); ++t)
#else
  for (std::vector<EntityHandle>::iterator t = tris.begin(); t != tris.end(); ++t)
#endif
  {
#ifndef MB_OBB_USE_TYPE_QUERIES
    if (TYPE_FROM_HANDLE(*t) != MBTRI)
      continue;
#endif
    
    const EntityHandle* conn;
    int num_conn;
    rval = tool->get_moab_instance()->get_connectivity( *t, conn, num_conn, true );
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval)
      return rval;

    CartVect coords[3];
    rval = tool->get_moab_instance()->get_coords( conn, 3, coords[0].array() );
    assert(MB_SUCCESS == rval);
    if (MB_SUCCESS != rval)
      return rval;

    if( raytri_test_count ) *raytri_test_count += 1; 

    double int_dist;
    GeomUtil::intersection_type int_type = GeomUtil::NONE;
    // Note: tol is not used in the ray-tri test.
    if (GeomUtil::plucker_ray_tri_intersect( coords, ray_origin, ray_direction, tol, int_dist, 
                                     nonneg_ray_len, neg_ray_len, surfTriOrient, &int_type )) {
      // Do not accept intersections if they are in the vector of previously intersected
      // facets.
      if( prevFacets &&
	  ((*prevFacets).end() != find((*prevFacets).begin(), (*prevFacets).end(), *t) ) ) continue;

      // Do not accept intersections if they are in the neighborhood of previous
      // intersections.
      bool same_neighborhood = false;
      for(unsigned i=0; i<neighborhoods.size(); ++i) {
        if( neighborhoods[i].end() != find(neighborhoods[i].begin(), 
		 			   neighborhoods[i].end(), *t ) ) {
          same_neighborhood = true;
          continue;
        }
      }
      if(same_neighborhood) continue;

      // Handle special case of edge/node intersection. Accept piercing 
      // intersections and reject glancing intersections.
      // The edge_node_intersection function needs to know surface sense wrt volume.
      // A less-robust implementation could work without sense information.
      // Would it ever be useful to accept a glancing intersection?
      if(GeomUtil::INTERIOR != int_type && rootSet && geomVol && senseTag) {
        // get triangles in the proximity of the intersection
        CartVect int_pt = ray_origin + int_dist*ray_direction;
	std::vector<EntityHandle> close_tris;
	std::vector<EntityHandle> close_surfs;
        rval = tool->sphere_intersect_triangles( int_pt.array(), tol, *rootSet,
	                                         close_tris, &close_surfs );
        assert(MB_SUCCESS == rval);
        if(MB_SUCCESS != rval) return rval; 

        // for each surface, get the surf sense wrt parent volume
	std::vector<int> close_senses(close_surfs.size());
        for(unsigned i=0; i<close_surfs.size(); ++i) {
          EntityHandle vols[2];
          rval = tool->get_moab_instance()->tag_get_data( *senseTag, &lastSet, 1, vols );
          assert(MB_SUCCESS == rval);
          if(MB_SUCCESS != rval) return rval;
          if(vols[0] == vols[1]) {
            std::cerr << "error: surf has positive and negative sense wrt same volume" << std::endl;
            return MB_FAILURE;
          }
          if       (*geomVol == vols[0]) {
            close_senses[i] = 1;
          } else if(*geomVol == vols[1]) {
            close_senses[i] = -1;
          } else {
            return MB_FAILURE;
          }
        }

        neighborhood.clear();
	bool piercing = edge_node_intersect( *t, ray_direction, int_type, close_tris,
                                             close_senses, tool->get_moab_instance(), &neighborhood );
        if(!piercing) continue;

      } else {
        neighborhood.clear();
	neighborhood.push_back( *t );
      }      
     
        // NOTE: add_intersection may modify the 'neg_ray_len' and 'nonneg_ray_len'
        //       members, which will affect subsequent calls to ray_tri_intersect 
        //       in this loop.
      add_intersection( int_dist, *t );
    }
  }
  return MB_SUCCESS;
}

/* Mode 1: Used if neg_ray_len and nonneg_ray_len are specified
     variables used:     nonneg_ray_len, neg_ray_len
     variables not used: min_tol_int, tol
     1) keep the closest nonneg intersection and one negative intersection, if closer

   Mode 2: Used if neg_ray_len is not specified
     variables used:     min_tol_int, tol, nonneg_ray_len
     variables not used: neg_ray_len
     1) if(min_tol_int<0) return all intersections
     2) otherwise return all inside tolerance and unless there are >min_tol_int
     inside of tolerance, return the closest outside of tolerance */
void RayIntersectSets::add_intersection( double t, EntityHandle facet ) {

  // Mode 1, detected by non-null neg_ray_len pointer
  // keep the closest nonneg intersection and one negative intersection, if closer
  if(neg_ray_len && nonneg_ray_len) {
    if(2 != intersections.size()) {
      intersections.resize(2,0);
      sets.resize(2,0);
      facets.resize(2,0);
      // must initialize this for comparison below
      intersections[0] = -std::numeric_limits<double>::max();
    }

    // negative case
    if(0.0>t) {
      intersections[0] = t;
      sets[0]          = lastSet;
      facets[0]        = facet;
      neg_ray_len      = &intersections[0];
    // nonnegative case
    } else {
      intersections[1] = t;
      sets[1]          = lastSet;
      facets[1]        = facet;
      nonneg_ray_len   = &intersections[1];
      // if the intersection is closer than the negative one, remove the negative one
      if(t < -(*neg_ray_len) ) {
        intersections[0] = -intersections[1];
        sets[0]          = 0;
        facets[0]        = 0;
        neg_ray_len      = &intersections[0]; 
      }
    }
    //    std::cout << "add_intersection: t = " << t << " neg_ray_len=" << *neg_ray_len
    //          << " nonneg_ray_len=" << *nonneg_ray_len << std::endl;
    return;
  }

  // ---------------------------------------------------------------------------
  // Mode 2
  // If minTolInt is less than zero, return all intersections
  if (minTolInt < 0 && t > -tol) {
    intersections.push_back(t);
    sets.push_back(lastSet);
    facets.push_back(facet);
    neighborhoods.push_back(neighborhood);
    return;
  }

    // Check if the 'len' pointer is pointing into the intersection
    // list.  If this is the case, then the list contains, at that
    // location, an intersection greater than the tolerance away from
    // the base point of the ray.
  int len_idx = -1;
  if (nonneg_ray_len && nonneg_ray_len >= &intersections[0] && 
      nonneg_ray_len < &intersections[0] + intersections.size())
    len_idx = nonneg_ray_len - &intersections[0];

    // If the intersection is within tol of the ray base point, we 
    // always add it to the list.
  if (t <= tol) {
      // If the list contains an intersection outside the tolerance...
    if (len_idx >= 0) {
        // If we no longer want an intersection outside the tolerance,
        // remove it.
      if ((int)intersections.size() >= minTolInt) {
        intersections[len_idx] = t;
        sets[len_idx] = lastSet;
        facets[len_idx] = facet;
          // From now on, we want only intersections within the tolerance,
          // so update length accordingly
        nonneg_ray_len = &tol;
      }
        // Otherwise appended to the list and update pointer
      else {
        intersections.push_back(t);
        sets.push_back(lastSet);
        facets.push_back(facet);
        nonneg_ray_len = &intersections[len_idx];
      }
    }
      // Otherwise just append it
    else {
      intersections.push_back(t);
      sets.push_back(lastSet);
      facets.push_back(facet);
        // If we have all the intersections we want, set
        // length such that we will only find further intersections
        // within the tolerance
      if ((int)intersections.size() >= minTolInt)
        nonneg_ray_len = &tol;
    }
  }
    // Otherwise the intersection is outside the tolerance
    // If we already have an intersection outside the tolerance and
    // this one is closer, replace the existing one with this one.
  else if (len_idx >= 0) {
    if (t <= *nonneg_ray_len) {
      intersections[len_idx] = t;
      sets[len_idx] = lastSet;
      facets[len_idx] = facet;
    }
  }
    // Otherwise if we want an intersection outside the tolerance
    // and don'thave one yet, add it.
  else if ((int)intersections.size() < minTolInt) {
    intersections.push_back( t );
    sets.push_back( lastSet );
    facets.push_back(facet);
      // update length.  this is currently the closest intersection, so
      // only want further intersections that are closer than this one.
    nonneg_ray_len = &intersections.back();
  }
}
  

ErrorCode OrientedBoxTreeTool::ray_intersect_sets( 
                                    std::vector<double>&             distances_out,
                                    std::vector<EntityHandle>&       sets_out,
                                    std::vector<EntityHandle>&       facets_out,
                                    EntityHandle                     root_set,
                                    double                           tolerance,
                                    int                              min_tolerace_intersections,
                                    const double                     ray_point[3],
                                    const double                     unit_ray_dir[3],
                                    const double*                    nonneg_ray_length,
                                    TrvStats*                        accum,
                                    const double*                    neg_ray_length,
                                    const EntityHandle*              geom_vol,
                                    const Tag*                       sense_tag,
                                    const int*                       desired_orient,
                                    const std::vector<EntityHandle>* prev_facets )
{
  RayIntersectSets op( this, ray_point, unit_ray_dir, nonneg_ray_length, neg_ray_length, tolerance, 
                       min_tolerace_intersections, distances_out, sets_out, facets_out,
                       &root_set, geom_vol, sense_tag, desired_orient, prev_facets, 
                       accum ? &(accum->ray_tri_tests_count) : NULL ); 
  return preorder_traverse( root_set, op, accum );
}



/********************** Closest Point code ***************/

struct OBBTreeCPFrame {
  OBBTreeCPFrame( double d, EntityHandle n, EntityHandle s, int dp )
    : dist_sqr(d), node(n), mset(s), depth(dp) {}
  double dist_sqr;
  EntityHandle node;
  EntityHandle mset;
  int depth;
};

ErrorCode OrientedBoxTreeTool::closest_to_location( 
                                     const double* point,
                                     EntityHandle root,
                                     double* point_out,
                                     EntityHandle& facet_out,
                                     EntityHandle* set_out,
                                     TrvStats* accum ) 
{
  ErrorCode rval;
  const CartVect loc( point );
  double smallest_dist_sqr = std::numeric_limits<double>::max();
  
  EntityHandle current_set = 0;
  Range sets;
  std::vector<EntityHandle> children(2);
  std::vector<double> coords;
  std::vector<OBBTreeCPFrame> stack;
  int max_depth = -1;

  stack.push_back( OBBTreeCPFrame(0.0, root, current_set, 0) );
  
  while( !stack.empty() ) {

      // pop from top of stack
    EntityHandle node = stack.back().node;
    double dist_sqr = stack.back().dist_sqr;
    current_set = stack.back().mset;
    int current_depth = stack.back().depth;
    stack.pop_back();

      // If current best result is closer than the box, skip this tree node.
    if (dist_sqr > smallest_dist_sqr)
      continue;

      // increment traversal statistics.  
    if( accum ){
      accum->increment( current_depth );
      max_depth = std::max( max_depth, current_depth );
    }

      // Check if this node has a set associated with it
    if (set_out && !current_set) {
      sets.clear();
      rval = instance->get_entities_by_type( node, MBENTITYSET, sets );
      if (MB_SUCCESS != rval)
        return rval;
      if (!sets.empty()) {
        if (sets.size() != 1)
          return MB_MULTIPLE_ENTITIES_FOUND;
        current_set = sets.front();
      }
    }

      // Get child boxes
    children.clear();
    rval = instance->get_child_meshsets( node, children );
    if (MB_SUCCESS != rval)
      return rval;

      // if not a leaf node
    if (!children.empty()) {
      if (children.size() != 2)
        return MB_MULTIPLE_ENTITIES_FOUND;
    
        // get boxes
      OrientedBox box1, box2;
      rval = box( children[0], box1 );
      if (MB_SUCCESS != rval) return rval;
      rval = box( children[1], box2 );
      if (MB_SUCCESS != rval) return rval;
      
        // get distance from each box
      CartVect pt1, pt2;
      box1.closest_location_in_box( loc, pt1 );
      box2.closest_location_in_box( loc, pt2 );
      pt1 -= loc;
      pt2 -= loc;
      const double dsqr1 = pt1 % pt1;
      const double dsqr2 = pt2 % pt2;
      
        // push children on tree such that closer one is on top
      if (dsqr1 < dsqr2) {
        stack.push_back( OBBTreeCPFrame(dsqr2, children[1], current_set, current_depth+1 ) );
        stack.push_back( OBBTreeCPFrame(dsqr1, children[0], current_set, current_depth+1 ) );
      }
      else {
        stack.push_back( OBBTreeCPFrame(dsqr1, children[0], current_set, current_depth+1 ) );
        stack.push_back( OBBTreeCPFrame(dsqr2, children[1], current_set, current_depth+1 ) );
      }
    }
    else { // LEAF NODE
      if( accum ){ accum->increment_leaf( current_depth ); }

      Range facets;
      rval = instance->get_entities_by_dimension( node, 2, facets );
      if (MB_SUCCESS != rval)
        return rval;
      
      const EntityHandle* conn = NULL;
      int len = 0;
      CartVect tmp, diff;
      for (Range::iterator i = facets.begin(); i != facets.end(); ++i) {
        rval = instance->get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval)
          return rval;
        
        coords.resize(3*len);
        rval = instance->get_coords( conn, len, &coords[0] );
        if (MB_SUCCESS != rval)
          return rval;
        
        if (len == 3) 
          GeomUtil::closest_location_on_tri( loc, (CartVect*)(&coords[0]), tmp );
        else
          GeomUtil::closest_location_on_polygon( loc, (CartVect*)(&coords[0]), len, tmp );
        
        diff = tmp - loc;
        dist_sqr = diff % diff;
        if (dist_sqr < smallest_dist_sqr) {
          smallest_dist_sqr = dist_sqr;
          facet_out = *i;
          tmp.get( point_out );
          if (set_out)
            *set_out = current_set;
        }
      }
    } // LEAF NODE
  }

  if( accum ){
    accum->end_traversal( max_depth );
  }
  
  return MB_SUCCESS;
}
                                     
ErrorCode OrientedBoxTreeTool::closest_to_location( const double* point,
                                     EntityHandle root,
                                     double tolerance,
                                     std::vector<EntityHandle>& facets_out,
                                     std::vector<EntityHandle>* sets_out, 
                                     TrvStats* accum )
{
  ErrorCode rval;
  const CartVect loc( point );
  double smallest_dist_sqr = std::numeric_limits<double>::max();
  double smallest_dist = smallest_dist_sqr;
  
  EntityHandle current_set = 0;
  Range sets;
  std::vector<EntityHandle> children(2);
  std::vector<double> coords;
  std::vector<OBBTreeCPFrame> stack;
  int max_depth = -1;

  stack.push_back( OBBTreeCPFrame(0.0, root, current_set, 0) );
  
  while( !stack.empty() ) {

      // pop from top of stack
    EntityHandle node = stack.back().node;
    double dist_sqr = stack.back().dist_sqr;
    current_set = stack.back().mset;
    int current_depth = stack.back().depth;
    stack.pop_back();

      // If current best result is closer than the box, skip this tree node.
    if (dist_sqr > smallest_dist_sqr + tolerance)
      continue;

      // increment traversal statistics.  
    if( accum ){
      accum->increment( current_depth );
      max_depth = std::max( max_depth, current_depth );
    }

      // Check if this node has a set associated with it
    if (sets_out && !current_set) {
      sets.clear();
      rval = instance->get_entities_by_type( node, MBENTITYSET, sets );
      if (MB_SUCCESS != rval)
        return rval;
      if (!sets.empty()) {
        if (sets.size() != 1)
          return MB_MULTIPLE_ENTITIES_FOUND;
        current_set = *sets.begin();
      }
    }

      // Get child boxes
    children.clear();
    rval = instance->get_child_meshsets( node, children );
    if (MB_SUCCESS != rval)
      return rval;

      // if not a leaf node
    if (!children.empty()) {
      if (children.size() != 2)
        return MB_MULTIPLE_ENTITIES_FOUND;
    
        // get boxes
      OrientedBox box1, box2;
      rval = box( children[0], box1 );
      if (MB_SUCCESS != rval) return rval;
      rval = box( children[1], box2 );
      if (MB_SUCCESS != rval) return rval;
      
        // get distance from each box
      CartVect pt1, pt2;
      box1.closest_location_in_box( loc, pt1 );
      box2.closest_location_in_box( loc, pt2 );
      pt1 -= loc;
      pt2 -= loc;
      const double dsqr1 = pt1 % pt1;
      const double dsqr2 = pt2 % pt2;
      
        // push children on tree such that closer one is on top
      if (dsqr1 < dsqr2) {
        stack.push_back( OBBTreeCPFrame(dsqr2, children[1], current_set, current_depth+1 ) );
        stack.push_back( OBBTreeCPFrame(dsqr1, children[0], current_set, current_depth+1 ) );
      }
      else {
        stack.push_back( OBBTreeCPFrame(dsqr1, children[0], current_set, current_depth+1 ) );
        stack.push_back( OBBTreeCPFrame(dsqr2, children[1], current_set, current_depth+1 ) );
      }
    }
    else { // LEAF NODE
      if( accum ){ accum->increment_leaf( current_depth ); }

      Range facets;
      rval = instance->get_entities_by_dimension( node, 2, facets );
      if (MB_SUCCESS != rval)
        return rval;
      
      const EntityHandle* conn = NULL;
      int len = 0;
      CartVect tmp, diff;
      for (Range::iterator i = facets.begin(); i != facets.end(); ++i) {
        rval = instance->get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval)
          return rval;
        
        coords.resize(3*len);
        rval = instance->get_coords( conn, len, &coords[0] );
        if (MB_SUCCESS != rval)
          return rval;
        
        if (len == 3) 
          GeomUtil::closest_location_on_tri( loc, (CartVect*)(&coords[0]), tmp );
        else
          GeomUtil::closest_location_on_polygon( loc, (CartVect*)(&coords[0]), len, tmp );
        
        diff = tmp - loc;
        dist_sqr = diff % diff;
        if (dist_sqr < smallest_dist_sqr) {
          if (0.5*dist_sqr < 0.5*smallest_dist_sqr + tolerance*(0.5*tolerance - smallest_dist)) {
            facets_out.clear();
            if (sets_out)
              sets_out->clear();
          }
          smallest_dist_sqr = dist_sqr;
          smallest_dist = sqrt(smallest_dist_sqr);
            // put closest result at start of lists
          facets_out.push_back( *i );
          std::swap( facets_out.front(), facets_out.back() );
          if (sets_out) {
            sets_out->push_back( current_set );
            std::swap( sets_out->front(), sets_out->back() );
          }
        }
        else if (dist_sqr <= smallest_dist_sqr + tolerance*(tolerance + 2*smallest_dist)) {
          facets_out.push_back(*i);
          if (sets_out)
            sets_out->push_back( current_set );
        }
      }
    } // LEAF NODE
  }

  if( accum ){
    accum->end_traversal( max_depth );
  }
  
  return MB_SUCCESS;
}
    
    

/********************** Tree Printing Code ****************************/


class TreeLayoutPrinter : public OrientedBoxTreeTool::Op
{
  public:
    TreeLayoutPrinter( std::ostream& stream,
                       Interface* instance );
    
    virtual ErrorCode visit( EntityHandle node, 
                             int          depth,
			     bool&        descend );
    virtual ErrorCode leaf( EntityHandle node );
  private:

    Interface* instance;
    std::ostream& outputStream;
    std::vector<bool> path;
};

TreeLayoutPrinter::TreeLayoutPrinter( std::ostream& stream,
                                      Interface* interface )
  : instance(interface),
    outputStream(stream)
  {}

ErrorCode TreeLayoutPrinter::visit( EntityHandle node, 
                                    int          depth,
				    bool&        descend )
{
  descend = true;
  
  if ((unsigned)depth > path.size()) {
    //assert(depth+1 == path.size); // preorder traversal
    path.push_back(true);
  }
  else {
    path.resize( depth );
    if (depth)
      path.back() = false;
  }
  
  for (unsigned i = 0; i+1 < path.size(); ++i) {
    if (path[i])
      outputStream << "|   ";
    else
      outputStream << "    ";
  }
  if (depth) {
    if (path.back())
      outputStream << "+---";
    else
      outputStream << "\\---";
  }
  outputStream << instance->id_from_handle( node ) << std::endl;
  return MB_SUCCESS;
}

ErrorCode TreeLayoutPrinter::leaf( EntityHandle ) { return MB_SUCCESS; }
    

class TreeNodePrinter : public OrientedBoxTreeTool::Op
{
  public:
    TreeNodePrinter( std::ostream& stream,
                     bool list_contents,
                     bool list_box,
                     const char* id_tag_name,
                     OrientedBoxTreeTool* tool_ptr );
    
    virtual ErrorCode visit( EntityHandle node, 
                             int          depth,
			     bool&        descend );
                               
    virtual ErrorCode leaf( EntityHandle ) { return MB_SUCCESS; }
  private:
  
    ErrorCode print_geometry( EntityHandle node );
    ErrorCode print_contents( EntityHandle node );
    ErrorCode print_counts( EntityHandle node );
  
    bool printContents;
    bool printGeometry;
    bool haveTag;
    Tag tag, gidTag, geomTag;
    Interface* instance;
    OrientedBoxTreeTool* tool;
    std::ostream& outputStream;
};


TreeNodePrinter::TreeNodePrinter( std::ostream& stream,
                                  bool list_contents,
                                  bool list_box,
                                  const char* id_tag_name,
                                  OrientedBoxTreeTool* tool_ptr )
  : printContents( list_contents ),
    printGeometry( list_box ),
    haveTag( false ),
    tag( 0 ),
    gidTag(0), geomTag(0),
    instance( tool_ptr->get_moab_instance() ),
    tool(tool_ptr),
    outputStream( stream )
{
  ErrorCode rval;
  if (id_tag_name) {
    rval = instance->tag_get_handle( id_tag_name, 1, MB_TYPE_INTEGER, tag );
    if (!rval) {
      std::cerr << "Could not get tag \"" << id_tag_name << "\"\n";
      stream << "Could not get tag \"" << id_tag_name << "\"\n";
    }
    else {
      haveTag = true;
    }
  }
  
  rval = instance->tag_get_handle( GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag );
  if (MB_SUCCESS != rval)
    gidTag = 0;
  rval = instance->tag_get_handle( GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag );
  if (MB_SUCCESS != rval)
    geomTag = 0;
}   

ErrorCode TreeNodePrinter::visit( EntityHandle node, int, bool& descend )
{
  descend = true;
  EntityHandle setid = instance->id_from_handle( node );
  outputStream << setid << ":" << std::endl;
  
  Range surfs;
  ErrorCode r3 = MB_SUCCESS;
  if (geomTag) {
    const int two = 2;
    const void* tagdata[] = {&two};
    r3 = instance->get_entities_by_type_and_tag( node, MBENTITYSET, &geomTag, tagdata, 1, surfs );
  
    if (MB_SUCCESS == r3 && surfs.size() == 1) {
      EntityHandle surf = *surfs.begin();
      int id;
      if (gidTag && MB_SUCCESS == instance->tag_get_data( gidTag, &surf, 1, &id ))
        outputStream << "  Surface " << id << std::endl;
      else
        outputStream << "  Surface w/ unknown ID (" << surf << ")" << std::endl;
    }
  }
  
  ErrorCode r1 = printGeometry ? print_geometry( node ) : MB_SUCCESS;
  ErrorCode r2 = printContents ? print_contents( node ) : print_counts( node );
  outputStream << std::endl;
  
  if (MB_SUCCESS != r1)
    return r1;
  else if (MB_SUCCESS != r2)
    return r2;
  else
    return r3;
}

ErrorCode TreeNodePrinter::print_geometry( EntityHandle node )
{
  OrientedBox box;
  ErrorCode rval= tool->box( node, box );
  if (MB_SUCCESS != rval)
    return rval;
  
  CartVect length = box.dimensions();
  
  outputStream << box.center << "  Radius: " 
               << box.inner_radius() << " - " << box.outer_radius() << std::endl
               << '+' << box.axis[0] << " : " << length[0] << std::endl
               << 'x' << box.axis[1] << " : " << length[1] << std::endl
               << 'x' << box.axis[2] << " : " << length[2] << std::endl;
  return MB_SUCCESS;
}

ErrorCode TreeNodePrinter::print_counts( EntityHandle node )
{
  for (EntityType type = MBVERTEX; type != MBMAXTYPE; ++type) {
    int count = 0;
    ErrorCode rval = instance->get_number_entities_by_type( node, type, count );
    if (MB_SUCCESS != rval)
      return rval;
    if(count > 0)
      outputStream << " " << count << " " << CN::EntityTypeName(type) << std::endl;
  }
  return MB_SUCCESS;
}

ErrorCode TreeNodePrinter::print_contents( EntityHandle node )
{
    // list contents
  for (EntityType type = MBVERTEX; type != MBMAXTYPE; ++type) {
    Range range;
    ErrorCode rval = instance->get_entities_by_type( node, type, range );
    if (MB_SUCCESS != rval)
      return rval;
    if (range.empty())
      continue;
    outputStream << " " << CN::EntityTypeName(type) << " ";  
    std::vector<int> ids( range.size() );
    if (haveTag) {
      rval = instance->tag_get_data( tag, range, &ids[0] );
      std::sort( ids.begin(), ids.end() );
    }
    else {
      Range::iterator ri = range.begin();
      std::vector<int>::iterator vi = ids.begin();
      while (ri != range.end()) {
        *vi = instance->id_from_handle( *ri );
        ++ri;
        ++vi;
      }
    }
    
    unsigned i = 0;
    for(;;) {
      unsigned beg = i, end;
      do { end = i++; } while (i < ids.size() && ids[end]+1 == ids[i]);
      if (end == beg)
        outputStream << ids[end];
      else if (end == beg+1) 
        outputStream << ids[beg] << ", " << ids[end];
      else
        outputStream << ids[beg] << "-" << ids[end];
        
      if (i == ids.size()) {
        outputStream << std::endl;
        break;
      }
      else 
        outputStream << ", ";
    }
  }
  
  return MB_SUCCESS;
}

  
void OrientedBoxTreeTool::print( EntityHandle set, std::ostream& str, bool list, const char* tag )
{
  TreeLayoutPrinter op1( str, instance );
  TreeNodePrinter op2( str, list, true, tag, this );
  ErrorCode r1 = preorder_traverse( set, op1 );
  str << std::endl;
  ErrorCode r2 = preorder_traverse( set, op2 );
  if (r1 != MB_SUCCESS || r2 != MB_SUCCESS) {
    std::cerr << "Errors encountered while printing tree\n";
    str << "Errors encountered while printing tree\n";
  }
}


/********************* Traversal Metrics Code  **************************/

void OrientedBoxTreeTool::TrvStats::reset(){
  nodes_visited_count.clear();
  leaves_visited_count.clear();
  traversals_ended_count.clear();
  ray_tri_tests_count = 0;
}

void OrientedBoxTreeTool::TrvStats::increment( unsigned depth ){

  while( nodes_visited_count.size() <= depth ){
    nodes_visited_count.push_back(0);
    leaves_visited_count.push_back(0);
    traversals_ended_count.push_back(0);
  }
  nodes_visited_count[depth] += 1;
}

void OrientedBoxTreeTool::TrvStats::increment_leaf( unsigned depth ){
  //assume safe depth, because increment is called first
  leaves_visited_count[depth] += 1;
}

void OrientedBoxTreeTool::TrvStats::end_traversal( unsigned depth ){
  // assume safe depth, because increment is always called on a given 
  // tree level first
  traversals_ended_count[depth] += 1;
}

void OrientedBoxTreeTool::TrvStats::print( std::ostream& str ) const {

  const std::string h1 = "OBBTree Depth";
  const std::string h2 = " - NodesVisited";
  const std::string h3 = " - LeavesVisited";
  const std::string h4 = " - TraversalsEnded";

  str << h1 << h2 << h3 << h4 << std::endl;

  unsigned num_visited = 0, num_leaves = 0, num_traversals = 0;
  for( unsigned i = 0; i < traversals_ended_count.size(); ++i){

    num_visited    += nodes_visited_count[i];
    num_leaves     += leaves_visited_count[i];
    num_traversals += traversals_ended_count[i];
   
    str << std::setw(h1.length()) << i 
        << std::setw(h2.length()) << nodes_visited_count[i] 
        << std::setw(h3.length()) << leaves_visited_count[i]
        << std::setw(h4.length()) << traversals_ended_count[i] 
        << std::endl;
  }
  
  str << std::setw(h1.length()) << "---- Totals:" 
      << std::setw(h2.length()) << num_visited 
      << std::setw(h3.length()) << num_leaves
      << std::setw(h4.length()) << num_traversals 
      << std::endl;
  
  if( ray_tri_tests_count ){
    str << std::setw(h1.length()) << "---- Total ray-tri tests: " 
	<< ray_tri_tests_count << std::endl;
  }

}

/********************** Tree Statistics Code ****************************/


class StatData { 
public:
  struct Ratio {
    double min, max, sum, sqr;
    int hist[10];
    Ratio() 
      : min(std::numeric_limits<double>::max()), 
        max(-std::numeric_limits<double>::max()), 
        sum(0.0), sqr(0.0)
      { hist[0] = hist[1] = hist[2] = hist[3] = hist[4] = hist[5] =
        hist[6] = hist[7] = hist[8] = hist[9] = 0; }
    void accum( double v ) {
      if (v < min) min = v;
      if (v > max) max = v;
      sum += v;
      sqr += v*v;
      int i = (int)(10*v);
      if (i < 0) i = 0;
      else if (i > 9) i = 9;
      ++hist[i];
    }
  };
  
  template <typename T> struct Stat {
    T min, max;
    double sum, sqr;
    Stat() : sum(0.0), sqr(0.0) {
      std::numeric_limits<T> lim;
      min = lim.max();
      if (lim.is_integer)
        max = lim.min();
      else
        max = -lim.max();
    }
    void accum( T v ) {
      if (v < min) min = v;
      if (v > max) max = v;
      sum += v;
      sqr += (double)v * v;
    }
  };

  StatData() :
    count(0) 
    {}

  Ratio volume;
  Ratio entities;
  Ratio radius;
  Stat<unsigned> leaf_ent;
  Stat<double> vol;
  Stat<double> area;
  std::vector<unsigned> leaf_depth;
  unsigned count;
};

static int measure( const CartVect& v, double& result )
{
  const double tol = 1e-6;
  int dims = 0;
  result = 1;
  for (int i = 0; i < 3; ++i)
    if (v[i] > tol) {
      ++dims; 
      result *= v[i];
  }
  return dims;
}
  

ErrorCode OrientedBoxTreeTool::recursive_stats( OrientedBoxTreeTool* tool,
                                    Interface* inst,
                                    EntityHandle set,
                                    int depth,
                                    StatData& data,
                                    unsigned& count_out,
                                    CartVect& dimensions_out )
{
  ErrorCode rval;
  OrientedBox tmp_box;
  std::vector<EntityHandle> children(2);
  unsigned counts[2];
  bool isleaf;
  
  ++data.count;
  
  rval = tool->box( set, tmp_box );
  if (MB_SUCCESS != rval) return rval;
  children.clear();
  rval = inst->get_child_meshsets( set, children );
  if (MB_SUCCESS != rval) return rval;
  isleaf = children.empty();
  if (!isleaf && children.size() != 2)
    return MB_MULTIPLE_ENTITIES_FOUND;
  
  dimensions_out = tmp_box.dimensions();
  data.radius.accum( tmp_box.inner_radius() / tmp_box.outer_radius());
  data.vol.accum( tmp_box.volume() );
  data.area.accum( tmp_box.area() );
  
  if (isleaf) {
    if (data.leaf_depth.size() <= (unsigned)depth)
      data.leaf_depth.resize( depth+1, 0 );
    ++data.leaf_depth[depth];
    
    int count;
    rval = inst->get_number_entities_by_handle( set, count );
    if (MB_SUCCESS != rval) return rval;
    count_out = count;
    data.leaf_ent.accum( count_out );
  }
  else {
    for (int i = 0; i < 2; ++i) {
      CartVect dims;
      rval = recursive_stats( tool, inst, children[i], depth+1, data, counts[i], dims );
      if (MB_SUCCESS != rval) return rval;
      double this_measure, chld_measure;
      int this_dim = measure( dimensions_out, this_measure );
      int chld_dim = measure( dims, chld_measure );
      double ratio;
      if (chld_dim < this_dim)
        ratio = 0;
      else
        ratio = chld_measure / this_measure;
  
      data.volume.accum( ratio );
    }
    count_out = counts[0] + counts[1];
    data.entities.accum( (double)counts[0] / count_out );
    data.entities.accum( (double)counts[1] / count_out );
  }
  return MB_SUCCESS;
}

static inline double std_dev( double sqr, double sum, double count )
{
  sum /= count;
  sqr /= count;
  return sqrt( sqr - sum*sum );
}

//#define WW <<std::setw(10)<<std::fixed<<
#define WE <<std::setw(10)<<
#define WW WE
ErrorCode OrientedBoxTreeTool::stats( EntityHandle set, 
                                          unsigned &total_entities,
                                          double &rv,
                                          double &tot_node_volume,
                                          double &tot_to_root_volume,
                                          unsigned &tree_height,
                                          unsigned &node_count,
                                          unsigned &num_leaves)
{
  StatData d;
  ErrorCode rval;
  unsigned i;
  CartVect total_dim;
  
  rval = recursive_stats( this, instance, set, 0, d, total_entities, total_dim );
  if (MB_SUCCESS != rval)
    return rval;
  
  tree_height = d.leaf_depth.size();
  unsigned min_leaf_depth = tree_height;
  num_leaves = 0;
  unsigned max_leaf_per_depth = 0;
  //double sum_leaf_depth = 0, sqr_leaf_depth = 0;
  for (i = 0; i < d.leaf_depth.size(); ++i) {
    unsigned val = d.leaf_depth[i];
    num_leaves += val;
    //sum_leaf_depth += (double)val*i;
    //sqr_leaf_depth += (double)val*i*i;
    if (val && i < min_leaf_depth)
      min_leaf_depth = i;
    if (max_leaf_per_depth < val)
      max_leaf_per_depth = val;
  }
  rv = total_dim[0]*total_dim[1]*total_dim[2];
  tot_node_volume = d.vol.sum;
  tot_to_root_volume = d.vol.sum/rv;
  node_count = d.count;

  return MB_SUCCESS;
}

ErrorCode OrientedBoxTreeTool::stats( EntityHandle set, std::ostream& s )
{
  StatData d;
  ErrorCode rval;
  unsigned total_entities, i;
  CartVect total_dim;
  
  rval = recursive_stats( this, instance, set, 0, d, total_entities, total_dim );
  if (MB_SUCCESS != rval)
    return rval;
  
  unsigned tree_height = d.leaf_depth.size();
  unsigned min_leaf_depth = tree_height, num_leaves = 0;
  unsigned max_leaf_per_depth = 0;
  double sum_leaf_depth = 0, sqr_leaf_depth = 0;
  for (i = 0; i < d.leaf_depth.size(); ++i) {
    unsigned val = d.leaf_depth[i];
    num_leaves += val;
    sum_leaf_depth += (double)val*i;
    sqr_leaf_depth += (double)val*i*i;
    if (val && i < min_leaf_depth)
      min_leaf_depth = i;
    if (max_leaf_per_depth < val)
      max_leaf_per_depth = val;
  }
  unsigned num_non_leaf = d.count - num_leaves;
  
  double rv = total_dim[0]*total_dim[1]*total_dim[2];
  s << "entities in tree:  " << total_entities << std::endl
    << "root volume:       " << rv << std::endl
    << "total node volume: " << d.vol.sum << std::endl
    << "total/root volume: " << d.vol.sum/rv << std::endl
    << "tree height:       " << tree_height << std::endl
    << "node count:        " << d.count << std::endl
    << "leaf count:        " << num_leaves << std::endl
    << std::endl;
  
  double avg_leaf_depth = sum_leaf_depth / num_leaves;
  double rms_leaf_depth = sqrt( sqr_leaf_depth / num_leaves );
  double std_leaf_depth = std_dev( sqr_leaf_depth, sum_leaf_depth, num_leaves );

  double avg_leaf_ent = d.leaf_ent.sum / num_leaves;
  double rms_leaf_ent = sqrt( d.leaf_ent.sqr / num_leaves );
  double std_leaf_ent = std_dev( d.leaf_ent.sqr, d.leaf_ent.sum, num_leaves );

  unsigned num_child = 2 * num_non_leaf;

  double avg_vol_ratio = d.volume.sum / num_child;
  double rms_vol_ratio = sqrt( d.volume.sqr / num_child );
  double std_vol_ratio = std_dev( d.volume.sqr, d.volume.sum, num_child);

  double avg_ent_ratio = d.entities.sum / num_child;
  double rms_ent_ratio = sqrt( d.entities.sqr / num_child );
  double std_ent_ratio = std_dev( d.entities.sqr, d.entities.sum, num_child);

  double avg_rad_ratio = d.radius.sum / d.count;
  double rms_rad_ratio = sqrt( d.radius.sqr / d.count );
  double std_rad_ratio = std_dev( d.radius.sqr, d.radius.sum, d.count );
  
  double avg_vol = d.vol.sum / d.count;
  double rms_vol = sqrt( d.vol.sqr / d.count );
  double std_vol = std_dev( d.vol.sqr, d.vol.sum, d.count );
  
  double avg_area = d.area.sum / d.count;
  double rms_area = sqrt( d.area.sqr / d.count );
  double std_area = std_dev( d.area.sqr, d.area.sum, d.count );
      
  int prec = s.precision();
  s <<                         "                   " WW "Minimum"      WW "Average"      WW "RMS"          WW "Maximum"             WW "Std.Dev."     << std::endl;
  s << std::setprecision(1) << "Leaf Depth         " WW min_leaf_depth WW avg_leaf_depth WW rms_leaf_depth WW d.leaf_depth.size()-1 WW std_leaf_depth << std::endl; 
  s << std::setprecision(0) << "Entities/Leaf      " WW d.leaf_ent.min WW avg_leaf_ent   WW rms_leaf_ent   WW d.leaf_ent.max        WW std_leaf_ent   << std::endl;
  s << std::setprecision(3) << "Child Volume Ratio " WW d.volume.min   WW avg_vol_ratio  WW rms_vol_ratio  WW d.volume.max          WW std_vol_ratio  << std::endl;
  s << std::setprecision(3) << "Child Entity Ratio " WW d.entities.min WW avg_ent_ratio  WW rms_ent_ratio  WW d.entities.max        WW std_ent_ratio  << std::endl;
  s << std::setprecision(3) << "Box Radius Ratio   " WW d.radius.min   WW avg_rad_ratio  WW rms_rad_ratio  WW d.radius.max          WW std_rad_ratio  << std::endl;
  s << std::setprecision(0) << "Box volume         " WE d.vol.min      WE avg_vol        WE rms_vol        WE d.vol.max             WE std_vol        << std::endl;
  s << std::setprecision(0) << "Largest side area  " WE d.area.min     WE avg_area       WE rms_area       WE d.area.max            WE std_area       << std::endl;
  s << std::setprecision(prec) << std::endl;
  
  s << "Leaf Depth Histogram (Root depth is 0)" << std::endl;
  double f = 60.0 / max_leaf_per_depth;
  for (i = min_leaf_depth; i < d.leaf_depth.size(); ++i)
    s << std::setw(2) << i << " " << std::setw(5) << d.leaf_depth[i] << " |"
      << std::setfill('*') << std::setw((int)floor(f*d.leaf_depth[i]+0.5)) << "" 
      << std::setfill(' ') << std::endl;
  s <<std::endl;
  
  s << "Child/Parent Volume Ratio Histogram" << std::endl;
  f = 60.0 / *(std::max_element(d.volume.hist, d.volume.hist+10));
  for (i = 0; i < 10u; ++i)
    s << "0." << i << " " << std::setw(5) << d.volume.hist[i] << " |"
      << std::setfill('*') << std::setw((int)floor(f*d.volume.hist[i]+0.5)) << ""
      << std::setfill(' ') << std::endl;
  s <<std::endl;
  
  s << "Child/Parent Entity Count Ratio Histogram" << std::endl;
  f = 60.0 / *(std::max_element(d.entities.hist, d.entities.hist+10));
  for (i = 0; i < 10u; ++i)
    s << "0." << i << " " << std::setw(5) << d.entities.hist[i] << " |"
      << std::setfill('*') << std::setw((int)floor(f*d.entities.hist[i]+0.5)) << ""
      << std::setfill(' ') << std::endl;
  s <<std::endl;
  
  s << "Inner/Outer Radius Ratio Histogram (~0.70 for cube)" << std::endl;
    // max radius ratio for a box is about 0.7071.  Move any boxes
    // in the .7 bucket into .6 and print .0 to .6.
  d.radius.hist[6] += d.radius.hist[7]; 
  f = 60.0 / *(std::max_element(d.entities.hist, d.entities.hist+7));
  for (i = 0; i < 7u; ++i)
    s << "0." << i << " " << std::setw(5) << d.entities.hist[i] << " |"
      << std::setfill('*') << std::setw((int)floor(f*d.entities.hist[i]+0.5)) << ""
      << std::setfill(' ') << std::endl;
  s <<std::endl;
  
  return MB_SUCCESS;
}  

} // namespace moab

