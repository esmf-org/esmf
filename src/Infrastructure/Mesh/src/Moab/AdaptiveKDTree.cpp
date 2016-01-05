/*
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**\file AdaptiveKDTree.cpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2007-04-1
 */

#include "moab/AdaptiveKDTree.hpp"
#include "moab/Interface.hpp"
#include "moab/GeomUtil.hpp"
#include "moab/Range.hpp"
#include "Internals.hpp"
#include <math.h>

#include <assert.h>
#include <algorithm>
#include <limits>
#include <iostream>

#if defined(_MSC_VER) || defined(__MINGW32__)
#  include <float.h>
#  define finite(A) _finite(A)
#endif

#if defined(MB_AD_KD_TREE_USE_SINGLE_TAG) && defined(HDF5_FILE)
  /* include our MPI header before any HDF5 because otherwise
     it will get included indirectly by HDF5 */
# ifdef USE_MPI
#  include "moab_mpi.h"
# endif 
# include <H5Tpublic.h>
#endif

namespace moab {

AdaptiveKDTree::Settings::Settings()
  : maxEntPerLeaf(6), 
    maxTreeDepth(30),
    candidateSplitsPerDir(3),
    candidatePlaneSet(SUBDIVISION_SNAP),
    minBoxWidth( 1e-10 )
  {}


#define MB_AD_KD_TREE_DEFAULT_TAG_NAME "AKDTree"

// If defined, use single tag for both axis and location of split plane
#define MB_AD_KD_TREE_USE_SINGLE_TAG 

// No effect if MB_AD_KD_TREE_USE_SINGLE_TAG is not defined.
// If defined, store plane axis as double so tag has consistent
// type (doubles for both location and axis).  If not defined,
// store struct Plane as opaque.
#define MB_AD_KD_TREE_USE_TWO_DOUBLE_TAG

#define MAKE_TAG( NAME, STORAGE, TYPE, COUNT, HANDLE, DEFAULT ) \
  if (MB_SUCCESS != make_tag( moab(), \
                              (NAME), \
                              (STORAGE), \
                              (TYPE), \
                              (COUNT), \
                              (DEFAULT), \
                              (HANDLE), \
                              ctl )) { \
    planeTag = axisTag = rootTag = (Tag)-1; \
    return; \
  }

static ErrorCode make_tag( Interface* iface,
                             std::string name,
                             TagType storage, 
                             DataType type,
                             int count,
                             void* default_val,
                             Tag& tag_handle,
                             std::vector<Tag>& created_tags )
{
  ErrorCode rval = iface->tag_get_handle( name.c_str(),
                                          count,
                                          type, 
                                          tag_handle,
                                          MB_TAG_CREAT|storage,
                                          default_val );

  if (MB_SUCCESS == rval) 
    created_tags.push_back( tag_handle );
  else
    while( !created_tags.empty() ) {
      iface->tag_delete( created_tags.back() );
      created_tags.pop_back();
    }
  
  return rval;
}

AdaptiveKDTree::AdaptiveKDTree( Interface* mb, const char* tagname, unsigned set_flags )
  : mbInstance(mb), meshSetFlags(set_flags), cleanUpTrees(false)
{ init(tagname); }

AdaptiveKDTree::AdaptiveKDTree( Interface* mb, bool clean_up, const char* tagname, unsigned set_flags )
  : mbInstance(mb), meshSetFlags(set_flags), cleanUpTrees(clean_up)
{ init(tagname); }

void AdaptiveKDTree::init( const char* tagname_in )
{
  const char* tagname = tagname_in ? tagname_in : MB_AD_KD_TREE_DEFAULT_TAG_NAME;
  std::vector<Tag> ctl;

#ifndef MB_AD_KD_TREE_USE_SINGLE_TAG
    // create two tags, one for axis direction and one for axis coordinate
  std::string n1(tagname), n2(tagname);
  n1 += "_coord";
  n2 += "_norm";
  MAKE_TAG( n1, MB_TAG_DENSE, MB_TYPE_DOUBLE, 1, planeTag, 0 )
  MAKE_TAG( n2, MB_TAG_DENSE, MB_TYPE_INT,    1, axisTag,  0 )

#elif defined(MB_AD_KD_TREE_USE_TWO_DOUBLE_TAG)
    // create tag to hold two doubles, one for location and one for axis
  MAKE_TAG( tagname, MB_TAG_DENSE, MB_TYPE_DOUBLE, 2, planeTag, 0 )
#else
    // create opaque tag to hold struct Plane
  MAKE_TAG( tagname, MB_TAG_DENSE, MB_TYPE_OPAQUE, sizeof(Plane), planeTag, 0 )

#ifdef HDF5_FILE  
    // create a mesh tag holding the HDF5 type for a struct Plane
  Tag type_tag;
  std::string type_tag_name = "__hdf5_tag_type_";
  type_tag_name += tagname;
  MAKE_TAG( type_tag_name, MB_TAG_MESH), MB_TYPE_OPAQUE, sizeof(hid_t), type_tag, 0 )
    // create HDF5 type object describing struct Plane
  Plane p;
  hid_t handle = H5Tcreate( H5T_COMPOUND, sizeof(Plane) );
  H5Tinsert( handle, "coord", &(p.coord) - &p, H5T_NATIVE_DOUBLE );
  H5Tinsert( handle, "norm", &(p.axis) - &p, H5T_NATIVE_INT );
  EntityHandle root = 0;
  mbInstance->tag_set_data( type_tag, &root, 1, &handle );
#endif
#endif

  std::string root_name(tagname);
  root_name += "_box";
  MAKE_TAG( root_name, MB_TAG_SPARSE, MB_TYPE_DOUBLE, 6, rootTag, 0 )

  myRoot = 0;
}

ErrorCode AdaptiveKDTree::get_split_plane( EntityHandle entity,
                                               Plane& plane )
{
#ifndef MB_AD_KD_TREE_USE_SINGLE_TAG
  ErrorCode r1, r2;
  r1 = moab()->tag_get_data( planeTag, &entity, 1, &plane.coord );
  r2 = moab()->tag_get_data( axisTag , &entity, 1, &plane.norm  );
  return MB_SUCCESS == r1 ? r2 : r1;
#elif defined(MB_AD_KD_TREE_USE_TWO_DOUBLE_TAG)
  double values[2];
  ErrorCode rval = moab()->tag_get_data( planeTag, &entity, 1, values );
  plane.coord = values[0];
  plane.norm = (int)values[1];
  return rval;
#else
  return moab()->tag_get_data( planeTag, &entity, 1, &plane );
#endif
}

AdaptiveKDTree::~AdaptiveKDTree()
{
  if (!cleanUpTrees)
    return;
  
  if (myRoot) {
    delete_tree( myRoot );
    myRoot = 0;
  }
}

ErrorCode AdaptiveKDTree::set_split_plane( EntityHandle entity, 
                                               const Plane& plane )
{
#ifndef MB_AD_KD_TREE_USE_SINGLE_TAG
  ErrorCode r1, r2;
  r1 = moab()->tag_set_data( planeTag, &entity, 1, &plane.coord );
  r2 = moab()->tag_set_data( axisTag , &entity, 1, &plane.norm  );
  return MB_SUCCESS == r1 ? r2 : r1;
#elif defined(MB_AD_KD_TREE_USE_TWO_DOUBLE_TAG)
  double values[2] = { plane.coord, static_cast<double>(plane.norm) };
  return moab()->tag_set_data( planeTag, &entity, 1, values );
#else
  return moab()->tag_set_data( planeTag, &entity, 1, &plane );
#endif
}


ErrorCode AdaptiveKDTree::set_tree_box( EntityHandle root_handle,
                                            const double box_min[3],
                                            const double box_max[3] )
{
  const double box[6] = { box_min[0], box_min[1], box_min[2],
                          box_max[0], box_max[1], box_max[2] };
  return moab()->tag_set_data( rootTag, &root_handle, 1, box );
}

ErrorCode AdaptiveKDTree::get_tree_box( EntityHandle root_handle,
                                            double box_min_out[3],
                                            double box_max_out[3] )
{
  double box[6];
  ErrorCode rval = moab()->tag_get_data( rootTag, &root_handle, 1, box );
  box_min_out[0] = box[0]; box_min_out[1] = box[1]; box_min_out[2] = box[2];
  box_max_out[0] = box[3]; box_max_out[1] = box[4]; box_max_out[2] = box[5];
  return rval;
}


ErrorCode AdaptiveKDTree::create_tree( const double box_min[3],
                                           const double box_max[3],
                                           EntityHandle& root_handle )
{
  if (myRoot) return MB_FAILURE;
  
  ErrorCode rval = moab()->create_meshset( meshSetFlags, root_handle );
  if (MB_SUCCESS != rval)
    return rval;
  
  rval = set_tree_box( root_handle, box_min, box_max );
  if (MB_SUCCESS != rval) {
    moab()->delete_entities( &root_handle, 1 );
    root_handle = 0;
    return rval;
  }
  
  myRoot = root_handle;
  
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::delete_tree( EntityHandle root_handle )
{
  ErrorCode rval;
  
  std::vector<EntityHandle> children, dead_sets, current_sets;
  assert(root_handle == myRoot);
  
  current_sets.push_back( root_handle );
  while (!current_sets.empty()) {
    EntityHandle set = current_sets.back();
    current_sets.pop_back();
    dead_sets.push_back( set );
    rval = moab()->get_child_meshsets( set, children );
    if (MB_SUCCESS != rval)
      return rval;
    std::copy( children.begin(), children.end(), std::back_inserter(current_sets) );
    children.clear();
  }
  
  rval = moab()->tag_delete_data( rootTag, &root_handle, 1 );
  if (MB_SUCCESS != rval)
    return rval;

  rval = moab()->delete_entities( &dead_sets[0], dead_sets.size() );
  if (MB_SUCCESS != rval)
    return rval;

  myRoot = 0;
  
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::find_all_trees( Range& results )
{
  return moab()->get_entities_by_type_and_tag( 0, MBENTITYSET, 
                                               &rootTag, 0, 1,
                                               results );
}

ErrorCode AdaptiveKDTree::get_tree_iterator( EntityHandle root,
                                                 AdaptiveKDTreeIter& iter )
{
  double box[6];
  ErrorCode rval = moab()->tag_get_data( rootTag, &root, 1, box );
  if (MB_SUCCESS != rval)
    return rval;
  
  return get_sub_tree_iterator( root, box, box+3, iter );
}

ErrorCode AdaptiveKDTree::get_last_iterator( EntityHandle root,
                                                 AdaptiveKDTreeIter& iter )
{
  double box[6];
  ErrorCode rval = moab()->tag_get_data( rootTag, &root, 1, box );
  if (MB_SUCCESS != rval)
    return rval;
  
  return iter.initialize( this, root, box, box+3, AdaptiveKDTreeIter::RIGHT );
}

ErrorCode AdaptiveKDTree::get_sub_tree_iterator( EntityHandle root,
                                                     const double min[3], 
                                                     const double max[3],
                                                     AdaptiveKDTreeIter& result ) 
{
  return result.initialize( this, root, min, max, AdaptiveKDTreeIter::LEFT );
}

ErrorCode AdaptiveKDTree::split_leaf( AdaptiveKDTreeIter& leaf,
                                          Plane plane,
                                          EntityHandle& left,
                                          EntityHandle& right )
{
  ErrorCode rval;
  
  rval = moab()->create_meshset( meshSetFlags, left );
  if (MB_SUCCESS != rval)
    return rval;
  
  rval = moab()->create_meshset( meshSetFlags, right );
  if (MB_SUCCESS != rval) {
    moab()->delete_entities( &left, 1 );
    return rval;
  }
  
  if (MB_SUCCESS != set_split_plane( leaf.handle(), plane ) ||
      MB_SUCCESS != moab()->add_child_meshset( leaf.handle(), left ) ||
      MB_SUCCESS != moab()->add_child_meshset( leaf.handle(), right) ||
      MB_SUCCESS != leaf.step_to_first_leaf(AdaptiveKDTreeIter::LEFT)) {
    EntityHandle children[] = { left, right };
    moab()->delete_entities( children, 2 );
    return MB_FAILURE;
  }
  
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::split_leaf( AdaptiveKDTreeIter& leaf,
                                          Plane plane )
{
  EntityHandle left, right;
  return split_leaf( leaf, plane, left, right );
}

ErrorCode AdaptiveKDTree::split_leaf( AdaptiveKDTreeIter& leaf, 
                                          Plane plane,
                                          const Range& left_entities,
                                          const Range& right_entities )
{
  EntityHandle left, right, parent = leaf.handle();
  ErrorCode rval = split_leaf( leaf, plane, left, right );
  if (MB_SUCCESS != rval)
    return rval;
  
  if (MB_SUCCESS == moab()->add_entities( left, left_entities ) &&
      MB_SUCCESS == moab()->add_entities(right,right_entities ) &&
      MB_SUCCESS == moab()->clear_meshset( &parent, 1 ))
    return MB_SUCCESS;
  
  moab()->remove_child_meshset( parent, left );
  moab()->remove_child_meshset( parent, right );
  EntityHandle children[] = { left, right };
  moab()->delete_entities( children, 2 );
  return MB_FAILURE;
}

ErrorCode AdaptiveKDTree::split_leaf( AdaptiveKDTreeIter& leaf, 
                                          Plane plane,
                                          const std::vector<EntityHandle>& left_entities,
                                          const std::vector<EntityHandle>& right_entities )
{
  EntityHandle left, right, parent = leaf.handle();
  ErrorCode rval = split_leaf( leaf, plane, left, right );
  if (MB_SUCCESS != rval)
    return rval;
  
  if (MB_SUCCESS == moab()->add_entities( left, &left_entities[0], left_entities.size() ) &&
      MB_SUCCESS == moab()->add_entities(right,&right_entities[0],right_entities.size() ) &&
      MB_SUCCESS == moab()->clear_meshset( &parent, 1 ))
    return MB_SUCCESS;
  
  moab()->remove_child_meshset( parent, left );
  moab()->remove_child_meshset( parent, right );
  EntityHandle children[] = { left, right };
  moab()->delete_entities( children, 2 );
  return MB_FAILURE;
}

ErrorCode AdaptiveKDTree::merge_leaf( AdaptiveKDTreeIter& iter )
{
  ErrorCode rval;
  if (iter.depth() == 1) // at root
    return MB_FAILURE;
  
    // Move iter to parent
  
  AdaptiveKDTreeIter::StackObj node = iter.mStack.back();
  iter.mStack.pop_back();
  
  iter.childVect.clear();
  rval = moab()->get_child_meshsets( iter.mStack.back().entity, iter.childVect );
  if (MB_SUCCESS != rval)
    return rval;
  Plane plane;
  rval = get_split_plane( iter.mStack.back().entity, plane );
  if (MB_SUCCESS != rval)
    return rval;
  
  int child_idx = iter.childVect[0] == node.entity ? 0 : 1;
  assert(iter.childVect[child_idx] == node.entity);
  iter.mBox[1-child_idx][plane.norm] = node.coord;
  

    // Get all entities from children and put them in parent
  EntityHandle parent = iter.handle();
  moab()->remove_child_meshset( parent, iter.childVect[0] );
  moab()->remove_child_meshset( parent, iter.childVect[1] );
  std::vector<EntityHandle> stack( iter.childVect );
  
  Range range;
  while (!stack.empty()) {
    EntityHandle h = stack.back();
    stack.pop_back();
    range.clear();
    rval = moab()->get_entities_by_handle( h, range );
    if (MB_SUCCESS != rval)
      return rval;
    rval = moab()->add_entities( parent, range );
    if (MB_SUCCESS != rval)
      return rval;
    
    iter.childVect.clear();
    moab()->get_child_meshsets( h, iter.childVect );
    if (!iter.childVect.empty()) {
     moab()->remove_child_meshset( h, iter.childVect[0] );
     moab()->remove_child_meshset( h, iter.childVect[1] );
     stack.push_back( iter.childVect[0] );
     stack.push_back( iter.childVect[1] );
    }
  
    rval = moab()->delete_entities( &h, 1 );
    if (MB_SUCCESS != rval)
      return rval;
  }
  
  return MB_SUCCESS;
}

  

ErrorCode AdaptiveKDTreeIter::initialize( AdaptiveKDTree* ttool,
                                              EntityHandle root,
                                              const double bmin[3],
                                              const double bmax[3],
                                              Direction direction )
{
  mStack.clear();
  treeTool = ttool;
  mBox[BMIN][0] = bmin[0];
  mBox[BMIN][1] = bmin[1];
  mBox[BMIN][2] = bmin[2];
  mBox[BMAX][0] = bmax[0];
  mBox[BMAX][1] = bmax[1];
  mBox[BMAX][2] = bmax[2];
  mStack.push_back( StackObj(root,0) );
  return step_to_first_leaf( direction );
}

ErrorCode AdaptiveKDTreeIter::step_to_first_leaf( Direction direction )
{
  ErrorCode rval;
  AdaptiveKDTree::Plane plane;
  const Direction opposite = static_cast<Direction>(1-direction);
  
  for (;;) {
    childVect.clear();
    rval = treeTool->moab()->get_child_meshsets( mStack.back().entity, childVect );
    if (MB_SUCCESS != rval)
      return rval;
    if (childVect.empty()) // leaf
      break;
  
    rval = treeTool->get_split_plane( mStack.back().entity, plane );
    if (MB_SUCCESS != rval)
      return rval;
  
    mStack.push_back( StackObj(childVect[direction],mBox[opposite][plane.norm]) );
    mBox[opposite][plane.norm] = plane.coord;
  }
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTreeIter::step( Direction direction )
{
  StackObj node, parent;
  ErrorCode rval;
  AdaptiveKDTree::Plane plane;
  const Direction opposite = static_cast<Direction>(1-direction);
  
    // If stack is empty, then either this iterator is uninitialized
    // or we reached the end of the iteration (and return 
    // MB_ENTITY_NOT_FOUND) already.
  if (mStack.empty())
    return MB_FAILURE;
    
    // Pop the current node from the stack.
    // The stack should then contain the parent of the current node.
    // If the stack is empty after this pop, then we've reached the end.
  node = mStack.back();
  mStack.pop_back();
  
  while(!mStack.empty()) {
      // Get data for parent entity
    parent = mStack.back();
    childVect.clear();
    rval = treeTool->moab()->get_child_meshsets( parent.entity, childVect );
    if (MB_SUCCESS != rval)
      return rval;
    rval = treeTool->get_split_plane( parent.entity, plane );
    if (MB_SUCCESS != rval)
      return rval;
    
      // If we're at the left child
    if (childVect[opposite] == node.entity) {
        // change from box of left child to box of parent
      mBox[direction][plane.norm] = node.coord;
        // push right child on stack
      node.entity = childVect[direction];
      node.coord = mBox[opposite][plane.norm];
      mStack.push_back( node );
        // change from box of parent to box of right child
      mBox[opposite][plane.norm] = plane.coord;
        // descend to left-most leaf of the right child
      return step_to_first_leaf(opposite);
    }
    
      // The current node is the right child of the parent,
      // continue up the tree.
    assert( childVect[direction] == node.entity );
    mBox[opposite][plane.norm] = node.coord;
    node = parent;
    mStack.pop_back();
  }
  
  return MB_ENTITY_NOT_FOUND;
}

ErrorCode AdaptiveKDTreeIter::get_neighbors( 
                      AdaptiveKDTree::Axis norm, bool neg,
                      std::vector<AdaptiveKDTreeIter>& results,
                      double epsilon ) const
{
  StackObj node, parent;
  ErrorCode rval;
  AdaptiveKDTree::Plane plane;
  int child_idx;
  
    // Find tree node at which the specified side of the box
    // for this node was created.
  AdaptiveKDTreeIter iter( *this ); // temporary iterator (don't modifiy *this)
  node = iter.mStack.back();
  iter.mStack.pop_back();
  for (;;) {
      // reached the root - original node was on boundary (no neighbors)
    if (iter.mStack.empty())
      return MB_SUCCESS;
    
      // get parent node data
    parent = iter.mStack.back();
    iter.childVect.clear();
    rval = treeTool->moab()->get_child_meshsets( parent.entity, iter.childVect );
    if (MB_SUCCESS != rval)
      return rval;
    rval = treeTool->get_split_plane( parent.entity, plane );
    if (MB_SUCCESS != rval)
      return rval;
    
    child_idx = iter.childVect[0] == node.entity ? 0 : 1;
    assert(iter.childVect[child_idx] == node.entity);
    
      // if we found the split plane for the desired side
      // push neighbor on stack and stop
    if (plane.norm == norm && (int)neg == child_idx) {
        // change from box of previous child to box of parent
      iter.mBox[1-child_idx][plane.norm] = node.coord;
        // push other child of parent onto stack
      node.entity = iter.childVect[1-child_idx];
      node.coord = iter.mBox[child_idx][plane.norm];
      iter.mStack.push_back( node );
        // change from parent box to box of new child
      iter.mBox[child_idx][plane.norm] = plane.coord;
      break;
    }
    
      // continue up the tree
    iter.mBox[1-child_idx][plane.norm] = node.coord;
    node = parent;
    iter.mStack.pop_back();
  }

    // now move down tree, searching for adjacent boxes
  std::vector<AdaptiveKDTreeIter> list;
    // loop over all potential paths to neighbors (until list is empty)
  for (;;) {
      // follow a single path to a leaf, append any other potential
      // paths to neighbors to 'list'
    node = iter.mStack.back();
    for (;;) { 
      iter.childVect.clear();
      rval = treeTool->moab()->get_child_meshsets( node.entity, iter.childVect );
      if (MB_SUCCESS != rval)
        return rval;
        
        // if leaf
      if (iter.childVect.empty()) {
        results.push_back( iter );
        break; 
      }
      
      rval = treeTool->get_split_plane( node.entity, plane );
      if (MB_SUCCESS != rval)
        return rval;
     
        // if split parallel to side
      if (plane.norm == norm) {
          // continue with whichever child is on the correct side of the split
        node.entity = iter.childVect[neg];
        node.coord = iter.mBox[1-neg][plane.norm];
        iter.mStack.push_back( node );
        iter.mBox[1-neg][plane.norm] = plane.coord;
      }
        // if left child is adjacent
      else if (this->mBox[BMIN][plane.norm] - plane.coord <= epsilon) {
          // if right child is also adjacent, add to list
        if (plane.coord - this->mBox[BMAX][plane.norm] <= epsilon) {
          list.push_back( iter );
          list.back().mStack.push_back( StackObj( iter.childVect[1], iter.mBox[BMIN][plane.norm] ) );
          list.back().mBox[BMIN][plane.norm] = plane.coord;
        }
          // continue with left child
        node.entity = iter.childVect[0];
        node.coord = iter.mBox[BMAX][plane.norm];
        iter.mStack.push_back( node );
        iter.mBox[BMAX][plane.norm] = plane.coord;
      }
        // right child is adjacent
      else {
          // if left child is not adjacent, right must be or something
          // is really messed up.
        assert(plane.coord - this->mBox[BMAX][plane.norm] <= epsilon);
           // continue with left child
        node.entity = iter.childVect[1];
        node.coord = iter.mBox[BMIN][plane.norm];
        iter.mStack.push_back( node );
        iter.mBox[BMIN][plane.norm] = plane.coord;
      }
    }
    
    if (list.empty())
      break;
    
    iter = list.back();
    list.pop_back();
  }
  
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTreeIter::sibling_side( 
                            AdaptiveKDTree::Axis& axis_out,
                            bool& neg_out ) const
{
  if (mStack.size() < 2) // at tree root
    return MB_ENTITY_NOT_FOUND;
  
  EntityHandle parent = mStack[mStack.size()-2].entity;
  AdaptiveKDTree::Plane plane;
  ErrorCode rval = tool()->get_split_plane( parent, plane );
  if (MB_SUCCESS != rval)
    return MB_FAILURE;
    
  childVect.clear();
  rval = tool()->moab()->get_child_meshsets( parent, childVect );
  if (MB_SUCCESS != rval || childVect.size() != 2)
    return MB_FAILURE;
  
  axis_out = static_cast<AdaptiveKDTree::Axis>(plane.norm);
  neg_out = (childVect[1] == handle());
  assert(childVect[neg_out] == handle());
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTreeIter::get_parent_split_plane( AdaptiveKDTree::Plane& plane ) const
{
  if (mStack.size() < 2) // at tree root
    return MB_ENTITY_NOT_FOUND;
  
  EntityHandle parent = mStack[mStack.size()-2].entity;
  return tool()->get_split_plane( parent, plane );
}


bool AdaptiveKDTreeIter::is_sibling( const AdaptiveKDTreeIter& other_leaf ) const
{
  const size_t s = mStack.size();
  return (s > 1) && (s == other_leaf.mStack.size()) &&
         (other_leaf.mStack[s-2].entity == mStack[s-2].entity) &&
         other_leaf.handle() != handle();
}

bool AdaptiveKDTreeIter::is_sibling( EntityHandle other_leaf ) const
{
  if (mStack.size() < 2 || other_leaf == handle())
    return false;
  EntityHandle parent = mStack[mStack.size()-2].entity;
  childVect.clear();
  ErrorCode rval = tool()->moab()->get_child_meshsets( parent, childVect );
  if (MB_SUCCESS != rval || childVect.size() != 2) {
    assert(false);
    return false;
  }
  return childVect[0] == other_leaf || childVect[1] == other_leaf;
}

bool AdaptiveKDTreeIter::sibling_is_forward() const
{
  if (mStack.size() < 2) // if root
    return false;
  EntityHandle parent = mStack[mStack.size()-2].entity;
  childVect.clear();
  ErrorCode rval = tool()->moab()->get_child_meshsets( parent, childVect );
  if (MB_SUCCESS != rval || childVect.size() != 2) {
    assert(false);
    return false;
  }
  return childVect[0] == handle();
}  

bool AdaptiveKDTreeIter::intersect_ray( const double ray_point[3],
                                          const double ray_vect[3],
                                          double& t_enter, 
                                          double& t_exit ) const
{
  return GeomUtil::ray_box_intersect( CartVect(box_min()),
                                        CartVect(box_max()),
                                        CartVect(ray_point),
                                        CartVect(ray_vect),
                                        t_enter, t_exit );
}

static ErrorCode intersect_children_with_elems(
                                        AdaptiveKDTree* tool,
                                        const Range& elems,
                                        AdaptiveKDTree::Plane plane,
                                        double eps,
                                        CartVect box_min,
                                        CartVect box_max,
                                        Range& left_tris,
                                        Range& right_tris,
                                        Range& both_tris,
                                        double& metric_value )
{
  left_tris.clear();
  right_tris.clear();
  both_tris.clear();
  CartVect coords[16];
  Interface *const moab = tool->moab();
  
    // get extents of boxes for left and right sides
  CartVect right_min( box_min ), left_max( box_max );
  right_min[plane.norm] = left_max[plane.norm] = plane.coord;
  const CartVect left_cen = 0.5*(left_max + box_min);
  const CartVect left_dim = 0.5*(left_max - box_min);
  const CartVect right_cen = 0.5*(box_max + right_min);
  const CartVect right_dim = 0.5*(box_max - right_min);
  const CartVect dim = box_max - box_min;
  const double max_tol = std::max(dim[0], std::max(dim[1], dim[2]))/10;
  
  
    // test each entity
  ErrorCode rval;
  int count, count2;
  const EntityHandle* conn, *conn2;
  
  const Range::const_iterator elem_begin = elems.lower_bound( MBEDGE );
  const Range::const_iterator poly_begin = elems.lower_bound( MBPOLYHEDRON, elem_begin );
  const Range::const_iterator set_begin = elems.lower_bound( MBENTITYSET, poly_begin );
  Range::iterator left_ins = left_tris.begin();
  Range::iterator right_ins = right_tris.begin();
  Range::iterator both_ins = both_tris.begin();
  Range::const_iterator i;
  
    // vertices
  for (i = elems.begin(); i != elem_begin; ++i) {
    rval = moab->get_coords( &*i, 1, coords[0].array() );
    if (MB_SUCCESS != rval)
      return rval;
    
    bool lo = false, ro = false;
    if (coords[0][plane.norm] <= plane.coord)
      lo = true;
    if (coords[0][plane.norm] >= plane.coord)
      ro = true;

    if (lo && ro)
      both_ins = both_tris.insert( both_ins, *i, *i );
    else if (lo)
      left_ins = left_tris.insert( left_ins, *i, *i );
    else // if (ro)
      right_ins = right_tris.insert( right_ins, *i, *i );
  }
  
    // non-polyhedron elements
  for (i = elem_begin; i != poly_begin; ++i) {
    rval = moab->get_connectivity( *i, conn, count, true );
    if (MB_SUCCESS != rval) 
      return rval;
    if (count > (int)(sizeof(coords)/sizeof(coords[0])))
      return MB_FAILURE;
    rval = moab->get_coords( &conn[0], count, coords[0].array() );
    if (MB_SUCCESS != rval) return rval;
    
    bool lo = false, ro = false;
    for (int j = 0; j < count; ++j) {
      if (coords[j][plane.norm] <= plane.coord)
        lo = true;
      if (coords[j][plane.norm] >= plane.coord)
        ro = true;
    }
    
      // Triangle must be in at least one leaf.  If test against plane
      // identified that leaf, then we're done.  If triangle is on both
      // sides of plane, do more precise test to ensure that it is really
      // in both.
    if (lo && ro) {
      double tol = eps;
      lo = ro = false;
      while (!lo && !ro && tol <= max_tol) {
        lo = GeomUtil::box_elem_overlap( coords, TYPE_FROM_HANDLE(*i), left_cen, left_dim+CartVect(tol) );
        ro = GeomUtil::box_elem_overlap( coords, TYPE_FROM_HANDLE(*i),right_cen,right_dim+CartVect(tol) );
        tol *= 10.0;
      }
    }
    if (lo && ro)
      both_ins = both_tris.insert( both_ins, *i, *i );
    else if (lo)
      left_ins = left_tris.insert( left_ins, *i, *i );
    else if (ro)
      right_ins = right_tris.insert( right_ins, *i, *i );
  }
  
    // polyhedra
  for (i = poly_begin; i != set_begin; ++i) {
    rval = moab->get_connectivity( *i, conn, count, true );
    if (MB_SUCCESS != rval) 
      return rval;
      
      // just check the bounding box of the polyhedron
    bool lo = false, ro = false;
    for (int j = 0; j < count; ++j) {
      rval = moab->get_connectivity( conn[j], conn2, count2, true );
      if (MB_SUCCESS != rval)
        return rval;
      
      for (int k = 0; k < count2; ++k) {
        rval = moab->get_coords( conn2 + k, 1, coords[0].array() );
        if (MB_SUCCESS != rval)
          return rval;
        if (coords[0][plane.norm] <= plane.coord)
          lo = true;
        if (coords[0][plane.norm] >= plane.coord)
          ro = true;
      }
    }
    
    if (lo && ro)
      both_ins = both_tris.insert( both_ins, *i, *i );
    else if (lo)
      left_ins = left_tris.insert( left_ins, *i, *i );
    else if (ro)
      right_ins = right_tris.insert( right_ins, *i, *i );
  }
  
    // sets
  CartVect tmin, tmax;
  for (i = set_begin; i != elems.end(); ++i) {
    rval = tool->get_tree_box( *i, tmin.array(), tmax.array() );
    if (MB_SUCCESS != rval)
      return rval;
    
    bool lo = false, ro = false;
    if (tmin[plane.norm] <= plane.coord)
      lo = true;
    if (tmax[plane.norm] >= plane.coord)
      ro = true;

    if (lo && ro)
      both_ins = both_tris.insert( both_ins, *i, *i );
    else if (lo)
      left_ins = left_tris.insert( left_ins, *i, *i );
    else // if (ro)
      right_ins = right_tris.insert( right_ins, *i, *i );
  }
  
  
  CartVect box_dim = box_max - box_min;
  double area_left = left_dim[0]*left_dim[1] + left_dim[1]*left_dim[2] + left_dim[2]*left_dim[0];
  double area_right = right_dim[0]*right_dim[1] + right_dim[1]*right_dim[2] + right_dim[2]*right_dim[0];
  double area_both = box_dim[0]*box_dim[1] + box_dim[1]*box_dim[2] + box_dim[2]*box_dim[0];
  metric_value = (area_left * left_tris.size() + area_right * right_tris.size()) / area_both + both_tris.size();
  return MB_SUCCESS;
}

static ErrorCode best_subdivision_plane( int num_planes,
                                           const AdaptiveKDTreeIter& iter,
                                           Range& best_left,
                                           Range& best_right,
                                           Range& best_both,
                                           AdaptiveKDTree::Plane& best_plane,
                                           double eps )
{
  double metric_val = std::numeric_limits<unsigned>::max();
  
  ErrorCode r;
  const CartVect box_min(iter.box_min());
  const CartVect box_max(iter.box_max());
  const CartVect diff(box_max - box_min);
  
  Range entities;
  r = iter.tool()->moab()->get_entities_by_handle( iter.handle(), entities );
  if (MB_SUCCESS != r)
    return r;
  const size_t p_count = entities.size();
  
  for (int axis = 0; axis < 3; ++axis) {
    int plane_count = num_planes;
    if ((num_planes+1)*eps >= diff[axis])
      plane_count = (int)(diff[axis] / eps) - 1;
  
    for (int p = 1; p <= plane_count; ++p) {
      AdaptiveKDTree::Plane plane = { box_min[axis] + (p/(1.0+plane_count)) * diff[axis], axis };
      Range left, right, both;
      double val;
      r = intersect_children_with_elems( iter.tool(),
                                         entities, plane, eps,
                                         box_min, box_max,
                                         left, right, both, 
                                         val );
      if (MB_SUCCESS != r)
        return r;
      const size_t sdiff = p_count - both.size();
      if (left.size() == sdiff || right.size() == sdiff)
        continue;
      
      if (val >= metric_val)
        continue;
      
      metric_val = val;
      best_plane = plane;
      best_left.swap(left);
      best_right.swap(right);
      best_both.swap(both);
    }
  }
      
  return MB_SUCCESS;
}


static ErrorCode best_subdivision_snap_plane( int num_planes,
                                           const AdaptiveKDTreeIter& iter,
                                           Range& best_left,
                                           Range& best_right,
                                           Range& best_both,
                                           AdaptiveKDTree::Plane& best_plane,
                                           std::vector<double>& tmp_data,
                                           double eps )
{
  double metric_val = std::numeric_limits<unsigned>::max();
  
  ErrorCode r;
  const CartVect box_min(iter.box_min());
  const CartVect box_max(iter.box_max());
  const CartVect diff(box_max - box_min);
  //const CartVect tol(eps*diff);
  
  Range entities, vertices;
  r = iter.tool()->moab()->get_entities_by_handle( iter.handle(), entities );
  if (MB_SUCCESS != r)
    return r;
  const size_t p_count = entities.size();
  r = iter.tool()->moab()->get_adjacencies( entities, 0, false, vertices, Interface::UNION );
  if (MB_SUCCESS != r)
    return r;

  tmp_data.resize( vertices.size() );
  for (int axis = 0; axis < 3; ++axis) {
    int plane_count = num_planes;
    if ((num_planes+1)*eps >= diff[axis])
      plane_count = (int)(diff[axis] / eps) - 1;

    double *ptrs[] = { 0, 0, 0 };
    ptrs[axis] = &tmp_data[0];
    r = iter.tool()->moab()->get_coords( vertices, ptrs[0], ptrs[1], ptrs[2] );
    if (MB_SUCCESS != r)
      return r;
  
    for (int p = 1; p <= plane_count; ++p) {
      double coord = box_min[axis] + (p/(1.0+plane_count)) * diff[axis];
      double closest_coord = tmp_data[0];
      for (unsigned i = 1; i < tmp_data.size(); ++i) 
        if (fabs(coord-tmp_data[i]) < fabs(coord-closest_coord))
          closest_coord = tmp_data[i];
      if (closest_coord - box_min[axis] <= eps || box_max[axis] - closest_coord <= eps)
        continue;
          
      AdaptiveKDTree::Plane plane = { closest_coord, axis };
      Range left, right, both;
      double val;
      r = intersect_children_with_elems( iter.tool(),
                                         entities, plane, eps,
                                         box_min, box_max,
                                         left, right, both, 
                                         val );
      if (MB_SUCCESS != r)
        return r;
      const size_t d = p_count - both.size();
      if (left.size() == d || right.size() == d)
        continue;
      
      if (val >= metric_val)
        continue;
      
      metric_val = val;
      best_plane = plane;
      best_left.swap(left);
      best_right.swap(right);
      best_both.swap(both);
    }
  }
     
  return MB_SUCCESS;
}

static ErrorCode best_vertex_median_plane( int num_planes,
                                           const AdaptiveKDTreeIter& iter,
                                           Range& best_left,
                                           Range& best_right,
                                           Range& best_both,
                                           AdaptiveKDTree::Plane& best_plane,
                                           std::vector<double>& coords,
                                           double eps)
{
  double metric_val = std::numeric_limits<unsigned>::max();
  
  ErrorCode r;
  const CartVect box_min(iter.box_min());
  const CartVect box_max(iter.box_max());
  
  Range entities, vertices;
  r = iter.tool()->moab()->get_entities_by_handle( iter.handle(), entities );
  if (MB_SUCCESS != r)
    return r;
  const size_t p_count = entities.size();
  r = iter.tool()->moab()->get_adjacencies( entities, 0, false, vertices, Interface::UNION );
  if (MB_SUCCESS != r)
    return r;

  coords.resize( vertices.size() );
  for (int axis = 0; axis < 3; ++axis) {
    if (box_max[axis] - box_min[axis] <= 2*eps)
      continue;
  
    double *ptrs[] = { 0, 0, 0 };
    ptrs[axis] = &coords[0];
    r = iter.tool()->moab()->get_coords( vertices, ptrs[0], ptrs[1], ptrs[2] );
    if (MB_SUCCESS != r)
      return r;
  
    std::sort( coords.begin(), coords.end() );
    std::vector<double>::iterator citer;
    citer = std::upper_bound( coords.begin(), coords.end(), box_min[axis] + eps );
    const size_t count = std::upper_bound( citer, coords.end(), box_max[axis] - eps ) - citer;
    size_t step;
    int np = num_planes;
    if (count < 2*(size_t)num_planes) {
      step = 1; np = count - 1;
    }
    else {
      step = count / (num_planes + 1);
    }
  
    for (int p = 1; p <= np; ++p) {
      
      citer += step;
      AdaptiveKDTree::Plane plane = { *citer, axis };
      Range left, right, both;
      double val;
      r = intersect_children_with_elems( iter.tool(),
                                         entities, plane, eps,
                                         box_min, box_max,
                                         left, right, both, 
                                         val );
      if (MB_SUCCESS != r)
        return r;
      const size_t diff = p_count - both.size();
      if (left.size() == diff || right.size() == diff)
        continue;
      
      if (val >= metric_val)
        continue;
      
      metric_val = val;
      best_plane = plane;
      best_left.swap(left);
      best_right.swap(right);
      best_both.swap(both);
    }
  }
      
  return MB_SUCCESS;
}


static ErrorCode best_vertex_sample_plane( int num_planes,
                                           const AdaptiveKDTreeIter& iter,
                                           Range& best_left,
                                           Range& best_right,
                                           Range& best_both,
                                           AdaptiveKDTree::Plane& best_plane,
                                           std::vector<double>& coords,
                                           std::vector<EntityHandle>& indices,
                                           double eps )
{
  const size_t random_elem_threshold = 20*num_planes;
  double metric_val = std::numeric_limits<unsigned>::max();
  
  ErrorCode r;
  const CartVect box_min(iter.box_min());
  const CartVect box_max(iter.box_max());
  
  Range entities, vertices;
  r = iter.tool()->moab()->get_entities_by_handle( iter.handle(), entities );
  if (MB_SUCCESS != r)
    return r;
    
    // We are selecting random vertex coordinates to use for candidate split
    // planes.  So if element list is large, begin by selecting random elements.
  const size_t p_count = entities.size();
  coords.resize( 3*num_planes );
  if (p_count < random_elem_threshold) {
    r = iter.tool()->moab()->get_adjacencies( entities, 0, false, vertices, Interface::UNION );
    if (MB_SUCCESS != r)
      return r;
  }
  else {
    indices.resize(random_elem_threshold);
    const int num_rand = p_count / RAND_MAX + 1;
    for (size_t j = 0; j < random_elem_threshold; ++j) {
      size_t rnd = rand();
      for (int i = num_rand; i > 1; --i)
        rnd *= rand();
      rnd %= p_count;
      indices[j] = entities[rnd];
    }
    r = iter.tool()->moab()->get_adjacencies( &indices[0], random_elem_threshold, 0, false, vertices, Interface::UNION );
    if (MB_SUCCESS != r)
      return r;
  }

  coords.resize( vertices.size() );
  for (int axis = 0; axis < 3; ++axis) {
    if (box_max[axis] - box_min[axis] <= 2*eps)
      continue;
  
    double *ptrs[] = { 0, 0, 0 };
    ptrs[axis] = &coords[0];
    r = iter.tool()->moab()->get_coords( vertices, ptrs[0], ptrs[1], ptrs[2] );
    if (MB_SUCCESS != r)
      return r;
      
    size_t num_valid_coords = 0;
    for (size_t i = 0; i < coords.size(); ++i) 
      if (coords[i] > box_min[axis]+eps && coords[i] < box_max[axis]-eps)
        ++num_valid_coords;
      
    if (2*(size_t)num_planes > num_valid_coords) {
      indices.clear();
      for (size_t i = 0; i < coords.size(); ++i) 
        if (coords[i] > box_min[axis]+eps && coords[i] < box_max[axis]-eps)
          indices.push_back( i );
    }
    else {
      indices.resize( num_planes );
        // make sure random indices are sufficient to cover entire range
      const int num_rand = coords.size() / RAND_MAX + 1;
      for (int j = 0; j < num_planes; ++j)
      {
        size_t rnd;
        do { 
          rnd = rand();
          for (int i = num_rand; i > 1; --i)
            rnd *= rand();
          rnd %= coords.size();
        } while (coords[rnd] <= box_min[axis]+eps || coords[rnd] >= box_max[axis]-eps);
        indices[j] = rnd;
      }
    }
  
    for (unsigned p = 0; p < indices.size(); ++p) {
      
      AdaptiveKDTree::Plane plane = { coords[indices[p]], axis };
      Range left, right, both;
      double val;
      r = intersect_children_with_elems( iter.tool(),
                                         entities, plane, eps,
                                         box_min, box_max,
                                         left, right, both, 
                                         val );
      if (MB_SUCCESS != r)
        return r;
      const size_t diff = p_count - both.size();
      if (left.size() == diff || right.size() == diff)
        continue;
      
      if (val >= metric_val)
        continue;
      
      metric_val = val;
      best_plane = plane;
      best_left.swap(left);
      best_right.swap(right);
      best_both.swap(both);
    }
  }
      
  return MB_SUCCESS;
}

static inline void box_accum( const CartVect& point,
                              CartVect& bmin,
                              CartVect& bmax )
{
  for (unsigned j = 0; j < 3; ++j) {
    if (point[j] < bmin[j])
      bmin[j] = point[j];
    if (point[j] > bmax[j])
      bmax[j] = point[j];
  }
}

ErrorCode AdaptiveKDTree::bounding_box( const Range& elems,
                                            double box_min[3],
                                            double box_max[3] )
{
  ErrorCode rval;
  CartVect bmin(HUGE_VAL), bmax(-HUGE_VAL);
  CartVect coords;
  EntityHandle const *conn, *conn2;
  int len, len2;
  Range::const_iterator i;
  
    // vertices
  const Range::const_iterator elem_begin = elems.lower_bound( MBEDGE );
  for (i = elems.begin(); i != elem_begin; ++i) {
    rval = moab()->get_coords( &*i, 1, coords.array() );
    if (MB_SUCCESS != rval)
      return rval;
    box_accum( coords, bmin, bmax );
  }

    // elements with vertex-handle connectivity list
  const Range::const_iterator poly_begin = elems.lower_bound( MBPOLYHEDRON, elem_begin );
  for (i = elem_begin; i != poly_begin; ++i) {
    rval = moab()->get_connectivity( *i, conn, len, true );
    if (MB_SUCCESS != rval)
      return rval;

    for (int j = 0; j < len; ++j) {
      rval = moab()->get_coords( conn+j, 1, coords.array() );
      if (MB_SUCCESS != rval)
        return rval;
      box_accum( coords, bmin, bmax );
    }
  }
  
    // polyhedra
  const Range::const_iterator set_begin  = elems.lower_bound( MBENTITYSET, poly_begin );
  for (i = poly_begin; i != set_begin; ++i) {
    rval = moab()->get_connectivity( *i, conn, len, true );
    if (MB_SUCCESS != rval)
      return rval;

    for (int j = 0; j < len; ++j) {
      rval = moab()->get_connectivity( conn[j], conn2, len2 );
      for (int k = 0; k < len2; ++k) {
        rval = moab()->get_coords( conn2+k, 1, coords.array() );
        if (MB_SUCCESS != rval)
          return rval;
        box_accum( coords, bmin, bmax );
      }
    }
  }
  
    // sets
  CartVect tmin, tmax;
  for (i = set_begin; i != elems.end(); ++i) {
    rval = get_tree_box( *i, tmin.array(), tmax.array() );
    if (MB_SUCCESS != rval)
      return rval;
      
    for (int j = 0; j < 3; ++j) {
      if (tmin[j] < bmin[j])
        bmin[j] = tmin[j];
      if (tmax[j] > bmax[j])
        bmax[j] = tmax[j];
    }
  }
  
  bmin.get( box_min );
  bmax.get( box_max );
  return MB_SUCCESS;
}


ErrorCode AdaptiveKDTree::build_tree( const Range& elems,
                                          EntityHandle& root_set_out,
                                          const Settings* settings_ptr )
{
  ErrorCode rval;
  Settings settings;
  if (settings_ptr)
    settings = *settings_ptr;
  if (settings.maxEntPerLeaf < 1)
    settings.maxEntPerLeaf = 1;
  if (settings.maxTreeDepth < 1)
    settings.maxTreeDepth = std::numeric_limits<unsigned>::max();
  if (settings.candidateSplitsPerDir < 1)
    settings.candidateSplitsPerDir = 1;
  
    // calculate bounding box of elements
  CartVect bmin, bmax;
  rval = bounding_box( elems, bmin.array(), bmax.array() );
  if (MB_SUCCESS != rval)
    return rval;
  
    // create tree root
  rval = create_tree( bmin.array(), bmax.array(), root_set_out );
  if (MB_SUCCESS != rval)
    return rval;
  rval = moab()->add_entities( root_set_out, elems );
  if (MB_SUCCESS != rval)
    return rval;
  
  AdaptiveKDTreeIter iter;
  iter.initialize( this, root_set_out, bmin.array(), bmax.array(), AdaptiveKDTreeIter::LEFT );
  
  std::vector<double> tmp_data;
  std::vector<EntityHandle> tmp_data2;
  for (;;) {
  
    int pcount;
    rval = moab()->get_number_entities_by_handle( iter.handle(), pcount );
    if (MB_SUCCESS != rval)
      break;

    const size_t p_count = pcount;
    Range best_left, best_right, best_both;
    Plane best_plane = { HUGE_VAL, -1 };
    if (p_count > settings.maxEntPerLeaf && iter.depth() < settings.maxTreeDepth) {
      switch (settings.candidatePlaneSet) {
        case AdaptiveKDTree::SUBDIVISION:
          rval = best_subdivision_plane( settings.candidateSplitsPerDir, 
                                               iter, 
                                               best_left, 
                                               best_right, 
                                               best_both, 
                                               best_plane, 
                                               settings.minBoxWidth );
          break;
        case AdaptiveKDTree::SUBDIVISION_SNAP:
          rval = best_subdivision_snap_plane( settings.candidateSplitsPerDir, 
                                               iter, 
                                               best_left, 
                                               best_right, 
                                               best_both, 
                                               best_plane, 
                                               tmp_data, 
                                               settings.minBoxWidth );
          break;
        case AdaptiveKDTree::VERTEX_MEDIAN:
          rval = best_vertex_median_plane( settings.candidateSplitsPerDir, 
                                               iter, 
                                               best_left, 
                                               best_right, 
                                               best_both, 
                                               best_plane, 
                                               tmp_data, 
                                               settings.minBoxWidth );
          break;
        case AdaptiveKDTree::VERTEX_SAMPLE:
          rval = best_vertex_sample_plane( settings.candidateSplitsPerDir, 
                                               iter, 
                                               best_left, 
                                               best_right, 
                                               best_both, 
                                               best_plane, 
                                               tmp_data, 
                                               tmp_data2,
                                               settings.minBoxWidth );
          break;
        default:
          rval = MB_FAILURE;
      }
    
      if (MB_SUCCESS != rval)
        return rval;
    }
    
    if (best_plane.norm >= 0) {
      best_left.merge( best_both );
      best_right.merge( best_both );
      rval = split_leaf( iter, best_plane,  best_left, best_right );
      if (MB_SUCCESS != rval)
        return rval;
    }
    else {
      rval = iter.step();
      if (MB_ENTITY_NOT_FOUND == rval) 
        return MB_SUCCESS;  // at end
      else if (MB_SUCCESS != rval)
        break;
    }
  }
  
  delete_tree( root_set_out );
  return rval;
}

ErrorCode AdaptiveKDTree::leaf_containing_point( EntityHandle tree_root,
                                                     const double point[3],
                                                 EntityHandle& leaf_out)
{
  std::vector<EntityHandle> children;
  Plane plane;
  EntityHandle node = tree_root;
  ErrorCode rval = moab()->get_child_meshsets( node, children );
  if (MB_SUCCESS != rval)
    return rval;
  while (!children.empty()) {
    rval = get_split_plane( node, plane );
    if (MB_SUCCESS != rval)
      return rval;
      
    const double d = point[plane.norm] - plane.coord;
    node = children[(d > 0.0)];
    
    children.clear();
    rval = moab()->get_child_meshsets( node, children );
    if (MB_SUCCESS != rval)
      return rval;
  }
  leaf_out = node;
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::leaf_containing_point( EntityHandle root,
                                                     const double point[3],
                                                     AdaptiveKDTreeIter& result )
{
    // get bounding box of tree
  ErrorCode rval = moab()->tag_get_data( rootTag, &root, 1, result.mBox );
  if (MB_SUCCESS != rval)
    return rval;
    
    // test that point is inside tree
  if (point[0] < result.box_min()[0] || point[0] > result.box_max()[0] ||
      point[1] < result.box_min()[1] || point[1] > result.box_max()[1] ||
      point[2] < result.box_min()[2] || point[2] > result.box_max()[2])
    return MB_ENTITY_NOT_FOUND;  

    // initialize iterator at tree root
  result.treeTool = this;
  result.mStack.clear();
  result.mStack.push_back( AdaptiveKDTreeIter::StackObj(root,0) );
    
    // loop until we reach a leaf
  AdaptiveKDTree::Plane plane;
  for(;;) {
      // get children
    result.childVect.clear();
    rval = moab()->get_child_meshsets( result.handle(), result.childVect );
    if (MB_SUCCESS != rval)
      return rval;
      
      // if no children, then at leaf (done)
    if (result.childVect.empty())
      break;

      // get split plane
    rval = get_split_plane( result.handle(), plane );
    if (MB_SUCCESS != rval) 
      return rval;
    
      // step iterator to appropriate child
      // idx: 0->left, 1->right
    const int idx = (point[plane.norm] > plane.coord);
    result.mStack.push_back( AdaptiveKDTreeIter::StackObj( result.childVect[idx], 
                                                             result.mBox[1-idx][plane.norm] ) );
    result.mBox[1-idx][plane.norm] = plane.coord;
  }
    
  return MB_SUCCESS;
}

struct NodeDistance {
  EntityHandle handle;
  CartVect dist; // from_point - closest_point_on_box
};

ErrorCode AdaptiveKDTree::leaves_within_distance( EntityHandle tree_root,
                                                      const double from_point[3],
                                                      const double distance,
                                                  std::vector<EntityHandle>& result_list,
                                                  std::vector<double> *result_dists)
{
  const double dist_sqr = distance * distance;
  const CartVect from(from_point);
  std::vector<NodeDistance> list, result_list_nodes;     // list of subtrees to traverse, and results 
    // pre-allocate space for default max tree depth
  Settings tmp_settings;
  list.reserve( tmp_settings.maxTreeDepth );

    // misc temporary values
  Plane plane;
  NodeDistance node; 
  ErrorCode rval;
  std::vector<EntityHandle> children;
  
    // Get distance from input position to bounding box of tree
    // (zero if inside box)
  double min[3], max[3];
  rval = get_tree_box( tree_root, min, max );
    // if bounding box is not available (e.g. not starting from true root)
    // just start with zero.  Less efficient, but will work.
  node.dist = CartVect(0.0);
  if (MB_SUCCESS == rval) {
    for (int i = 0; i < 3; ++i) {
      if (from_point[i] < min[i])
        node.dist[i] = min[i] - from_point[i];
      else if (from_point[i] > max[i])
        node.dist[i] = from_point[i] - max[i];
    }
    if (node.dist % node.dist > dist_sqr)
      return MB_SUCCESS;
  }
  
    // begin with root in list  
  node.handle = tree_root;
  list.push_back( node );
  
  while( !list.empty() ) {

    node = list.back();
    list.pop_back();
      
      // If leaf node, test contained triangles
    children.clear();
    rval = moab()->get_child_meshsets( node.handle, children );
    if (children.empty()) {
      result_list_nodes.push_back( node);
      continue;
    }
      
      // If not leaf node, add children to working list
    rval = get_split_plane( node.handle, plane );
    if (MB_SUCCESS != rval)
      return rval;
    
    const double d = from[plane.norm] - plane.coord;
    
      // right of plane?
    if (d > 0) {
      node.handle = children[1];
      list.push_back( node );
        // if the split plane is close to the input point, add
        // the left child also (we'll check the exact distance
        /// when we pop it from the list.)
      if (d <= distance) {
        node.dist[plane.norm] = d;
        if (node.dist % node.dist <= dist_sqr) {
          node.handle = children[0];
          list.push_back( node );
        }
      }
    }
      // left of plane
    else {
      node.handle = children[0];
      list.push_back( node );
        // if the split plane is close to the input point, add
        // the right child also (we'll check the exact distance
        /// when we pop it from the list.)
      if (-d <= distance) {
        node.dist[plane.norm] = -d;
        if (node.dist % node.dist <= dist_sqr) {
          node.handle = children[1];
          list.push_back( node );
        }
      }
    }
  }

    // separate loops to avoid if test inside loop
  result_list.reserve(result_list_nodes.size());
  for (std::vector<NodeDistance>::iterator vit = result_list_nodes.begin(); 
       vit != result_list_nodes.end(); vit++)
    result_list.push_back((*vit).handle);
  
  if (result_dists && distance > 0.0) {
    result_dists->reserve(result_list_nodes.size());
    for (std::vector<NodeDistance>::iterator vit = result_list_nodes.begin(); 
         vit != result_list_nodes.end(); vit++)
      result_dists->push_back((*vit).dist.length());
  }
  
  return MB_SUCCESS;
}

static ErrorCode closest_to_triangles( Interface* moab,
                                         const Range& tris,
                                         const CartVect& from,
                                         double& shortest_dist_sqr,
                                         CartVect& closest_pt,
                                         EntityHandle& closest_tri )
{
  ErrorCode rval;
  CartVect pos, diff, verts[3];
  const EntityHandle* conn;
  int len;
      
  for (Range::iterator i = tris.begin(); i != tris.end(); ++i) {
    rval = moab->get_connectivity( *i, conn, len );
    if (MB_SUCCESS != rval)
      return rval;

    rval = moab->get_coords( conn, 3, verts[0].array() );
    if (MB_SUCCESS != rval)
      return rval;

    GeomUtil::closest_location_on_tri( from, verts, pos );
    diff = pos - from;
    double dist_sqr = diff % diff;
    if (dist_sqr < shortest_dist_sqr) {
        // new closest location
      shortest_dist_sqr = dist_sqr;
      closest_pt = pos;
      closest_tri = *i;
    }
  }
  
  return MB_SUCCESS;
}


static ErrorCode closest_to_triangles( Interface* moab,
                                         EntityHandle set_handle,
                                         const CartVect& from,
                                         double& shortest_dist_sqr,
                                         CartVect& closest_pt,
                                         EntityHandle& closest_tri )
{
  ErrorCode rval;
  Range tris;
  
  rval = moab->get_entities_by_type( set_handle, MBTRI, tris );
  if (MB_SUCCESS != rval)
    return rval;

  return closest_to_triangles( moab, tris, from, shortest_dist_sqr, closest_pt, closest_tri );
}

ErrorCode AdaptiveKDTree::find_close_triangle( EntityHandle root,
                                                   const double from[3],
                                                   double pt[3],
                                                   EntityHandle& triangle )
{
  ErrorCode rval;
  Range tris;
  Plane split;
  std::vector<EntityHandle> stack;
  std::vector<EntityHandle> children(2);
  stack.reserve(30);
  stack.push_back( root );
  
  while (!stack.empty()) {
    EntityHandle node = stack.back();
    stack.pop_back();
    
    for (;;) {  // loop until we find a leaf
    
      children.clear();
      rval = moab()->get_child_meshsets( node, children );
      if (MB_SUCCESS != rval)
        return rval;
        
        // loop termination criterion
      if (children.empty())
        break;
      
        // if not a leaf, get split plane
      rval = get_split_plane( node, split );
      if (MB_SUCCESS != rval)
        return rval;
      
        // continue down the side that contains the point,
        // and push the other side onto the stack in case
        // we need to check it later.
      int rs = split.right_side( from );
      node = children[rs];
      stack.push_back( children[1-rs] );
    }
    
      // We should now be at a leaf.  
      // If it has some triangles, we're done.
      // If not, continue searching for another leaf.
    tris.clear();
    rval = moab()->get_entities_by_type( node, MBTRI, tris );
    if (!tris.empty()) {
      double dist_sqr = HUGE_VAL;
      CartVect point(pt);
      rval = closest_to_triangles( moab(), tris, CartVect(from), dist_sqr, point, triangle );
      point.get(pt);
      return rval;
    }
  }
  
    // If we got here, then we traversed the entire tree 
    // and all the leaves were empty.
  return MB_ENTITY_NOT_FOUND;
}

/** Find the triangles in a set that are closer to the input
 *  position than any triangles in the 'closest_tris' list.
 *
 *  closest_tris is assumed to contain a list of triangles for 
 *  which the first is the closest known triangle to the input 
 *  position and the first entry in 'closest_pts' is the closest
 *  location on that triangle.  Any other values in the lists must
 *  be other triangles for which the closest point is within the
 *  input tolernace of the closest closest point.  This function
 *  will update the lists as appropriate if any closer triangles
 *  or triangles within the tolerance of the current closest location
 *  are found.  The fisrt entry is maintaned as the closest of the
 *  list of triangles.
 */
/*
static ErrorCode closest_to_triangles( Interface* moab,
                                         EntityHandle set_handle,
                                         double tolerance,
                                         const CartVect& from,
                                         std::vector<EntityHandle>& closest_tris,
                                         std::vector<CartVect>& closest_pts )
{
  ErrorCode rval;
  Range tris;
  CartVect pos, diff, verts[3];
  const EntityHandle* conn;
  int len;
  double shortest_dist_sqr = HUGE_VAL;
  if (!closest_pts.empty()) {
    diff = from - closest_pts.front();
    shortest_dist_sqr = diff % diff;
  }
  
  rval = moab->get_entities_by_type( set_handle, MBTRI, tris );
  if (MB_SUCCESS != rval)
    return rval;
      
  for (Range::iterator i = tris.begin(); i != tris.end(); ++i) {
    rval = moab->get_connectivity( *i, conn, len );
    if (MB_SUCCESS != rval)
      return rval;

    rval = moab->get_coords( conn, 3, verts[0].array() );
    if (MB_SUCCESS != rval)
      return rval;

    GeomUtil::closest_location_on_tri( from, verts, pos );
    diff = pos - from;
    double dist_sqr = diff % diff;
    if (dist_sqr < shortest_dist_sqr) {
        // new closest location
      shortest_dist_sqr = dist_sqr;

      if (closest_pts.empty()) {
        closest_tris.push_back( *i );
        closest_pts.push_back( pos );
      }
        // if have a previous closest location
      else {
          // if previous closest is more than 2*tolerance away
          // from new closest, then nothing in the list can
          // be within tolerance of new closest point.
        diff = pos - closest_pts.front();
        dist_sqr = diff % diff;
        if (dist_sqr > 4.0 * tolerance * tolerance) {
          closest_tris.clear();
          closest_pts.clear();
          closest_tris.push_back( *i );
          closest_pts.push_back( pos );
        }
          // otherwise need to remove any triangles that are
          // not within tolerance of the new closest point.
        else {
          unsigned r = 0, w = 0;
          for (r = 0; r < closest_pts.size(); ++r) {
            diff = pos - closest_pts[r];
            if (diff % diff <= tolerance*tolerance) {
              closest_pts[w] = closest_pts[r];
              closest_tris[w] = closest_tris[r];
              ++w;
            }
          }
          closest_pts.resize( w + 1 );
          closest_tris.resize( w + 1 );
            // always put the closest one in the front
          if (w > 0) {
            closest_pts.back() = closest_pts.front();
            closest_tris.back() = closest_tris.front();
          }
          closest_pts.front() = pos;
          closest_tris.front() = *i;
        }
      }
    }
    else {
        // If within tolerance of old closest triangle,
        // add this one to the list.
      diff = closest_pts.front() - pos;
      if (diff % diff <= tolerance*tolerance) {
        closest_pts.push_back( pos );
        closest_tris.push_back( *i );
      }
    }
  }
  
  return MB_SUCCESS;
}
*/

ErrorCode AdaptiveKDTree::closest_triangle( EntityHandle tree_root,
                                 const double from_coords[3],
                                 double closest_point_out[3],
                                 EntityHandle& triangle_out )
{
  ErrorCode rval;
  double shortest_dist_sqr = HUGE_VAL;
  std::vector<EntityHandle> leaves;
  const CartVect from(from_coords);
  CartVect closest_pt;
  
    // Find the leaf containing the input point
    // This search does not take into account any bounding box for the
    // tree, so it always returns one leaf.
  rval = find_close_triangle( tree_root, from_coords, closest_pt.array(), triangle_out );
  if (MB_SUCCESS != rval) return rval;
  
    // Find any other leaves for which the bounding box is within
    // the same distance from the input point as the current closest
    // point is.
  CartVect diff = closest_pt - from;
  rval = leaves_within_distance( tree_root, from_coords, 
                                 sqrt(diff%diff), leaves );
  if (MB_SUCCESS != rval) return rval;

    // Check any close leaves to see if they contain triangles that
    // are as close to or closer than the current closest triangle(s).
  for (unsigned i = 0; i < leaves.size(); ++i) {
    rval = closest_to_triangles( moab(), leaves[i], from, shortest_dist_sqr, 
                                 closest_pt, triangle_out );
    if (MB_SUCCESS != rval) return rval;
  }
  
    // pass back resulting position
  closest_pt.get( closest_point_out );
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::sphere_intersect_triangles( 
                                   EntityHandle tree_root,
                                   const double center[3],
                                   double radius,
                                   std::vector<EntityHandle>& triangles )
{
  ErrorCode rval;
  std::vector<EntityHandle> leaves;
  const CartVect from(center);
  CartVect closest_pt;
  const EntityHandle* conn;
  CartVect coords[3];
  int conn_len;

    // get leaves of tree that intersect sphere
  rval = leaves_within_distance( tree_root, center, radius, leaves );
  if (MB_SUCCESS != rval) return rval;
  
    // search each leaf for triangles intersecting sphere
  for (unsigned i = 0; i < leaves.size(); ++i) {
    Range tris;
    rval = moab()->get_entities_by_type( leaves[i], MBTRI, tris );
    if (MB_SUCCESS != rval) return rval;
    
    for (Range::iterator j = tris.begin(); j != tris.end(); ++j) {
      rval = moab()->get_connectivity( *j, conn, conn_len );
      if (MB_SUCCESS != rval) return rval;
      rval = moab()->get_coords( conn, 3, coords[0].array() );
      if (MB_SUCCESS != rval) return rval;
      GeomUtil::closest_location_on_tri( from, coords, closest_pt );
      closest_pt -= from;
      if ((closest_pt % closest_pt) <= (radius*radius)) 
        triangles.push_back( *j );
    }
  }
  
    // remove duplicates from triangle list
  std::sort( triangles.begin(), triangles.end() );
  triangles.erase( std::unique( triangles.begin(), triangles.end() ), triangles.end() );
  return MB_SUCCESS;
}
  
      

struct NodeSeg {
  NodeSeg( EntityHandle h, double b, double e )
    : handle(h), beg(b), end(e) {}
  EntityHandle handle;
  double beg, end;
};

ErrorCode AdaptiveKDTree::ray_intersect_triangles( EntityHandle root,
                                                   const double tol,
                                                   const double ray_dir_in[3],
                                                   const double ray_pt_in[3],
                                                   std::vector<EntityHandle>& tris_out,
                                                   std::vector<double>& dists_out,
                                                   int max_ints,
                                                   double ray_end )
{
  ErrorCode rval;
  double ray_beg = 0.0;
  if (ray_end < 0.0)
    ray_end = HUGE_VAL;
  
    // if root has bounding box, trim ray to that box
  CartVect bmin, bmax, tvec(tol);
  const CartVect ray_pt( ray_pt_in ), ray_dir( ray_dir_in );
  rval = get_tree_box( root, bmin.array(), bmax.array() );
  if (MB_SUCCESS == rval) {
    if (!GeomUtil::segment_box_intersect( bmin-tvec, bmax+tvec, ray_pt, ray_dir, ray_beg, ray_end ))
      return MB_SUCCESS; // ray misses entire tree.
  }
  
  Range tris;
  Range::iterator iter;
  CartVect tri_coords[3];
  const EntityHandle* tri_conn;
  int conn_len;
  double tri_t;
  
  Plane plane;
  std::vector<EntityHandle> children;
  std::vector<NodeSeg> list;
  NodeSeg seg(root, ray_beg, ray_end);
  list.push_back( seg );
  
  while (!list.empty()) {
    seg = list.back();
    list.pop_back();
    
      // If we are limited to a certain number of intersections
      // (max_ints != 0), then ray_end will contain the distance
      // to the furthest intersection we have so far.  If the
      // tree node is further than that, skip it.
    if (seg.beg > ray_end) 
      continue;

      // Check if at a leaf 
    children.clear();
    rval = moab()->get_child_meshsets( seg.handle, children );
    if (MB_SUCCESS != rval)
      return rval;
    if (children.empty()) { // leaf

      tris.clear();
      rval = moab()->get_entities_by_type( seg.handle, MBTRI, tris );
      if (MB_SUCCESS != rval)
        return rval;
    
      for (iter = tris.begin(); iter != tris.end(); ++iter) {
        rval = moab()->get_connectivity( *iter, tri_conn, conn_len );
        if (MB_SUCCESS != rval) return rval;
        rval = moab()->get_coords( tri_conn, 3, tri_coords[0].array() );
        if (MB_SUCCESS != rval) return rval;
        
        if (GeomUtil::ray_tri_intersect( tri_coords, ray_pt, ray_dir, tol, tri_t, &ray_end )) {
          if (!max_ints) {
            if (std::find(tris_out.begin(),tris_out.end(),*iter) == tris_out.end()) {
              tris_out.push_back( *iter );
              dists_out.push_back( tri_t );
            }
          } 
          else if (tri_t < ray_end) {
            if (std::find(tris_out.begin(),tris_out.end(),*iter) == tris_out.end()) {
              if (tris_out.size() < (unsigned)max_ints) {
                tris_out.resize( tris_out.size() + 1 );
                dists_out.resize( dists_out.size() + 1 );
              }
              int w = tris_out.size() - 1;
              for (; w > 0 && tri_t < dists_out[w-1]; --w) {
                tris_out[w] = tris_out[w-1];
                dists_out[w] = dists_out[w-1];
              }
              tris_out[w] = *iter;
              dists_out[w] = tri_t;
              ray_end = dists_out.back();
            }
          }
        }
      }

      continue;
    }
    
    rval = get_split_plane( seg.handle, plane );
    if (MB_SUCCESS != rval)
      return rval;
    
      // Consider two planes that are the split plane +/- the tolerance.
      // Calculate the segment parameter at which the line segment intersects
      // the true plane, and also the difference between that value and the
      // intersection with either of the +/- tol planes.
    const double inv_dir = 1.0/ray_dir[plane.norm]; // only do division once
    const double t = (plane.coord - ray_pt[plane.norm]) * inv_dir; // intersection with plane
    const double diff = tol * inv_dir; // t adjustment for +tol plane
    //const double t0 = t - diff; // intersection with -tol plane
    //const double t1 = t + diff; // intersection with +tol plane
    
      // The index of the child tree node (0 or 1) that is on the
      // side of the plane to which the ray direction points.  That is,
      // if the ray direction is opposite the plane normal, the index
      // of the child corresponding to the side beneath the plane.  If
      // the ray direction is the same as the plane normal, the index
      // of the child corresponding to the side above the plane.
    const int fwd_child = (ray_dir[plane.norm] > 0.0);
    
      // Note: we maintain seg.beg <= seg.end at all times, so assume that here.
    
      // If segment is parallel to plane
    if (!finite(t)) {
      if (ray_pt[plane.norm] - tol <= plane.coord)
        list.push_back( NodeSeg( children[0], seg.beg, seg.end ) );
      if (ray_pt[plane.norm] + tol >= plane.coord)
        list.push_back( NodeSeg( children[1], seg.beg, seg.end ) );
    }
      // If segment is entirely to one side of plane such that the
      // intersection with the split plane is past the end of the segment
    else if (seg.end + diff < t) {
        // If segment direction is opposite that of plane normal, then
        // being past the end of the segment means that we are to the
        // right (or above) the plane and what the right child (index == 1).
        // Otherwise we want the left child (index == 0);
      list.push_back( NodeSeg( children[1-fwd_child], seg.beg, seg.end ) );
    }
      // If the segment is entirely to one side of the plane such that
      // the intersection with the split plane is before the start of the
      // segment
    else if (seg.beg - diff > t) {
        // If segment direction is opposite that of plane normal, then
        // being before the start of the segment means that we are to the
        // left (or below) the plane and what the left child (index == 0).
        // Otherwise we want the right child (index == 1);
      list.push_back( NodeSeg( children[fwd_child], seg.beg, seg.end ) );
    }
      // Otherwise we must intersect the plane.
      // Note: be careful not to grow the segment if t is slightly
      // outside the current segment, as doing so would effectively
      // increase the tolerance as we descend the tree.
    else if (t <= seg.beg) {
      list.push_back( NodeSeg( children[1-fwd_child], seg.beg, seg.beg ) );
      list.push_back( NodeSeg( children[  fwd_child], seg.beg, seg.end ) );
    }
    else if (t >= seg.end) {
      list.push_back( NodeSeg( children[1-fwd_child], seg.beg, seg.end ) );
      list.push_back( NodeSeg( children[  fwd_child], seg.end, seg.end ) );
    }
    else {
      list.push_back( NodeSeg( children[1-fwd_child], seg.beg, t ) );
      list.push_back( NodeSeg( children[  fwd_child], t, seg.end ) );
    }
  }
  
  return MB_SUCCESS;
}

ErrorCode AdaptiveKDTree::depth( EntityHandle root, 
                                     unsigned int& min_depth,
                                     unsigned int& max_depth )
{
  AdaptiveKDTreeIter iter;
  get_tree_iterator( root, iter );
  iter.step_to_first_leaf(AdaptiveKDTreeIter::LEFT);
  min_depth = max_depth = iter.depth();
  
  int num_of_elements = 0, max, min;
  moab()->get_number_entities_by_handle( iter.handle(), num_of_elements);
  max = min= num_of_elements;
  int k = 0;
  while (MB_SUCCESS == iter.step()) {
    int temp = 0;
    moab()->get_number_entities_by_handle( iter.handle(), temp);
    num_of_elements += temp;
    max = std::max( max, temp);
    min = std::min( min, temp);
    if (iter.depth() > max_depth)
      max_depth = iter.depth();
    else if (iter.depth() < min_depth)
      min_depth = iter.depth();
    ++k;
  }
  std::cout << std::endl << "# of leafs: " << k+1 << std::endl;
  std::cout << std::endl << "max depth: " << max << std::endl;
  std::cout << std::endl << "min depth: " << min << std::endl;
  std::cout << std::endl << "# of elements " << num_of_elements << std::endl;
  return MB_SUCCESS;
}
          
ErrorCode AdaptiveKDTree::get_info(EntityHandle root,
                                       double min[3], double max[3], 
                                       unsigned int &dep) 
{
  ErrorCode result = get_tree_box(root, min, max);
  if (MB_SUCCESS != result) return result;
  
  unsigned min_depth;
  return depth( root, min_depth, dep );
}

} // namespace moab
