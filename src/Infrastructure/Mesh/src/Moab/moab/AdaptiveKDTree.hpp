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

/**\file AdaptiveKDTree.hpp
 * \class moab::AdaptiveKDTree
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2007-04-1
 * \brief Adaptive KD tree, for sorting and searching entities spatially
 */

#ifndef MOAB_ADAPTIVE_KD_TREE_HPP
#define MOAB_ADAPTIVE_KD_TREE_HPP

#include "moab/Types.hpp"

#include <string>
#include <vector>
#include <math.h>

namespace moab {

class AdaptiveKDTreeIter;
class Interface;
class Range;

class AdaptiveKDTree
{
private:

  Interface* mbInstance;
  Tag planeTag, axisTag, rootTag;
  unsigned meshSetFlags;
  bool cleanUpTrees;
  EntityHandle myRoot;
  
public:

  AdaptiveKDTree( Interface* iface, 
                    const char* tagname = 0,
                    unsigned meshset_creation_flags = MESHSET_SET );

  AdaptiveKDTree( Interface* iface, 
                    bool destroy_created_trees,
                    const char* tagname = 0,
                    unsigned meshset_creation_flags = MESHSET_SET );

  ~AdaptiveKDTree();

  //! Enumeriate split plane directions
  enum Axis { X = 0, Y = 1, Z = 2 };
  
  //! Split plane 
  struct Plane {
    double coord;  //!< Location of plane as coordinate on normal axis
    int norm; //!< The principal axis that is the normal of the plane;
    
      /** return true if point is below/to the left of the split plane */
    bool left_side( const double point[3] ) {
      return point[norm] < coord;
    }
      /** return true if point is above/to the right of the split plane */
    bool right_side( const double point[3] ) {
      return point[norm] > coord;
    }
      /** return distance from point to plane */
    double distance( const double point[3] ) const {
      return fabs(point[norm] - coord);
    }
  };
  
  //! Get split plane for tree node
  ErrorCode get_split_plane( EntityHandle node, Plane& plane );
  
  //! Set split plane for tree node
  ErrorCode set_split_plane( EntityHandle node, const Plane& plane );
  
  //! Get bounding box for entire tree
  ErrorCode get_tree_box( EntityHandle root_node,
                            double box_min_out[3], 
                            double box_max_out[3] );
  
  //! Set bounding box for entire tree
  ErrorCode set_tree_box( EntityHandle root_node,
                            const double box_min[3], 
                            const double box_max[3] );
  
  //! Create tree root node
  ErrorCode create_tree( const double box_min[3],
                           const double box_max[3],
                           EntityHandle& root_handle );

  //! Find all tree roots
  ErrorCode find_all_trees( Range& results );

  //! Destroy a tree
  ErrorCode delete_tree( EntityHandle root_handle );

  Interface* moab() { return mbInstance; }

  //! Get iterator for tree
  ErrorCode get_tree_iterator( EntityHandle tree_root,
                                 AdaptiveKDTreeIter& result );
  
  //! Get iterator at right-most ('last') leaf.
  ErrorCode get_last_iterator( EntityHandle tree_root,
                                 AdaptiveKDTreeIter& result );

  //! Get iterator for tree or subtree
  ErrorCode get_sub_tree_iterator( EntityHandle tree_root,
                                     const double box_min[3], 
                                     const double box_max[3],
                                     AdaptiveKDTreeIter& result );

  //! Split leaf of tree
  //! Updates iterator location to point to first new leaf node.
  ErrorCode split_leaf( AdaptiveKDTreeIter& leaf, Plane plane );

  //! Split leaf of tree
  //! Updates iterator location to point to first new leaf node.
  ErrorCode split_leaf( AdaptiveKDTreeIter& leaf, 
                          Plane plane,
                          EntityHandle& left_child,
                          EntityHandle& right_child );
  //! Split leaf of tree
  //! Updates iterator location to point to first new leaf node.
  ErrorCode split_leaf( AdaptiveKDTreeIter& leaf, 
                          Plane plane,
                          const Range& left_entities,
                          const Range& right_entities );

  //! Split leaf of tree
  //! Updates iterator location to point to first new leaf node.
  ErrorCode split_leaf( AdaptiveKDTreeIter& leaf, 
                          Plane plane,
                          const std::vector<EntityHandle>& left_entities,
                          const std::vector<EntityHandle>& right_entities );
  
  //! Merge the leaf pointed to by the current iterator with it's
  //! sibling.  If the sibling is not a leaf, multiple merges may
  //! be done.
  ErrorCode merge_leaf( AdaptiveKDTreeIter& iter );
  
    //! methods for selecting candidate split planes
  enum CandidatePlaneSet {
    //! Candidiate planes at evenly spaced intervals 
    SUBDIVISION,
    //! Like SUBDIVISION, except snap to closest vertex coordinate
    SUBDIVISION_SNAP,
    //! Median vertex coodinate values
    VERTEX_MEDIAN,
    //! Random sampling of vertex coordinate values
    VERTEX_SAMPLE
  };
  
    //! Settings used for tree construction
  struct Settings {
    Settings(); //!< initialize to defaults
    unsigned maxEntPerLeaf; //!< split leafs with more entities than this
    unsigned maxTreeDepth;  //!< limit on the depth of the tree
    unsigned candidateSplitsPerDir; //!< number of cadiditate split planes to consider in each axial direction
    CandidatePlaneSet candidatePlaneSet;
    double minBoxWidth; //!< Tolerance
  };
  
  //! Build a tree
  ErrorCode build_tree( const Range& entities,
                          EntityHandle& root_set_out,
                          const Settings* settings = 0 );

  ErrorCode depth( EntityHandle root,
                     unsigned int& min_depth,
                     unsigned int& max_depth );

    //! get some information about the tree
  ErrorCode get_info(EntityHandle root,
                       double min[3], double max[3], 
                       unsigned int &max_dep);
  
  //! Find triangle closest to input position. 
  //!\param from_coords  The input position to test against
  //!\param closest_point_out  The closest point on the set of triangles in the tree
  //!\param triangle_out The triangle closest to the input position
  ErrorCode closest_triangle( EntityHandle tree_root,
                                const double from_coords[3],
                                double closest_point_out[3],
                                EntityHandle& triangle_out );

  ErrorCode sphere_intersect_triangles( EntityHandle tree_root,
                                          const double center[3],
                                          double radius,
                                          std::vector<EntityHandle>& triangles );

  ErrorCode ray_intersect_triangles( EntityHandle tree_root,
                                       const double tolerance,
                                       const double ray_unit_dir[3],
                                       const double ray_base_pt[3],
                                       std::vector<EntityHandle>& triangles_out,
                                       std::vector<double>& distance_out,
                                       int result_count_limit = 0,
                                       double distance_limit = -1.0);

  //! Get leaf contianing input position.
  //!
  //! Does not take into account global bounding box of tree.
  //! - Therefore there is always one leaf containing the point.
  //! - If caller wants to account for global bounding box, then
  //!   caller can test against that box and not call this method
  //!   at all if the point is outside the box, as there is no leaf
  //!   containing the point in that case.
  ErrorCode leaf_containing_point( EntityHandle tree_root,
                                   const double point[3],
                                   EntityHandle& leaf_out);

  //! Get iterator at leaf containing input position.
  //! 
  //! Returns MB_ENTITY_NOT_FOUND if point is not within
  //! bounding box of tree.
  ErrorCode leaf_containing_point( EntityHandle tree_root,
                                     const double xyz[3],
                                   AdaptiveKDTreeIter& result);

  //! Find all leaves within a given distance from point in space.
  //! If dists_out input non-NULL, also returns distances from each leaf; if
  //! point i is inside leaf, 0 is given as dists_out[i]
  ErrorCode leaves_within_distance( EntityHandle tree_root,
                                      const double from_point[3],
                                      const double distance,
                                    std::vector<EntityHandle>& leaves_out,
                                    std::vector<double> *dists_out = NULL);

  //! Calculate bounding box for entities.
  ErrorCode bounding_box( const Range& entities,
                            double box_min_out[3],
                            double box_max_out[3] );

private:
  
  void init( const char* tagname );
  
  /**\brief find a triangle near the input point */
  ErrorCode find_close_triangle( EntityHandle root,
                                   const double from_point[3],
                                   double pt[3],
                                   EntityHandle& triangle );
};
                    

//! Iterate over leaves of an adapative kD-tree
class AdaptiveKDTreeIter
{
public:

  enum Direction { LEFT = 0, RIGHT = 1 };

private:
  
  struct StackObj {
    StackObj( EntityHandle e, double c ) : entity(e), coord(c) {}
    StackObj() {}
    EntityHandle entity; //!< handle for tree node
    double coord;          //!< box coordinate of parent
  };
  
  enum { BMIN = 0, BMAX = 1 };  //!< indices into mBox and child list
  
  double mBox[2][3];                //!< min and max corners of bounding box
  AdaptiveKDTree* treeTool;       //!< tool for tree
  std::vector<StackObj> mStack;     //!< stack storing path through tree
  mutable std::vector<EntityHandle> childVect; //!< tempory storage of child handles
  
  //! Descend tree to left most leaf from current position
  //! No-op if at leaf.
  ErrorCode step_to_first_leaf( Direction direction );

  friend class AdaptiveKDTree;
public:

  AdaptiveKDTreeIter() : treeTool(0), childVect(2) {}
  
  ErrorCode initialize( AdaptiveKDTree* tool,
                          EntityHandle root,
                          const double box_min[3],
                          const double box_max[3],
                          Direction direction );

  AdaptiveKDTree* tool() const
    { return treeTool; }

    //! Get handle for current leaf
  EntityHandle handle() const
    { return mStack.back().entity; }
  
    //! Get min corner of axis-aligned box for current leaf
  const double* box_min() const 
    { return mBox[BMIN]; }
    
    //! Get max corner of axis-aligned box for current leaf
  const double* box_max() const 
    { return mBox[BMAX]; }
  
  double volume() const
    { return (mBox[BMAX][0] - mBox[BMIN][0]) * 
             (mBox[BMAX][1] - mBox[BMIN][1]) * 
             (mBox[BMAX][2] - mBox[BMIN][2]); }
  
    //! test if a plane intersects the leaf box
  bool intersects( const AdaptiveKDTree::Plane& plane ) const
    { return mBox[BMIN][plane.norm] <= plane.coord &&
             mBox[BMAX][plane.norm] >= plane.coord; }
  
    //! Get depth in tree. root is at depth of 1.
  unsigned depth() const
    { return mStack.size(); }
  
  //! Advance the iterator either left or right in the tree
  //! Note:  stepping past the end of the tree will invalidate
  //!        the iterator.  It will *not* be work step the
  //!        other direction.
  ErrorCode step( Direction direction );

    //! Advance to next leaf
    //! Returns MB_ENTITY_NOT_FOUND if at end.
    //! Note: steping past the end of the tree will invalidate
    //!       the iterator. Calling back() will not work.
  ErrorCode step() { return step(RIGHT); }

    //! Move back to previous leaf
    //! Returns MB_ENTITY_NOT_FOUND if at beginning.
    //! Note: steping past the start of the tree will invalidate
    //!       the iterator. Calling step() will not work.
  ErrorCode back() { return step(LEFT); }
  
  
    //! Return the side of the box bounding this tree node
    //! that is shared with the immediately adjacent sibling
    //! (the tree node that shares a common parent node with
    //! this node in the binary tree.)
    //!
    //!\param axis_out The principal axis orthogonal to the side of the box
    //!\param neg_out  true if the side of the box is toward the decreasing
    //!                direction of the principal axis indicated by axis_out,
    //!                false if it is toward the increasing direction.
    //!\return MB_ENTITY_NOT FOUND if root node.
    //!        MB_FAILURE if internal error.
    //!        MB_SUCCESS otherwise.
  ErrorCode sibling_side( AdaptiveKDTree::Axis& axis_out, bool& neg_out ) const;

    //! Get adjacent leaf nodes on side indicated by norm and neg.
    //!
    //! E.g. if norm == X and neg == true, then get neighbor(s)
    //! adjacent to the side of the box contained in the plane
    //! with normal to the X axis and with the x coordinate equal 
    //! to the minimum x of the bounding box.
    //!
    //! E.g. if norm == Y and neg == false, then get neighbor(s)
    //! adjacent to the side of the box with y = maximum y of bounding box.
    //!
    //!\param norm  Normal vector for box side (X, Y, or Z)
    //!\param neg   Which of two planes with norm (true->smaller coord, 
    //!             false->larget coord)
    //!\param results List to which to append results.  This function does
    //!             *not* clear existing values in list.
    //!\param epsilon Tolerance on overlap.  A positive value E will
    //!              result in nodes that are separated by as much as E
    //!              to be considered touching.  A negative value -E will
    //!              cause leaves that do not overlap by at least E to be
    //!              considered non-overlapping.  Amongst other things, 
    //!              this value can be used to control whether or not
    //!              leaves adjacent at only their edges or corners are
    //!              returned.
  ErrorCode get_neighbors( AdaptiveKDTree::Axis norm, bool neg,
                             std::vector<AdaptiveKDTreeIter>& results,
                             double epsilon = 0.0 ) const;
  
    //! Get split plane that separates this node from its immediate sibling.
  ErrorCode get_parent_split_plane( AdaptiveKDTree::Plane& plane ) const;
  
    //! Return true if thos node and the passed node share the
    //! same immediate parent.
  bool is_sibling( const AdaptiveKDTreeIter& other_leaf ) const;
  
    //! Return true if thos node and the passed node share the
    //! same immediate parent.
  bool is_sibling( EntityHandle other_leaf ) const;
  
    //! Returns true if calling step() will advance to the
    //! immediate sibling of the current node.  Returns false
    //! if current node is root or back() will move to the 
    //! immediate sibling.
  bool sibling_is_forward( ) const;
  
    //! Find range of overlap between ray and leaf.
    //!
    //!\param ray_point Coordinates of start point of ray
    //!\param ray_vect  Directionion vector for ray such that
    //!                 the ray is defined by r(t) = ray_point + t * ray_vect
    //!                 for t > 0.
    //!\param t_enter   Output: if return value is true, this value
    //!                 is the parameter location along the ray at which
    //!                 the ray entered the leaf.  If return value is false,
    //!                 then this value is undefined.
    //!\param t_exit    Output: if return value is true, this value
    //!                 is the parameter location along the ray at which
    //!                 the ray exited the leaf.  If return value is false,
    //!                 then this value is undefined.
    //!\return true if ray intersects leaf, false otherwise.
  bool intersect_ray( const double ray_point[3],
                      const double ray_vect[3],
                      double& t_enter, double& t_exit ) const;
};

} // namespace moab 

#endif
