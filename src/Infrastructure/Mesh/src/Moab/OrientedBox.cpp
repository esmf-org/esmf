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

/* 
 * The algorithms for the calculation of the oriented box from a
 * set of points or a set of cells was copied from the implementation
 " in the "Visualization Toolkit".  J.K. - 2006-07-19
 *
 * Program:   Visualization Toolkit
 * Module:    $RCSfile$
 *
 * Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 * All rights reserved.
 * See Copyright.txt or http://www.kitware.com/Copyright.htm for details.
 */

/**\file OrientedBox.cpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2006-07-18
 */

#include "moab/Interface.hpp"
#include "moab/CN.hpp"
#include "moab/OrientedBox.hpp"
#include "moab/Range.hpp"
#include "moab/Matrix3.hpp"
#include "moab/Util.hpp"
#include <ostream>
#include <assert.h>
#include <limits>

namespace moab {

std::ostream& operator<<( std::ostream& s, const OrientedBox& b )
{
  return s << b.center 
           << " + " 
           << b.axis[0] 
#if MB_ORIENTED_BOX_UNIT_VECTORS
           << ":" << b.length[0] 
#endif
           << " x " 
           << b.axis[1] 
#if MB_ORIENTED_BOX_UNIT_VECTORS
           << ":" << b.length[1] 
#endif
           << " x " 
           << b.axis[2]
#if MB_ORIENTED_BOX_UNIT_VECTORS
           << ":" << b.length[2] 
#endif
            ;
}

/**\brief Find closest point on line
 *
 * Find the point on the line for which a line trough the 
 * input point \a p and the result position is orthogonal to
 * the input line.
 * \param p  The point for which to find the perpendicular
 * \param b  A point on the line
 * \param m  The direction of the line
 * \return   The location on the line specified as 't' in the
 *           formula t * m + b
 */
static double point_perp( const CartVect& p,   // closest to this point
                          const CartVect& b,   // point on line
                          const CartVect& m )  // line direction
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  double t = (m % (p - b));
#else
  double t = (m % (p - b)) / (m % m);
#endif
  return Util::is_finite(t) ? t : 0.0;
}

OrientedBox::OrientedBox( const CartVect axes[3], const CartVect& mid )
  : center(mid)
{
    // re-order axes by length
  CartVect len( axes[0].length(), axes[1].length(), axes[2].length() );
  axis[0] = axes[0];
  axis[1] = axes[1];
  axis[2] = axes[2];
  
  if (len[2] < len[1])
  {
    if (len[2] < len[0]) {
      std::swap( len[0], len[2] );
      std::swap( axis[0], axis[2] );
    }
  }
  else if (len[1] < len[0]) {
    std::swap( len[0], len[1] );
    std::swap( axis[0], axis[1] );
  }
  if (len[1] > len[2]) {
    std::swap( len[1], len[2] );
    std::swap( axis[1], axis[2] );
  }
  
#if MB_ORIENTED_BOX_UNIT_VECTORS
  this->length = len;
  if (len[0] > 0.0)
    axis[0] /= len[0];
  if (len[1] > 0.0)
    axis[1] /= len[1];
  if (len[2] > 0.0)
    axis[2] /= len[2];
#endif

#if MB_ORIENTED_BOX_OUTER_RADIUS
  radius = len.length();
#endif
}

ErrorCode OrientedBox::tag_handle( Tag& handle_out,
                                       Interface* instance,
                                       const char* name)
{
    // We're going to assume this when mapping the OrientedBox
    // to tag data, so assert it.  
#if MB_ORIENTED_BOX_OUTER_RADIUS
  const int rad_size = 1;
#else
  const int rad_size = 0;
#endif
#if MB_ORIENTED_BOX_UNIT_VECTORS
  const int SIZE = rad_size + 15;
#else
  const int SIZE = rad_size + 12;
#endif
  assert( sizeof(OrientedBox) == SIZE*sizeof(double) );
  
  return instance->tag_get_handle( name, SIZE, MB_TYPE_DOUBLE,
                                   handle_out, MB_TAG_DENSE|MB_TAG_CREAT );
}

/**\brief Common code for box calculation
 *
 * Given the orientation of the box and an approximate center,
 * calculate the exact center and extents of the box.
 * 
 *\param result.center  As input, the approximate center of the box.
 *                      As output, the exact center of the box.
 *\param result.axes    As input, directions of principal axes corresponding
 *                      to the orientation of the box.  Axes are assumed to
 *                      be unit-length on input.  Output will include extents
 *                      of box.
 *\param points  The set of points the box should contain.
 */
static ErrorCode box_from_axes( OrientedBox& result,
                                  Interface* instance,
                                  const Range& points )
{ 
  ErrorCode rval;
  
    // project points onto axes to get box extents
  CartVect min(std::numeric_limits<double>::max()), 
             max(-std::numeric_limits<double>::max());
  for (Range::iterator i = points.begin(); i != points.end(); ++i)
  {
    CartVect coords;
    rval = instance->get_coords( &*i, 1, coords.array() );
    if (MB_SUCCESS != rval)
      return rval;
    
    for (int d = 0; d < 3; ++d)
    {
      double t = point_perp( coords, result.center, result.axis[d] );
      if (t < min[d])
        min[d] = t;
      if (t > max[d])
        max[d] = t;
    }
  }
  
    // We now have a box defined by three orthogonal line segments
    // that intersect at the center of the box.  Each line segment
    // is defined as result.center + t * result.axis[i], where the
    // range of t is [min[i], max[i]].
  
    // Calculate new center
  CartVect mid = 0.5 * (min + max);
  result.center += mid[0] * result.axis[0] +
                   mid[1] * result.axis[1] +
                   mid[2] * result.axis[2];
  
    // reorder axes by length
  CartVect range = 0.5 * (max - min);
  if (range[2] < range[1])
  {
    if (range[2] < range[0]) {
      std::swap( range[0], range[2] );
      std::swap( result.axis[0], result.axis[2] );
    }
  }
  else if (range[1] < range[0]) {
    std::swap( range[0], range[1] );
    std::swap( result.axis[0], result.axis[1] );
  }
  if (range[1] > range[2]) {
    std::swap( range[1], range[2] );
    std::swap( result.axis[1], result.axis[2] );
  }

    // scale axis to encompass all points, divide in half
#if MB_ORIENTED_BOX_UNIT_VECTORS
  result.length = range;
#else
  result.axis[0] *= range[0];
  result.axis[1] *= range[1];
  result.axis[2] *= range[2];
#endif

#if MB_ORIENTED_BOX_OUTER_RADIUS
  result.radius = range.length();
#endif

  return MB_SUCCESS;
}


ErrorCode OrientedBox::compute_from_vertices( OrientedBox& result,
                                                  Interface* instance,
                                                  const Range& vertices )
{
  const Range::iterator begin = vertices.lower_bound( MBVERTEX );
  const Range::iterator end = vertices.upper_bound( MBVERTEX );
  size_t count = 0;
  
    // compute mean
  CartVect v;
  result.center = CartVect( 0, 0, 0 );
  for (Range::iterator i = begin; i != end; ++i)
  {
    ErrorCode rval = instance->get_coords( &*i, 1, v.array() );
    if (MB_SUCCESS != rval)
      return rval;
    result.center += v;
    ++count;
  }
  result.center /= count;
  
    // compute covariance matrix
  Matrix3 a( 0.0 );
  for (Range::iterator i = begin; i != end; ++i)
  {
    ErrorCode rval = instance->get_coords( &*i, 1, v.array() );
    if (MB_SUCCESS != rval)
      return rval;
  
    v -= result.center;
    a += outer_product( v, v );
  }
  a /= count;

    // Get axes (Eigenvectors) from covariance matrix
  double lambda[3];
  moab::Matrix::EigenDecomp( a, lambda, result.axis );
  
    // Calculate center and extents of box given orientation defined by axes
  return box_from_axes( result, instance, vertices );
}

ErrorCode OrientedBox::covariance_data_from_tris( CovarienceData& result,
                                                 Interface* instance,
                                                 const Range& elements )
{
  ErrorCode rval;
  const Range::iterator begin = elements.lower_bound( CN::TypeDimensionMap[2].first );
  const Range::iterator end = elements.lower_bound( CN::TypeDimensionMap[3].first );
  
    // compute mean and moments
  result.matrix = Matrix3(0.0);
  result.center = CartVect(0.0);
  result.area = 0.0;
  for (Range::iterator i = begin; i != end; ++i)
  {
    const EntityHandle* conn = NULL;
    int conn_len = 0;
    rval = instance->get_connectivity( *i, conn, conn_len );
    if (MB_SUCCESS != rval)
      return rval;
    
      // for each triangle in the 2-D cell
    for (int j = 2; j < conn_len; ++j)
    {
      EntityHandle vertices[3] = { conn[0], conn[j-1], conn[j] };
      CartVect coords[3];
      rval = instance->get_coords( vertices, 3, coords[0].array() );
      if (MB_SUCCESS != rval)
        return rval;
      
        // edge vectors
      const CartVect edge0 = coords[1] - coords[0];
      const CartVect edge1 = coords[2] - coords[0];
      const CartVect centroid = (coords[0] + coords[1] + coords[2]) / 3;
      const double tri_area2 = (edge0 * edge1).length();
      result.area += tri_area2;
      result.center += tri_area2 * centroid;
      
      result.matrix += tri_area2 * (9 * outer_product( centroid,  centroid  ) +
                                    outer_product( coords[0], coords[0] ) +
                                    outer_product( coords[1], coords[1] ) +
                                    outer_product( coords[2], coords[2] ));
    } // for each triangle
  } // for each element

  return MB_SUCCESS;
}


ErrorCode OrientedBox::compute_from_2d_cells( OrientedBox& result,
                                                  Interface* instance,
                                                  const Range& elements )
{
    // Get orientation data from elements
  CovarienceData data;
  ErrorCode rval = covariance_data_from_tris( data, instance, elements );
  if (MB_SUCCESS != rval)
    return rval;
  
    // get vertices from elements
  Range points;
  rval = instance->get_adjacencies( elements, 0, false, points, Interface::UNION );
  if (MB_SUCCESS != rval)
    return rval;
    
    // Calculate box given points and orientation data
  return compute_from_covariance_data( result, instance, data, points );
}

ErrorCode OrientedBox::compute_from_covariance_data(
                                                OrientedBox& result,
                                                Interface* instance,
                                                CovarienceData& data,
                                                const Range& vertices )
{
  if (data.area <= 0.0) {
    CartVect axis[3] = { CartVect(0.), CartVect(0.), CartVect(0.) };
    result = OrientedBox( axis, CartVect(0.) );
    return MB_SUCCESS;
  }

    // get center from sum
  result.center = data.center / data.area;

    // get covariance matrix from moments
  data.matrix /= 12 * data.area;
  data.matrix -= outer_product( result.center, result.center );

    // get axes (Eigenvectors) from covariance matrix
  double lamda[3];
  moab::Matrix::EigenDecomp( data.matrix, lamda, result.axis );

    // We now have only the axes.  Calculate proper center
    // and extents for enclosed points.
  return box_from_axes( result, instance, vertices );
}      

bool OrientedBox::contained( const CartVect& point, double tol ) const
{
  CartVect from_center = point - center;
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return fabs(from_center % axis[0]) - length[0] <= tol &&
         fabs(from_center % axis[1]) - length[1] <= tol &&
         fabs(from_center % axis[2]) - length[2] <= tol ;
#else
  for (int i = 0; i < 3; ++i) {
    double length = axis[i].length();
    if (fabs(from_center % axis[i]) - length*length > length*tol)
      return false;
  }
  return true;
#endif
}

ErrorCode OrientedBox::compute_from_covariance_data( OrientedBox& result,
                                                Interface* moab_instance,
                                                const CovarienceData* data,
                                                unsigned data_length,
                                                const Range& vertices )
{
    // Sum input CovarienceData structures
  CovarienceData data_sum( Matrix3(0.0), CartVect(0.0), 0.0 );
  for (const CovarienceData* const end = data+data_length; data != end; ++data) {
    data_sum.matrix += data->matrix;
    data_sum.center += data->center;
    data_sum.area += data->area;
  }
    // Compute box from sum of structs
  return compute_from_covariance_data( result, moab_instance, data_sum, vertices );
}



//bool OrientedBox::contained( const OrientedBox& box, double tol ) const
//{
//  for (int i = -1; i < 2; i += 2) 
//  {
//    for (int j = -1; j < 2; j += 2) 
//    {
//      for (int k = -1; k < 2; k += 2) 
//      {
//        CartVect corner( center );
//#ifdef MB_ORIENTED_BOX_UNIT_VECTORS
//        corner += i * box.length[0] * box.axis[0];
//        corner += j * box.length[1] * box.axis[1];
//        corner += k * box.length[2] * box.axis[2];
//#else
//        corner += i * box.axis[0];
//        corner += j * box.axis[1];
//        corner += k * box.axis[2];
//#endif
//        if (!contained( corner, tol ))
//          return false;
//      }
//    }
//  }
//  return true;
//}


/* This is a helper function to check limits on ray length, turning the box-ray 
 * intersection test into a box-segment intersection test. Use this to test the
 * limits against one side (plane) of the box. The side of the box (plane) is
 * normal to an axis.
 *
 *   normal_par_pos  Coordinate of particle's position along axis normal to side of box
 *   normal_par_dir  Coordinate of particle's direction along axis normal to side of box
 *   half_extent     Distance between center of box and side of box
 *   nonneg_ray_len  Maximum ray length in positive direction (in front of origin)
 *   neg_ray_len     Maximum ray length in negative direction (behind origin)
 *   return          true if intersection with plane occurs within distance limits
 *
 * ray equation:   intersection = origin + dist*direction
 * plane equation: intersection.plane_normal = half_extent
 * 
 * Assume plane_normal and direction are unit vectors. Combine equations.
 *
 *     (origin + dist*direction).plane_normal = half_extent
 *     origin.plane_normal + dist*direction.plane_normal = half_extent
 *     dist = (half_extent - origin.plane_normal)/(direction.plane_normal)
 *
 * Although this solves for distance, avoid floating point division.
 *
 *     dist*direction.plane_normal = half_extent - origin.plane_normal
 *
 * Use inequalities to test dist against ray length limits. Be aware that 
 * inequalities change due to sign of direction.plane_normal.
 */ 
inline bool check_ray_limits(const double  normal_par_pos,
                             const double  normal_par_dir,
                             const double  half_extent,
                             const double* nonneg_ray_len,
                             const double* neg_ray_len ) {

  const double extent_pos_diff = half_extent - normal_par_pos;

  // limit in positive direction
  if(nonneg_ray_len) { // should be 0 <= t <= nonneg_ray_len
    assert(0 <= *nonneg_ray_len);
    if       (normal_par_dir>0) { // if/else if needed for pos/neg divisor
      if(*nonneg_ray_len*normal_par_dir>=extent_pos_diff && extent_pos_diff>=0) return true;
    } else if(normal_par_dir<0) {
      if(*nonneg_ray_len*normal_par_dir<=extent_pos_diff && extent_pos_diff<=0) return true;
    }
  } else {            // should be 0 <= t
    if       (normal_par_dir>0) { // if/else if needed for pos/neg divisor
      if(extent_pos_diff>=0) return true;
    } else if(normal_par_dir<0) {
      if(extent_pos_diff<=0) return true;
    }
  }

  // limit in negative direction
  if(neg_ray_len) {   // should be neg_ray_len <= t < 0
    assert(0 >= *neg_ray_len);
    if       (normal_par_dir>0) { // if/else if needed for pos/neg divisor
      if(*neg_ray_len*normal_par_dir<=extent_pos_diff && extent_pos_diff<0) return true;
    } else if(normal_par_dir<0) {
      if(*neg_ray_len*normal_par_dir>=extent_pos_diff && extent_pos_diff>0) return true;
    }
  }

  return false;
}

/* This implementation copied from cgmMC (overlap.C).
 * Original author:  Tim Tautges?
 */
bool OrientedBox::intersect_ray( const CartVect& ray_origin,
                                 const CartVect& ray_direction,
				 const double    reps,
				 const double*   nonneg_ray_len,
                                 const double*   neg_ray_len ) const
{
  // test distance from box center to line
  const CartVect cx       = center - ray_origin;
  const double dist_s     = cx % ray_direction;
  const double dist_sq    = cx % cx - (dist_s*dist_s);
  const double max_diagsq = outer_radius_squared(reps);
  
  // For the largest sphere, no intersections exist if discriminant is negative.
  // Geometrically, if distance from box center to line is greater than the 
  // longest diagonal, there is no intersection.
  // manipulate the discriminant: 0 > dist_s*dist_s - cx%cx + max_diagsq
  if(dist_sq > max_diagsq) return false;

  // If the closest possible intersection must be closer than nonneg_ray_len. Be 
  // careful with absolute value, squaring distances, and subtracting squared
  // distances.
  if (nonneg_ray_len) {
    assert(0<=*nonneg_ray_len);
    double max_len;
    if(neg_ray_len) {
      assert(0>=*neg_ray_len);
      max_len = std::max(*nonneg_ray_len,-(*neg_ray_len));
    } else {
      max_len = *nonneg_ray_len;
    }
    const double temp = fabs(dist_s) - max_len;
    if(0.0<temp && temp*temp>max_diagsq) return false;
  } 

  // if smaller than shortest diagonal, we do hit
  if (dist_sq < inner_radius_squared(reps)) {
    // nonnegative direction
    if(dist_s>=0.0 ) {
      if(nonneg_ray_len) {
        if(*nonneg_ray_len>dist_s) return true;
      } else {
        return true;
      }
    // negative direction 
    } else {
      if(neg_ray_len && *neg_ray_len<dist_s) return true;
    }
  }
  
    // get transpose of axes
    // Note: if axes were stored as a matrix, could skip
    // transpose and just switch order of operands in
    // matrix-vector multiplies below. - J.K.
  //Matrix3 B( axis[0][0], axis[1][0], axis[2][0],
  //             axis[0][1], axis[1][1], axis[2][1],
  //             axis[0][2], axis[1][2], axis[2][2] );
  Matrix3 B( axis[0][0], axis[0][1], axis[0][2],
               axis[1][0], axis[1][1], axis[1][2],
               axis[2][0], axis[2][1], axis[2][2] );
  //CartVect T = B * -center;
  
    // transform ray to box coordintae system
  //CartVect par_pos = T + B * b;
  CartVect par_pos = B * (ray_origin - center);
  CartVect par_dir = B * ray_direction;

  // Fast Rejection Test: Ray will not intersect if it is going away from the box.
  // This will not work for rays with neg_ray_len. length[0] is half of box width 
  // along axis[0].
  const double half_x = length[0] + reps;
  const double half_y = length[1] + reps;
  const double half_z = length[2] + reps;
  if(!neg_ray_len) {
    if ((par_pos[0] >  half_x && par_dir[0] >= 0) ||
	(par_pos[0] < -half_x && par_dir[0] <= 0))
      return false;
  
    if ((par_pos[1] >  half_y && par_dir[1] >= 0) ||
	(par_pos[1] < -half_y && par_dir[1] <= 0))
      return false;
    
    if ((par_pos[2] >  half_z && par_dir[2] >= 0) ||
	(par_pos[2] < -half_z && par_dir[2] <= 0))
      return false;
  }

  // test if ray_origin is inside box
  if (par_pos[0] <= half_x && par_pos[0] >= -half_x &&
      par_pos[1] <= half_y && par_pos[1] >= -half_y &&
      par_pos[2] <= half_z && par_pos[2] >= -half_z)
    return true;

    //test two xy plane
  if (fabs(par_dir[0] * (half_z - par_pos[2]) + par_dir[2] * par_pos[0]) 
        <= fabs(par_dir[2] * half_x) &&  // test against x extents using z
      fabs(par_dir[1] * (half_z - par_pos[2]) + par_dir[2] * par_pos[1]) 
        <= fabs(par_dir[2] * half_y) &&  // test against y extents using z
      check_ray_limits( par_pos[2], par_dir[2], half_z, nonneg_ray_len, neg_ray_len ) )
    return true;
  if (fabs(par_dir[0] * (-half_z - par_pos[2]) + par_dir[2] * par_pos[0]) 
        <= fabs(par_dir[2] * half_x) && 
      fabs(par_dir[1] * (-half_z - par_pos[2]) + par_dir[2] * par_pos[1]) 
        <= fabs(par_dir[2] * half_y) &&
      check_ray_limits( par_pos[2], par_dir[2], -half_z, nonneg_ray_len, neg_ray_len ) )
    return true;

    //test two xz plane
  if (fabs(par_dir[0] * (half_y - par_pos[1]) + par_dir[1] * par_pos[0]) 
        <= fabs(par_dir[1] * half_x) && 
      fabs(par_dir[2] * (half_y - par_pos[1]) + par_dir[1] * par_pos[2]) 
        <= fabs(par_dir[1] * half_z) &&
      check_ray_limits( par_pos[1], par_dir[1], half_y, nonneg_ray_len, neg_ray_len ) )
    return true;
  if (fabs(par_dir[0] * (-half_y - par_pos[1]) + par_dir[1] * par_pos[0]) 
        <= fabs(par_dir[1] * half_x) && 
      fabs(par_dir[2] * (-half_y - par_pos[1]) + par_dir[1] * par_pos[2])
        <= fabs(par_dir[1] * half_z) &&
      check_ray_limits( par_pos[1], par_dir[1], -half_y, nonneg_ray_len, neg_ray_len ) )
    return true;

    //test two yz plane
  if (fabs(par_dir[1] * (half_x - par_pos[0]) + par_dir[0] * par_pos[1]) 
        <= fabs(par_dir[0] * half_y) &&
      fabs(par_dir[2] * (half_x - par_pos[0]) + par_dir[0] * par_pos[2]) 
        <= fabs(par_dir[0] * half_z) &&
      check_ray_limits( par_pos[0], par_dir[0], half_x, nonneg_ray_len, neg_ray_len ) )
    return true;
  if (fabs(par_dir[1] * (-half_x - par_pos[0]) + par_dir[0] * par_pos[1])
        <= fabs(par_dir[0] * half_y) &&
      fabs(par_dir[2] * (-half_x - par_pos[0]) + par_dir[0] * par_pos[2]) 
        <= fabs(par_dir[0] * half_z) &&
      check_ray_limits( par_pos[0], par_dir[0], -half_x, nonneg_ray_len, neg_ray_len ) )
    return true;

  return false;
}

ErrorCode OrientedBox::make_hex( EntityHandle& hex, Interface* instance )
{
  ErrorCode rval;
  int signs[8][3] = { { -1, -1, -1 },
                      {  1, -1, -1 },
                      {  1,  1, -1 },
                      { -1,  1, -1 },
                      { -1, -1,  1 },
                      {  1, -1,  1 },
                      {  1,  1,  1 },
                      { -1,  1,  1 } };
                      
  std::vector<EntityHandle> vertices;
  for (int i = 0; i < 8; ++i)
  {
    CartVect coords(center);
    for (int j = 0; j < 3; ++j){
#if MB_ORIENTED_BOX_UNIT_VECTORS
      coords += signs[i][j] * (axis[j]*length[j]);
#else
      coords += signs[i][j] * axis[j];
#endif
    }
    EntityHandle handle;
    rval = instance->create_vertex( coords.array(), handle );
    if (MB_SUCCESS != rval) {
      instance->delete_entities( &vertices[0], vertices.size() );
      return rval;
    }
    vertices.push_back( handle );
  }
  
  rval = instance->create_element( MBHEX, &vertices[0], vertices.size(), hex );
  if (MB_SUCCESS != rval) {
    instance->delete_entities( &vertices[0], vertices.size() );
    return rval;
  }
  
  return MB_SUCCESS;
}
  
void OrientedBox::closest_location_in_box( 
                                    const CartVect& input_position,
                                    CartVect& output_position ) const
{
    // get coordinates on box axes
  const CartVect from_center = input_position - center;

#if MB_ORIENTED_BOX_UNIT_VECTORS
  CartVect local( from_center % axis[0],
                    from_center % axis[1],
                    from_center % axis[2] );

  for (int i = 0; i < 3; ++i) {
    if (local[i] < -length[i])
      local[i] = -length[i];
    else if (local[i] > length[i])
      local[i] =  length[i];
  }
#else
  CartVect local( (from_center % axis[0]) / (axis[0] % axis[0]),
                    (from_center % axis[1]) / (axis[1] % axis[1]),
                    (from_center % axis[2]) / (axis[2] % axis[2]) );

  for (int i = 0; i < 3; ++i) {
    if (local[i] < -1.0)
      local[i] = -1.0;
    else if (local[i] > 1.0)
      local[i] = 1.0;
  }
#endif

  output_position = center
                  + local[0] * axis[0] 
                  + local[1] * axis[1]
                  + local[2] * axis[2];
}
  
} // namespace moab
