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

#ifndef MB_ORIENTED_BOX_HPP
#define MB_ORIENTED_BOX_HPP

#include "moab/Forward.hpp"
#include "moab/CartVect.hpp"
#include "moab/Matrix3.hpp"

#include <iosfwd>

namespace moab {

#define MB_ORIENTED_BOX_UNIT_VECTORS 1
#define MB_ORIENTED_BOX_OUTER_RADIUS 1

class Range;


/**\brief Oriented bounding box
 */
class OrientedBox
{
public:
  CartVect center;  //!< Box center
  CartVect axis[3]; //!< Box axes, unit vectors sorted by extent of box along axis
#if MB_ORIENTED_BOX_UNIT_VECTORS
  CartVect length;  //!< distance from center to plane along each axis
#endif
#if MB_ORIENTED_BOX_OUTER_RADIUS
  double radius;      //!< outer radius (1/2 diagonal length) of box
#endif

  inline OrientedBox() : radius(0.0) {}

  OrientedBox( const CartVect axis[3], const CartVect& center );

  inline double inner_radius() const; //!< radius of inscribed sphere
  inline double outer_radius() const; //!< radius of circumscribed sphere
  inline double outer_radius_squared(const double reps) const; //!< square of (radius+at least epsilon) of circumsphere
  inline double inner_radius_squared(const double reps) const; //!< square of (radius-epsilon) of inscribed sphere
  inline double volume() const;               //!< volume of box
  inline CartVect dimensions() const;       //!< number of dimensions for which box is not flat
  inline double area() const;                 //!< largest side area
  inline CartVect scaled_axis( int index ) const; //!< get vector in direction of axis, from box center to face
  
  /** Test if point is contained in box */
  bool contained( const CartVect& point, double tolerance ) const;
  
  //bool contained( const OrientedBox& other, double tolerance ) const;
  
  /**\brief get tag handle for storing oriented box
   *
   * Get the handle for the tag with the specified name and
   * check that the tag is appropriate for storing instances
   * of OrientedBox.  The resulting tag may be used to store
   * instances of OrientedBox directly.
   *
   *\param handle_out  The TagHandle, passed back to caller
   *\param name        The tag name
   *\param create      If true, tag will be created if it does not exist
   */
  static ErrorCode tag_handle( Tag& handle_out,
                                 Interface* instance, 
                                 const char* name);

  /**\brief Calculate an oriented box from a set of vertices */
  static ErrorCode compute_from_vertices( OrientedBox& result,
                                            Interface* instance,
                                            const Range& vertices );
                                  
  /**\brief Calculate an oriented box from a set of 2D elements */
  static ErrorCode compute_from_2d_cells( OrientedBox& result,
                                            Interface* instance,
                                            const Range& elements );

    /** Structure to hold temporary accumulated triangle data for
     *  calculating box orientation.  See box_from_covariance_data
     *  to see how this is used to calculate the final covariance matrix
     *  and resulting box orientation.
     */
  struct CovarienceData {
    CovarienceData() : area(0.0) {}
    CovarienceData( const Matrix3& m, const CartVect& c, double a)
      : matrix(m), center(c), area(a) {}
    Matrix3 matrix;    //!< Running sum for covariance matrix
    CartVect center;   //!< Sum of triangle centroids weighted by 2*triangle area
    double area;         //!< 2x the sum of the triangle areas
  };
  
    /** Calculate a CovarienceData struct from a list of triangles */
  static ErrorCode covariance_data_from_tris( CovarienceData& result,
                                                Interface* moab_instance,
                                                const Range& elements );
  
    /** Calculate an OrientedBox given an array of CovarienceData and
     *  the list  of vertices the box is to bound.
     */
  static ErrorCode compute_from_covariance_data( OrientedBox& result,
                                          Interface* moab_instance,
                                          const CovarienceData* orient_array,
                                          unsigned orient_array_length,
                                          const Range& vertices );
  
    /** Test for intersection of a ray (or line segment) with this box.
     *  Ray length limits are used to optimize Monte Carlo particle tracking.
     *\param ray_start_point     The base point of the ray
     *\param ray_unit_direction  The direction of the ray (must be unit length)
     *\param distance_tolerance  Tolerance to use in intersection checks
     *\param nonnegative_ray_len Optional length of ray in forward direction
     *\param negative_ray_len    Optional length of ray in reverse direction
     */
  bool intersect_ray( const CartVect& ray_start_point,
                      const CartVect& ray_unit_direction,
                      const double    distance_tolerance,
                      const double*   nonnegatve_ray_len = 0,
                      const double*   negative_ray_len   = 0 ) const;
                      
    /**\brief Find closest position on/within box to input position.
     * 
     * Find the closest position in the solid box to the input position.
     * If the input position is on or within the box, then the output
     * position will be the same as the input position.  If the input
     * position is outside the box, the outside position will be the
     * closest point on the box boundary to the input position.
     */
  void closest_location_in_box( const CartVect& input_position,
                                CartVect& output_position ) const;
                      
    //! Construct a hexahedral element with the same shape as this box.
  ErrorCode make_hex( EntityHandle& hex, Interface* instance );
                                    
  
    /** Calculate an OrientedBox given a CovarienceData struct and
     *  the list of points the box is to bound.
     */
  static ErrorCode compute_from_covariance_data( OrientedBox& result,
                                          Interface* moab_instance,
                                          CovarienceData& orientation_data,
                                          const Range& vertices );
};

std::ostream& operator<<( std::ostream&, const OrientedBox& );

double OrientedBox::inner_radius() const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return length[0];
#else
  return axis[0].length();
#endif
}

double OrientedBox::outer_radius() const
{
#if MB_ORIENTED_BOX_OUTER_RADIUS
  return radius;
#elif MB_ORIENTED_BOX_UNIT_VECTORS
  return length.length();
#else
  return (axis[0] + axis[1] + axis[2]).length();
#endif
}

// Add at least epsilon to the radius, before squaring it.
double OrientedBox::outer_radius_squared(const double reps) const
{
#if MB_ORIENTED_BOX_OUTER_RADIUS
  return (radius+reps)*(radius+reps);
#elif MB_ORIENTED_BOX_UNIT_VECTORS
  CartVect tmp(length[0]+reps,length[1]+reps,length[2]+reps);
  return tmp % tmp;
#else
  CartVect half_diag = axis[0] + axis[1] + axis[2];
  half_diag += CartVect(reps,reps,reps);
  return half_diag % half_diag;
#endif
}

// Subtract epsilon from the length of the shortest axis, before squaring it.
double OrientedBox::inner_radius_squared(const double reps) const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return (length[0]-reps) * (length[0]-reps);
#else
  CartVect tmp = axis[0];
  tmp -= CartVect(reps,reps,reps);
  return (tmp % tmp);
#endif
}

double OrientedBox::volume() const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return 8 * length[0] * length[1] * length[2];
#else
  return fabs(8 * axis[0] % (axis[1] * axis[2]));
#endif
}

CartVect OrientedBox::dimensions() const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return 2.0 * length;
#else
  return 2.0 * CartVect( axis[0].length(), axis[1].length(), axis[2].length() );
#endif
}

double OrientedBox::area() const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return 4 * length[1] * length[2];
#else
  return 4 * (axis[1] * axis[2]).length();
#endif
}

CartVect OrientedBox::scaled_axis( int index ) const
{
#if MB_ORIENTED_BOX_UNIT_VECTORS
  return length[index] * axis[index];
#else
  return axis[index];
#endif
}
  
} // namespace moab

#endif
