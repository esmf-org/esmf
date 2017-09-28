/**
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


#ifndef MB_AXIS_BOX_HPP
#define MB_AXIS_BOX_HPP

#include <limits>
#include "moab/Interface.hpp"

namespace moab {

/**
 * \brief Class representing axis-aligned bounding box
 * \author Jason Kraftcheck (kraftche@cae.wisc.edu)
 * \date August, 2006
 */
class AxisBox {
  public:
  
    inline AxisBox();
    
    inline AxisBox( const double* min, const double* max );
    
    inline AxisBox( const double* point );
    
    static ErrorCode get_tag( Tag& tag_handle_out,
                                Interface* interface,
                                const char* tag_name = 0 );
    
    /** Calculate a box bounding the entities contained in the passed set */
    static ErrorCode calculate( AxisBox& box_out,
                                  EntityHandle set,
                                  Interface* interface );
                                      
    /** Calculate a box bounding the vertices/elements in the passed Range */
    static ErrorCode calculate( AxisBox& box_out,
                                  const Range& elements,
                                  Interface* interface );
                                  
    /** intersect */
    inline AxisBox& operator &=( const AxisBox& other );

    /** unite */
    inline AxisBox& operator |=( const AxisBox& other );
    
    /** unite */
    inline AxisBox& operator |=( const double* point );
    
    inline const double* minimum() const { return minVect; }
    
    inline const double* maximum() const { return maxVect; }
    
    inline double* minimum() { return minVect; }
    
    inline double* maximum() { return maxVect; }
    
    inline void center( double* center_out ) const;
    
    inline void diagonal( double* diagonal_out ) const;
    
    /**\brief Check if two boxes intersect.
     *
     * Check if two boxes are within the specified tolerance of
     * each other.  If tolerance is less than zero, then boxes must
     * overlap by at least the magnitude of the tolerance to be
     * considered intersecting.
     */
    inline bool intersects( const AxisBox& other, 
                            double tolerance ) const;
  
    /**\brief Check if box contains point
     *
     * Check if a position is in or on the box, within the specified tolerance
     */
    inline bool intersects( const double* point,
                            double tolerance ) const;
    
    /**\brief Check that box is valid
     *
     * Check that box is defined (contains at least a single point.)
     */
    inline bool valid() const;
                      
    /**\brief Find closest position on/within box to input position.
     * 
     * Find the closest position in the solid box to the input position.
     * If the input position is on or within the box, then the output
     * position will be the same as the input position.  If the input
     * position is outside the box, the outside position will be the
     * closest point on the box boundary to the input position.
     */
     inline void closest_position_within_box( const double* input_position,
                                    double* output_position ) const;
                            
  private:
  
    double minVect[3], maxVect[3];
};

/** intersect */
inline AxisBox operator&( const AxisBox& a, const AxisBox& b )
  { return AxisBox(a) &= b; }

/** unite */
inline AxisBox operator|( const AxisBox& a, const AxisBox& b )
  { return AxisBox(a) |= b; }

/** intersects */
inline bool operator||( const AxisBox& a, const AxisBox& b )
{
  return a.minimum()[0] <= b.maximum()[0]
      && a.minimum()[1] <= b.maximum()[1]
      && a.minimum()[2] <= b.maximum()[2]
      && a.maximum()[0] >= b.minimum()[0]
      && a.maximum()[1] >= b.minimum()[1]
      && a.maximum()[2] >= b.minimum()[2];
}


inline AxisBox::AxisBox()
{
  minVect[0] = minVect[1] = minVect[2] =  std::numeric_limits<double>::max();
  maxVect[0] = maxVect[1] = maxVect[2] = -std::numeric_limits<double>::max();
}

inline AxisBox::AxisBox( const double* min, const double* max )
{   
  minVect[0] = min[0];
  minVect[1] = min[1];
  minVect[2] = min[2];
  maxVect[0] = max[0];
  maxVect[1] = max[1];
  maxVect[2] = max[2];
}

inline AxisBox::AxisBox( const double* point )
{
  minVect[0] = maxVect[0] = point[0];
  minVect[1] = maxVect[1] = point[1];
  minVect[2] = maxVect[2] = point[2];
}
                                  
inline AxisBox& AxisBox::operator &=( const AxisBox& other )
{
  for (int i = 0; i < 3; ++i) {
    if (minVect[i] < other.minVect[i])
      minVect[i] = other.minVect[i];
    if (maxVect[i] > other.maxVect[i])
      maxVect[i] = other.maxVect[i];
  }
  return *this;
}

inline AxisBox& AxisBox::operator |=( const AxisBox& other )
{
  for (int i = 0; i < 3; ++i) {
    if (minVect[i] > other.minVect[i])
      minVect[i] = other.minVect[i];
    if (maxVect[i] < other.maxVect[i])
      maxVect[i] = other.maxVect[i];
  }
  return *this;
}

inline AxisBox& AxisBox::operator |=( const double* point )
{
  for (int i = 0; i < 3; ++i) {
    if (minVect[i] > point[i])
      minVect[i] = point[i];
    if (maxVect[i] < point[i])
      maxVect[i] = point[i];
  }
  return *this;
}
    
inline void AxisBox::center( double* center_out ) const
{
  center_out[0] = 0.5 * (minVect[0] + maxVect[0]);
  center_out[1] = 0.5 * (minVect[1] + maxVect[1]);
  center_out[2] = 0.5 * (minVect[2] + maxVect[2]);
}
    
inline void AxisBox::diagonal( double* diagonal_out ) const
{
  diagonal_out[0] = maxVect[0] - minVect[0];
  diagonal_out[1] = maxVect[1] - minVect[1];
  diagonal_out[2] = maxVect[2] - minVect[2];
}

inline bool AxisBox::intersects( const AxisBox& other, 
                                   double tolerance ) const
{
  return minVect[0] - other.maxVect[0] <= tolerance &&
         minVect[1] - other.maxVect[1] <= tolerance &&
         minVect[2] - other.maxVect[2] <= tolerance &&
         other.minVect[0] - maxVect[0] <= tolerance &&
         other.minVect[1] - maxVect[1] <= tolerance &&
         other.minVect[2] - maxVect[2] <= tolerance;
}
  
inline bool AxisBox::intersects( const double* point,
                                   double tolerance ) const
{
  return minVect[0] - point[0] <= tolerance &&
         minVect[1] - point[1] <= tolerance &&
         minVect[2] - point[2] <= tolerance &&
         maxVect[0] - point[0] <= tolerance &&
         maxVect[1] - point[1] <= tolerance &&
         maxVect[2] - point[2] <= tolerance;
}


inline bool AxisBox::valid() const
{
  return minVect[0] <= maxVect[0]
      && minVect[1] <= maxVect[1]
      && minVect[2] <= maxVect[2];
}

inline void AxisBox::closest_position_within_box( 
                                    const double* input_position,
                                    double* output_position ) const
{
  for (int i = 0; i < 3; ++i) {
    if (input_position[i] < minVect[i])
      output_position[i] = minVect[i];
    else if (input_position[i] > maxVect[i])
      output_position[i] = maxVect[i];
    else
      output_position[i] = input_position[i];
  }
}
  
} // namespace moab

#endif

