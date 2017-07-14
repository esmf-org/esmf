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

/**
 * \class AffineXform
 * \brief Define an affine transformatino
 * \author Jason Kraftcheck (kraftche@cae.wisc.edu)
 * \date August, 2006
 */

#ifdef _MSC_VER  /* windows */
#  define _USE_MATH_DEFINES //For M_PI
#endif

#include "AffineXform.hpp"
#include "moab/Interface.hpp"
#include <assert.h>

namespace moab {

// Don't include tag-related stuff in test build because we don't
// link to MOAB to test all other functionality.  
const char* const AFFINE_XFORM_TAG_NAME = "AFFINE_TRANSFORM";

ErrorCode AffineXform::get_tag( Tag& tag_out,
                                    Interface* interface,
                                    const char* tagname )
{
  assert( sizeof(AffineXform) == 12*sizeof(double) );
  
  if (!tagname)
    tagname = AFFINE_XFORM_TAG_NAME;
 
  return interface->tag_get_handle( tagname, 
                                    sizeof(AffineXform),
                                    MB_TYPE_DOUBLE,
                                    tag_out,
                                    MB_TAG_BYTES|MB_TAG_CREAT|MB_TAG_DENSE );
}


AffineXform AffineXform::rotation( const double* from_vec, const double* to_vec )
{
  CartVect from(from_vec);
  CartVect to(to_vec);
  CartVect a = from * to;
  double len = a.length();
  
  // If input vectors are not parallel (the normal case)
  if (len >= std::numeric_limits<double>::epsilon()) {
    from.normalize();
    to.normalize();
    return rotation( from % to, (from * to).length(), a/len );
  }
  
  // Vectors are parallel:
  //
  // If vectors are in same direction then rotation is identity (no transform)
  if (from % to >= 0.0)
    return AffineXform(); 
    
  // Parallel vectors in opposite directions:
  //
  // NOTE:  This case is ill-defined.  There are infinitely
  // many rotations that can align the two vectors.  The angle
  // of rotation is 180 degrees, but the axis of rotation may 
  // be any unit vector orthogonal to the input vectors.
  //
  from.normalize();
  double lenxy = std::sqrt( from[0]*from[0] + from[1]*from[1] );
  CartVect axis( -from[0]*from[2]/lenxy, 
                 -from[1]*from[2]/lenxy,
                                  lenxy );
  return rotation( -1, 0, axis );
}

  
} // namespace moab

