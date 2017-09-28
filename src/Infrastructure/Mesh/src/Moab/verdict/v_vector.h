/*=========================================================================
  Original version -- Copyright (c) 2006 Sandia Corporation.
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#ifndef VERDICT_VECTOR
#define VERDICT_VECTOR

#include "moab/verdict.h"
#include <cmath>
#include <cassert>

// computes the dot product of 3d vectors
inline double dot_product( double vec1[], double vec2[] )
{
  double answer =  vec1[0] * vec2[0] +
     vec1[1] * vec2[1] +
     vec1[2] * vec2[2];
  return answer;
}

// normalize a vector
inline void normalize( double vec[] )
{
  double x = sqrt( vec[0]*vec[0] +
             vec[1]*vec[1] +
             vec[2]*vec[2] );

  vec[0] /= x;
  vec[1] /= x;
  vec[2] /= x;

}

// computes the cross product
inline double * cross_product( double vec1[], double vec2[], double answer[] )
{
  answer[0] = vec1[1] * vec2[2] - vec1[2] * vec2[1];
  answer[1] = vec1[2] * vec2[0] - vec1[0] * vec2[2];
  answer[2] = vec1[0] * vec2[1] - vec1[1] * vec2[0];
  return answer;
}

// computes the length of a vector
inline double length ( double vec[] )
{
  return sqrt ( vec[0] * vec[0] + vec[1] * vec[1] + vec[2] * vec[2] );
}

// computes the square length of a vector
inline double length_squared (double vec[] )
{
  return (vec[0] * vec[0] + vec[1] * vec[1] + vec[2] * vec[2] );
}

// computes the interior angle between 2 vectors in degrees
inline double interior_angle( double vec1[], double vec2[] )
{
  double cosAngle, angleRad;
  double length1 = length(vec1);
  double length2 = length(vec2);
  assert( (length1 > 0.0 && length2 > 0.0) );

  cosAngle = dot_product(vec1, vec2) / (length1 * length2);

  if ((cosAngle > 1.0) && (cosAngle < 1.0001))
  {
    cosAngle = 1.0;
  }
  else if (cosAngle < -1.0 && cosAngle > -1.0001)
  {
    cosAngle = -1.0;
  }
  else
  {
    assert(cosAngle < 1.0001 && cosAngle > -1.0001);
  }

  angleRad = acos(cosAngle);
  return( (angleRad * 180.) / VERDICT_PI );
}

#endif

