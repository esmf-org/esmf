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


#ifndef MOAB_AFFINE_XFORM_HPP
#define MOAB_AFFINE_XFORM_HPP

#include "moab/Forward.hpp"
#include "moab/CartVect.hpp"
#include "moab/Matrix3.hpp"

#include <cmath>
#include <limits>

namespace moab {

/**
 * \brief Define an affine transformation
 * \author Jason Kraftcheck (kraftche@cae.wisc.edu)
 * \date August, 2006
 */
class AffineXform
{
  public:
  
    inline AffineXform();
    
    inline AffineXform( const double* three_by_three, 
                          const double* translation );
                          
    inline AffineXform( const Matrix3& mat, const CartVect& off );
    
    /** move */
    static inline AffineXform translation( const double* vector );
    /** rotate about axis through origin */
    static inline AffineXform rotation( double radians, const double* axis );
    /** define rotation such that if applied to \c from_vec the result aligned with \c to_vec */
    static        AffineXform rotation( const double* from_vec, const double* to_vec );
    /** reflect about plane through origin */
    static inline AffineXform reflection( const double* plane_normal );
    /** scale about origin */
    static inline AffineXform scale( double f );
    /** scale about origin */
    static inline AffineXform scale( const double* fractions );
    /** scale about a point */
    static inline AffineXform scale( double f, const double* point );
    /** scale about a point */
    static inline AffineXform scale( const double* fractions, const double* point );
    
    /** incorporate the passed transform into this one such that the
     *  resulting transform is the cumulative affect of this initial
     *  transform followed by the passed transform */
    inline void accumulate( const AffineXform& other );
    
    /** apply transform to a point */
    inline void xform_point( const double* input, double* output ) const;
    /** apply transform to a point */
    inline void xform_point( double* in_out ) const;
    
    /** apply transform to a vector */
    inline void xform_vector( const double* input, double* output ) const;
    /** apply transform to a vector */
    inline void xform_vector( double* in_out ) const;
    
    /** get transform that is the inverse of this transform */
    AffineXform inverse() const;
    
    /** get a tag that can be used to store an instance of this class */
    static ErrorCode get_tag( Tag& tag_handle_out,
                                Interface* moab,
                                const char* tagname = 0 );
    
    /** get 3x3 matrix portion of transform */
    const Matrix3& matrix() const { return mMatrix; }
    /** get translation portion of transform */
    const CartVect& offset() const { return mOffset; }
    
    /** Is this transform a reflection 
     *
     * A relfecting transform will require the reversal of the
     * order of edges in a loop, etc. because it produces a 
     * mirror-image of the input geometry.  This method tests
     * if this is such a transform.  A reflection may be created
     * with by an explicit transform, scaling with a negative
     * scale factor, etc.  If multiple transforms are combined
     * such that the transform is no longer a reflection (e.g. 
     * two reflections that are effectively a rotation), this method
     * will return false.
     */
    inline bool reflection() const;
    
    /** Does this transform do any scaling */
    inline bool scale() const;

  private:
  
    static inline AffineXform rotation( double cos_angle,
                                        double sin_angle,
                                        const CartVect& unit_axis );
  
    Matrix3 mMatrix;
    CartVect mOffset;
};

/** create a new transform equivalent to transform \c A followed
 *  by transform \c B 
 */
inline AffineXform operator*( const AffineXform& A, const AffineXform& B )
{
  AffineXform result(A);
  result.accumulate(B);
  return result;
}

inline AffineXform::AffineXform()
  : mMatrix(1.0), mOffset(0.0) 
  {}

inline AffineXform::AffineXform( const double* three_by_three, 
                                     const double* trans )
 : mMatrix(three_by_three), mOffset(trans)
 {}

inline AffineXform::AffineXform( const Matrix3& mat, const CartVect& off )
  : mMatrix(mat), mOffset(off)
  {}

inline AffineXform AffineXform::translation( const double* vector )
{
  return AffineXform( Matrix3(1.0), CartVect(vector) );
}

inline AffineXform AffineXform::rotation( double angle, const double* axis )
{
  CartVect a(axis);
  a.normalize();
  return AffineXform::rotation( std::cos(angle), std::sin(angle), a );
}

inline AffineXform AffineXform::rotation( double c,
                                          double s,
                                          const CartVect& a )
{
  const Matrix3 m1(    c,   -a[2]*s, a[1]*s,
                     a[2]*s,   c,   -a[0]*s,
                    -a[1]*s, a[0]*s,   c    );
  return AffineXform( m1 + (1.0-c)*outer_product( a, a ), CartVect(0.0) );
}

inline AffineXform AffineXform::reflection( const double* plane_normal )
{
  double i = plane_normal[0];
  double j = plane_normal[1];
  double k = plane_normal[2];
  Matrix3 m( j*j+k*k-i*i,    -2.0*i*j,    -2.0*i*k,
                  -2.0*i*j, i*i+k*k-j*j,    -2.0*j*k,
                  -2.0*i*k,    -2.0*j*k, i*i+j*j-k*k );
  m *= 1.0 / (i*i + j*j + k*k); //normalize
  return AffineXform( m, CartVect(0.0) );
}

inline AffineXform AffineXform::scale( const double* f )
{
  return AffineXform( Matrix3( CartVect(f) ), CartVect( 0.0 ) );
}

inline AffineXform AffineXform::scale( double f )
{
  return AffineXform( Matrix3( CartVect(f) ), CartVect( 0.0 ) );
}

inline AffineXform AffineXform::scale( double f, const double* point )
{
  double fs[] = { f, f, f };
  return AffineXform::scale( fs, point );
}

inline AffineXform AffineXform::scale( const double* f, const double* p )
{
  double offset[] = { p[0] * (1 - f[0]), 
                      p[1] * (1 - f[1]),
                      p[2] * (1 - f[2]) };
  return AffineXform( Matrix3( CartVect(f) ), CartVect(offset) );
}

inline void AffineXform::accumulate( const AffineXform& other )
{
  mMatrix = other.mMatrix * mMatrix;
  other.xform_point( mOffset.array() );
}

inline void AffineXform::xform_point( const double* input, double* output ) const
{
  xform_vector( input, output );
  output[0] += mOffset[0];
  output[1] += mOffset[1];
  output[2] += mOffset[2];
}

inline void AffineXform::xform_point( double* in_out ) const
{
  xform_vector( in_out );
  in_out[0] += mOffset[0];
  in_out[1] += mOffset[1];
  in_out[2] += mOffset[2];
} 

inline void AffineXform::xform_vector( const double* input, double* output ) const
{
  output[0] = input[0]*mMatrix[0][0] + input[1]*mMatrix[0][1] + input[2]*mMatrix[0][2];
  output[1] = input[0]*mMatrix[1][0] + input[1]*mMatrix[1][1] + input[2]*mMatrix[1][2];
  output[2] = input[0]*mMatrix[2][0] + input[1]*mMatrix[2][1] + input[2]*mMatrix[2][2];
}

inline void AffineXform::xform_vector( double* in_out ) const
{
  double input[] = { in_out[0], in_out[1], in_out[2] };
  xform_vector( input, in_out );
}

inline AffineXform AffineXform::inverse() const
{
  Matrix3 m = mMatrix.inverse();
  return AffineXform( m, m * -mOffset );
}

inline bool AffineXform::reflection() const
{
  return mMatrix.determinant() < 0.0;
}

inline bool AffineXform::scale() const
{
  return fabs(fabs(mMatrix.determinant()) - 1) > std::numeric_limits<double>::epsilon();
}
  
} // namespace moab

#endif
