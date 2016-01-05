/**
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

/**
 * \class AffineXform
 * \brief Define an affine transformatino
 * \author Jason Kraftcheck (kraftche@cae.wisc.edu)
 * \date August, 2006
 */

#include "AffineXform.hpp"
#include "moab/Interface.hpp"
#include <assert.h>


namespace moab {

// Don't include tag-related stuff in test build because we don't
// link to MOAB to test all other functionality.  
#ifndef TEST

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

#endif //TEST

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


#ifdef TEST // ******************* Unit Test code ***********************

using namespace moab;

#include <iostream>
#define ASSERT_VECTORS_EQUAL(A, B) assert_vectors_equal( (A), (B), #A, #B, __LINE__ )
#define ASSERT_DOUBLES_EQUAL(A, B) assert_doubles_equal( (A), (B), #A, #B, __LINE__ )
#define ASSERT(B) assert_bool( (B), #B, __LINE__ )

const double TOL = 1e-6;

int error_count = 0;

void assert_vectors_equal( const double* a, const double* b,
                           const char* sa, const char* sb,
                           int lineno )
{
  if (fabs(a[0] - b[0]) > TOL ||
      fabs(a[1] - b[1]) > TOL ||
      fabs(a[2] - b[2]) > TOL) {
    std::cerr << "Assertion failed at line " << lineno << std::endl
              << "\t" << sa << " == " << sb << std::endl
              << "\t[" << a[0] << ", " << a[1] << ", " << a[2] << "] == ["
              << b[0] << ", " << b[1] << ", " << b[2] << "]" << std::endl;
    ++error_count;
  }
}

void assert_vectors_equal( const CartVect& a, const CartVect& b, 
                           const char* sa, const char* sb,
                           int lineno )
{
  assert_vectors_equal( a.array(), b.array(), sa, sb, lineno );
}

void assert_doubles_equal( double a, double b, const char* sa, const char* sb, int lineno )
{
  if (fabs(a - b) > TOL) {
    std::cerr << "Assertion failed at line " << lineno << std::endl
              << "\t" << sa << " == " << sb << std::endl
              << "\t" << a << " == " << b << std::endl;
    ++error_count;
  }
}

void assert_bool( bool b, const char* sb, int lineno )
{
  if (!b) {
    std::cerr << "Assertion failed at line " << lineno << std::endl
              << "\t" << sb << std::endl;
    ++error_count;
  }
}


const CartVect point1( 0.0, 0.0, 0.0 ), point2( 3.5, 1000, -200 );
const CartVect vect1( 0.0, 0.0, -100.0 ), vect2( 1.0, 0.0, 1.0 );


void test_none()
{
    // default xform should do nothing.
  CartVect output;
  AffineXform none;
  none.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 );
  none.xform_point( point2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point2 );
  none.xform_vector( vect1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, vect1 );
  none.xform_vector( vect2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, vect2 );
}

void test_translation()
{
  CartVect offset( 1.0, 2.0, 3.0 );
  CartVect output;
  
  AffineXform move = AffineXform::translation( offset.array() );
  
  // test that points are moved by offset
  move.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 + offset );
  move.xform_point( point2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point2 + offset );
  
  // vectors should not be changed by a translation
  move.xform_vector( vect1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, vect1 );
  move.xform_vector( vect2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, vect2 );
}

void test_rotation()
{
  CartVect output;
  
  // rotate 90 degress about Z axis
  
  AffineXform rot = AffineXform::rotation( M_PI/2.0, CartVect(0,0,1).array() );
  ASSERT_DOUBLES_EQUAL( rot.matrix().determinant(), 1.0 );
  
  rot.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 ); // origin not affected by transform
  
  CartVect expectedz( -point2[1], point2[0], point2[2] ); // in first quadrant
  rot.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, expectedz );
  
  rot.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, vect1 );
  
  rot.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect( 0, 1, 1 ) );
  
  // rotate 90 degress about Y axis
  
  rot = AffineXform::rotation( M_PI/2.0, CartVect(0,1,0).array() );
  ASSERT_DOUBLES_EQUAL( rot.matrix().determinant(), 1.0 );
  
  rot.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 ); // origin not affected by transform
  
  CartVect expectedy( point2[2], point2[1], -point2[0] ); // in second quadrant
  rot.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, expectedy );
  
  rot.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect(-100,0,0) );
  
  rot.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect( 1, 0, -1 ) );
  
  // rotate 90 degress about X axis
  
  rot = AffineXform::rotation( M_PI/2.0, CartVect(1,0,0).array() );
  ASSERT_DOUBLES_EQUAL( rot.matrix().determinant(), 1.0 );
  
  rot.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 ); // origin not affected by transform
  
  CartVect expectedx( point2[0], -point2[2], point2[1] ); // in third quadrant
  rot.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, expectedx );
  
  rot.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect(0,100,0) );
  
  rot.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect( 1, -1, 0 ) );
  
  // rotate 180 degrees about vector in XY plane
  
  rot = AffineXform::rotation( M_PI, CartVect( 1, 1, 0 ).array() );
  ASSERT_DOUBLES_EQUAL( rot.matrix().determinant(), 1.0 );
  
  rot.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 ); // origin not affected by transform
  
  rot.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect( point2[1], point2[0], -point2[2] ) );
  
  rot.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, -vect1 ); // vector is in xy plane
  
  rot.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect( 0, 1, -1 ) );
}

void test_rotation_from_vec( )
{
  CartVect v1( 1, 1, 1 );
  CartVect v2( 1, 0, 0 );
  AffineXform rot = AffineXform::rotation( v1.array(), v2.array() );
  CartVect result;
  rot.xform_vector( v1.array(), result.array() );
    // vectors should be parallel, but not same length
  ASSERT_DOUBLES_EQUAL( result.length(), v1.length() );
  result.normalize();
  ASSERT_VECTORS_EQUAL( result, v2 );
  
  double v3[] = { -1, 0, 0 };
  rot = AffineXform::rotation( v3, v2.array() );
  rot.xform_vector( v3, result.array() );
  ASSERT_VECTORS_EQUAL( result, v2 );
}

CartVect refl( const CartVect& vect, const CartVect& norm )
{
  CartVect n(norm);
  n.normalize();
  double d = vect % n;
  return vect - 2 * d * n;
}

void test_reflection()
{
  CartVect output;
  
  // reflect about XY plane
  AffineXform ref = AffineXform::reflection( CartVect( 0, 0, 1 ).array() );
  ASSERT_DOUBLES_EQUAL( ref.matrix().determinant(), -1.0 );
  ref.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 );
  ref.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect(point2[0],point2[1],-point2[2]) );
  ref.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, -vect1 );
  ref.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, CartVect(1,0,-1) );
  
  // reflect about arbitrary palne
  CartVect norm( 3, 2, 1 );
  ref = AffineXform::reflection( norm.array() );
  ASSERT_DOUBLES_EQUAL( ref.matrix().determinant(), -1.0 );
  ref.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, point1 );
  ref.xform_point( point2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), point2.length() );
  ASSERT_VECTORS_EQUAL( output, refl(point2,norm) );
  ref.xform_vector( vect1.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect1.length() );
  ASSERT_VECTORS_EQUAL( output, refl(vect1,norm) );
  ref.xform_vector( vect2.array(), output.array() );
  ASSERT_DOUBLES_EQUAL( output.length(), vect2.length() );
  ASSERT_VECTORS_EQUAL( output, refl(vect2,norm) );
}

void test_scale()
{
  CartVect output;
  
  AffineXform scale = AffineXform::scale( 1.0 );
  ASSERT(!scale.scale());
  scale = AffineXform::scale( -1.0 );
  ASSERT(!scale.scale());
  
  //scale in X only
  scale = AffineXform::scale( CartVect(2,1,1).array() );
  ASSERT(scale.scale());
  scale.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, CartVect(2*point1[0],point1[1],point1[2]) );
  scale.xform_point( point2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, CartVect(2*point2[0],point2[1],point2[2]) );
  scale.xform_vector( vect1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, CartVect(2*vect1[0],vect1[1],vect1[2]) );
  scale.xform_vector( vect2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, CartVect(2*vect2[0],vect2[1],vect2[2]) );
  
  // scale in all
  scale = AffineXform::scale( CartVect(0.5,0.5,0.5).array() );
  ASSERT(scale.scale());
  scale.xform_point( point1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, 0.5*point1 );
  scale.xform_point( point2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, 0.5*point2 );
  scale.xform_vector( vect1.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, 0.5*vect1 );
  scale.xform_vector( vect2.array(), output.array() );
  ASSERT_VECTORS_EQUAL( output, 0.5*vect2 );
}

void test_scale_point()
{
  const double point[] = {2,3,4};
  const double f[] = { 0.2, 0.1, 0.3 };
  double result[3];
  AffineXform scale = AffineXform::scale( f, point );
  scale.xform_point( point, result );
  ASSERT_VECTORS_EQUAL( result, point );
  
  const double delta[3] = { 1, 0, 2 };
  const double pt2[] = { point[0]+delta[0],
                         point[1]+delta[1],
                         point[2]+delta[2] };
  scale = AffineXform::scale( f, point );
  scale.xform_point( pt2, result );
  
  const double expected[] = { point[0] + f[0]*delta[0],
                              point[1] + f[1]*delta[1],
                              point[2] + f[2]*delta[2] };
  ASSERT_VECTORS_EQUAL( result, expected );
}

void test_accumulate()
{
  CartVect indiv, accum;
  
  // build an group of transforms.  make sure translation is somewhere in the middle
  AffineXform move, scal, rot1, rot2, refl;
  move = AffineXform::translation( CartVect( 5, -5, 1 ).array() );
  scal = AffineXform::scale( CartVect( 1, 0.5, 2 ).array() );
  rot1 = AffineXform::rotation( M_PI/3, CartVect( 0.5, 0.5, 1 ).array() );
  rot2 = AffineXform::rotation( M_PI/4, CartVect( 1.0, 0.0, 0.0 ).array() );
  refl = AffineXform::reflection( CartVect( -1, -1, 0 ).array() );
  AffineXform accu;
  accu.accumulate( scal );
  accu.accumulate( rot1 );
  accu.accumulate( move );
  accu.accumulate( refl );
  accu.accumulate( rot2 );
  
  accu.xform_point( point1.array(), accum.array() );
  scal.xform_point( point1.array(), indiv.array() );
  rot1.xform_point( indiv.array() );
  move.xform_point( indiv.array() );
  refl.xform_point( indiv.array() );
  rot2.xform_point( indiv.array() );
  ASSERT_VECTORS_EQUAL( accum, indiv );
  
  accu.xform_point( point2.array(), accum.array() );
  scal.xform_point( point2.array(), indiv.array() );
  rot1.xform_point( indiv.array() );
  move.xform_point( indiv.array() );
  refl.xform_point( indiv.array() );
  rot2.xform_point( indiv.array() );
  ASSERT_VECTORS_EQUAL( accum, indiv );
  
  accu.xform_vector( vect1.array(), accum.array() );
  scal.xform_vector( vect1.array(), indiv.array() );
  rot1.xform_vector( indiv.array() );
  move.xform_vector( indiv.array() );
  refl.xform_vector( indiv.array() );
  rot2.xform_vector( indiv.array() );
  ASSERT_VECTORS_EQUAL( accum, indiv );
  
  accu.xform_vector( vect2.array(), accum.array() );
  scal.xform_vector( vect2.array(), indiv.array() );
  rot1.xform_vector( indiv.array() );
  move.xform_vector( indiv.array() );
  refl.xform_vector( indiv.array() );
  rot2.xform_vector( indiv.array() );
  ASSERT_VECTORS_EQUAL( accum, indiv );
}

void test_inversion() {
  CartVect result;
  
  // build an group of transforms.  make sure translation is somewhere in the middle
  AffineXform move, scal, rot1, rot2, refl;
  move = AffineXform::translation( CartVect( 5, -5, 1 ).array() );
  scal = AffineXform::scale( CartVect( 1, 0.5, 2 ).array() );
  rot1 = AffineXform::rotation( M_PI/3, CartVect( 0.5, 0.5, 1 ).array() );
  rot2 = AffineXform::rotation( M_PI/4, CartVect( 1.0, 0.0, 0.0 ).array() );
  refl = AffineXform::reflection( CartVect( -1, -1, 0 ).array() );
  AffineXform acc;
  acc.accumulate( scal );
  acc.accumulate( rot1 );
  acc.accumulate( move );
  acc.accumulate( refl );
  acc.accumulate( rot2 );
  
  AffineXform inv = acc.inverse();
  
  acc.xform_point( point1.array(), result.array() );
  inv.xform_point( result.array() );
  ASSERT_VECTORS_EQUAL( point1, result );
  
  acc.xform_point( point2.array(), result.array() );
  inv.xform_point( result.array() );
  ASSERT_VECTORS_EQUAL( point2, result );
  
  acc.xform_vector( vect1.array(), result.array() );
  inv.xform_vector( result.array() );
  ASSERT_VECTORS_EQUAL( vect1, result );
  
  acc.xform_vector( vect2.array(), result.array() );
  inv.xform_vector( result.array() );
  ASSERT_VECTORS_EQUAL( vect2, result );
}
  
void test_is_reflection()
{
  AffineXform refl1, refl2, scale;
  refl1 = AffineXform::reflection( CartVect( -1, -1, 0).array() );
  refl2 = AffineXform::reflection( CartVect(  1,  0, 0).array() );
  scale = AffineXform::scale( CartVect( -1, 1, 1 ).array() );
  
  ASSERT( refl1.reflection() );
  ASSERT( refl2.reflection() );
  ASSERT( scale.reflection() );
  
  AffineXform inv1, inv2, inv3;
  inv1 = refl1.inverse();
  inv2 = refl2.inverse();
  inv3 = scale.inverse();
  
  ASSERT( inv1.reflection() );
  ASSERT( inv2.reflection() );
  ASSERT( inv3.reflection() );
  
  refl1.accumulate( refl2 );
  refl2.accumulate( scale );
  ASSERT( ! refl1.reflection() );
  ASSERT( ! refl2.reflection() );
  
  AffineXform rot, mov;
  rot = AffineXform::rotation( M_PI/4, CartVect(1,1,1).array() );
  mov = AffineXform::translation( CartVect(-5,6,7).array() );
  ASSERT( !rot.reflection() );
  ASSERT( !mov.reflection() );
}

int main()
{
  test_none();
  test_translation();
  test_rotation();
  test_reflection();
  test_rotation_from_vec();
  test_scale();
  test_scale_point();
  test_accumulate();
  test_inversion();
  test_is_reflection();
  return error_count;
}

#endif  // #ifdef TEST
