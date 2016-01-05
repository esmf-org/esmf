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

#include "moab/HomXform.hpp"
#include <assert.h>

namespace moab {

HomCoord HomCoord::unitv[3] = {HomCoord(1,0,0), HomCoord(0,1,0), HomCoord(0,0,1)};
HomCoord HomCoord::IDENTITY(1, 1, 1);

int dum[] = {1, 0, 0, 0,
             0, 1, 0, 0,
             0, 0, 1, 0,
             0, 0, 0, 1};
HomXform HomXform::IDENTITY(dum);

void HomXform::three_pt_xform(const HomCoord &p1, const HomCoord &q1,
                              const HomCoord &p2, const HomCoord &q2,
                              const HomCoord &p3, const HomCoord &q3) 
{
    // pmin and pmax are min and max bounding box corners which are mapped to
    // qmin and qmax, resp.  qmin and qmax are not necessarily min/max corners,
    // since the mapping can change the orientation of the box in the q reference
    // frame.  Re-interpreting the min/max bounding box corners does not change
    // the mapping.

    // change that: base on three points for now (figure out whether we can
    // just use two later); three points are assumed to define an orthogonal
    // system such that (p2-p1)%(p3-p1) = 0
  
    // use the three point rule to compute the mapping, from Mortensen, 
    // "Geometric Modeling".  If p1, p2, p3 and q1, q2, q3 are three points in
    // the two coordinate systems, the three pt rule is:
    //
    // v1 = p2 - p1
    // v2 = p3 - p1
    // v3 = v1 x v2
    // w1-w3 similar, with q1-q3
    // V = matrix with v1-v3 as rows
    // W similar, with w1-w3
    // R = V^-1 * W
    // t = q1 - p1 * W
    // Form transform matrix M from R, t

    // check to see whether unity transform applies
  if (p1 == q1 && p2 == q2 && p3 == q3) {
    *this = HomXform::IDENTITY;
    return;
  }

    // first, construct 3 pts from input
  HomCoord v1 = p2 - p1;
  assert(v1.i() != 0 || v1.j() != 0 || v1.k() != 0);
  HomCoord v2 = p3 - p1;
  HomCoord v3 = v1 * v2;
  
  if (v3.length_squared() == 0) {
      // 1d coordinate system; set one of v2's coordinates such that
      // it's orthogonal to v1
    if (v1.i() == 0) v2.set(1,0,0);
    else if (v1.j() == 0) v2.set(0,1,0);
    else if (v1.k() == 0) v2.set(0,0,1);
    else assert(false);
    v3 = v1 * v2;
    assert(v3.length_squared() != 0);
  }
    // assert to make sure they're each orthogonal
  assert(v1%v2 == 0 && v1%v3 == 0 && v2%v3 == 0);
  v1.normalize(); v2.normalize(); v3.normalize();
    // Make sure h is set to zero here, since it'll mess up things if it's one
  v1.homCoord[3] = v2.homCoord[3] = v3.homCoord[3] = 0;

  HomCoord w1 = q2 - q1;
  assert(w1.i() != 0 || w1.j() != 0 || w1.k() != 0);
  HomCoord w2 = q3 - q1;
  HomCoord w3 = w1 * w2;
  
  if (w3.length_squared() == 0) {
      // 1d coordinate system; set one of w2's coordinates such that
      // it's orthogonal to w1
    if (w1.i() == 0) w2.set(1,0,0);
    else if (w1.j() == 0) w2.set(0,1,0);
    else if (w1.k() == 0) w2.set(0,0,1);
    else assert(false);
    w3 = w1 * w2;
    assert(w3.length_squared() != 0);
  }
    // assert to make sure they're each orthogonal
  assert(w1%w2 == 0 && w1%w3 == 0 && w2%w3 == 0);
  w1.normalize(); w2.normalize(); w3.normalize();
    // Make sure h is set to zero here, since it'll mess up things if it's one
  w1.homCoord[3] = w2.homCoord[3] = w3.homCoord[3] = 0;
  
    // form v^-1 as transpose (ok for orthogonal vectors); put directly into
    // transform matrix, since it's eventually going to become R
  *this = HomXform(v1.i(), v2.i(), v3.i(), 0,
                   v1.j(), v2.j(), v3.j(), 0,
                   v1.k(), v2.k(), v3.k(), 0,
                   0, 0, 0, 1);

    // multiply by w to get R
  *this *= HomXform(w1.i(), w1.j(), w1.k(), 0,
                    w2.i(), w2.j(), w2.k(), 0,
                    w3.i(), w3.j(), w3.k(), 0,
                    0, 0, 0, 1);
  
    // compute t and put into last row
  HomCoord t = q1 - p1 * *this;
  (*this).XFORM(3,0) = t.i();
  (*this).XFORM(3,1) = t.j();
  (*this).XFORM(3,2) = t.k();

    // M should transform p to q
  assert((q1 == p1 * *this) &&
         (q2 == p2 * *this) &&
         (q3 == p3 * *this));
}
  
} // namespace moab


#ifdef TEST

using namespace moab;

#include <iostream>

// unit test for the HomCoord and HomXform classes
//

int test_get_set();

int test_coord_operators();

int test_xform_operators();

int test_xform_functions();

int test_coord_xform_operators();

int main(int /*argc*/, char**/* argv[]*/)
{
    // first test HomCoord

    // constructors
    // following shouldn't compile, since bare constructor is private
//  HomCoord mycoord;
  
  int errors = 0;

  errors += test_get_set();

  errors += test_coord_operators();

  errors += test_xform_operators();

  errors += test_xform_functions();
  
  errors += test_coord_xform_operators();

  if (errors > 0)
    std::cout << errors << " errors found." << std::endl;
  else
    std::cout << "All tests passed." << std::endl;
    
  return errors;
}

int test_get_set() 
{
  int errors = 0;
  
    // other constructors
  int coordsa[4] = {1, 2, 3, 1};
  int coordsb[4] = {4, 3, 2, 1};
      
  
  HomCoord coords1(coordsa);
  HomCoord coords2(4, 3, 2, 1);
  HomCoord coords3(4, 3, 2);
  HomCoord coords4(1, 1, 1, 1);

    // set
  coords2.set(coordsb);
  
    // hom_coord()
  for (int i = 0; i < 4; i++) {
    if (coords2.hom_coord()[i] != coordsb[i]) {
      std::cout << "Get test failed." << std::endl;
      errors++;
    }
  }

    // ijkh
  if (coords2.i() != coordsb[0] ||
      coords2.j() != coordsb[1] ||
      coords2.k() != coordsb[2] ||
      coords2.h() != coordsb[3]) {
    std::cout << "ijkh test failed." << std::endl;
    errors++;
  }
  
    // set
  coords2.set(3, 3, 3);
  
    // hom_coord()
  if (coords2.hom_coord()[0] != 3 || coords2.hom_coord()[1] != 3 ||
      coords2.hom_coord()[2] != 3 || coords2.hom_coord()[3] != 1) {
    std::cout << "Set (int) test failed." << std::endl;
    errors++;
  }
  
  return errors;
}

int test_coord_operators() 
{
  int errors = 0;
  
  HomCoord coords1(1, 2, 3, 1);
  HomCoord coords2(4, 3, 2, 1);
  HomCoord coords3(4, 3, 2);
  HomCoord coords4(1, 1, 1, 1);

    // operator>=
  bool optest = (coords2 >= coords4 &&
                 coords2 >= coords3 &&
                 coords3 >= coords2);
  if (!optest) {
    std::cout << "Test failed for operator >=." << std::endl;
    errors++;
  }

  optest = (coords4 <= coords2 &&
            coords2 <= coords3 &&
            coords3 <= coords2);
  if (!optest) {
    std::cout << "Test failed for operator <=." << std::endl;
    errors++;
  }

    // operator>
  optest = (coords2 > coords4 &&
            !(coords2 > coords3) &&
            !(coords3 > coords2));
  if (!optest) {
    std::cout << "Test failed for operator >." << std::endl;
    errors++;
  }

  optest = (coords4 < coords2 &&
            !(coords2 < coords3) &&
            !(coords3 < coords2));
  if (!optest) {
    std::cout << "Test failed for operator <." << std::endl;
    errors++;
  }

    // operator[]
  for (int i = 0; i < 3; i++) {
    if (coords1[i] != coords2[3-i]) {
      std::cout << "Test failed for operator[]." << std::endl;
      errors++;
    }
  }

    // operator+
  HomCoord coords5(2*coords1[0], 2*coords1[1], 2*coords1[2]);
  HomCoord coords6 = coords1 + coords1;
  if (coords5 != coords6) {
      std::cout << "Test failed for operator+." << std::endl;
      errors++;
  }
  
    // operator-
  if (coords5-coords1 != coords1) {
      std::cout << "Test failed for operator-." << std::endl;
      errors++;
  }

  return errors;
}

int test_xform_constructors() 
{
  int errors = 0;
  
    // integer constructor
  int test_int[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  HomXform xform(test_int);
  for (int i = 0; i < 16; i++) {
    if (xform[i] != i) {
      std::cout << "HomXform integer array constructor failed." << std::endl;
      errors++;
    }
  }

  HomXform xform3(test_int[0], test_int[1], test_int[2], test_int[3],
                  test_int[4], test_int[5], test_int[6], test_int[7],
                  test_int[8], test_int[9], test_int[10], test_int[11],
                  test_int[12], test_int[13], test_int[14], test_int[15]);
  for (int i = 0; i < 16; i++) {
    if (xform3[i] != i) {
      std::cout << "HomXform integer constructor failed." << std::endl;
      errors++;
    }
  }

    // sample rotation, translation, and scaling matrices/vectors
  int rotate[] = {0, 1, 0, -1, 0, 0, 0, 0, 1};
  int translate[] = {4, 5, 6};
  int scale[] = {1, 1, 1};
  
    // construct an xform matrix based on those
  HomXform xform2(rotate, scale, translate);
  
    // test where those went in the xform
  if (!(xform2[XFORM_INDEX(0,0)] == rotate[0] &&
        xform2[XFORM_INDEX(0,1)] == rotate[1] &&
        xform2[XFORM_INDEX(0,2)] == rotate[2] &&
        xform2[XFORM_INDEX(1,0)] == rotate[3] &&
        xform2[XFORM_INDEX(1,1)] == rotate[4] &&
        xform2[XFORM_INDEX(1,2)] == rotate[5] &&
        xform2[XFORM_INDEX(2,0)] == rotate[6] &&
        xform2[XFORM_INDEX(2,1)] == rotate[7] &&
        xform2[XFORM_INDEX(2,2)] == rotate[8] &&
        xform2[XFORM_INDEX(3,0)] == translate[0] &&
        xform2[XFORM_INDEX(3,1)] == translate[1] &&
        xform2[XFORM_INDEX(3,2)] == translate[2] &&
        xform2[XFORM_INDEX(0,3)] == 0 &&
        xform2[XFORM_INDEX(1,3)] == 0 &&
        xform2[XFORM_INDEX(2,3)] == 0 &&
        xform2[XFORM_INDEX(3,3)] == 1)) {
      std::cout << "HomXform rotate, scale, translate constructor failed." << std::endl;
      errors++;
  }

  return errors;
}

int test_xform_operators() 
{
  int errors = 0;
  
    // sample rotation, translation, and scaling matrices/vectors
  int rotate[] = {0, 1, 0, -1, 0, 0, 0, 0, 1};
  int translate[] = {4, 5, 6};
  int scale[] = {1, 1, 1};
  
    // construct an xform matrix based on those
  HomXform xform1(rotate, scale, translate);
  HomXform xform1a(rotate, scale, translate);
  
    // test operator==
  if (!(xform1 == xform1a)) {
    std::cout << "HomXform operator== failed." << std::endl;
    errors++;
  }

    // test operator!=
  xform1a[1] = 0;
  if (!(xform1 != xform1a)) {
    std::cout << "HomXform operator!= failed." << std::endl;
    errors++;
  }

    // test operator=
  HomXform xform1c = xform1;
  if (!(xform1c == xform1)) {
    std::cout << "HomXform operator= failed." << std::endl;
    errors++;
  }

  HomXform xform3 = xform1 * HomXform::IDENTITY;
  if (xform3 != xform1) {
    std::cout << "HomXform operator * failed." << std::endl;
    errors++;
  }

    // test operator*=
  xform3 *= HomXform::IDENTITY;
  if (xform3 != xform1) {
    std::cout << "HomXform operator *= failed." << std::endl;
    errors++;
  }

  return errors;
}

int test_xform_functions() 
{
  int errors = 0;

    // HomCoord functions
    // length() and length_squared()
  HomCoord coord1(3, 4, 5);
  if (coord1.length_squared() != 50 ||
      coord1.length() != 7)   {
    std::cout << "HomCoord length() or length_squared() failed." << std::endl;
    errors++;
  }

    // normalize()
  coord1.normalize();
  HomCoord coord2(3, 0, 0);
  coord2.normalize();
  if (coord1.length_squared() != 0 ||
      coord2.length_squared() != 1)   {
    std::cout << "HomCoord normalize failed." << std::endl;
    errors++;
  }
  
    // sample rotation, translation, and scaling matrices/vectors
  int inv_int[] = {0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 1, 0, 7, 5, 0, 1};
  
    // construct an xform matrix based on those
  HomXform xform1(inv_int);

  HomXform xform1_inv = xform1.inverse();

  HomXform xform2 = xform1 * xform1_inv;
  if (xform2 != HomXform::IDENTITY) {
    std::cout << "HomXform inverse failed." << std::endl;
    errors++;
  }

  return errors;
}

int test_coord_xform_operators() 
{
  int errors = 0;
  
    // sample pt
  HomCoord test_pt(1, 2, 3);
  
  HomCoord test_pt2 = test_pt * HomXform::IDENTITY;
  if (test_pt2 != test_pt) {
    std::cout << "Coord-xform operator* failed." << std::endl;
    errors++;
  }

    // get an inverse transform quickly
  int rotate[] = {0, 1, 0, -1, 0, 0, 0, 0, 1};
  int translate[] = {4, 5, 6};
  int scale[] = {1, 1, 1};
  HomXform xform2(rotate, scale, translate);

    // operator*
  HomCoord ident(1, 1, 1, 1);
  HomCoord test_pt3 = ident * HomXform::IDENTITY;
  if (test_pt3 != ident) {
    std::cout << "Coord-xform operator* failed." << std::endl;
    errors++;
  }
    
    // operator/
  test_pt2 = (test_pt * xform2) / xform2;
  if (test_pt2 != test_pt) {
    std::cout << "Coord-xform operator/ failed." << std::endl;
    errors++;
  }


    // test three_pt_xform; use known transforms in most cases
  HomXform xform;

    // first test: i = j, j = -i, k = k, t = (7,5,0)
  xform.three_pt_xform(HomCoord(0,0,0,1), HomCoord(7,5,0,1),
                       HomCoord(0,3,0,1), HomCoord(4,5,0,1),
                       HomCoord(0,0,3,1), HomCoord(7,5,3,1));

  HomXform solution(0,1,0,0, -1,0,0,0,0,0,1,0,7,5,0,1);
  if (xform != solution) {
    std::cout << "Three-pt transform (general) test failed." << std::endl;
    errors++;
  }
  
    // now test 1d
  xform.three_pt_xform(HomCoord(0,0,0,1), HomCoord(7,5,0,1),
                       HomCoord(6,0,0,1), HomCoord(7,11,0,1),
                       HomCoord(0,0,0,1), HomCoord(7,5,0,1));

  solution = HomXform(0,1,0,0,1,0,0,0,0,0,-1,0,7,5,0,1);
  if (xform != solution) {
    std::cout << "Three-pt transform (1d) test failed." << std::endl;
    errors++;
  }
  
  
  return errors;
}
  
#endif
