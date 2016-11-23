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

