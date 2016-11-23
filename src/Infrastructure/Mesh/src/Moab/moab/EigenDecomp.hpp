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

#ifndef MB_EIGENDECOMP_HPP
#define MB_EIGENDECOMP_HPP

#include <math.h>
#include <iostream>

namespace moab {
//TODO: is this copied code actually able to be licensed
//TODO: Actually should have license text here, not link
// Copied from "Visualization Toolkit"
//  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
//  All rights reserved.
//  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.
//
// Jacobi iteration for the solution of eigenvectors/eigenvalues of a nxn
// real symmetric matrix. Square nxn matrix a; size of matrix in n;
// output eigenvalues in w; and output eigenvectors in v. Resulting
// eigenvalues/vectors are sorted in decreasing order; eigenvectors are
// normalized.
// TODO: Remove this
#define VTK_ROTATE(a,i,j,k,l) g=a[i][j];h=a[k][l];a[i][j]=g-s*(h+g*tau);\
        a[k][l]=h+s*(g-h*tau)

//TODO: Refactor this method into subroutines
//use a namespace { }  with no name to 
//contain subroutines so that the compiler
//automatically inlines them.

template< typename Matrix, typename Vector>
ErrorCode EigenDecomp( const Matrix & _a, 
                       double w[3],
                       Vector v[3] ) {
  const int MAX_ROTATIONS = 20;
  const double one_ninth = 1./9;
  int i, j, k, iq, ip, numPos;
  double tresh, theta, tau, t, sm, s, h, g, c, tmp;
  double b[3], z[3];
  Matrix a( _a);

  // initialize
  for (ip=0; ip<3; ip++) {
    for (iq=0; iq<3; iq++){
      v[ip][iq] = 0.0;
    }
    v[ip][ip] = 1.0;
  }
  for (ip=0; ip<3; ip++) {
    b[ip] = w[ip] = a[ip][ip];
    z[ip] = 0.0;
  }

  // begin rotation sequence
  for (i=0; i<MAX_ROTATIONS; i++){
    sm = 0.0;
    for (ip=0; ip<2; ip++){
      for (iq=ip+1; iq<3; iq++){ sm += fabs(a[ip][iq]); }
    }

    if ( sm == 0.0 ){ break; }
    // first 3 sweeps
    tresh = (i < 3)? 0.2*sm*one_ninth : 0.0;
    for (ip=0; ip<2; ip++) {
      for (iq=ip+1; iq<3; iq++) {
        g = 100.0*fabs(a[ip][iq]);

        // after 4 sweeps
        if ( i > 3 && (fabs(w[ip])+g) == fabs(w[ip])
        	   && (fabs(w[iq])+g) == fabs(w[iq])) {
          a[ip][iq] = 0.0; 
	}
        else if ( fabs(a[ip][iq]) > tresh) {
          h = w[iq] - w[ip];
          if ( (fabs(h)+g) == fabs(h)){ t = (a[ip][iq]) / h; }
          else {
            theta = 0.5*h / (a[ip][iq]);
            t = 1.0 / (fabs(theta)+sqrt(1.0+theta*theta));
            if (theta < 0.0) { t = -t;}
          }
          c = 1.0 / sqrt(1+t*t);
          s = t*c;
          tau = s/(1.0+c);
          h = t*a[ip][iq];
          z[ip] -= h;
          z[iq] += h;
          w[ip] -= h;
          w[iq] += h;
          a[ip][iq]=0.0;
          // ip already shifted left by 1 unit
          for (j = 0;j <= ip-1;j++) { VTK_ROTATE(a,j,ip,j,iq); }
          // ip and iq already shifted left by 1 unit
          for (j = ip+1;j <= iq-1;j++) { VTK_ROTATE(a,ip,j,j,iq); }
          // iq already shifted left by 1 unit
          for (j=iq+1; j<3; j++) { VTK_ROTATE(a,ip,j,iq,j); }
          for (j=0; j<3; j++) { VTK_ROTATE(v,j,ip,j,iq); }
          }
        }
      }

    for (ip=0; ip<3; ip++) {
      b[ip] += z[ip];
      w[ip] = b[ip];
      z[ip] = 0.0;
    }
  }

  //// this is NEVER called
  if ( i >= MAX_ROTATIONS ) {
      std::cerr << "Matrix3D: Error extracting eigenfunctions" << std::endl;
      return MB_FAILURE;
  }

  // sort eigenfunctions                 these changes do not affect accuracy 
  for (j=0; j<2; j++){                  // boundary incorrect
    k = j;
    tmp = w[k];
    for (i=j+1; i<3; i++){                // boundary incorrect, shifted already
      if (w[i] >= tmp){                  // why exchage if same?
        k = i;
        tmp = w[k];
        }
    }
    if (k != j){
      w[k] = w[j];
      w[j] = tmp;
      for (i=0; i<3; i++){
        tmp = v[i][j];
        v[i][j] = v[i][k];
        v[i][k] = tmp;
        }
    }
  }
  // insure eigenvector consistency (i.e., Jacobi can compute vectors that
  // are negative of one another (.707,.707,0) and (-.707,-.707,0). This can
  // reek havoc in hyperstreamline/other stuff. We will select the most
  // positive eigenvector.
  int ceil_half_n = (3 >> 1) + (3 & 1);
  for (j=0; j<3; j++) {
    for (numPos=0, i=0; i<3; i++) {
      if ( v[i][j] >= 0.0 ) { numPos++; }
    }
//    if ( numPos < ceil(double(n)/double(2.0)) )
    if ( numPos < ceil_half_n) {
      for(i=0; i<3; i++) { v[i][j] *= -1.0; }
    }
  }
  return MB_SUCCESS;
}

} // namespace moab
#endif //MB_EIGENDECOMP_HPP
