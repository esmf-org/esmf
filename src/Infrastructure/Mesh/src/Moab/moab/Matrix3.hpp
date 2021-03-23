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
 */

/**\file Matrix3.hpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2006-07-18
 *\date 2012-08-2 Updated by rhl to be more generic. less code that does more!
 * TODO: Remove all 'inline' keywords as it is only a suggestion to the compiler
 * anyways, and it will ignore it or add it when it thinks its necessary.
 *\date 2016-08-03 Updated to use Eigen3 support underneath to improve performance
 */

#ifndef MOAB_MATRIX3_HPP
#define MOAB_MATRIX3_HPP

#include <iostream>
#include <iosfwd>
#include <limits>
#include <cmath>
#include <cassert>

#include "moab/MOABConfig.h"
#include "moab/ErrorHandler.hpp"
#include "moab/Util.hpp"
#include "moab/Types.hpp"
#include "moab/CartVect.hpp"

#ifdef MOAB_HAVE_EIGEN

#ifdef __GNUC__
// save diagnostic state
#pragma GCC diagnostic push
// turn off the specific warning. Can also use "-Wshadow"
#pragma GCC diagnostic ignored "-Wshadow"
#endif

#define EIGEN_DEFAULT_TO_ROW_MAJOR
#define EIGEN_INITIALIZE_MATRICES_BY_ZERO
// #define EIGEN_NO_STATIC_ASSERT
#include "Eigen/Dense"

#ifdef __GNUC__
// turn the warnings back on
#pragma GCC diagnostic pop
#endif

#else // Check for LAPACK

// We will rely on LAPACK directly
#ifndef MOAB_HAVE_LAPACK
#error Need either Eigen3 or BLAS/LAPACK libraries
#endif

// RLO: Only use ESMF-internal LAPACK subroutines with ESMF internal LAPACK.
//      It seems like only dgeev and dsyevd are currently in use with MOAB.
//      ESMF-internal LAPACK does not currently support dsyevr, dgetrf, dgetri.
#if defined ESMF_LAPACK_INTERNAL
#define MOAB_dgeev  MOAB_FC_FUNC(esmf_dgeev, ESMF_DGEEV)
#define MOAB_dsyevd MOAB_FC_FUNC(esmf_dsyevd, ESMF_DSYEVD)
#elif defined ESMF_LAPACK
#define MOAB_dgeev  MOAB_FC_FUNC(dgeev, DGEEV)
#define MOAB_dsyevd MOAB_FC_FUNC(dsyevd, DSYEVD)
#endif

// #define MOAB_dsyevr MOAB_FC_FUNC(dsyevr, DSYEVR)
// #define MOAB_dgetrf MOAB_FC_FUNC(dgetrf, DGETRF)
// #define MOAB_dgetri MOAB_FC_FUNC(dgetri, DGETRI)

extern "C" {

// Computes all eigenvalues and, optionally, eigenvectors of a
// real symmetric matrix A. If eigenvectors are desired, it uses a
// divide and conquer algorithm.
void MOAB_dsyevd ( char *jobz, char *uplo, int *n,
                   double a[], int *lda, double w[], double work[],
                   int *lwork, int iwork[], int *liwork,
                   int *info);

// Computes selected eigenvalues and, optionally, eigenvectors
// of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
// selected by specifying either a range of values or a range of
// indices for the desired eigenvalues.
void MOAB_dsyevr ( char *jobz, char *range, char *uplo, int *n,
                   double *a, int *lda, double *vl, double *vu,
                   int *il, int *iu, double *abstol, int *m, double *w,
                   double *z, int *ldz, int *isuppz, double *work,
                   int *lwork, int *iwork, int *liwork, int *info);

// Computes for an N-by-N real nonsymmetric matrix A, the
// eigenvalues and, optionally, the left and/or right eigenvectors.
void MOAB_dgeev  ( char *jobvl, char *jobvr, int *n, double * a,
                   int *lda, double *wr, double *wi, double *vl,
                   int *ldvl, double *vr, int *ldvr, double *work,
                   int *lwork, int *info);

// Computes an LU factorization of a general M-by-N matrix A
// using partial pivoting with row interchanges.
void MOAB_dgetrf ( int* M, int *N, double* A,
                   int* lda, int* IPIV,
                   int* INFO);

// Computes the inverse of a matrix using the LU factorization
// computed by DGETRF.
void MOAB_dgetri ( int* N, double* A,
                   int* lda, int* IPIV,
                   double* WORK, int* lwork,
                   int* INFO);

}

#include <cstring>
#define MOAB_DMEMZERO(a,b) memset(a, 0, b*sizeof(double))

#endif

namespace moab {

namespace Matrix{

	template< typename Matrix>
	inline Matrix mmult3( const Matrix& a, const Matrix& b ) {
	  return Matrix( a(0,0) * b(0,0) + a(0,1) * b(1,0) + a(0,2) * b(2,0),
	                 a(0,0) * b(0,1) + a(0,1) * b(1,1) + a(0,2) * b(2,1),
	                 a(0,0) * b(0,2) + a(0,1) * b(1,2) + a(0,2) * b(2,2),
	                 a(1,0) * b(0,0) + a(1,1) * b(1,0) + a(1,2) * b(2,0),
	                 a(1,0) * b(0,1) + a(1,1) * b(1,1) + a(1,2) * b(2,1),
	                 a(1,0) * b(0,2) + a(1,1) * b(1,2) + a(1,2) * b(2,2),
	                 a(2,0) * b(0,0) + a(2,1) * b(1,0) + a(2,2) * b(2,0),
	                 a(2,0) * b(0,1) + a(2,1) * b(1,1) + a(2,2) * b(2,1),
	                 a(2,0) * b(0,2) + a(2,1) * b(1,2) + a(2,2) * b(2,2) );
	}

	template< typename Matrix>
	inline const Matrix inverse( const Matrix & d){
		const double det = 1.0/determinant3( d);
		return inverse( d, det);
	}

	template< typename Vector, typename Matrix>
	inline Vector vector_matrix( const Vector& v, const Matrix& m ) {
	  return Vector( v[0] * m(0,0) + v[1] * m(1,0) + v[2] * m(2,0),
	                 v[0] * m(0,1) + v[1] * m(1,1) + v[2] * m(2,1),
	                 v[0] * m(0,2) + v[1] * m(1,2) + v[2] * m(2,2) );
	}

	template< typename Vector, typename Matrix>
	inline Vector matrix_vector( const Matrix& m, const Vector& v ){
	   Vector res = v;
	   res[ 0] = v[0] * m(0,0) + v[1] * m(0,1) + v[2] * m(0,2);
	   res[ 1] = v[0] * m(1,0) + v[1] * m(1,1) + v[2] * m(1,2);
	   res[ 2] = v[0] * m(2,0) + v[1] * m(2,1) + v[2] * m(2,2);
	   return res;
	}

} //namespace Matrix

class Matrix3  {

public:
  const static int size = 9;

private:

#ifdef MOAB_HAVE_EIGEN
  Eigen::Matrix3d _mat;
#else
  double _mat[size];
#endif

public:

  //Default Constructor
  inline Matrix3() {
#ifdef MOAB_HAVE_EIGEN
    _mat.fill(0.0);
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
#endif
  }

#ifdef MOAB_HAVE_EIGEN
  inline Matrix3(Eigen::Matrix3d mat) : _mat(mat) {
  }
#endif

  //TODO: Deprecate this.
  //Then we can go from three Constructors to one.
  inline Matrix3( double diagonal ) {
#ifdef MOAB_HAVE_EIGEN
    _mat << diagonal, 0.0, 0.0,
            0.0, diagonal, 0.0,
            0.0, 0.0, diagonal;
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
    _mat[0] = _mat[4] = _mat[8] = diagonal;
#endif
  }

  inline Matrix3( const CartVect & diagonal ) {
#ifdef MOAB_HAVE_EIGEN
    _mat << diagonal[0], 0.0, 0.0,
            0.0, diagonal[1], 0.0,
            0.0, 0.0, diagonal[2];
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
    _mat[0] = diagonal[0];
    _mat[4] = diagonal[1];
    _mat[8] = diagonal[2];
#endif
  }

  //TODO: not strictly correct as the Matrix3 object
  //is a double d[ 9] so the only valid model of T is
  //double, or any refinement (int, float)
  //*but* it doesn't really matter anything else
  //will fail to compile.
  inline Matrix3( const std::vector<double> & diagonal ) {
#ifdef MOAB_HAVE_EIGEN
    _mat << diagonal[0], 0.0, 0.0,
            0.0, diagonal[1], 0.0,
            0.0, 0.0, diagonal[2];
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
    _mat[0] = diagonal[0];
    _mat[4] = diagonal[1];
    _mat[8] = diagonal[2];
#endif
  }

  inline Matrix3( double v00, double v01, double v02,
                double v10, double v11, double v12,
                double v20, double v21, double v22 ) {
#ifdef MOAB_HAVE_EIGEN
    _mat << v00, v01, v02,
            v10, v11, v12,
            v20, v21, v22;
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
    _mat[0] = v00; _mat[1] = v01; _mat[2] = v02;
    _mat[3] = v10; _mat[4] = v11; _mat[5] = v12;
    _mat[6] = v20; _mat[7] = v21; _mat[8] = v22;
#endif
  }

  //Copy constructor
  Matrix3 ( const Matrix3 & f)
  {
#ifdef MOAB_HAVE_EIGEN
    _mat = f._mat;
#else
    memcpy(_mat, f._mat, size*sizeof(double));
#endif
  }

  //Weird constructors
  template< typename Vector>
  inline Matrix3( const Vector & row0,
                  const Vector & row1,
                  const Vector & row2,
                  const bool isRow) {
#ifdef MOAB_HAVE_EIGEN
    if (isRow) {
      _mat << row0[0], row0[1], row0[2],
              row1[0], row1[1], row1[2],
              row2[0], row2[1], row2[2];
    }
    else {
      _mat << row0[0], row1[0], row2[0],
              row0[1], row1[1], row2[1],
              row0[2], row1[2], row2[2];
    }
#else
    MOAB_DMEMZERO(_mat, Matrix3::size);
    if (isRow) {
      _mat[0] = row0[0]; _mat[1] = row0[1]; _mat[2] = row0[2];
      _mat[3] = row1[0]; _mat[4] = row1[1]; _mat[5] = row1[2];
      _mat[6] = row2[0]; _mat[7] = row2[1]; _mat[8] = row2[2];
    }
    else {
      _mat[0] = row0[0]; _mat[1] = row1[0]; _mat[2] = row2[0];
      _mat[3] = row0[1]; _mat[4] = row1[1]; _mat[5] = row2[1];
      _mat[6] = row0[2]; _mat[7] = row1[2]; _mat[8] = row2[2];
    }
#endif
  }

#ifndef DEPRECATED
  #ifdef __GNUC__
    #define DEPRECATED __attribute__((deprecated))
  #else
    #pragma message("WARNING: You need to implement DEPRECATED for this compiler")
    #define DEPRECATED
  #endif
#endif

  /*
   * \deprecated { Use instead the constructor with explicit fourth argument, bool isRow, above }
   *
   */
  inline Matrix3( const double v[size] ) {
#ifdef MOAB_HAVE_EIGEN
    _mat << v[0], v[1], v[2],
            v[3], v[4], v[5],
            v[6], v[7], v[8];
#else
    memcpy(_mat, v, size*sizeof(double));
#endif
  }

  inline void copyto( double v[Matrix3::size] ) {
#ifdef MOAB_HAVE_EIGEN
    std::copy(_mat.data(), _mat.data()+size, v);
#else
    memcpy(v, _mat, size*sizeof(double));
#endif
  }

  inline Matrix3& operator=( const Matrix3& m ){
#ifdef MOAB_HAVE_EIGEN
    _mat = m._mat;
#else
    memcpy(_mat, m._mat, size*sizeof(double));
#endif
    return *this;
  }

  inline Matrix3& operator=( const double v[size] ){
#ifdef MOAB_HAVE_EIGEN
    _mat << v[0], v[1], v[2],
            v[3], v[4], v[5],
            v[6], v[7], v[8];
#else
    memcpy(_mat, v, size*sizeof(double));
#endif
    return *this;
 }

  inline double* operator[]( unsigned i )
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat.row(i).data();
#else
    return &_mat[i*3]; // Row Major
#endif
  }

  inline const double* operator[]( unsigned i ) const
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat.row(i).data();
#else
    return &_mat[i*3];
#endif
  }

  inline double& operator()(unsigned r, unsigned c)
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat(r,c);
#else
    return _mat[r*3+c];
#endif
  }

  inline double operator()(unsigned r, unsigned c) const
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat(r,c);
#else
    return _mat[r*3+c];
#endif
  }

  inline double& operator()(unsigned i)
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat(i);
#else
    return _mat[i];
#endif
  }

  inline double operator()(unsigned i) const
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat(i);
#else
    return _mat[i];
#endif
  }

  // get pointer to array of nine doubles
  inline double* array()
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat.data();
#else
    return _mat;
#endif
  }

  inline const double* array() const
  {
#ifdef MOAB_HAVE_EIGEN
    return _mat.data();
#else
    return _mat;
#endif
  }

  inline Matrix3& operator+=( const Matrix3& m ){
#ifdef MOAB_HAVE_EIGEN
    _mat += m._mat;
#else
    for (int i=0; i < Matrix3::size; ++i) _mat[i] += m._mat[i];
#endif
    return *this;
  }

  inline Matrix3& operator-=( const Matrix3& m ){
#ifdef MOAB_HAVE_EIGEN
    _mat -= m._mat;
#else
    for (int i=0; i < Matrix3::size; ++i) _mat[i] -= m._mat[i];
#endif
    return *this;
  }

  inline Matrix3& operator*=( double s ){
#ifdef MOAB_HAVE_EIGEN
    _mat *= s;
#else
    for (int i=0; i < Matrix3::size; ++i) _mat[i] *= s;
#endif
    return *this;
  }

  inline Matrix3& operator/=( double s ){
#ifdef MOAB_HAVE_EIGEN
    _mat /= s;
#else
    for (int i=0; i < Matrix3::size; ++i) _mat[i] /= s;
#endif
    return *this;
  }

  inline Matrix3& operator*=( const Matrix3& m ){
#ifdef MOAB_HAVE_EIGEN
    _mat *= m._mat;
#else
    // Uncomment below if you want point-wise multiplication instead (.*)
    // for (int i=0; i < Matrix3::size; ++i) _mat[i] *= m._mat[i];
    std::vector<double> dmat; dmat.assign(_mat, _mat+size);
    _mat[0] = dmat[0] * m._mat[0] + dmat[1] * m._mat[3] + dmat[2] * m._mat[6];
    _mat[1] = dmat[0] * m._mat[1] + dmat[1] * m._mat[4] + dmat[2] * m._mat[7];
    _mat[2] = dmat[0] * m._mat[2] + dmat[1] * m._mat[5] + dmat[2] * m._mat[8];
    _mat[3] = dmat[3] * m._mat[0] + dmat[4] * m._mat[3] + dmat[5] * m._mat[6];
    _mat[4] = dmat[3] * m._mat[1] + dmat[4] * m._mat[4] + dmat[5] * m._mat[7];
    _mat[5] = dmat[3] * m._mat[2] + dmat[4] * m._mat[5] + dmat[5] * m._mat[8];
    _mat[6] = dmat[6] * m._mat[0] + dmat[7] * m._mat[3] + dmat[8] * m._mat[6];
    _mat[7] = dmat[6] * m._mat[1] + dmat[7] * m._mat[4] + dmat[8] * m._mat[7];
    _mat[8] = dmat[6] * m._mat[2] + dmat[7] * m._mat[5] + dmat[8] * m._mat[8];
#endif
	  return *this;
  }

  inline bool is_symmetric() {
    const double EPS = 1e-13;
#ifdef MOAB_HAVE_EIGEN
    if ((fabs(_mat(1) - _mat(3)) < EPS) && (fabs(_mat(2) - _mat(6)) < EPS) && (fabs(_mat(5) - _mat(7)) < EPS))
      return true;
#else
    if ((fabs(_mat[1] - _mat[3]) < EPS) && (fabs(_mat[2] - _mat[6]) < EPS) && (fabs(_mat[5] - _mat[7]) < EPS))
      return true;
#endif
    else return false;
  }

  inline bool is_positive_definite() {
#ifdef MOAB_HAVE_EIGEN
    double subdet6 = _mat(1)*_mat(5)-_mat(2)*_mat(4);
    double subdet7 = _mat(2)*_mat(3)-_mat(0)*_mat(5);
    double subdet8 = _mat(0)*_mat(4)-_mat(1)*_mat(3);
    // Determinant:= d(6)*subdet6 + d(7)*subdet7 + d(8)*subdet8;
    const double det = _mat(6)*subdet6 + _mat(7)*subdet7 + _mat(8)*subdet8;
    return _mat(0) > 0 && subdet8 > 0 && det > 0;
#else
    double subdet6 = _mat[1]*_mat[5]-_mat[2]*_mat[4];
    double subdet7 = _mat[2]*_mat[3]-_mat[0]*_mat[5];
    double subdet8 = _mat[0]*_mat[4]-_mat[1]*_mat[3];
    // Determinant:= d(6)*subdet6 + d(7)*subdet7 + d(8)*subdet8;
    const double det = _mat[6]*subdet6 + _mat[7]*subdet7 + _mat[8]*subdet8;
    return _mat[0] > 0 && subdet8 > 0 && det > 0;
#endif
  }


  template <typename Vector>
  inline ErrorCode eigen_decomposition(Vector& evals, Matrix3& evecs)
  {
    const bool bisSymmetric = this->is_symmetric();
#ifdef MOAB_HAVE_EIGEN
    if (bisSymmetric) {
      Eigen::SelfAdjointEigenSolver<Eigen::Matrix3d> eigensolver(this->_mat);
      if (eigensolver.info() != Eigen::Success)
        return MB_FAILURE;
      const Eigen::SelfAdjointEigenSolver<Eigen::Matrix3d>::RealVectorType& e3evals = eigensolver.eigenvalues();
      evals[0] = e3evals(0); evals[1] = e3evals(1); evals[2] = e3evals(2);
      evecs._mat = eigensolver.eigenvectors(); //.col(1)
      return MB_SUCCESS;
    }
    else {
      MB_CHK_SET_ERR(MB_FAILURE, "Unsymmetric matrix implementation with Eigen3 is currently not provided.");
      // Eigen::EigenSolver<Eigen::Matrix3d> eigensolver(this->_mat, true);
      // if (eigensolver.info() != Eigen::Success)
      //   return MB_FAILURE;
      // const Eigen::EigenSolver<Eigen::Matrix3d>::EigenvalueType& e3evals = eigensolver.eigenvalues().real();
      // evals[0] = e3evals(0); evals[1] = e3evals(1); evals[2] = e3evals(2);
      // evecs._mat = eigensolver.eigenvectors().real(); //.col(1)
      // return MB_SUCCESS;
    }
#else
    int info;
    /* Solve eigenproblem */
    double devreal[3], drevecs[9];
    if (!bisSymmetric) {
      double devimag[3], dlevecs[9], dwork[102];
      char dgeev_opts[2] = {'N', 'V'};
      int N=3,LWORK=102,NL=1,NR=N;
      std::vector<double> devmat; devmat.assign(_mat, _mat+size);
      MOAB_dgeev (&dgeev_opts[0], &dgeev_opts[1],
                  &N, &devmat[0],
                  &N, devreal, devimag,
                  dlevecs, &NL,
                  drevecs, &NR,
                  dwork, &LWORK,
                  &info);
      // The result eigenvalues are ordered as high-->low
      evals[0]=devreal[2]; evals[1]=devreal[1]; evals[2]=devreal[0];
      evecs._mat[0]=drevecs[6]; evecs._mat[1]=drevecs[3]; evecs._mat[2]=drevecs[0];
      evecs._mat[3]=drevecs[7]; evecs._mat[4]=drevecs[4]; evecs._mat[5]=drevecs[1];
      evecs._mat[6]=drevecs[8]; evecs._mat[7]=drevecs[5]; evecs._mat[8]=drevecs[2];
      std::cout << "DGEEV: Optimal work vector: dsize = " << dwork[0] << ".\n";
    }
    else {
      char dgeev_opts[2] = {'V', 'L'};
      const bool find_optimal = false;
      std::vector<int> iwork(18);
      std::vector<double> devmat(9,0.0);
      std::vector<double> dwork(38);
      int N=3,lwork=38,liwork=18;
      devmat[0]=_mat[0]; devmat[1]=_mat[1]; devmat[2]=_mat[2];
      devmat[4]=_mat[4]; devmat[5]=_mat[5]; devmat[8]=_mat[8];
      if (find_optimal)
      {
        int _lwork  = -1;
        int _liwork = -1;
        double query_work_size = 0;
        int query_iwork_size = 0;
        // Make an empty call to find the optimal work vector size
        MOAB_dsyevd ( &dgeev_opts[0], &dgeev_opts[1], &N,
                      NULL, &N, NULL,
                      &query_work_size, &_lwork,
                      &query_iwork_size, &_liwork,
                      &info);
        lwork = (int) query_work_size;
        dwork.resize(lwork);
        liwork = query_iwork_size;
        iwork.resize(liwork);
        std::cout << "DSYEVD: Optimal work vector: dsize = " << lwork << ", and isize = " << liwork << ".\n";
      }

      MOAB_dsyevd ( &dgeev_opts[0], &dgeev_opts[1], &N,
                    &devmat[0], &N, devreal,
                    &dwork[0], &lwork,
                    &iwork[0], &liwork,
                    &info);
      for (int i=0; i < 9; ++i) drevecs[i] = devmat[i];
      // The result eigenvalues are ordered as low-->high, but vectors are in rows of A.
      evals[0]=devreal[0]; evals[1]=devreal[1]; evals[2]=devreal[2];
      evecs._mat[0]=drevecs[0]; evecs._mat[3]=drevecs[1]; evecs._mat[6]=drevecs[2];
      evecs._mat[1]=drevecs[3]; evecs._mat[4]=drevecs[4]; evecs._mat[7]=drevecs[5];
      evecs._mat[2]=drevecs[6]; evecs._mat[5]=drevecs[7]; evecs._mat[8]=drevecs[8];
    }

    if (!info) {
      return MB_SUCCESS;
    }
    else {
      std::cout << "Failure in LAPACK_" << (bisSymmetric ? "DSYEVD" : "DGEEV") << " call for eigen decomposition.\n";
      std::cout << "Failed with error = " << info << ".\n";
      return MB_FAILURE;
    }
#endif
  }

  inline void transpose_inplace()
  {
#ifdef MOAB_HAVE_EIGEN
    _mat.transposeInPlace();
#else
    Matrix3 mtmp(*this);
    _mat[1] = mtmp._mat[3];
    _mat[3] = mtmp._mat[1];
    _mat[2] = mtmp._mat[6];
    _mat[6] = mtmp._mat[2];
    _mat[5] = mtmp._mat[7];
    _mat[7] = mtmp._mat[5];
#endif
  }

  inline Matrix3 transpose() const
  {
#ifdef MOAB_HAVE_EIGEN
    return Matrix3( _mat.transpose() );
#else
    Matrix3 mtmp(*this);
    mtmp._mat[1] = _mat[3];
    mtmp._mat[3] = _mat[1];
    mtmp._mat[2] = _mat[6];
    mtmp._mat[6] = _mat[2];
    mtmp._mat[5] = _mat[7];
    mtmp._mat[7] = _mat[5];
    return mtmp;
#endif
  }

  template <typename Vector>
  inline void copycol(int index, Vector& vol) {
#ifdef MOAB_HAVE_EIGEN
  	_mat.col(index).swap(vol);
#else
    switch(index) {
      case 0:
        _mat[0] = vol[0]; _mat[3] = vol[1]; _mat[6] = vol[2];
        break;
      case 1:
        _mat[1] = vol[0]; _mat[4] = vol[1]; _mat[7] = vol[2];
        break;
      case 2:
        _mat[2] = vol[0]; _mat[5] = vol[1]; _mat[8] = vol[2];
        break;
    }
#endif
  }

  inline void swapcol(int srcindex, int destindex) {
    assert(srcindex < Matrix3::size);
    assert(destindex < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
  	_mat.col(srcindex).swap(_mat.col(destindex));
#else
    CartVect svol = this->vcol<CartVect>(srcindex);
    CartVect dvol = this->vcol<CartVect>(destindex);
    switch(srcindex) {
      case 0:
        _mat[0] = dvol[0]; _mat[3] = dvol[1]; _mat[6] = dvol[2];
        break;
      case 1:
        _mat[1] = dvol[0]; _mat[4] = dvol[1]; _mat[7] = dvol[2];
        break;
      case 2:
        _mat[2] = dvol[0]; _mat[5] = dvol[1]; _mat[8] = dvol[2];
        break;
    }
    switch(destindex) {
      case 0:
        _mat[0] = svol[0]; _mat[3] = svol[1]; _mat[6] = svol[2];
        break;
      case 1:
        _mat[1] = svol[0]; _mat[4] = svol[1]; _mat[7] = svol[2];
        break;
      case 2:
        _mat[2] = svol[0]; _mat[5] = svol[1]; _mat[8] = svol[2];
        break;
    }
#endif
  }

  template <typename Vector>
  inline Vector vcol(int index) const {
    assert(index < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
  	return _mat.col(index);
#else
    switch(index) {
      case 0:
        return Vector(_mat[0], _mat[3], _mat[6]);
      case 1:
        return Vector(_mat[1], _mat[4], _mat[7]);
      case 2:
        return Vector(_mat[2], _mat[5], _mat[8]);
    }
    return Vector(0.0);
#endif
  }

  inline void colscale(int index, double scale) {
    assert(index < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
  	_mat.col(index) *= scale;
#else
    switch(index) {
      case 0:
        _mat[0] *= scale; _mat[3] *= scale; _mat[6] *= scale;
        break;
      case 1:
        _mat[1] *= scale; _mat[4] *= scale; _mat[7] *= scale;
        break;
      case 2:
        _mat[2] *= scale; _mat[5] *= scale; _mat[8] *= scale;
        break;
    }
#endif
  }

  inline void rowscale(int index, double scale) {
    assert(index < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
  	_mat.row(index) *= scale;
#else
    switch(index) {
      case 0:
        _mat[0] *= scale; _mat[1] *= scale; _mat[2] *= scale;
        break;
      case 1:
        _mat[3] *= scale; _mat[4] *= scale; _mat[5] *= scale;
        break;
      case 2:
        _mat[6] *= scale; _mat[7] *= scale; _mat[8] *= scale;
        break;
    }
#endif
  }

  inline CartVect col(int index) const{
    assert(index < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
    Eigen::Vector3d mvec = _mat.col(index);
    return CartVect(mvec[0], mvec[1], mvec[2]);
#else
    switch(index) {
      case 0:
        return CartVect(_mat[0], _mat[3], _mat[6]);
      case 1:
        return CartVect(_mat[1], _mat[4], _mat[7]);
      case 2:
        return CartVect(_mat[2], _mat[5], _mat[8]);
    }
    return CartVect(0.0);
#endif
  }

  inline CartVect row(int index) const{
    assert(index < Matrix3::size);
#ifdef MOAB_HAVE_EIGEN
    Eigen::Vector3d mvec = _mat.row(index);
    return CartVect(mvec[0], mvec[1], mvec[2]);
#else
    switch(index) {
      case 0:
        return CartVect(_mat[0], _mat[1], _mat[2]);
      case 1:
        return CartVect(_mat[3], _mat[4], _mat[5]);
      case 2:
        return CartVect(_mat[6], _mat[7], _mat[8]);
    }
    return CartVect(0.0);
#endif
  }

  friend Matrix3 operator+( const Matrix3& a, const Matrix3& b );
  friend Matrix3 operator-( const Matrix3& a, const Matrix3& b );
  friend Matrix3 operator*( const Matrix3& a, const Matrix3& b );

  inline double determinant() const{
#ifdef MOAB_HAVE_EIGEN
  	return _mat.determinant();
#else
    return (_mat[0] * _mat[4] * _mat[8]
          + _mat[1] * _mat[5] * _mat[6]
          + _mat[2] * _mat[3] * _mat[7]
          - _mat[0] * _mat[5] * _mat[7]
          - _mat[1] * _mat[3] * _mat[8]
          - _mat[2] * _mat[4] * _mat[6]);
#endif
  }

  inline Matrix3 inverse() const {
#ifdef MOAB_HAVE_EIGEN
    return Matrix3(_mat.inverse());
#else
    // return Matrix::compute_inverse( *this, this->determinant() );
    Matrix3 m(0.0);
    const double d_determinant = 1.0/this->determinant();
    m._mat[0] = d_determinant * (_mat[4] * _mat[8] - _mat[5] * _mat[7]);
    m._mat[1] = d_determinant * (_mat[2] * _mat[7] - _mat[8] * _mat[1]);
    m._mat[2] = d_determinant * (_mat[1] * _mat[5] - _mat[4] * _mat[2]);
    m._mat[3] = d_determinant * (_mat[5] * _mat[6] - _mat[8] * _mat[3]);
    m._mat[4] = d_determinant * (_mat[0] * _mat[8] - _mat[6] * _mat[2]);
    m._mat[5] = d_determinant * (_mat[2] * _mat[3] - _mat[5] * _mat[0]);
    m._mat[6] = d_determinant * (_mat[3] * _mat[7] - _mat[6] * _mat[4]);
    m._mat[7] = d_determinant * (_mat[1] * _mat[6] - _mat[7] * _mat[0]);
    m._mat[8] = d_determinant * (_mat[0] * _mat[4] - _mat[3] * _mat[1]);
    return m;
#endif
  }

  inline bool invert() {
    bool invertible=false;
    double d_determinant;
#ifdef MOAB_HAVE_EIGEN
    Eigen::Matrix3d invMat;
    _mat.computeInverseAndDetWithCheck(invMat, d_determinant, invertible);
    if (!Util::is_finite(d_determinant))
      return false;
    _mat = invMat;
    return invertible;
#else
    d_determinant = this->determinant();
    if (d_determinant > 1e-13) invertible = true;
    d_determinant = 1.0/d_determinant; // invert the determinant
    std::vector<double> _m; _m.assign(_mat, _mat+size);
    _mat[0] = d_determinant * (_m[4] * _m[8] - _m[5] * _m[7]);
    _mat[1] = d_determinant * (_m[2] * _m[7] - _m[8] * _m[1]);
    _mat[2] = d_determinant * (_m[1] * _m[5] - _m[4] * _m[2]);
    _mat[3] = d_determinant * (_m[5] * _m[6] - _m[8] * _m[3]);
    _mat[4] = d_determinant * (_m[0] * _m[8] - _m[6] * _m[2]);
    _mat[5] = d_determinant * (_m[2] * _m[3] - _m[5] * _m[0]);
    _mat[6] = d_determinant * (_m[3] * _m[7] - _m[6] * _m[4]);
    _mat[7] = d_determinant * (_m[1] * _m[6] - _m[7] * _m[0]);
    _mat[8] = d_determinant * (_m[0] * _m[4] - _m[3] * _m[1]);
#endif
    return invertible;
  }

  // Calculate determinant of 2x2 submatrix composed of the
  // elements not in the passed row or column.
  inline double subdet( int r, int c ) const{
    assert(r >= 0 && c >= 0);
    if (r < 0 || c < 0) return DBL_MAX;
#ifdef MOAB_HAVE_EIGEN
    const int r1 = (r+1)%3, r2 = (r+2)%3;
    const int c1 = (c+1)%3, c2 = (c+2)%3;
    return _mat(r1,c1)*_mat(r2,c2) - _mat(r1,c2)*_mat(r2,c1);
#else
    const int r1 = Matrix3::size*((r+1)%3), r2 = Matrix3::size*((r+2)%3);
    const int c1 = (c+1)%3, c2 = (c+2)%3;
    return _mat[r1+c1]*_mat[r2+c2] - _mat[r1+c2]*_mat[r2+c1];
#endif
  }

  inline void print( std::ostream& s ) const {
#ifdef MOAB_HAVE_EIGEN
    s <<  "| " << _mat(0) << " " << _mat(1) << " " << _mat(2)
      << " | " << _mat(3) << " " << _mat(4) << " " << _mat(5)
      << " | " << _mat(6) << " " << _mat(7) << " " << _mat(8)
      << " |" ;
#else
    s <<  "| " << _mat[0] << " " << _mat[1] << " " << _mat[2]
      << " | " << _mat[3] << " " << _mat[4] << " " << _mat[5]
      << " | " << _mat[6] << " " << _mat[7] << " " << _mat[8]
      << " |" ;
#endif
  }

}; //class Matrix3


template< typename Vector>
inline Matrix3 outer_product( const Vector & u,
                              const Vector & v ) {
  return Matrix3( u[0] * v[0], u[0] * v[1], u[0] * v[2],
                  u[1] * v[0], u[1] * v[1], u[1] * v[2],
                  u[2] * v[0], u[2] * v[1], u[2] * v[2] );
}

inline Matrix3 operator+( const Matrix3& a, const Matrix3& b ) {
#ifdef MOAB_HAVE_EIGEN
  return Matrix3(a._mat + b._mat);
#else
  Matrix3 s(a);
  for (int i=0; i < Matrix3::size; ++i) s(i) += b._mat[i];
  return s;
#endif
}

inline Matrix3 operator-( const Matrix3& a, const Matrix3& b ){
#ifdef MOAB_HAVE_EIGEN
  return Matrix3(a._mat - b._mat);
#else
  Matrix3 s(a);
  for (int i=0; i < Matrix3::size; ++i) s(i) -= b._mat[i];
  return s;
#endif
}

inline Matrix3 operator*( const Matrix3& a, const Matrix3& b ) {
#ifdef MOAB_HAVE_EIGEN
  return Matrix3(a._mat * b._mat);
#else
  return Matrix::mmult3(a, b);
#endif
}

template< typename T>
inline std::vector< T> operator*( const Matrix3& m, const std::vector<T> & v){
		return moab::Matrix::matrix_vector( m, v);
}

template< typename T>
inline std::vector< T> operator*( const std::vector<T>& v, const Matrix3& m){
		return moab::Matrix::vector_matrix( v, m);
}

inline CartVect operator*( const Matrix3& m,  const CartVect& v){
		return moab::Matrix::matrix_vector( m, v);
}

inline CartVect operator*( const CartVect& v, const Matrix3& m){
		return moab::Matrix::vector_matrix( v, m);
}

} // namespace moab


#ifndef MOAB_HAVE_EIGEN
#undef MOAB_DMEMZERO
#endif

#ifndef MOAB_MATRIX3_OPERATORLESS
#define MOAB_MATRIX3_OPERATORLESS
inline std::ostream& operator<<( std::ostream& s, const moab::Matrix3& m ){
  return s <<  "| " << m(0,0) << " " << m(0,1) << " " << m(0,2)
           << " | " << m(1,0) << " " << m(1,1) << " " << m(1,2)
           << " | " << m(2,0) << " " << m(2,1) << " " << m(2,2)
           << " |" ;
}
#endif//MOAB_MATRIX3_OPERATORLESS
#endif //MOAB_MATRIX3_HPP
