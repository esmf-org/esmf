#ifndef ESMCI_PolyFit_H
#define ESMCI_PolyFit_H

#include <iostream>
#include <vector>
#include <cassert>
#include <cmath>
#include <complex.h>
#include "lapacke.h"
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"

namespace ESMCI{
  namespace MapperUtil{
    typedef enum{
      POLY_FIT_LS_LAPACK
    } PolyFitAlg;

    inline int LAPACK_2D_Solver(int max_deg, const std::vector<float>& xvals, const std::vector<float> &yvals, std::vector<float> &coeffs)
    {
      //const int MAX_DEG_POLY = 2;
      if(xvals.size() == 0) return 0;
      assert(xvals.size() == yvals.size());
      // Using LAPACK to solve minimize || Ax - B ||
      int num_obs = xvals.size();
      int NUM_ROWS_IN_A = num_obs;
      const int NUM_COLS_IN_A = max_deg + 1;
      int NUM_ROWS_IN_B = num_obs;
      const int NUM_COLS_IN_B = 1;

      lapack_int info, m, n, lda, ldb, nrhs;
      m = NUM_ROWS_IN_A;
      n = NUM_COLS_IN_A;
      lda = NUM_COLS_IN_A;
      ldb = NUM_COLS_IN_B;
      nrhs = NUM_COLS_IN_B;

      float *A = (float *)calloc(NUM_ROWS_IN_A * NUM_COLS_IN_A, sizeof(float));
      float *B = (float *)calloc(NUM_ROWS_IN_B * NUM_COLS_IN_B, sizeof(float));
      assert(A && B);

      for(int i=0; i<num_obs; i++){
        for(int j=0; j<NUM_COLS_IN_A; j++){
          A[i * NUM_COLS_IN_A + j] = powf(xvals[i], j);
        }
        B[i] = yvals[i];
      }

      info = LAPACKE_sgels(LAPACK_ROW_MAJOR, 'N', m, n, nrhs, A, lda, B, ldb);
      /* info == 0 is success */
      if(info < 0){
        std::cout << "LAPACKE_sgels failed, the " << -info
          << "th arg in the input array, A[" << -info << "] ="
          << A[-info] << " is invalid\n";
        return 1;
      }
      else if(info > 0){
        std::cout << "LAPACKE_sgels failed, the " << info
          << "th diagonal element of the triangular factor of input array, A == 0, "
          << "so A has no full rank and LES solution cannot be computed\n";
        return 1;
      }

      coeffs.resize(max_deg + 1);
      for(int i=0; i<max_deg+1; i++)
      {
        coeffs[i] = B[max_deg - i];
      }
      return 0;
    }

    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, int max_deg, const std::vector<VType>& xvals, const std::vector<VType>& yvals, UVIDPoly<CType> &poly)
    {
      std::vector<CType> coeffs;
      assert(alg == POLY_FIT_LS_LAPACK);

      int ret = LAPACK_2D_Solver(max_deg, xvals, yvals, coeffs);
      assert(ret == 0);

      poly.set_coeffs(coeffs); 
      return 0;
    }

  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_PolyFit_H
