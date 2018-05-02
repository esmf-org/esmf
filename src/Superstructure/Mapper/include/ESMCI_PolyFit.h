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
      POLY_FIT_2D_LS_LAPACK
    } PolyFitAlg;

    inline int LAPACK_2D_Solver(const std::vector<float>& xvals, const std::vector<float> &yvals, std::vector<float> &coeffs)
    {
      const int MAX_DEG_POLY = 2;
      if(xvals.size() == 0) return 0;
      assert(xvals.size() == yvals.size());
      // Using LAPACK to solve minimize || Ax - B ||
      int num_obs = xvals.size();
      int NUM_ROWS_IN_A = num_obs;
      const int NUM_COLS_IN_A = MAX_DEG_POLY + 1;
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

      coeffs.resize(MAX_DEG_POLY + 1);
      for(int i=0; i<MAX_DEG_POLY+1; i++)
      {
        coeffs[i] = B[i];
      }
      return 0;
    }

    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, const std::vector<VType>& xvals, const std::vector<VType>& yvals, UVIDPoly<CType> &poly)
    {
      std::vector<CType> coeffs;
      assert(alg == POLY_FIT_2D_LS_LAPACK);

      int ret = LAPACK_2D_Solver(xvals, yvals, coeffs);
      assert(ret == 0);

      poly.set_coeffs(coeffs); 
      return 0;
    }

  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_PolyFit_H
