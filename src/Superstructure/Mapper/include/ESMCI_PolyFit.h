#ifndef ESMCI_PolyFit_H
#define ESMCI_PolyFit_H

#include "ESMCI_Macros.h"
#include <iostream>
#include <vector>
#include <cassert>
#include <cmath>
#include <algorithm>
#include <complex.h>
#include "lapacke.h"
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"

namespace ESMCI{
  namespace MapperUtil{
    typedef enum{
      POLY_FIT_LS_LAPACK
    } PolyFitAlg;

    namespace PolyFitUtil{

      inline lapack_int LAPACKE_Gels(int matrix_layout, char trans,
        lapack_int m, lapack_int n, lapack_int nrhs,
        float *A, lapack_int lda, float *B, lapack_int ldb)
      {
        return LAPACKE_sgels(matrix_layout, trans, m, n, nrhs, A, lda, B, ldb);
      }

      inline lapack_int LAPACKE_Gels(int matrix_layout, char trans,
        lapack_int m, lapack_int n, lapack_int nrhs,
        double *A, lapack_int lda, double *B, lapack_int ldb)
      {
        return LAPACKE_dgels(matrix_layout, trans, m, n, nrhs, A, lda, B, ldb);
      }

      /* Use LAPACK to minimize Least square errors and fit a polynomial (of maximum
       * degree, max_deg) on a 2D user data set. The 2D user data is specified via
       * xvals (x values for the user data) and yvals (y values for the user data),
       * and the resulting fit polynomial coefficients are stored (returned via) in
       * coeffs
       */
      template<typename T>
      inline int LAPACK_2D_Solver(int max_deg, const std::vector<T>& xvals, const std::vector<T> &yvals, std::vector<T> &coeffs)
      {
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

        T *A = (T *)calloc(NUM_ROWS_IN_A * NUM_COLS_IN_A, sizeof(T));
        T *B = (T *)calloc(NUM_ROWS_IN_B * NUM_COLS_IN_B, sizeof(T));
        assert(A && B);

        /*
         */ 
        for(int i=0; i<num_obs; i++){
          for(int j=0; j<NUM_COLS_IN_A; j++){
            A[i * NUM_COLS_IN_A + j] = powf(xvals[i], j);
          }
          B[i] = yvals[i];
        }

        info = LAPACKE_Gels(LAPACK_ROW_MAJOR, 'N', m, n, nrhs, A, lda, B, ldb);
        /* info == 0 is success */
        if(info < 0){
          std::cout << "LAPACKE_sgels failed, the " << -info
            << "th arg in the input array, A[" << -info << "] ="
            << A[-info] << " is invalid\n";
          return ESMF_FAILURE;
        }
        else if(info > 0){
          std::cout << "LAPACKE_sgels failed, the " << info
            << "th diagonal element of the triangular factor of input array, A == 0, "
            << "so A has no full rank and LES solution cannot be computed\n";
          return ESMF_FAILURE;
        }

        coeffs.resize(max_deg + 1);
        for(int i=0; i<max_deg+1; i++)
        {
          coeffs[i] = B[max_deg - i];
        }
        return ESMF_SUCCESS;
      }

      template<typename VType>
      VType GetMean(const std::vector<VType> &vals)
      {
        if(vals.size() == 0){
          return 0;
        }
        VType init_val = 0;
        VType sum = std::accumulate(vals.cbegin(), vals.cend(), init_val);
        return sum/vals.size();
      }

      /* In the pair returned the first one is the mean and the second is
       * the sample standard deviation
       */ 
      template<typename VType>
      std::pair<VType, VType> GetMeanAndStdDev(const std::vector<VType> &vals)
      {

        assert(vals.size() > 0);

        if(vals.size() == 1){
          return std::pair<VType, VType>(vals[0], 0);
        }

        /* Find sample mean */
        VType mean = GetMean(vals);
        typename std::vector<VType> vals_minus_mean_sq(vals.size(), 0);
        typename std::vector<VType>::const_iterator citer = vals.cbegin();
        for(typename std::vector<VType>::iterator iter = vals_minus_mean_sq.begin();
            (iter != vals_minus_mean_sq.end()) &&
            (citer != vals.cend()); ++iter, ++citer){
          *iter = (*citer - mean) * (*citer - mean);
        }
        VType init_val = 0;
        VType sum = std::accumulate(vals_minus_mean_sq.cbegin(),
                      vals_minus_mean_sq.cend(), init_val);
        /* Find the sample standard deviation */
        VType stddev = sqrt(sum/(vals.size() - 1));

        return std::pair<VType, VType>(mean, stddev);
      }

      template<typename VType>
      PolyCSInfo<VType> CenterAndScale(std::vector<VType> &vals)
      {
        PolyCSInfo<VType> csinfo = GetMeanAndStdDev(vals);
        csinfo.center_and_scale(vals);
        return csinfo;
      }

    } // namespace PolyFitUtil

    /* Fit a polynomial of degree, max_deg, on a 2D user data set with xvalues
     * specified via xvals and y values specified via yvals. The resulting fit
     * polynomial is returned in poly
     */
    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, int max_deg, const std::vector<VType>& xvals, const std::vector<VType>& yvals, UVIDPoly<CType> &poly)
    {
      std::vector<CType> coeffs;
      assert(alg == POLY_FIT_LS_LAPACK);

      std::vector<VType> xvals_centered_and_scaled = xvals;
      PolyCSInfo<VType> csinfo = PolyFitUtil::CenterAndScale(xvals_centered_and_scaled);

      int ret = PolyFitUtil::LAPACK_2D_Solver(max_deg, xvals_centered_and_scaled, yvals, coeffs);
      assert(ret == ESMF_SUCCESS);

      poly.set_coeffs(coeffs); 
      poly.set_cs_info(csinfo);
      return ESMF_SUCCESS;
    }

  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_PolyFit_H
