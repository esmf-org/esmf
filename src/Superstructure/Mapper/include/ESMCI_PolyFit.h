#ifndef ESMCI_PolyFit_H
#define ESMCI_PolyFit_H

#include "ESMCI_Macros.h"
#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <cassert>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <random>
#include <complex.h>
#include "lapacke.h"
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyTwoDV.h"

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
      inline int LAPACK_Minimize_LSE(int max_deg, const std::vector<T>& xvals, const std::vector<T> &yvals, std::vector<T> &coeffs)
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

      /* Use LAPACK to minimize Least square errors and fit a polynomial
       * (of maximum degree, max_deg) on a 3D user data set. The 3D user
       * data is specified via x1vals, x2vals (x values for the user data)
       * and yvals (y values for the user data),
       * and the resulting fit polynomial coefficients are stored
       * (returned via) in coeffs
       */
      template<typename T>
      inline int LAPACK_Minimize_LSE(int max_deg,
        const std::vector<T>& x1vals,
        const std::vector<T>& x2vals,
        const std::vector<T> &yvals,
        std::vector<T> &coeffs)
      {
        int NCOEFFS_IN_2DEG_2VAR_POLY = 6;
        assert(x1vals.size() == yvals.size());
        assert(x2vals.size() == yvals.size());

        assert(max_deg == 2);
        // Using LAPACK to solve minimize || Ax - B ||
        int num_obs = static_cast<int>(x1vals.size());
        int NUM_ROWS_IN_A = num_obs;
        const int NUM_COLS_IN_A = NCOEFFS_IN_2DEG_2VAR_POLY;
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

        for(int i=0; i<num_obs; i++){
          A[i * NUM_COLS_IN_A + 0] =  x1vals[i] * x1vals[i];
          A[i * NUM_COLS_IN_A + 1] =  x1vals[i] * x2vals[i];
          A[i * NUM_COLS_IN_A + 2] =  x2vals[i] * x2vals[i];
          A[i * NUM_COLS_IN_A + 3] =  x1vals[i];
          A[i * NUM_COLS_IN_A + 4] =  x2vals[i];
          A[i * NUM_COLS_IN_A + 5] =  1;
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

        coeffs.resize(NCOEFFS_IN_2DEG_2VAR_POLY);
        /*
         * We represent polynomials in the form :
         * b3 * x1_sq + b5 * x1 * x2 + b4 * x2_sq + b1 * x1 + b2 * x2 + b0
         */
        coeffs[0] = B[0];
        coeffs[1] = B[1];
        coeffs[2] = B[2];
        coeffs[3] = B[3];
        coeffs[4] = B[4];
        coeffs[5] = B[5];

        return ESMF_SUCCESS;
      }

      template<typename T>
      inline int LAPACK_Minimize_LSE_old(int max_deg,
        const std::vector<T>& x1vals,
        const std::vector<T>& x2vals,
        const std::vector<T> &yvals,
        std::vector<T> &coeffs)
      {
        assert(x1vals.size() == yvals.size());
        assert(x2vals.size() == yvals.size());

        assert(max_deg == 2);
        // Using LAPACK to solve minimize || Ax - B ||
        int num_obs = static_cast<int>(x1vals.size());
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
         * Initialize A
         * [  n, Sum(x1i), Sum(x2i), Sum(x1i_sq), Sum(x2i_sq), Sum(x1i * x2i);
         *    Sum(x1i), Sum(x1i_sq), Sum(x1i * x2i), Sum(x1i_cubed),
         *      Sum(x1i * x2i_sq), Sum(x1i_sq * x2i);
         *    Sum(x2i), Sum(x1i * x2i), Sum(x2i_sq), Sum(x1i_sq * x2i),
         *      Sum(x2i_cubed), Sum(x1i * x2i_sq);
         *    Sum(x1i_sq), Sum(x1i_cubed), Sum(x1i_sq * x2i), Sum(x1i_rfour),
         *      Sum(x1i_sq * x2i_sq), Sum(x1i_cubed * x2i);
         *    Sum(x2i_sq), Sum(x1i * x2i_sq), Sum(x2i_cubed),
         *      Sum(x1i_sq * x2i_sq), Sum(x2i_rfour), Sum(x1i * x2i_cubed);
         *    Sum(x1i * x2i), Sum(x1i_sq * x2i), Sum(x1i * x2i_sq),
         *      Sum(x1i_cubed * x2i), Sum(x1i * x2i_cubed),
         *      Sum(x1i_sq * x2i_sq); ]
         *
         *  Initialize B
         * [  Sum(yi), Sum(x1i * yi), Sum(x2i * yi), Sum(x1i_sq * yi),
         *      Sum(x2i_sq * yi), Sum(x1i * x2i * yi) ]
         */

        T ival = 0;
        T sum_x1i = std::accumulate(x1vals.begin(), x1vals.end(), ival);
        T sum_x2i = std::accumulate(x2vals.begin(), x2vals.end(), ival);
        std::vector<T> x1i_sq(x1vals);
        std::transform(x1i_sq.begin(), x1i_sq.end(), x1i_sq.begin(),
                        x1i_sq.begin(), std::multiplies<T>());
        T sum_x1i_sq = std::accumulate(x1i_sq.begin(), x1i_sq.end(), ival);
        std::vector<T> x2i_sq(x2vals);
        std::transform(x2i_sq.begin(), x2i_sq.end(), x2i_sq.begin(),
                        x2i_sq.begin(), std::multiplies<T>());
        T sum_x2i_sq = std::accumulate(x2i_sq.begin(), x2i_sq.end(), ival);
        std::vector<T> x1i_x2i(x1vals);
        std::transform(x1i_x2i.begin(), x1i_x2i.end(), x2vals.begin(),
                        x1i_x2i.begin(), std::multiplies<T>());
        T sum_x1i_x2i = std::accumulate(x1i_x2i.begin(), x1i_x2i.end(), ival);

        std::vector<T> x1i_cubed(x1i_sq);
        std::transform(x1i_cubed.begin(), x1i_cubed.end(), x1vals.begin(),
                        x1i_cubed.begin(), std::multiplies<T>());
        T sum_x1i_cubed = std::accumulate(x1i_cubed.begin(), x1i_cubed.end(),
                            ival);
        std::vector<T> x1i_x2isq(x1vals);
        std::transform(x1i_x2isq.begin(), x1i_x2isq.end(), x2i_sq.begin(),
                        x1i_x2isq.begin(), std::multiplies<T>());
        T sum_x1i_x2isq = std::accumulate(x1i_x2isq.begin(), x1i_x2isq.end(),
                            ival);
        std::vector<T> x1isq_x2i(x1i_sq);
        std::transform(x1isq_x2i.begin(), x1isq_x2i.end(), x2vals.begin(),
                        x1isq_x2i.begin(), std::multiplies<T>());
        T sum_x1isq_x2i = std::accumulate(x1isq_x2i.begin(),
                            x1isq_x2i.end(), ival);
        std::vector<T> x2i_cubed(x2i_sq);
        std::transform(x2i_cubed.begin(), x2i_cubed.end(), x2vals.begin(),
                        x2i_cubed.begin(), std::multiplies<T>());
        T sum_x2i_cubed = std::accumulate(x2i_cubed.begin(),
                            x2i_cubed.end(), ival);
        std::vector<T> x1i_rfour(x1i_cubed);
        std::transform(x1i_rfour.begin(), x1i_rfour.end(), x1vals.begin(),
                        x1i_rfour.begin(), std::multiplies<T>());
        T sum_x1i_rfour = std::accumulate(x1i_rfour.begin(),
                            x1i_rfour.end(), ival);
        std::vector<T> x1isq_x2isq(x1i_sq);
        std::transform(x1isq_x2isq.begin(), x1isq_x2isq.end(), x2i_sq.begin(),
                        x1isq_x2isq.begin(), std::multiplies<T>());
        T sum_x1isq_x2isq = std::accumulate(x1isq_x2isq.begin(),
                              x1isq_x2isq.end(), ival);
        std::vector<T> x1icubed_x2i(x1i_cubed);
        std::transform(x1icubed_x2i.begin(), x1icubed_x2i.end(), x2vals.begin(),
                        x1icubed_x2i.begin(), std::multiplies<T>());
        T sum_x1icubed_x2i = std::accumulate(x1icubed_x2i.begin(),
                                x1icubed_x2i.end(), ival);
        std::vector<T> x2i_rfour(x2i_cubed);
        std::transform(x2i_rfour.begin(), x2i_rfour.end(), x2vals.begin(),
                        x2i_rfour.begin(), std::multiplies<T>());
        T sum_x2i_rfour = std::accumulate(x2i_rfour.begin(),
                            x2i_rfour.end(), ival);
        std::vector<T> x1i_x2icubed(x1vals);
        std::transform(x1i_x2icubed.begin(), x1i_x2icubed.end(),
                        x2i_cubed.begin(),
                        x1i_x2icubed.begin(), std::multiplies<T>());
        T sum_x1i_x2icubed = std::accumulate(x1i_x2icubed.begin(),
                              x1i_x2icubed.end(), ival);

        T sum_yi = std::accumulate(yvals.begin(), yvals.end(), ival);
        std::vector<T> x1i_yi(x1vals);
        std::transform(x1i_yi.begin(), x1i_yi.end(), yvals.begin(),
                        x1i_yi.begin(), std::multiplies<T>());

        T sum_x1i_yi = std::accumulate(x1i_yi.begin(), x1i_yi.end(), ival);
        std::vector<T> x2i_yi(x2vals);
        std::transform(x2i_yi.begin(), x2i_yi.end(), yvals.begin(),
                        x2i_yi.begin(), std::multiplies<T>());

        T sum_x2i_yi = std::accumulate(x2i_yi.begin(), x2i_yi.end(), ival);
        std::vector<T> x1isq_yi(x1i_sq);
        std::transform(x1isq_yi.begin(), x1isq_yi.end(), yvals.begin(),
                        x1isq_yi.begin(), std::multiplies<T>());

        T sum_x1isq_yi = std::accumulate(x1isq_yi.begin(),
                          x1isq_yi.end(), ival);

        std::vector<T> x2isq_yi(x2i_sq);
        std::transform(x2isq_yi.begin(), x2isq_yi.end(), yvals.begin(),
                        x2isq_yi.begin(), std::multiplies<T>());

        T sum_x2isq_yi = std::accumulate(x2isq_yi.begin(),
                          x2isq_yi.end(), ival);

        std::vector<T> x1i_x2i_yi(x1i_yi);
        std::transform(x1i_x2i_yi.begin(), x1i_x2i_yi.end(), x2vals.begin(),
                        x1i_x2i_yi.begin(), std::multiplies<T>());
        T sum_x1i_x2i_yi = std::accumulate(x1i_x2i_yi.begin(),
                            x1i_x2i_yi.end(), ival);

        A[0 * NUM_COLS_IN_A + 0] = num_obs;
        A[0 * NUM_COLS_IN_A + 1] = A[1 * NUM_COLS_IN_A + 0] = sum_x1i;
        A[0 * NUM_COLS_IN_A + 2] = A[2 * NUM_COLS_IN_A + 0] = sum_x2i;
        A[0 * NUM_COLS_IN_A + 3] = A[3 * NUM_COLS_IN_A + 0] = sum_x1i_sq;
        A[0 * NUM_COLS_IN_A + 4] = A[4 * NUM_COLS_IN_A + 0] = sum_x2i_sq;
        A[0 * NUM_COLS_IN_A + 5] = A[5 * NUM_COLS_IN_A + 0] = sum_x1i_x2i;

        A[1 * NUM_COLS_IN_A + 1] = sum_x1i_sq;
        A[1 * NUM_COLS_IN_A + 2] = A[2 * NUM_COLS_IN_A + 1] = sum_x1i_x2i;
        A[1 * NUM_COLS_IN_A + 3] = A[3 * NUM_COLS_IN_A + 1] = sum_x1i_cubed;
        A[1 * NUM_COLS_IN_A + 4] = A[4 * NUM_COLS_IN_A + 1] = sum_x1i_x2isq;
        A[1 * NUM_COLS_IN_A + 5] = A[5 * NUM_COLS_IN_A + 1] = sum_x1isq_x2i;

        A[2 * NUM_COLS_IN_A + 2] = sum_x2i_sq;
        A[2 * NUM_COLS_IN_A + 3] = A[3 * NUM_COLS_IN_A + 2] = sum_x1isq_x2i;
        A[2 * NUM_COLS_IN_A + 4] = A[4 * NUM_COLS_IN_A + 2] = sum_x2i_cubed;
        A[2 * NUM_COLS_IN_A + 5] = A[5 * NUM_COLS_IN_A + 2] = sum_x1i_x2isq;

        A[3 * NUM_COLS_IN_A + 3] = sum_x1i_rfour;
        A[3 * NUM_COLS_IN_A + 4] = A[4 * NUM_COLS_IN_A + 3] = sum_x1isq_x2isq;
        A[3 * NUM_COLS_IN_A + 5] = A[5 * NUM_COLS_IN_A + 3] = sum_x1icubed_x2i;

        A[4 * NUM_COLS_IN_A + 4] = sum_x2i_rfour;
        A[4 * NUM_COLS_IN_A + 5] = A[5 * NUM_COLS_IN_A + 4] = sum_x1i_x2icubed;

        A[5 * NUM_COLS_IN_A + 5] = sum_x1isq_x2isq;

        B[0] = sum_yi;
        B[1] = sum_x1i_yi;
        B[2] = sum_x2i_yi;
        B[3] = sum_x1isq_yi;
        B[4] = sum_x2isq_yi;
        B[5] = sum_x1i_x2i_yi;

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

        int NCOEFFS_IN_2DEG_2VAR_POLY = 6;
        coeffs.resize(NCOEFFS_IN_2DEG_2VAR_POLY);
        /* The polynomial solved is :
         * b0 + b1 * x1 + b2 * x2 + b3 * x1_sq + b4 * x2_sq + b5 * x1 * x2
         * We represent polynomials in the form :
         * b3 * x1_sq + b5 * x1 * x2 + b4 * x2_sq + b1 * x1 + b2 * x2 + b0
         */
        coeffs[0] = B[3];
        coeffs[1] = B[5];
        coeffs[2] = B[4];
        coeffs[3] = B[1];
        coeffs[4] = B[2];
        coeffs[5] = B[0];

        return ESMF_SUCCESS;
      }

      /* Center and scale the given values, vals, and return the 
       * center and scale information using a PolyCSInfo type obj
       */
      template<typename VType>
      PolyCSInfo<VType> CenterAndScale(std::vector<VType> &vals)
      {
        PolyCSInfo<VType> csinfo = PolyCSInfo<VType>::create_poly_csinfo(vals);
        csinfo.center_and_scale(vals);
        return csinfo;
      }

      template<typename VType>
      class PairUniqByFirst{
        public:
        bool operator()( const std::pair<VType, VType> &a,
                        const std::pair<VType, VType> &b){
          return (a.first < b.first);
        }
      };

      template<typename VType>
      class TupleUniqByFirstAndSecond{
        public:
        bool operator()( const std::tuple<VType, VType, VType> &a,
                        const std::tuple<VType, VType, VType> &b){
          return (  (std::get<0>(a) < std::get<0>(b)) ||
                    (std::get<1>(a) < std::get<1>(b)) );
        }
      };

      /* Find unique pairs of xvals and yvals such that no two
       * pairs have the same xval
       */
      template<typename VType>
      void find_unique( const std::vector<VType> &xvals,
                        const std::vector<VType> &yvals,
                        std::vector<VType> &uniq_xvals,
                        std::vector<VType> &uniq_yvals)
      {
        std::set<std::pair<VType, VType>, PairUniqByFirst<VType> > xyvals_uniq;
        assert(xvals.size() == yvals.size());
        for(typename std::vector<VType>::const_iterator
              citer1 = xvals.cbegin(),
              citer2 = yvals.cbegin();
              (citer1 != xvals.cend()) && (citer2 != yvals.cend());
              ++citer1, ++citer2){
          xyvals_uniq.insert(std::pair<VType, VType>(*citer1, *citer2));
        }
        uniq_xvals.reserve(xvals.size());
        uniq_yvals.reserve(yvals.size());
        for(typename std::set<std::pair<VType, VType> >::const_iterator
            citer = xyvals_uniq.cbegin();
            citer != xyvals_uniq.cend(); ++citer){
          uniq_xvals.push_back((*citer).first);
          uniq_yvals.push_back((*citer).second);
        }
      }
    } // namespace PolyFitUtil

    /* Fit a polynomial of degree, max_deg, on a 2D user data set with xvalues
     * specified via xvals and y values specified via yvals. The resulting fit
     * polynomial is returned in poly
     */
    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, int max_deg, const std::vector<VType>& xvals, const std::vector<VType>& yvals, UVIDPoly<CType> &poly)
    {
      const int MIN_VALS_REQD_FOR_POLYFIT = 3;
      std::vector<CType> coeffs;
      assert(alg == POLY_FIT_LS_LAPACK);

      std::vector<VType> yvals_uniq;
      std::vector<VType> xvals_centered_and_scaled;
      PolyFitUtil::find_unique(xvals, yvals, xvals_centered_and_scaled, yvals_uniq);
      if(yvals_uniq.size() < MIN_VALS_REQD_FOR_POLYFIT){
        std::cout << "WARNING: Not enough unique vals (" << yvals_uniq.size() << ") available for a polyfit\n";
        return ESMF_FAILURE;
      }

      PolyCSInfo<VType> csinfo = PolyFitUtil::CenterAndScale(xvals_centered_and_scaled);

      int ret = PolyFitUtil::LAPACK_Minimize_LSE(max_deg, xvals_centered_and_scaled, yvals_uniq, coeffs);
      assert(ret == ESMF_SUCCESS);

      poly.set_coeffs(coeffs); 
      poly.set_cs_info(csinfo);
      return ESMF_SUCCESS;
    }

    /* Find an approximate fit for a two variable polynomial */
    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, int max_deg,
          const std::vector<VType> &x1vals,
          const std::vector<VType> &x2vals,
          const std::vector<VType> &yvals,
          TwoDVIDPoly<CType> &poly)
    {
      std::vector<CType> coeffs;
      assert(alg == POLY_FIT_LS_LAPACK);

      std::vector<VType> x1vals_centered_and_scaled = x1vals;
      std::vector<VType> x2vals_centered_and_scaled = x2vals;

      std::vector<PolyCSInfo<VType> > csinfos;
      csinfos.push_back(PolyFitUtil::CenterAndScale(x1vals_centered_and_scaled));

      csinfos.push_back(PolyFitUtil::CenterAndScale(x2vals_centered_and_scaled));

      int ret = PolyFitUtil::LAPACK_Minimize_LSE(max_deg,
                  x1vals_centered_and_scaled,
                  x2vals_centered_and_scaled,
                  yvals,
                  coeffs);
      assert(ret == ESMF_SUCCESS);

      poly.set_coeffs(coeffs);
      poly.set_cs_infos(csinfos);

      return ESMF_SUCCESS;
    }

    template<typename CType, typename VType>
    int PolyFit(PolyFitAlg alg, int max_deg,
          const TwoDVIDPoly<CType> &ipoly,
          TwoDVIDPoly<CType> &opoly,
          const std::vector<VType> &ux1vals,
          const std::vector<VType> &ux2vals)
    {
      int ret = ESMF_SUCCESS;
      std::vector<VType> yvals;
      std::map<VType, bool> yvals_map;
      std::vector<VType> x1vals;
      std::vector<VType> x2vals;

      assert(alg == POLY_FIT_LS_LAPACK);
      assert(ux1vals.size() == ux2vals.size());
      assert(ux1vals.size() > 0);

      VType x1vals_max = 0;
      VType x2vals_max = 0;
      for(typename std::vector<VType>::const_iterator citer1 = ux1vals.cbegin(),
          citer2 = ux2vals.cbegin();
          (citer1 != ux1vals.cend()) && (citer2 != ux2vals.cend());
          ++citer1, ++citer2){
        x1vals_max = std::max(x1vals_max, *citer1);
        x2vals_max = std::max(x2vals_max, *citer2);
        std::vector<VType> ipoly_vals = {*citer1, *citer2};
        VType yval = ipoly.deval(ipoly_vals);
        std::pair<typename std::map<VType,bool>::iterator, bool>
          yvals_map_iinfo = yvals_map.insert(std::make_pair(yval, true));
        /* Only insert unique yvals */
        if(yvals_map_iinfo.second){
          yvals.push_back(yval);
          x1vals.push_back(*citer1);
          x2vals.push_back(*citer2);
        }
      }

      /* Find avgs of x values from the user */
      VType x1vals_sum = std::accumulate(x1vals.cbegin(), x1vals.cend(),
                          static_cast<VType>(0));
      assert(x1vals.size() > 0);
      VType x1vals_avg = x1vals_sum / x1vals.size();
      VType x2vals_sum = std::accumulate(x2vals.cbegin(), x2vals.cend(),
                          static_cast<VType>(0));
      assert(x2vals.size() > 0);
      VType x2vals_avg = x2vals_sum / x2vals.size();

      /* Add some values to cover the range from (2, 2) to 
       * (x1vals_max, x2vals_max)
       * - increasing exponentially
       */
      if(x1vals_max > 0){
        const VType filler_multiplier = static_cast<VType>(2);
        VType x1vals_filler = filler_multiplier;
        VType x2vals_filler = filler_multiplier;
        while((x1vals_filler < x1vals_max) &&
              (x2vals_filler < x2vals_max)){
          std::vector<VType> ipoly_vals = {x1vals_filler, x2vals_filler};
          VType yval = ipoly.deval(ipoly_vals);
          std::pair<typename std::map<VType,bool>::iterator, bool>
            yvals_map_iinfo = yvals_map.insert(std::make_pair(yval, true));
          /* Only insert unique yvals */
          if(yvals_map_iinfo.second){
            yvals.push_back(yval);
            x1vals.push_back(x1vals_filler);
            x2vals.push_back(x2vals_filler);
          }
          x1vals_filler *= filler_multiplier;
          x2vals_filler *= filler_multiplier;
        }
        while(x1vals_filler < x1vals_max){
          std::vector<VType> ipoly_vals = {x1vals_filler, x2vals_filler};
          VType yval = ipoly.deval(ipoly_vals);
          std::pair<typename std::map<VType,bool>::iterator, bool>
            yvals_map_iinfo = yvals_map.insert(std::make_pair(yval, true));
          /* Only insert unique yvals */
          if(yvals_map_iinfo.second){
            yvals.push_back(yval);
            x1vals.push_back(x1vals_filler);
            x2vals.push_back(x2vals_filler);
          }
          x1vals_filler *= filler_multiplier;
        }
        while(x2vals_filler < x2vals_max){
          std::vector<VType> ipoly_vals = {x1vals_filler, x2vals_filler};
          VType yval = ipoly.deval(ipoly_vals);
          std::pair<typename std::map<VType,bool>::iterator, bool>
            yvals_map_iinfo = yvals_map.insert(std::make_pair(yval, true));
          /* Only insert unique yvals */
          if(yvals_map_iinfo.second){
            yvals.push_back(yval);
            x1vals.push_back(x1vals_filler);
            x2vals.push_back(x2vals_filler);
          }
          x2vals_filler *= filler_multiplier;
        }
      }
      
      /* Add NUM_NDIST_FILLVALS values that are normal distributed
       * around the mean of the user provided xvals
       * The stddev is assumed to be 1/3 of the mean
       */
      const int NUM_NDIST_FILLVALS = 100;
      const int X1VALS_DEFAULT_SEED = 1234;
      const int X2VALS_DEFAULT_SEED = 5678;
      std::default_random_engine x1vals_generator(X1VALS_DEFAULT_SEED);
      std::default_random_engine x2vals_generator(X2VALS_DEFAULT_SEED);
      /* We only use vals in range (0, xvals_avg * 3) */
      const std::pair<VType, VType>
        X1VALS_NDIST_FILLVALS_RANGE(0, x1vals_avg * 3.0);
      const std::pair<VType, VType>
        X2VALS_NDIST_FILLVALS_RANGE(0, x2vals_avg * 3.0);
      VType x1vals_stddev = x1vals_avg / 3.0;
      VType x2vals_stddev = x2vals_avg / 3.0;
      std::normal_distribution<VType>
        x1vals_distribution(x1vals_avg, x1vals_stddev);
      std::normal_distribution<VType>
        x2vals_distribution(x2vals_avg, x2vals_stddev);
      for(int i=0; i<NUM_NDIST_FILLVALS; i++){
        VType x1val = x1vals_distribution(x1vals_generator);
        VType x2val = x2vals_distribution(x2vals_generator);
        if( (x1val > X1VALS_NDIST_FILLVALS_RANGE.first) &&
            (x1val < X1VALS_NDIST_FILLVALS_RANGE.second) &&
            (x2val > X2VALS_NDIST_FILLVALS_RANGE.first) &&
            (x2val < X2VALS_NDIST_FILLVALS_RANGE.second) ){
          std::vector<VType> ipoly_vals = {x1val, x2val};
          VType yval = ipoly.deval(ipoly_vals);
          std::pair<typename std::map<VType,bool>::iterator, bool>
            yvals_map_iinfo = yvals_map.insert(std::make_pair(yval, true));
          /* Only insert unique yvals */
          if(yvals_map_iinfo.second){
            yvals.push_back(yval);
            x1vals.push_back(x1val);
            x2vals.push_back(x2val);
          }
        }
      }

      opoly = ipoly;
      ret = PolyFit(alg, max_deg, x1vals, x2vals, yvals, opoly);
      if(ret != ESMF_SUCCESS){
        std::cerr << "Approx fitting poly failed\n";
        return ret;
      }

      return ret;

    }

    template<typename CType>
    int PolyFit(PolyFitAlg alg, int max_deg,
          const TwoDVIDPoly<CType> &ipoly,
          TwoDVIDPoly<CType> &opoly)
    {
      int ret = ESMF_SUCCESS;
      std::vector<double> ux1vals, ux2vals;

      /* Add some common values for PET counts */
      ux1vals.push_back(64);
      ux1vals.push_back(256);
      ux1vals.push_back(1024);
      ux1vals.push_back(4096);
      ux2vals = ux1vals;

      ret = PolyFit(alg, max_deg, ipoly, opoly, ux1vals, ux2vals);
      return ret;
    }
  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_PolyFit_H
