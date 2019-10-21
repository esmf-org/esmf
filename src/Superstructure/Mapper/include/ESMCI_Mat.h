#ifndef ESMCI_Mat_H
#define ESMCI_Mat_H

#include "ESMCI_Macros.h"
#include <stdexcept>
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>
#include <initializer_list>
#include <complex.h>
#include <cblas.h>
#include <lapacke.h>

namespace ESMCI{
  namespace MapperUtil{

    /* A Matrix class
     * The matrix class is used to create an n-dimensional matrix of values
     */
    template<typename T>
    class Matrix{
      public:
        Matrix(const std::vector<int> &dims, const std::vector<T> &data);
        Matrix(std::initializer_list<int> dims, std::initializer_list<T> data);
        Matrix(const std::vector<int> &dims, const T &val);
        
        /* Get the matrix dimensions */
        std::vector<int> get_dims(void ) const;
        /* Get a reference to the data strored in the matrix */
        const T *get_data_by_ref(void ) const;
        T *get_data_by_ref(void );
        /* Get the data stored in the matrix */
        std::vector<T> get_data(void ) const;

        /* Get the transpose of the matrix */
        Matrix<T> transpose(void ) const;
        /* Get the inverse of the matrix */
        Matrix<T> inv(void ) const;
        /* Get the pseudo inverse of the matrix */
        Matrix<T> pinv(void ) const;

        /* Returns true if the matrix, m, is equal to this matrix,
         * false otherwise. This function uses a tolerance, tol, to
         * compare the values stored in the matrix (unlike == that
         * does not provide a tolerance)
         */
        bool equals(const Matrix<T> &m, double tol) const;

        /* Compare if two matrices are equal */
        template<typename U>
        friend bool operator==(const Matrix<U> &lhs, const Matrix<U> &rhs);
        /* Compare if all values in the matrix is equal to val */
        template<typename U>
        friend bool operator==(const Matrix<U> &m, const U &val);
        template<typename U>
        friend bool operator==(const U &val, const Matrix<U> &m);
        /* Operator to add/subtract/multiply two matrices */
        template<typename U>
        friend Matrix<U> operator+(const Matrix<U> &lhs, const Matrix<U> &rhs);
        template<typename U>
        friend Matrix<U> operator-(const Matrix<U> &lhs, const Matrix<U> &rhs);
        template<typename U>
        friend Matrix<U> operator*(const Matrix<U> &lhs, const Matrix<U> &rhs);
        /* Operator to scale a matrix by a scalar */
        template<typename U, typename V>
        friend Matrix<U> operator*(const Matrix<U> &lhs, const V &rhs);
        template<typename U, typename V>
        friend Matrix<V> operator*(const U &lhs, const Matrix<V> &rhs);
        template<typename U>
        friend std::ostream& operator<<(std::ostream &ostr, const Matrix<U> &m);
      private:
        std::vector<int> dims_;
        std::vector<T> data_;
    };

    template<typename T>
    Matrix<T>::Matrix(const std::vector<int> &dims, const std::vector<T> &data):dims_(dims),data_(data)
    {}


    template<typename T>
    Matrix<T>::Matrix(std::initializer_list<int> dims, std::initializer_list<T> data):dims_(dims.begin(),dims.end()),data_(data.begin(),data.end())
    {}

    template<typename T>
    Matrix<T>::Matrix(const std::vector<int> &dims, const T& val):dims_(dims)
    {
      assert(dims.size() > 0);
      int data_sz = std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<int>());
      assert(data_sz > 0);
      data_.resize(data_sz, val);
    }

    template<typename T>
    std::vector<int> Matrix<T>::get_dims(void ) const
    {
      return dims_;
    }

    template<typename T>
    const T *Matrix<T>::get_data_by_ref(void ) const
    {
      return ((data_.size() > 0 ) ? (&(data_[0])) : NULL);
    }

    template<typename T>
    T *Matrix<T>::get_data_by_ref(void )
    {
      return ((data_.size() > 0 ) ? (&(data_[0])) : NULL);
    }

    template<typename T>
    std::vector<T> Matrix<T>::get_data(void ) const
    {
      return data_;
    }

    template<typename T>
    Matrix<T> Matrix<T>::transpose(void ) const
    {
      assert(dims_.size() <= 2);
      int nelems = static_cast<int>(data_.size());
      if(dims_.size() == 1){
        return Matrix<T>(dims_, data_);
      }
      else{
        std::vector<int> dims(dims_.rbegin(), dims_.rend());
        if((dims_[0] == 1) || dims_[1] == 1){
          return Matrix<T>(dims, data_);
        }
        else{
          // FIXME: Use inplace transpose
          std::vector<T> data(data_.size(), static_cast<T>(0));
          int nrows = dims_[0];
          int ncols = dims_[1];
          int tnrows = ncols;
          int tncols = nrows;
          for(int idx = 0; idx < nelems; idx++){
            int ridx = idx/ncols;
            int cidx = idx - ridx * ncols;
            int tridx = cidx;
            int tcidx = ridx;
            int tidx = tridx * tncols + tcidx;
            data[tidx] = data_[idx];
          }
          return Matrix<T>(dims, data);  
        }
      }
    }

    /* Calculate the inverse of the matrix using LAPACK */
    inline int LAPACK_Minv(int m, float *A)
    {
      lapack_int n = static_cast<lapack_int>(m);
      lapack_int *ipiv = (lapack_int *)calloc(n+1, sizeof(lapack_int));
      lapack_int info;
      /* Calculate LU decomposition of the matrix */
      info = LAPACKE_sgetrf(LAPACK_ROW_MAJOR, n, n, A, n, ipiv);
      /* info == 0 => success */
      if(info < 0){
        std::cout << "LAPACKE_sgetrf failed, the "
          << -info << "th arg in input array, A[" << -info << "] = "
          << A[-info] << " is invalid\n";
        return ESMF_FAILURE;
      }
      else if(info > 0){
        std::cout << "LAPACKE_sgetrf failed, the U["
          << info << "," << info << "] = 0, U is singular and div by zero can occur"
          << " if used to solve a system of equations\n";
        return ESMF_FAILURE;
      }
      /* Calculate the inverse of the matrix */
      info = LAPACKE_sgetri(LAPACK_ROW_MAJOR, n, A, n, ipiv);
      /* info == 0 => success */
      if(info < 0){
        std::cout << "LAPACKE_sgetri failed, the "
          << -info << "th arg in input array, A[" << -info << "] = "
          << A[-info] << " is invalid\n";
        return ESMF_FAILURE;
      }
      else if(info > 0){
        std::cout << "LAPACKE_sgetri failed, the U["
          << info << "," << info << "] = 0, the matrix is singular and its inverse "
          << "cannot be computed\n";
        return ESMF_FAILURE;
      }
      free(ipiv);

      return ESMF_SUCCESS;
    }

    /* Calculate the inverse of the matrix using LAPACK */
    inline int LAPACK_Minv(int m, double *A)
    {
      lapack_int n = static_cast<lapack_int>(m);
      lapack_int *ipiv = (lapack_int *)calloc(n+1, sizeof(lapack_int));
      lapack_int info;
      /* Calculate LU decomposition of the matrix */
      info = LAPACKE_dgetrf(LAPACK_ROW_MAJOR, n, n, A, n, ipiv);
      /* info == 0 => success */
      if(info < 0){
        std::cout << "LAPACKE_dgetrf failed, the "
          << -info << "th arg in input array, A[" << -info << "] = "
          << A[-info] << " is invalid\n";
        return ESMF_FAILURE;
      }
      else if(info > 0){
        std::cout << "LAPACKE_dgetrf failed, the U["
          << info << "," << info << "] = 0, U is singular and div by zero can occur"
          << " if used to solve a system of equations\n";
        return ESMF_FAILURE;
      }
      /* Calculate the inverse of the matrix */
      info = LAPACKE_dgetri(LAPACK_ROW_MAJOR, n, A, n, ipiv);
      /* info == 0 => success */
      if(info < 0){
        std::cout << "LAPACKE_dgetri failed, the "
          << -info << "th arg in input array, A[" << -info << "] = "
          << A[-info] << " is invalid\n";
        return ESMF_FAILURE;
      }
      else if(info > 0){
        std::cout << "LAPACKE_dgetri failed, the U["
          << info << "," << info << "] = 0, the matrix is singular and its inverse "
          << "cannot be computed\n";
        return ESMF_FAILURE;
      }
      free(ipiv);

      return ESMF_SUCCESS;
    }

    /* Calculate the inverse of the matrix */
    template<typename T>
    Matrix<T> Matrix<T>::inv(void ) const
    {
      Matrix<T> res = *this;
      if(dims_.size() == 1){
        return res;
      }

      // We are only concerned about 2d matrices for now
      assert(dims_.size() == 2);
      assert(std::adjacent_find(dims_.cbegin(), dims_.cend(), std::not_equal_to<T>()) == dims_.cend());

      int ret = LAPACK_Minv(res.dims_[0], res.get_data_by_ref());
      //assert(ret == ESMF_SUCCESS);
      if(ret != ESMF_SUCCESS){
        std::cerr << "Finding matrix inv failed for : \n" << *this << "\n";
        std::string err_msg("LAPACK routine to find inv failed");
        throw std::runtime_error(err_msg);
      }

      return res;
    }

    /* Compute pseudo inverse = Inverse(A_transpose * A) * A_transpose */
    template<typename T>
    Matrix<T> Matrix<T>::pinv(void ) const
    {
      Matrix<T> trans = transpose();
      try{
        // Left inverse
        return ((trans * (*this)).inv() * trans);
      }
      catch(...){
        // Right inverse
        return (trans * ((*this) * trans).inv());
      }
    }

    template<typename T>
    bool Matrix<T>::equals(const Matrix<T> &m, double tol) const
    {
      std::vector<int> m_dims = m.get_dims();
      std::vector<T> m_data = m.get_data();

      if(m_dims != dims_){
        return false;
      }

      if(m_data.size() != data_.size()){
        return false;
      }

      for(typename std::vector<T>::const_iterator citer1 = m_data.cbegin(),
          citer2 = data_.cbegin();
          (citer1 != m_data.cend()) && (citer2 != data_.cend());
          ++citer1, ++citer2){
        if(fabs(*citer1 - *citer2) > tol){
          return false;
        }
      }

      return true;
    }

    template<typename T>
    bool operator==(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
      if(lhs.dims_ != rhs.dims_){
        return false; 
      }
      if(lhs.data_ != rhs.data_){
        return false;
      }
      return true;
    }

    template<typename T>
    bool operator==(const Matrix<T> &m, const T& val)
    {
      for(typename std::vector<T>::const_iterator citer = m.data_.cbegin();
          citer != m.data_.cend(); ++citer){
        if(*citer != val){
          return false;
        }
      }
      return true;
    }

    template<typename T>
    bool operator==(const T &val, const Matrix<T> &m)
    {
      return (m == val);
    }

    template<typename T>
    Matrix<T> operator+(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
      assert(lhs.dims_.size() == rhs.dims_.size());
      assert(std::equal(lhs.dims_.begin(), lhs.dims_.end(), rhs.dims_.begin()));
      Matrix<T> res = lhs;
      std::transform(res.data_.begin(), res.data_.end(), rhs.data_.begin(),
                      res.data_.begin(), std::plus<T>());
      return res;
    }

    template<typename U, typename V>
    Matrix<U> operator+(const Matrix<U> &lhs, const V &rhs)
    {
      Matrix<U> res = lhs;
      std::transform(res.data_.begin(), res.data_.end(),
                      res.data_.begin(),
                      std::bind1st(std::plus<U>(), static_cast<U>(rhs)));
      return res;
    }

    template<typename U, typename V>
    Matrix<V> operator+(const U &lhs, const Matrix<V> &rhs)
    {
      return rhs + lhs;
    }

    template<typename T>
    Matrix<T> operator-(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
      assert(lhs.dims_.size() == rhs.dims_.size());
      assert(std::equal(lhs.dims_.begin(), lhs.dims_.end(), rhs.dims_.begin()));
      Matrix<T> res = lhs;
      std::transform(res.data_.begin(), res.data_.end(), rhs.data_.begin(),
                      res.data_.begin(), std::minus<T>());
      return res;
    }

    template<typename U, typename V>
    Matrix<U> operator-(const Matrix<U> &lhs, const V &rhs)
    {
      Matrix<U> res = lhs;
      std::transform(res.data_.begin(), res.data_.end(),
                      res.data_.begin(),
                      std::bind1st(std::minus<U>(), static_cast<U>(rhs)));
      return res;
    }

    /* Multiply two matrices using BLAS
     */
    inline int BLAS_Mmult(int m, int n, int k, int alpha,
                          const float *A, int lda, const float *B, int ldb,
                          int beta, float *C, int ldc)
    {
      /* cblas_sgemm has no return code ! */
      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                  m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
      return ESMF_SUCCESS;
    }

    /* Multiply two matrices using BLAS
     */
    inline int BLAS_Mmult(int m, int n, int k, int alpha,
                          const double *A, int lda, const double *B, int ldb,
                          int beta, double *C, int ldc)
    {
      /* cblas_sgemm has no return code ! */
      cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                  m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
      return ESMF_SUCCESS;
    }

    /* Multiply two matrices, currently uses BLAS routines for matrix
     * multiplication
     */
    template<typename T>
    Matrix<T> operator*(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
      assert((lhs.dims_.size() > 0) && (rhs.dims_.size() > 0));
      // No tensor products for now
      assert((lhs.dims_.size() <= 2) && (rhs.dims_.size() <= 2));
      assert(lhs.dims_[lhs.dims_.size()-1] == rhs.dims_[0]);
      /* Determine inputs to the BLAS routine to perform
       * matrix multiplication
       */
      int m, n, k, alpha, lda, ldb, beta, ldc;
      alpha = 1;
      beta = 1;

      std::vector<int> res_dims;
      if(lhs.dims_.size() == 1){
        res_dims.push_back(lhs.dims_[0]);
        m = 1;
        k = 1;
        n = 1;
        lda = 1;
        ldb = 1;
        ldc = 1;
      }
      else{
        assert(lhs.dims_.size() == 2);
        res_dims.push_back(lhs.dims_[0]);
        res_dims.push_back(rhs.dims_[rhs.dims_.size()-1]);
        m = lhs.dims_[0];
        k = lhs.dims_[lhs.dims_.size() - 1];
        n = rhs.dims_[rhs.dims_.size() - 1];
        lda = k;
        ldb = n;
        ldc = n;
      }

      Matrix<T> res(res_dims, 0);
      const T *A = lhs.get_data_by_ref();
      const T *B = rhs.get_data_by_ref();
      T *C = res.get_data_by_ref();

      /* Use BLAS for multiplying the two matrices */
      int ret = BLAS_Mmult(m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
      assert(ret == ESMF_SUCCESS);

      return res;
    }

    template<typename U, typename V>
    Matrix<U> operator*(const Matrix<U> &lhs, const V &rhs)
    {
      Matrix<U> res = lhs;
      std::transform(res.data_.begin(), res.data_.end(),
                      res.data_.begin(),
                      std::bind1st(std::multiplies<U>(), static_cast<U>(rhs)));
      return res;
    }
    template<typename U, typename V>
    Matrix<V> operator*(const U &lhs, const Matrix<V> &rhs)
    {
      return rhs * lhs;
    }

    template<typename T>
    std::ostream& operator<<(std::ostream &ostr, const Matrix<T> &m)
    {
      std::vector<int> dim_wgts(m.dims_);
      //std::vector<int> dim_wgts(m.dims_.size(), 0);
      //std::partial_sum(m.dims_.rbegin(), m.dims_.rend(), dim_wgts.rbegin(), std::multiplies<int>());
      std::vector<int> dim_wgts_counter(dim_wgts.size(), 0);
      assert(dim_wgts.size() > 0);
      std::size_t dim_wgts_counter_idx = dim_wgts.size() - 1;
      for(typename std::vector<T>::const_iterator citer = m.data_.cbegin();
          citer != m.data_.cend(); ++citer){
        ostr << *citer << ", ";
        dim_wgts_counter[dim_wgts_counter_idx]++;
        if(dim_wgts_counter[dim_wgts_counter_idx] >= 
            dim_wgts[dim_wgts_counter_idx]){
          while(dim_wgts_counter[dim_wgts_counter_idx]>=dim_wgts[dim_wgts_counter_idx]){
            dim_wgts_counter[dim_wgts_counter_idx] = 0;
            ostr << "\n";
            if(dim_wgts_counter_idx > 0){
              dim_wgts_counter[dim_wgts_counter_idx - 1]++;
              dim_wgts_counter_idx--;
            }
            else{
              break;
            }
          }

          dim_wgts_counter_idx = dim_wgts.size() - 1;
        }
      }
      return ostr;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif //ESMCI_Mat_H
