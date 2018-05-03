#ifndef ESMCI_Mat_H
#define ESMCI_Mat_H

#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <initializer_list>
#include <complex.h>
#include <cblas.h>
#include <lapacke.h>

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class Matrix{
      public:
        Matrix(const std::vector<int> &dims, const std::vector<T> &data);
        Matrix(std::initializer_list<int> dims, std::initializer_list<T> data);
        Matrix(const std::vector<int> &dims, const T &val);
        
        std::vector<int> get_dims(void ) const;
        const T *get_data_by_ref(void ) const;
        T *get_data_by_ref(void );
        std::vector<T> get_data(void ) const;

        Matrix<T> inv(void ) const;

        template<typename U>
        friend Matrix<U> operator+(const Matrix<U> &lhs, const Matrix<U> &rhs);
        template<typename U>
        friend Matrix<U> operator-(const Matrix<U> &lhs, const Matrix<U> &rhs);
        template<typename U>
        friend Matrix<U> operator*(const Matrix<U> &lhs, const Matrix<U> &rhs);
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

    inline int LAPACK_Minv(int m, float *A)
    {
      lapack_int n = static_cast<lapack_int>(m);
      lapack_int *ipiv = (lapack_int *)calloc(n+1, sizeof(lapack_int));
      lapack_int info;
      info = LAPACKE_sgetrf(LAPACK_ROW_MAJOR, n, n, A, n, ipiv);
      assert(info >= 0);
      info = LAPACKE_sgetri(LAPACK_ROW_MAJOR, n, A, n, ipiv);
      assert(info >= 0);
      free(ipiv);

      return 0;
    }

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
      assert(ret == 0);

      return res;
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

    inline int BLAS_Mmult(int m, int n, int k, int alpha,
                          const float *A, int lda, const float *B, int ldb,
                          int beta, float *C, int ldc)
    {
      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                  m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
      return 0;
    }

    template<typename T>
    Matrix<T> operator*(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
      assert((lhs.dims_.size() > 0) && (rhs.dims_.size() > 0));
      // No tensor products for now
      assert((lhs.dims_.size() <= 2) && (rhs.dims_.size() <= 2));
      assert(lhs.dims_[lhs.dims_.size()-1] == rhs.dims_[0]);
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

      int ret = BLAS_Mmult(m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
      assert(ret == 0);

      return res;
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
