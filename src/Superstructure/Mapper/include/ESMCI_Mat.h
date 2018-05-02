#ifndef ESMCI_Mat_H
#define ESMCI_Mat_H

#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <initializer_list>

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class Matrix{
      public:
        Matrix(const std::vector<int> &dims, const std::vector<T> &data);
        Matrix(std::initializer_list<int> dims, std::initializer_list<T> data);
        Matrix(const std::vector<int> &dims, const T &val);
        
        std::vector<int> get_dims(void ) const;
        T *get_data_by_ref(void );
        std::vector<T> get_data(void ) const;

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

    template<typename T>
    Matrix<T> operator*(const Matrix<T> &lhs, const Matrix<T> &rhs)
    {
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
