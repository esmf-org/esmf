#ifndef ESMCI_Poly_H
#define ESMCI_Poly_H

#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
//#include <cstdlib>
#include <cmath>
#include <numeric>

namespace ESMCI{
  namespace MapperUtil{

    namespace PolyCSInfoUtil{
      /* Find and return the mean of the values in vals */
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

    } //namespace PolyCSInfoUtil

    /* Polynomial center and scale info */
    template<typename T>
    class PolyCSInfo{
      public:
        PolyCSInfo();
        PolyCSInfo(const std::pair<T, T> &mean_and_stddev);
        PolyCSInfo(const T &mean, const T &stddev);
        T get_mean(void ) const;
        T get_stddev(void ) const;
        /* Functions to center and scale values */
        T center_and_scale(const T& val) const;
        void center_and_scale(T &val) const;
        std::vector<T> center_and_scale(const std::vector<T> &vals) const;
        void center_and_scale(std::vector<T> &vals) const;
        bool operator==(const PolyCSInfo<T> &other) const;
        /* Function to create a center and scale info for a polynomial */
        static PolyCSInfo<T> create_poly_csinfo(const std::vector<T> &vals);
      private:
        T mean_;
        T stddev_;
    };

    template<typename T>
    PolyCSInfo<T>::PolyCSInfo():mean_(0), stddev_(0)
    {}

    template<typename T>
    PolyCSInfo<T>::PolyCSInfo(const std::pair<T, T> &mean_and_stddev):mean_(mean_and_stddev.first), stddev_(mean_and_stddev.second)
    {}

    template<typename T>
    PolyCSInfo<T>::PolyCSInfo(const T& mean, const T &stddev):mean_(mean), stddev_(stddev)
    {}

    template<typename T>
    T PolyCSInfo<T>::get_mean(void ) const
    {
      return mean_;
    }

    template<typename T>
    T PolyCSInfo<T>::get_stddev(void ) const
    {
      return stddev_;
    }

    /* Center and scale the value so that the value is centered around the mean
     * and has a unit standard deviation
     */
    template<typename T>
    T PolyCSInfo<T>::center_and_scale(const T &val) const
    {
      T scale = (stddev_ != 0) ? stddev_ : 1;
      return (val - mean_)/scale;
    }

    template<typename T>
    void PolyCSInfo<T>::center_and_scale(T &val) const
    {
      T scale = (stddev_ != 0) ? stddev_ : 1;
      val = (val - mean_)/scale;
    }

    template<typename T>
    std::vector<T> PolyCSInfo<T>::center_and_scale(const std::vector<T> &vals) const
    {
      std::vector<T> ret_vals = vals;
      T scale = (stddev_ != 0) ? stddev_ : 1;
      for(typename std::vector<T>::iterator iter = ret_vals.begin();
          iter != ret_vals.end(); ++iter){
        *iter = (*iter - mean_)/scale;
      }
      return ret_vals;
    }

    template<typename T>
    void PolyCSInfo<T>::center_and_scale(std::vector<T> &vals) const
    {
      T scale = (stddev_ != 0) ? stddev_ : 1;
      for(typename std::vector<T>::iterator iter = vals.begin();
          iter != vals.end(); ++iter){
        *iter = (*iter - mean_)/scale;
      }
    }

    template<typename T>
    bool PolyCSInfo<T>::operator==(const PolyCSInfo<T> &other) const
    {
      return ((mean_ == other.mean_) && (stddev_ == other.stddev_));
    }

    template<typename T>
    PolyCSInfo<T> PolyCSInfo<T>::create_poly_csinfo(const std::vector<T> &vals)
    {
      PolyCSInfo<T> csinfo = PolyCSInfoUtil::GetMeanAndStdDev(vals);
      return csinfo;
    }
    /* The generic polynomial class
     * This class is abstract and is extended to implement
     * univariable/ 2 variable polynomials
     */
    template<typename CType, typename DType>
    class GenPoly{
      public:
        virtual ~GenPoly() = default;
        virtual void set_coeffs(const std::vector<CType>& coeffs) = 0;
        virtual void set_coeffs(std::initializer_list<CType> coeffs) = 0;
        virtual void set_vnames(const std::vector<std::string> &vnames) = 0;
        //virtual void set_cs_info(const PolyCSInfo<CType> &csinfo) = 0;
        virtual std::vector<std::string> get_vnames(void ) const = 0;
        virtual void set_degs(const std::vector<std::vector<DType> >& degs) = 0;
        virtual int get_max_deg(void ) const = 0;
        virtual std::vector<CType> get_coeffs(void ) const = 0;
        virtual std::vector<std::vector<DType> > get_degs(void ) const = 0;
        virtual CType eval(const std::vector<CType> &vvals) const = 0;
        bool equals(const GenPoly<CType, DType> &poly, double tol) const;
    }; // class GenPoly

    /* Compare two polynomials using a user-specified tolerance, tol.
     * The tolerance is used to compare the polynomial coefficients
     */
    template<typename CType, typename DType>
    bool GenPoly<CType,DType>::equals(const GenPoly<CType, DType> &poly,
                                      double tol) const
    {
      if(this->get_max_deg() != poly.get_max_deg()){
        return false;
      }
      std::vector<std::string> vnames = this->get_vnames();
      std::vector<std::string> vnames_poly = poly.get_vnames();
      if(vnames.size() != vnames_poly.size()){
        return false;
      }

      if(vnames != vnames_poly){
        return false;
      }

      std::vector<CType> coeffs = this->get_coeffs();
      std::vector<CType> coeffs_poly = poly.get_coeffs();
      if(coeffs.size() != coeffs_poly.size()){
        return false;
      }

      for(typename std::vector<CType>::const_iterator citer1 = coeffs.cbegin(),
          citer2 = coeffs_poly.cbegin();
          (citer1 != coeffs.cend()) && (citer2 != coeffs_poly.cend());
          ++citer1, ++citer2){
        if(fabs(*citer1 - *citer2) > tol){
          return false;
        }
      }

      return true;
    }

    /* Check if two polynomials are equal */
    template<typename CType, typename DType>
    bool operator==(const GenPoly<CType, DType> &poly1,
                    const GenPoly<CType, DType> &poly2)
    {
      if(poly1.get_max_deg() != poly2.get_max_deg()){
        return false;
      }
      std::vector<std::string> vnames_poly1 = poly1.get_vnames();
      std::vector<std::string> vnames_poly2 = poly2.get_vnames();
      if(vnames_poly1.size() != vnames_poly2.size()){
        return false;
      }
      if(vnames_poly1 != vnames_poly2){
        return false;
      }

      std::vector<CType> coeffs_poly1 = poly1.get_coeffs();
      std::vector<CType> coeffs_poly2 = poly2.get_coeffs();
      if(coeffs_poly1.size() != coeffs_poly2.size()){
        return false;
      }
      if(coeffs_poly1 != coeffs_poly2){
        return false;
      }

      return true;
    }

    /* Check if two polynomials are equal */
    template<typename CType, typename DType>
    bool operator!=(const GenPoly<CType, DType> &poly1,
                    const GenPoly<CType, DType> &poly2)
    {
      return (!(poly1 == poly2));
    }

    /* Check if all coefficients of a polynomial is equal to val */
    template<typename CType, typename DType>
    bool operator==(const GenPoly<CType, DType> &poly, const CType &val)
    {
      std::vector<CType> coeffs_poly = poly.get_coeffs();
      for(typename std::vector<CType>::const_iterator citer = coeffs_poly.cbegin();
          citer != coeffs_poly.end(); ++citer){
        if(*citer != val){
          return false;
        }
      }

      return true;
    }

    template<typename CType, typename DType>
    bool operator!=(const GenPoly<CType, DType> &poly, const CType &val)
    {
      return (!(poly == val));
    }

    /* Check if all coefficients of a polynomial is equal to val */
    template<typename CType, typename DType>
    bool operator==(const CType &val, const GenPoly<CType, DType> &poly)
    {
      return (poly == val);
    }

    template<typename CType, typename DType>
    bool operator!=(const CType &val, const GenPoly<CType, DType> &poly)
    {
      return(!(val == poly));
    }

    /* This is a custom < so that algos that want to compute equality
     * between two polys work
     * Here,
     * poly_a < poly_b iff poly_a != poly_b
     */
    template<typename CType, typename DType>
    bool operator<(const GenPoly<CType, DType> &lpoly,
      const GenPoly<CType, DType> &rpoly)
    {
      return (lpoly != rpoly);
    }
  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_Poly_H
