#ifndef ESMCI_PolyUV_H
#define ESMCI_PolyUV_H

#include <vector>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include <cmath>
#include "ESMCI_Poly.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Univariate (1 variable) polynomial class
     * This class is abstract and derives from the general polynomial class
     */
    template<typename CType, typename DType>
    class UniVPoly : public GenPoly<CType, DType>{
      public:
        virtual ~UniVPoly() = default;
        void set_degs(const std::vector<std::vector<DType> >& degs);
        std::vector<std::vector<DType> > get_degs(void ) const;
    }; // class UniVPoly

    template<typename CType, typename DType>
    inline void UniVPoly<CType, DType>::set_degs(
                  const std::vector<std::vector<DType> >&degs)
    {
      assert(0);
    }

    template<typename CType, typename DType>
    inline std::vector<std::vector<DType> > UniVPoly<CType, DType>::get_degs(void ) const
    {
      assert(0);
    }

    /* Univariate polynomial with integer degrees
     * This concrete class derives from the univariate polynomial class
     */
    template<typename CType>
    class UVIDPoly : public UniVPoly<CType, int>{
      public:
        UVIDPoly();
        virtual ~UVIDPoly() = default;
        UVIDPoly(const CType &coeff);
        UVIDPoly(const std::vector<CType>& coeffs);
        UVIDPoly(std::initializer_list<CType> coeffs);
        int get_max_deg(void ) const;
        void set_vnames(const std::vector<std::string> &vnames);
        std::vector<std::string> get_vnames(void ) const;
        void set_coeffs(const std::vector<CType>& coeffs); 
        void set_coeffs(std::initializer_list<CType> coeffs);
        void set_cs_info(const PolyCSInfo<CType> &csinfo);
        bool has_cs_info(void ) const;
        PolyCSInfo<CType> get_cs_info(void ) const;
        std::vector<CType> get_coeffs(void ) const;
        CType eval(const std::vector<CType> &vvals) const;
        CType eval(const CType &vval) const;
      private:
        std::vector<CType> coeffs_;
        std::vector<std::string> vnames_;
        PolyCSInfo<CType> csinfo_;
        bool has_cs_info_;
    }; // class UVIDPoly

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly():has_cs_info_(false)
    {
      vnames_.push_back("x");
    }

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly(const CType &coeff):has_cs_info_(false)
    {
      coeffs_.push_back(coeff);
      vnames_.push_back("x");
    }

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly(const std::vector<CType>& coeffs):coeffs_(coeffs), has_cs_info_(false)
    {
      vnames_.push_back("x");
    }

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end()), has_cs_info_(false)
    {
      vnames_.push_back("x");
    }

    template<typename CType>
    inline int UVIDPoly<CType>::get_max_deg(void ) const
    {
      return coeffs_.size() - 1;
    }

    template<typename CType>
    inline void UVIDPoly<CType>::set_vnames(const std::vector<std::string>& vnames)
    {
      assert(vnames.size() == 1);
      vnames_ = vnames;
    }

    template<typename CType>
    inline std::vector<std::string> UVIDPoly<CType>::get_vnames(void ) const
    {
      return vnames_;
    }

    template<typename CType>
    inline void UVIDPoly<CType>::set_coeffs(const std::vector<CType>& coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
    }

    template<typename CType>
    inline void UVIDPoly<CType>::set_coeffs(std::initializer_list<CType> coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
    }

    template<typename CType>
    inline void UVIDPoly<CType>::set_cs_info(const PolyCSInfo<CType> &csinfo)
    {
      csinfo_ = csinfo;
      has_cs_info_ = true;
    }

    template<typename CType>
    inline bool UVIDPoly<CType>::has_cs_info(void ) const
    {
      return has_cs_info_;
    }

    template<typename CType>
    inline PolyCSInfo<CType> UVIDPoly<CType>::get_cs_info(void ) const
    {
      return csinfo_;
    }

    template<typename CType>
    inline std::vector<CType> UVIDPoly<CType>::get_coeffs(void ) const
    {
      return coeffs_;
    }
 
    /* Evaluate the polynomial */
    template<typename CType>
    inline CType UVIDPoly<CType>::eval(const std::vector<CType> &vvals) const
    {
      assert(vvals.size() == 1);
      std::vector<CType> sc_vvals = 
        (has_cs_info_) ? csinfo_.center_and_scale(vvals) : vvals;

      CType res = 0;
      std::size_t cur_deg = coeffs_.size() - 1;
      for(typename std::vector<CType>::const_iterator citer = coeffs_.cbegin();
          citer != coeffs_.cend(); ++citer){
        res += (*citer) * pow(sc_vvals[0], cur_deg);
        cur_deg--;
      }

      return res;
    }

    template<typename CType>
    inline CType UVIDPoly<CType>::eval(const CType &vval) const
    {
      std::vector<CType> vvals(1, vval);
      return eval(vvals);
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const UVIDPoly<CType>& p)
    {
      std::vector<CType> coeffs(p.get_coeffs());
      std::vector<std::string> vnames = p.get_vnames();
      assert(vnames.size() == 1);
      std::size_t cur_deg = coeffs.size()-1;
      for(typename std::vector<CType>::iterator iter = coeffs.begin();
            iter != coeffs.end(); ++iter){
        if(cur_deg > 1){
          ostr << *iter << " " << vnames[0].c_str() << "^" << cur_deg-- << " + ";
        }else if(cur_deg == 1){
          ostr << *iter << " " << vnames[0].c_str() << " + ";
          cur_deg--;
        }
        else{
          ostr << *iter;
        }
      }
      return ostr;
    }

    template<typename CType>
    UVIDPoly<CType> operator+(const UVIDPoly<CType> &lhs, const UVIDPoly<CType> &rhs)
    {
      std::vector<CType> res_coeffs;
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();

      typename std::vector<CType>::const_reverse_iterator criter_lhs =
          lhs_coeffs.crbegin();
      typename std::vector<CType>::const_reverse_iterator criter_rhs =
          rhs_coeffs.crbegin();
      for(;(criter_lhs != lhs_coeffs.crend()) && (criter_rhs != rhs_coeffs.crend());
          ++criter_lhs, ++criter_rhs){
        res_coeffs.push_back(*criter_lhs + *criter_rhs);
      }

      for(;(criter_lhs != lhs_coeffs.crend()); ++criter_lhs){
        res_coeffs.push_back(*criter_lhs);
      }
      for(;(criter_rhs != rhs_coeffs.crend()); ++criter_rhs){
        res_coeffs.push_back(*criter_rhs);
      }
      std::reverse(res_coeffs.begin(), res_coeffs.end());
      UVIDPoly<CType> res(res_coeffs);

      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_info() == rhs.get_cs_info());
        }
        res.set_cs_info(lhs.get_cs_info());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_info(rhs.get_cs_info());
      }

      return res;
    }

    template<typename CLType, typename CRType>
    UVIDPoly<CRType> operator+(const CLType &lhs, const UVIDPoly<CRType> &rhs)
    {
      UVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs + rhs;
    }

    template<typename CType>
    UVIDPoly<CType> operator-(const UVIDPoly<CType> &lhs, const UVIDPoly<CType> &rhs)
    {
      std::vector<CType> res_coeffs;
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();

      typename std::vector<CType>::const_reverse_iterator criter_lhs =
        lhs_coeffs.crbegin();
      typename std::vector<CType>::const_reverse_iterator criter_rhs =
        rhs_coeffs.crbegin();
      for(;(criter_lhs != lhs_coeffs.crend()) && (criter_rhs != rhs_coeffs.crend());
          ++criter_lhs, ++criter_rhs){
        res_coeffs.push_back(*criter_lhs - *criter_rhs);
      }

      for(;(criter_lhs != lhs_coeffs.crend()); ++criter_lhs){
        res_coeffs.push_back(*criter_lhs);
      }
      for(;(criter_rhs != rhs_coeffs.crend()); ++criter_rhs){
        res_coeffs.push_back(-1 * *criter_rhs);
      }
      std::reverse(res_coeffs.begin(), res_coeffs.end());
      UVIDPoly<CType> res(res_coeffs);

      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_info() == rhs.get_cs_info());
        }
        res.set_cs_info(lhs.get_cs_info());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_info(rhs.get_cs_info());
      }

      return res;
    }

    template<typename CLType, typename CRType>
    UVIDPoly<CRType> operator-(const CLType &lhs, const UVIDPoly<CRType> &rhs)
    {
      UVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs - rhs;
    }

    template<typename CType>
    UVIDPoly<CType> operator*(const UVIDPoly<CType> &lhs, const UVIDPoly<CType> &rhs)
    {
      UVIDPoly<CType> res;
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();

      int cur_deg_coeff_shift = 0;
      for(typename std::vector<CType>::const_reverse_iterator criter_rhs =
          rhs_coeffs.crbegin();
          criter_rhs != rhs_coeffs.crend(); ++criter_rhs){
        std::vector<CType> tmp_res_coeffs;
        for(typename std::vector<CType>::const_iterator citer_lhs = lhs_coeffs.cbegin();
          citer_lhs != lhs_coeffs.cend(); ++citer_lhs){
          tmp_res_coeffs.push_back((*citer_lhs) * (*criter_rhs));
        }

        for(int i=0; i<cur_deg_coeff_shift; i++){
          tmp_res_coeffs.push_back(0);
        }
        cur_deg_coeff_shift++;

        UVIDPoly<CType> tmp_res(tmp_res_coeffs);
        res = res + tmp_res;
      }

      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_info() == rhs.get_cs_info());
        }
        res.set_cs_info(lhs.get_cs_info());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_info(rhs.get_cs_info());
      }

      return res;
    }

    template<typename CLType, typename CRType>
    UVIDPoly<CRType> operator*(const CLType &lhs, const UVIDPoly<CRType> &rhs)
    {
      UVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs * rhs;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_PolyUV_H
