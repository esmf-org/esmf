#ifndef ESMCI_PolyTwoV_H
#define ESMCI_PolyTwoV_H

#include <vector>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include "ESMCI_Poly.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename CType, typename DType>
    class TwoVPoly : public GenPoly<CType, DType>{
      public:
        virtual ~TwoVPoly() = default;
        void set_degs(const std::vector<std::vector<DType> >& degs);
        std::vector<std::vector<DType> > get_degs(void ) const;
    }; // class TwoVPoly

    template<typename CType, typename DType>
    inline void TwoVPoly<CType, DType>::set_degs(
                  const std::vector<std::vector<DType> >&degs)
    {
      assert(0);
    }

    template<typename CType, typename DType>
    inline std::vector<std::vector<DType> > TwoVPoly<CType, DType>::get_degs(void ) const
    {
      assert(0);
    }

    namespace TwoVIDPolyUtil{
      inline static int get_max_deg(int ncoeffs)
      {
        int coeffs_sz = ncoeffs;

        if(coeffs_sz == 0){
          return 0;
        }

        int deg = -1;
        int ncoeffs_deg = 1;
        do{
          deg++;
          coeffs_sz -= ncoeffs_deg;
          ncoeffs_deg++;
        }while(coeffs_sz > 0);

        assert(coeffs_sz == 0);
        return deg;
      }
    } // namespace TwoVIDPolyUtil

    template<typename CType>
    class TwoVIDPoly : public TwoVPoly<CType, int>{
      public:
        TwoVIDPoly() = default;
        virtual ~TwoVIDPoly() = default;
        TwoVIDPoly(const CType &coeff);
        TwoVIDPoly(const std::vector<CType>& coeffs);
        TwoVIDPoly(std::initializer_list<CType> coeffs);
        int get_max_deg(void ) const;
        void set_coeffs(const std::vector<CType>& coeffs); 
        void set_coeffs(std::initializer_list<CType> coeffs);
        std::vector<CType> get_coeffs(void ) const;
      private:
        int max_deg_;
        std::vector<CType> coeffs_;
    }; // class TwoVIDPoly

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const CType &coeff)
    {
      coeffs_.push_back(coeff);
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
    }

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const std::vector<CType>& coeffs):coeffs_(coeffs)
    {
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
    }

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end())
    {
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
    }

    template<typename CType>
    int TwoVIDPoly<CType>::get_max_deg(void ) const
    {
      return max_deg_;
    }

    template<typename CType>
    inline void TwoVIDPoly<CType>::set_coeffs(const std::vector<CType>& coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
    }

    template<typename CType>
    inline void TwoVIDPoly<CType>::set_coeffs(std::initializer_list<CType> coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
    }

    template<typename CType>
    inline std::vector<CType> TwoVIDPoly<CType>::get_coeffs(void ) const
    {
      return coeffs_;
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const TwoVIDPoly<CType>& p)
    {
      std::vector<CType> coeffs(p.get_coeffs());
      int cur_deg = p.get_max_deg();
      int cur_xdeg = cur_deg;
      int cur_ydeg = 0;
      int ncoeffs_in_cur_deg=cur_deg+1;
      for(typename std::vector<CType>::iterator iter = coeffs.begin();
            iter != coeffs.end(); ++iter){
        CType val = *iter;
        if(val != 0){
          ostr << *iter << " ";
        }
        if(val != 0){
          if(cur_xdeg > 1){
            ostr << "x^" << cur_xdeg;
          }
          else if(cur_xdeg == 1){
            ostr << "x";
          }
          if(cur_ydeg > 1){
            ostr << "y^" << cur_ydeg;
          }
          else if(cur_ydeg == 1){
            ostr << "y";
          }

          if(cur_deg > 0){
            ostr << " + ";
          }
        }
        ncoeffs_in_cur_deg--;
        if(ncoeffs_in_cur_deg == 0){
          cur_deg--;
          ncoeffs_in_cur_deg = cur_deg+1;
          cur_xdeg = cur_deg;
          cur_ydeg = 0;
        }
        else{
          cur_xdeg--;
          cur_ydeg++;
        }
      }
      return ostr;
    }

    template<typename CType>
    TwoVIDPoly<CType> operator+(const TwoVIDPoly<CType> &lhs, const TwoVIDPoly<CType> &rhs)
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
      TwoVIDPoly<CType> res(res_coeffs);

      return res;
    }

    template<typename CLType, typename CRType>
    TwoVIDPoly<CRType> operator+(const CLType &lhs, const TwoVIDPoly<CRType> &rhs)
    {
      TwoVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs + rhs;
    }

    template<typename CType>
    TwoVIDPoly<CType> operator-(const TwoVIDPoly<CType> &lhs, const TwoVIDPoly<CType> &rhs)
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
      TwoVIDPoly<CType> res(res_coeffs);

      return res;
    }

    template<typename CLType, typename CRType>
    TwoVIDPoly<CRType> operator-(const CLType &lhs, const TwoVIDPoly<CRType> &rhs)
    {
      TwoVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs - rhs;
    }

    namespace TwoVIDPolyUtil{
      static inline int get_ncoeffs(int max_deg)
      {
        int ncoeffs = 1;

        while(max_deg){
          ncoeffs += (max_deg + 1);
          max_deg--;
        }

        return ncoeffs;
      }

      static inline int get_coeff_idx(int max_deg, int v1deg, int v2deg)
      {
        int deg = v1deg + v2deg;
        int deg_off = max_deg - deg;
        int coeff_idx = 0;

        int cur_deg = max_deg;
        while(deg_off > 0){
          coeff_idx += (cur_deg + 1);
          cur_deg--;
          deg_off--;
        }

        coeff_idx += v2deg;

        std::cout << "(" << v1deg << "," << v2deg << ") = " << coeff_idx << "\n";

        return coeff_idx;
      }
    } // namespace TwoVIDPolyUtil

    template<typename CType>
    TwoVIDPoly<CType> operator*(const TwoVIDPoly<CType> &lhs, const TwoVIDPoly<CType> &rhs)
    {
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();
      int lhs_max_deg = lhs.get_max_deg();
      int rhs_max_deg = rhs.get_max_deg();
      int res_max_deg = lhs_max_deg + rhs_max_deg;
      int res_ncoeffs = TwoVIDPolyUtil::get_ncoeffs(res_max_deg);

      int cur_deg_lhs = lhs_max_deg;
      int cur_xdeg_lhs = cur_deg_lhs;
      int cur_ydeg_lhs = 0;
      int nrem_coeffs_in_deg_lhs = cur_deg_lhs + 1;
      int cur_deg_rhs = rhs_max_deg;
      int cur_xdeg_rhs = cur_deg_rhs;
      int cur_ydeg_rhs = 0;
      int nrem_coeffs_in_deg_rhs = cur_deg_rhs + 1;

      std::vector<CType> res_coeffs(res_ncoeffs, 0);
      for(typename std::vector<CType>::const_iterator citer_rhs =
          rhs_coeffs.cbegin();
          citer_rhs != rhs_coeffs.cend(); ++citer_rhs){
        for(typename std::vector<CType>::const_iterator citer_lhs =
          lhs_coeffs.cbegin();
          citer_lhs != lhs_coeffs.cend(); ++citer_lhs){
          CType val = (*citer_rhs) * (*citer_lhs);
          if(val > 0){
            int coeff_idx = TwoVIDPolyUtil::get_coeff_idx(res_max_deg, cur_xdeg_lhs + cur_xdeg_rhs, cur_ydeg_lhs + cur_ydeg_rhs);
            assert((coeff_idx >= 0) && (coeff_idx < res_ncoeffs));
            res_coeffs[coeff_idx] += val;
          }

          nrem_coeffs_in_deg_lhs--;
          if(nrem_coeffs_in_deg_lhs == 0){
            cur_deg_lhs--;
            cur_xdeg_lhs = cur_deg_lhs;
            cur_ydeg_lhs = 0;
            nrem_coeffs_in_deg_lhs = cur_deg_lhs + 1;
          }
          else{
            cur_xdeg_lhs--;
            cur_ydeg_lhs++;
          }
        }

        nrem_coeffs_in_deg_rhs--;
        if(nrem_coeffs_in_deg_rhs == 0){
          cur_deg_rhs--;
          cur_xdeg_rhs = cur_deg_rhs;
          cur_ydeg_rhs = 0;
          nrem_coeffs_in_deg_rhs = cur_deg_rhs + 1;
        }
        else{
          cur_xdeg_rhs--;
          cur_ydeg_rhs++;
        }

        cur_deg_lhs = lhs_max_deg;
        cur_xdeg_lhs = cur_deg_lhs;
        cur_ydeg_lhs = 0;
        nrem_coeffs_in_deg_lhs = cur_deg_lhs + 1;
      }
      TwoVIDPoly<CType> res(res_coeffs);
      return res;
    }

    template<typename CLType, typename CRType>
    TwoVIDPoly<CRType> operator*(const CLType &lhs, const TwoVIDPoly<CRType> &rhs)
    {
      TwoVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs * rhs;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_PolyTwoV_H
