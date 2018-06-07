#ifndef ESMCI_LPolyMV_H
#define ESMCI_LPolyMV_H

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
    class MVLPoly : public GenPoly<CType, DType>{
      public:
        virtual ~MVLPoly() = default;
        void set_degs(const std::vector<std::vector<DType> >& degs);
        std::vector<std::vector<DType> > get_degs(void ) const;
    }; // class MVLPoly

    template<typename CType, typename DType>
    inline void MVLPoly<CType, DType>::set_degs(
                  const std::vector<std::vector<DType> >&degs)
    {
      assert(0);
    }

    template<typename CType, typename DType>
    inline std::vector<std::vector<DType> > MVLPoly<CType, DType>::get_degs(void ) const
    {
      assert(0);
    }

    /* Univariate polynomial with integer degrees
     * This concrete class derives from the univariate polynomial class
     */
    template<typename CType>
    class MVIDLPoly : public MVLPoly<CType, int>{
      public:
        MVIDLPoly();
        virtual ~MVIDLPoly() = default;
        MVIDLPoly(const CType &coeff);
        MVIDLPoly(const std::vector<CType>& coeffs);
        MVIDLPoly(std::initializer_list<CType> coeffs);
        int get_max_deg(void ) const;
        void set_vnames(const std::vector<std::string> &vnames);
        std::vector<std::string> get_vnames(void ) const;
        void set_coeffs(const std::vector<CType>& coeffs); 
        void set_coeffs(std::initializer_list<CType> coeffs);
        void set_cs_info(const PolyCSInfo<CType> &csinfo);
        std::vector<CType> get_coeffs(void ) const;
        CType eval(const std::vector<CType> &vvals) const;
        CType eval(const CType &vval) const;
      private:
        std::vector<CType> coeffs_;
        std::vector<std::string> vnames_;
        PolyCSInfo<CType> csinfo_;
    }; // class MVIDLPoly

    namespace MVIDLPolyUtil{
      std::vector<string> append_int2str(const std::string &str, int range_start,
        int range_end)
      {
        std::vector<string> res;
        assert(range_end > range_start);
        for(int i=range_start; i<range_end; i++){
          std::stringstream ostr;
          ostr << str.c_str() << i;
          res.push_back(ostr.str());
        }

        return res;
      }
    } // namespace MVIDLPolyUtil


    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly()
    {
      vnames_.push_back("x");
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(const CType &coeff)
    {
      coeffs_.push_back(coeff);
      vnames_.push_back("x");
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(const std::vector<CType>& coeffs):coeffs_(coeffs)
    {
      vnames_ = MVIDPolyUtil::append_int2str(std::string("x"), 0, coeffs.size());
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end())
    {
      vnames_ = MVIDPolyUtil::append_int2str(std::string("x"), 0, coeffs.size());
    }

    template<typename CType>
    inline int MVIDLPoly<CType>::get_max_deg(void ) const
    {
      return 1;
    }

    template<typename CType>
    inline void MVIDLPoly<CType>::set_vnames(const std::vector<std::string>& vnames)
    {
      assert(vnames.size() == coeffs_.size() - 1);
      vnames_ = vnames;
    }

    template<typename CType>
    inline std::vector<std::string> MVIDLPoly<CType>::get_vnames(void ) const
    {
      return vnames_;
    }

    template<typename CType>
    inline void MVIDLPoly<CType>::set_coeffs(const std::vector<CType>& coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
    }

    template<typename CType>
    inline void MVIDLPoly<CType>::set_coeffs(std::initializer_list<CType> coeffs)
    {
      coeffs_.assign(coeffs.begin(), coeffs.end());
    }

    template<typename CType>
    inline void MVIDLPoly<CType>::set_cs_info(const PolyCSInfo<CType> &csinfo)
    {
      csinfo_ = csinfo;
    }

    template<typename CType>
    inline std::vector<CType> MVIDLPoly<CType>::get_coeffs(void ) const
    {
      return coeffs_;
    }
 
    /* Evaluate the polynomial */
    template<typename CType>
    inline CType MVIDLPoly<CType>::eval(const std::vector<CType> &vvals) const
    {
      assert(vvals.size() == vnames_.size());
      assert(vvals.size() < coeffs_.size());

      CType res = 0;
      typename std::vector<CType>::const_iterator coeffs_iter = coeffs_.cbegin();
      typename std::vector<CType>::const_iterator vvals_iter = vvals.cbegin();
      for(;(coeffs_iter != coeffs_.cend()) && (vvals_iter != vvals.cend());
            ++coeffs_iter, ++vvals_iter){
        res += (*coeffs_iter) * (*vvals_iter);
      }

      for(;coeffs_iter != coeffs_.cend(); ++coeffs_iter){
        res += *coeffs_iter;
      }

      return res;
    }

    template<typename CType>
    inline CType MVIDLPoly<CType>::eval(const CType &vval) const
    {
      std::vector<CType> vvals(1, vval);
      return eval(vvals);
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const MVIDLPoly<CType>& p)
    {
      std::vector<CType> coeffs(p.get_coeffs());
      std::vector<std::string> vnames = p.get_vnames();
      assert(vnames.size() == coeffs.size() - 1);

      typename std::vector<CType>::const_iterator coeffs_iter = coeffs_.cbegin();
      typename std::vector<CType>::const_iterator vnames_iter = vnames_.cbegin();
      for(;(coeffs_iter != coeffs_.cend()) && (vnames_iter != vnames_.cend());
          ++coeffs_iter, ++vnames_iter){
        ostr << *coeffs_iter << *vnames_iter << " + ";
      }
      for(;coeffs_iter != coeffs_cend(); ++coeffs_iter){
        ostr << *coeffs_iter;
      }

      return ostr;
    }

    template<typename CType>
    MVIDLPoly<CType> operator+(const MVIDLPoly<CType> &lhs, const MVIDLPoly<CType> &rhs)
    {
      std::vector<CType> res_coeffs;
      std::vector<std::string> res_vnames;
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<std::string> lhs_vnames = lhs.get_vnames();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();
      std::vector<std::string> rhs_vnames = rhs.get_vnames();

      // Info on whether rhs coeff already included in the result
      std::vector<bool> rhs_coeffs_in_res(rhs_vnames.size(), false);

      typename std::vector<CType>>::const_iterator citer_lhs = lhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_lhs = lhs_vnames.cbegin();

      typename std::vector<CType>::const_iterator citer_rhs = rhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_rhs = rhs_vnames.cbegin();
      typename std::vector<bool>::iterator biter_rhs = rhs_coeffs_in_res.cbegin();
      for(;(citer_lhs != lhs_coeffs.cend()) && (viter_lhs != lhs_vnames.cend());
          ++citer_lhs, ++viter_lhs){
        CType res = *citer_lhs;

        citer_rhs = rhs_coeffs.cbegin();
        viter_rhs = rhs_vnames.cbegin();
        biter_rhs = rhs_coeffs_in_res.cbegin();
        for(; (citer_rhs != rhs_coeffs.cend()) &&
              (viter_rhs != rhs_vnames.cend()) &&
              (biter_rhs != rhs_coeffs_in_res.cend());
              ++citer_rhs, ++viter_rhs, ++biter_rhs){
          if(*viter_lhs == *viter_rhs){
            res += *citer_rhs;
            *biter_rhs = true;
            break;
          }
        }

        res_coeffs.push_back(res);
        res_vnames.push_back(*viter_lhs);
      }
      citer_rhs = rhs_coeffs.cbegin();
      viter_rhs = rhs_vnames.cbegin();
      biter_rhs = rhs_coeffs_in_res.cbegin();
      for(; (citer_rhs != rhs_coeffs.cend()) &&
            (viter_rhs != rhs_vnames.cend()) &&
            (biter_rhs != rhs_coeffs_in_res.cend());
            ++citer_rhs, ++viter_rhs, ++biter_rhs){
        if(!(*biter_rhs)){
          *biter_rhs = true;
          res_coeffs.push_back(*citer_rhs);
          res_vnames.push_back(*viter_rhs);
        }
      }

      assert(citer_lhs != lhs_coeffs_.cend());
      assert(citer_rhs != rhs_coeffs_.cend());
      res_coeffs.push_back(*citer_lhs + *citer_rhs);

      MVIDLPoly<CType> res(res_coeffs);
      res.set_vnames(res_vnames);

      return res;
    }

    template<typename CLType, typename CRType>
    MVIDLPoly<CRType> operator+(const CLType &lhs, const MVIDLPoly<CRType> &rhs)
    {
      MVIDLPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs + rhs;
    }

    template<typename CType>
    MVIDLPoly<CType> operator-(const MVIDLPoly<CType> &lhs, const MVIDLPoly<CType> &rhs)
    {
      std::vector<CType> res_coeffs;
      std::vector<std::string> res_vnames;
      std::vector<CType> lhs_coeffs = lhs.get_coeffs();
      std::vector<std::string> lhs_vnames = lhs.get_vnames();
      std::vector<CType> rhs_coeffs = rhs.get_coeffs();
      std::vector<std::string> rhs_vnames = rhs.get_vnames();

      // Info on whether rhs coeff already included in the result
      std::vector<bool> rhs_coeffs_in_res(rhs_vnames.size(), false);

      typename std::vector<CType>>::const_iterator citer_lhs = lhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_lhs = lhs_vnames.cbegin();

      typename std::vector<CType>::const_iterator citer_rhs = rhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_rhs = rhs_vnames.cbegin();
      typename std::vector<bool>::iterator biter_rhs = rhs_coeffs_in_res.cbegin();
      for(;(citer_lhs != lhs_coeffs.cend()) && (viter_lhs != lhs_vnames.cend());
          ++citer_lhs, ++viter_lhs){
        CType res = *citer_lhs;

        citer_rhs = rhs_coeffs.cbegin();
        viter_rhs = rhs_vnames.cbegin();
        biter_rhs = rhs_coeffs_in_res.cbegin();
        for(; (citer_rhs != rhs_coeffs.cend()) &&
              (viter_rhs != rhs_vnames.cend()) &&
              (biter_rhs != rhs_coeffs_in_res.cend());
              ++citer_rhs, ++viter_rhs, ++biter_rhs){
          if(*viter_lhs == *viter_rhs){
            res -= *citer_rhs;
            *biter_rhs = true;
            break;
          }
        }

        res_coeffs.push_back(res);
        res_vnames.push_back(*viter_lhs);
      }
      citer_rhs = rhs_coeffs.cbegin();
      viter_rhs = rhs_vnames.cbegin();
      biter_rhs = rhs_coeffs_in_res.cbegin();
      for(; (citer_rhs != rhs_coeffs.cend()) &&
            (viter_rhs != rhs_vnames.cend()) &&
            (biter_rhs != rhs_coeffs_in_res.cend());
            ++citer_rhs, ++viter_rhs, ++biter_rhs){
        if(!(*biter_rhs)){
          *biter_rhs = true;
          res_coeffs.push_back(-1 * (*citer_rhs));
          res_vnames.push_back(*viter_rhs);
        }
      }

      assert(citer_lhs != lhs_coeffs_.cend());
      assert(citer_rhs != rhs_coeffs_.cend());
      res_coeffs.push_back(*citer_lhs - *citer_rhs);

      MVIDLPoly<CType> res(res_coeffs);
      res.set_vnames(res_vnames);

      return res;
    }

    template<typename CLType, typename CRType>
    MVIDLPoly<CRType> operator-(const CLType &lhs, const MVIDLPoly<CRType> &rhs)
    {
      MVIDLPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs - rhs;
    }

    template<typename CType>
    MVIDLPoly<CType> operator*(const MVIDLPoly<CType> &lhs, const CType &rhs)
    {
      MVIDLPoly<CType> res;
      std::vector<CType> res_coeffs = lhs.get_coeffs();
      std::vector<std::string> res_vnames = lhs_get_vnames();

      for(typename std::vector<CType>::iterator iter = res_coeffs.begin();
          iter != res_coeffs.end(); ++iter){
        *iter *= rhs;
      }

      MVIDLPoly<CType> res(res_coeffs);
      res.set_vnames(res_vnames);
      return res;
    }

    template<typename CLType, typename CRType>
    MVIDLPoly<CRType> operator*(const CLType &lhs, const MVIDLPoly<CRType> &rhs)
    {
      CRType plhs(static_cast<CRType>(lhs));
      return rhs * plhs;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_LPolyMV_H
