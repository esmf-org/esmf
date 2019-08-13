#ifndef ESMCI_LPolyMV_H
#define ESMCI_LPolyMV_H

#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <initializer_list>
#include <cassert>
#include <cmath>
#include "ESMCI_Poly.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Multivariate (multiple variables) linear polynomial class
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

    /* Multivariate linear polynomial with integer degrees
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
        bool has_cs_info(void ) const;
        std::vector<CType> get_coeffs(void ) const;
        CType eval(const std::vector<CType> &vvals) const;
        CType eval(const CType &vval) const;
      private:
        std::vector<CType> coeffs_;
        std::vector<std::string> vnames_;
        PolyCSInfo<CType> csinfo_;
        bool has_cs_info_;
    }; // class MVIDLPoly

    namespace MVIDLPolyUtil{
      inline std::vector<std::string> append_int2str(const std::string &str, int range_start,
        int range_end)
      {
        std::vector<std::string> res;
        assert(range_end > range_start);
        for(int i=range_start; i<range_end; i++){
          std::ostringstream ostr;
          ostr << str.c_str() << i;
          res.push_back(ostr.str());
        }

        return res;
      }
    } // namespace MVIDLPolyUtil


    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(): has_cs_info_(false)
    {
      vnames_.push_back("x");
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(const CType &coeff): has_cs_info_(false)
    {
      coeffs_.push_back(coeff);
      vnames_.push_back("x");
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(const std::vector<CType>& coeffs):coeffs_(coeffs), has_cs_info_(false)
    {
      assert(coeffs.size() > 0);
      vnames_ = MVIDLPolyUtil::append_int2str(std::string("x"), 0, coeffs.size()-1);
    }

    template<typename CType>
    inline MVIDLPoly<CType>::MVIDLPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end()), has_cs_info_(false)
    {
      assert(coeffs.size() > 0);
      vnames_ = MVIDLPolyUtil::append_int2str(std::string("x"), 0, coeffs.size()-1);
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
      has_cs_info_ = true;
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

      /* We don't need cs infos right now for this poly */
      assert(!has_cs_info_);

      CType res = 0;

      std::size_t i=0;
      for(; i < vvals.size(); i++){
        res += coeffs_[i] * vvals[i];
      }
      for(; i < coeffs_.size(); i++){
        res += coeffs_[i];
      }
      /*
      typename std::vector<CType>::const_iterator coeffs_iter = coeffs_.cbegin();
      typename std::vector<CType>::const_iterator vvals_iter = vvals.cbegin();
      for(;(coeffs_iter != coeffs_.cend()) && (vvals_iter != vvals.cend());
            ++coeffs_iter, ++vvals_iter){
        res += (*coeffs_iter) * (*vvals_iter);
      }

      for(;coeffs_iter != coeffs_.cend(); ++coeffs_iter){
        res += *coeffs_iter;
      }
      */

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

      typename std::vector<CType>::const_iterator coeffs_iter = coeffs.cbegin();
      typename std::vector<std::string>::const_iterator vnames_iter = vnames.cbegin();
      for(;(coeffs_iter != coeffs.cend()) && (vnames_iter != vnames.cend());
          ++coeffs_iter, ++vnames_iter){
        ostr << *coeffs_iter << *vnames_iter << " + ";
      }
      for(;coeffs_iter != coeffs.cend(); ++coeffs_iter){
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

      if(lhs_coeffs.size() == 0){
        MVIDLPoly<CType> res = rhs;
        return res;
      }

      if(rhs_coeffs.size() == 0){
        MVIDLPoly<CType> res = lhs;
        return res;
      }

      // FIXME : Get rid of these special checks
      if(lhs_coeffs.size() == 1){
        std::vector<CType> res_coeffs;
        std::vector<std::string> res_vnames;
        if(rhs_coeffs.size() > 0){
          res_coeffs = rhs.get_coeffs();
          res_vnames = rhs.get_vnames();
          res_coeffs[res_coeffs.size()-1] += lhs_coeffs[0];
        }
        else{
          res_coeffs = lhs.get_coeffs();
          res_vnames = lhs.get_vnames();
        }
        MVIDLPoly<CType> res(res_coeffs);
        res.set_vnames(res_vnames);
        return res;
      }

      if(rhs_coeffs.size() == 1){
        std::vector<CType> res_coeffs;
        std::vector<std::string> res_vnames;
        if(lhs_coeffs.size() > 0){
          res_coeffs = lhs.get_coeffs();
          res_vnames = lhs.get_vnames();
          res_coeffs[res_coeffs.size()-1] += rhs_coeffs[0];
        }
        else{
          res_coeffs = rhs.get_coeffs();
          res_vnames = rhs.get_vnames();
        }
        MVIDLPoly<CType> res(res_coeffs);
        res.set_vnames(res_vnames);
        return res;
      }

      // Info on whether rhs coeff already included in the result
      std::vector<bool> rhs_coeffs_in_res(rhs_vnames.size(), false);

      typename std::vector<CType>::const_iterator citer_lhs = lhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_lhs = lhs_vnames.cbegin();

      typename std::vector<CType>::const_iterator citer_rhs = rhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_rhs = rhs_vnames.cbegin();
      typename std::vector<bool>::iterator biter_rhs = rhs_coeffs_in_res.begin();
      for(;(citer_lhs != lhs_coeffs.cend()) && (viter_lhs != lhs_vnames.cend());
          ++citer_lhs, ++viter_lhs){
        CType res = *citer_lhs;

        citer_rhs = rhs_coeffs.cbegin();
        viter_rhs = rhs_vnames.cbegin();
        biter_rhs = rhs_coeffs_in_res.begin();
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
      biter_rhs = rhs_coeffs_in_res.begin();
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

      assert(citer_lhs != lhs_coeffs.cend());
      assert(citer_rhs != rhs_coeffs.cend());
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

    template<typename CLType, typename CRType>
    MVIDLPoly<CLType> operator+(const MVIDLPoly<CLType> &lhs, const CRType &rhs)
    {
      MVIDLPoly<CLType> prhs(static_cast<CLType>(rhs));
      return lhs + prhs;
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

      // FIXME : Get rid of these special checks
      if(lhs_coeffs.size() == 1){
        std::vector<CType> res_coeffs;
        std::vector<std::string> res_vnames;
        if(rhs_coeffs.size() > 0){
          res_coeffs = rhs.get_coeffs();
          res_vnames = rhs.get_vnames();
          res_coeffs[res_coeffs.size()-1] -= lhs_coeffs[0];
        }
        else{
          res_coeffs = lhs.get_coeffs();
          res_vnames = lhs.get_vnames();
        }
        MVIDLPoly<CType> res(res_coeffs);
        res.set_vnames(res_vnames);
        return res;
      }

      if(rhs_coeffs.size() == 1){
        std::vector<CType> res_coeffs;
        std::vector<std::string> res_vnames;
        if(lhs_coeffs.size() > 0){
          res_coeffs = lhs.get_coeffs();
          res_vnames = lhs.get_vnames();
          res_coeffs[res_coeffs.size()-1] -= rhs_coeffs[0];
        }
        else{
          res_coeffs = rhs.get_coeffs();
          res_vnames = rhs.get_vnames();
        }
        MVIDLPoly<CType> res(res_coeffs);
        res.set_vnames(res_vnames);
        return res;
      }

      // Info on whether rhs coeff already included in the result
      std::vector<bool> rhs_coeffs_in_res(rhs_vnames.size(), false);

      typename std::vector<CType>::const_iterator citer_lhs = lhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_lhs = lhs_vnames.cbegin();

      typename std::vector<CType>::const_iterator citer_rhs = rhs_coeffs.cbegin();
      typename std::vector<std::string>::const_iterator viter_rhs = rhs_vnames.cbegin();
      typename std::vector<bool>::iterator biter_rhs = rhs_coeffs_in_res.begin();
      for(;(citer_lhs != lhs_coeffs.cend()) && (viter_lhs != lhs_vnames.cend());
          ++citer_lhs, ++viter_lhs){
        CType res = *citer_lhs;

        citer_rhs = rhs_coeffs.cbegin();
        viter_rhs = rhs_vnames.cbegin();
        biter_rhs = rhs_coeffs_in_res.begin();
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
      biter_rhs = rhs_coeffs_in_res.begin();
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

      assert(citer_lhs != lhs_coeffs.cend());
      assert(citer_rhs != rhs_coeffs.cend());
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

    template<typename CLType, typename CRType>
    MVIDLPoly<CLType> operator-(const MVIDLPoly<CLType> &lhs, const CRType &rhs)
    {
      MVIDLPoly<CLType> prhs(static_cast<CLType>(rhs));
      return lhs - prhs;
    }

    template<typename CType>
    MVIDLPoly<CType> operator*(const MVIDLPoly<CType> &lhs, const CType &rhs)
    {
      std::vector<CType> res_coeffs = lhs.get_coeffs();
      std::vector<std::string> res_vnames = lhs.get_vnames();

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
