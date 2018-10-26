#ifndef ESMCI_PolyTwoV_H
#define ESMCI_PolyTwoV_H

#include <vector>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"

namespace ESMCI{
  namespace MapperUtil{

    /* A polynomial with 2 variables
     * This abstract class extends from the generic polynomial class
     */
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

    /* A namespace for utils used with two variable polynomials */
    namespace TwoVIDPolyUtil{
      /* Get the max degree of a polynomial with two variables */
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

    /* A polynomial with 2 variables and integer degrees
     * This concrete class extends from the two variable polynomial class
     */
    template<typename CType>
    class TwoVIDPoly : public TwoVPoly<CType, int>{
      public:
        TwoVIDPoly();
        virtual ~TwoVIDPoly() = default;
        TwoVIDPoly(const CType &coeff);
        TwoVIDPoly(const std::vector<CType>& coeffs);
        TwoVIDPoly(std::initializer_list<CType> coeffs);
        TwoVIDPoly(const UVIDPoly<CType> &uvpoly);
        TwoVIDPoly(const UVIDPoly<CType> &uvpoly, const std::vector<std::string> &vnames);
        int get_max_deg(void ) const;
        void set_vnames(const std::vector<std::string>& vnames); 
        std::vector<std::string> get_vnames(void ) const; 
        void set_coeffs(const std::vector<CType>& coeffs); 
        void set_coeffs(std::initializer_list<CType> coeffs);
        void set_cs_infos(const std::vector<PolyCSInfo<CType> > &csinfos);
        bool has_cs_info(void ) const;
        std::vector<PolyCSInfo<CType> > get_cs_infos(void ) const;
        std::vector<CType> get_coeffs(void ) const;
        CType eval(const std::vector<CType> &vvals) const;
      private:
        int max_deg_;
        std::vector<CType> coeffs_;
        std::vector<std::string> vnames_;
        std::vector<PolyCSInfo<CType> > csinfos_;
        bool has_cs_info_;
    }; // class TwoVIDPoly

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(): has_cs_info_(false)
    {
      vnames_.push_back("x");
      vnames_.push_back("y");
    }

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const CType &coeff): has_cs_info_(false)
    {
      coeffs_.push_back(coeff);
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
      vnames_.push_back("x");
      vnames_.push_back("y");
    }

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const std::vector<CType>& coeffs):coeffs_(coeffs), has_cs_info_(false)
    {
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
      vnames_.push_back("x");
      vnames_.push_back("y");
    }

    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end()), has_cs_info_(false)
    {
      max_deg_ = TwoVIDPolyUtil::get_max_deg(coeffs_.size());
      vnames_.push_back("x");
      vnames_.push_back("y");
    }

    /* Create a two variable polynomial from a univariate polynomial */
    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const UVIDPoly<CType> &uvpoly): has_cs_info_(false)
    {
      max_deg_ = uvpoly.get_max_deg();

      int cur_deg = max_deg_;
      int ncoeffs_in_cur_deg=cur_deg+1;

      std::vector<CType> uvpoly_coeffs = uvpoly.get_coeffs();
      std::vector<std::string> uvpoly_vnames = uvpoly.get_vnames();
      assert(uvpoly_vnames.size() == 1);

      for(typename std::vector<CType>::const_iterator citer = uvpoly_coeffs.cbegin();
          citer != uvpoly_coeffs.cend(); ++citer){

        coeffs_.push_back(*citer);
        
        ncoeffs_in_cur_deg--;
        while(ncoeffs_in_cur_deg != 0){
          coeffs_.push_back(0);
          ncoeffs_in_cur_deg--;
        }
        cur_deg--;
        ncoeffs_in_cur_deg = cur_deg+1;
      }
      
      vnames_.push_back(uvpoly_vnames[0]);
      vnames_.push_back("y");
      //csinfo_ = uvpoly.get_cs_info();
      if(uvpoly.has_cs_info()){
        csinfos_.push_back(uvpoly.get_cs_info());
        PolyCSInfo<CType> dummy_csinfo;
        csinfos_.push_back(dummy_csinfo);
        has_cs_info_ = true;
      }
    }

    /* Create a two variable polynomial from a univariate polynomial
     * The user also provides the variable names to be used in the 
     * two variable polynomial.
     * Note that the variable name in the univariate polynomial needs
     * to be one of the variable names provided via vnames.
     */
    template<typename CType>
    inline TwoVIDPoly<CType>::TwoVIDPoly(const UVIDPoly<CType> &uvpoly,
              const std::vector<std::string> &vnames):
                vnames_(vnames), has_cs_info_(false)
    {
      max_deg_ = uvpoly.get_max_deg();

      std::vector<std::string> uvpoly_vnames = uvpoly.get_vnames();
      assert(uvpoly_vnames.size() == 1);

      bool is_x_var = (uvpoly_vnames[0] == vnames_[0]) ? true : false;
      int cur_deg = max_deg_;
      int ncoeffs_in_cur_deg=cur_deg+1;

      std::vector<CType> uvpoly_coeffs = uvpoly.get_coeffs();

      if(is_x_var){
        for(typename std::vector<CType>::const_iterator citer = uvpoly_coeffs.cbegin();
            citer != uvpoly_coeffs.cend(); ++citer){

          coeffs_.push_back(*citer);
          
          ncoeffs_in_cur_deg--;
          while(ncoeffs_in_cur_deg != 0){
            coeffs_.push_back(0);
            ncoeffs_in_cur_deg--;
          }
          cur_deg--;
          ncoeffs_in_cur_deg = cur_deg+1;
        }
      }
      else{
        for(typename std::vector<CType>::const_iterator citer = uvpoly_coeffs.cbegin();
            citer != uvpoly_coeffs.cend(); ++citer){

          ncoeffs_in_cur_deg--;
          while(ncoeffs_in_cur_deg != 0){
            coeffs_.push_back(0);
            ncoeffs_in_cur_deg--;
          }
          coeffs_.push_back(*citer);
          
          cur_deg--;
          ncoeffs_in_cur_deg = cur_deg+1;
        }
      }
      //csinfo_ = uvpoly.get_cs_info();
      if(uvpoly.has_cs_info()){
        PolyCSInfo<CType> dummy_csinfo;
        if(is_x_var){
          csinfos_.push_back(uvpoly.get_cs_info());
          csinfos_.push_back(dummy_csinfo);
        }
        else{
          csinfos_.push_back(dummy_csinfo);
          csinfos_.push_back(uvpoly.get_cs_info());
        }
      }
    }

    template<typename CType>
    int TwoVIDPoly<CType>::get_max_deg(void ) const
    {
      return max_deg_;
    }

    template<typename CType>
    inline void TwoVIDPoly<CType>::set_vnames(const std::vector<std::string>& vnames)
    {
      assert(vnames.size() == 2);
      vnames_ = vnames;
    }

    template<typename CType>
    inline std::vector<std::string> TwoVIDPoly<CType>::get_vnames(void ) const
    {
      return vnames_;
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
    inline void TwoVIDPoly<CType>::set_cs_infos(
      const std::vector<PolyCSInfo<CType> > &csinfos)
    {
      csinfos_ = csinfos;
      has_cs_info_ = true;
    }

    template<typename CType>
    inline bool TwoVIDPoly<CType>::has_cs_info(void ) const
    {
      return has_cs_info_;
    }
    template<typename CType>
    inline std::vector<PolyCSInfo<CType> > TwoVIDPoly<CType>::get_cs_infos(void ) const
    {
      return csinfos_;
    }

    template<typename CType>
    inline std::vector<CType> TwoVIDPoly<CType>::get_coeffs(void ) const
    {
      return coeffs_;
    }

    /* Evaluate a polynomial */
    template<typename CType>
    inline CType TwoVIDPoly<CType>::eval(const std::vector<CType> &orig_vvals) const
    {
      CType res = 0;
      int cur_deg = max_deg_;
      int cur_xdeg = cur_deg;
      int cur_ydeg = 0;
      int ncoeffs_in_cur_deg=cur_deg+1;

      assert(orig_vvals.size() == 2);
      std::vector<CType> vvals(orig_vvals);
      if(has_cs_info_){
        vvals[0] = csinfos_[0].center_and_scale(orig_vvals[0]);
        vvals[1] = csinfos_[1].center_and_scale(orig_vvals[1]);
      }
      for(typename std::vector<CType>::const_iterator citer = coeffs_.cbegin();
          citer != coeffs_.cend(); ++citer){

        res += (*citer) * pow(vvals[0], cur_xdeg) * pow(vvals[1], cur_ydeg);

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

      return res;
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const TwoVIDPoly<CType>& p)
    {
      std::vector<CType> coeffs(p.get_coeffs());
      int cur_deg = p.get_max_deg();
      int cur_xdeg = cur_deg;
      int cur_ydeg = 0;
      int ncoeffs_in_cur_deg=cur_deg+1;
      std::vector<std::string> vnames = p.get_vnames();
      assert(vnames.size() == 2);
      for(typename std::vector<CType>::iterator iter = coeffs.begin();
            iter != coeffs.end(); ++iter){
        CType val = *iter;
        if(val != 0){
          ostr << *iter << " ";
        }
        if(val != 0){
          if(cur_xdeg > 1){
            ostr << vnames[0].c_str() << "^" << cur_xdeg;
          }
          else if(cur_xdeg == 1){
            ostr << vnames[0].c_str();
          }
          if(cur_ydeg > 1){
            ostr << vnames[1].c_str() << "^" << cur_ydeg;
          }
          else if(cur_ydeg == 1){
            ostr << vnames[1].c_str();
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
      std::vector<std::string> lhs_vnames = lhs.get_vnames();
      std::vector<std::string> rhs_vnames = rhs.get_vnames();

      assert(lhs_vnames == rhs_vnames);

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

      res.set_vnames(lhs_vnames);
      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_infos() == rhs.get_cs_infos());
        }
        res.set_cs_infos(lhs.get_cs_infos());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_infos(rhs.get_cs_infos());
      }

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
      std::vector<std::string> lhs_vnames = lhs.get_vnames();
      std::vector<std::string> rhs_vnames = rhs.get_vnames();

      assert(lhs_vnames == rhs_vnames);

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

      res.set_vnames(lhs_vnames);
      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_infos() == rhs.get_cs_infos());
        }
        res.set_cs_infos(lhs.get_cs_infos());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_infos(rhs.get_cs_infos());
      }


      return res;
    }

    template<typename CLType, typename CRType>
    TwoVIDPoly<CRType> operator-(const CLType &lhs, const TwoVIDPoly<CRType> &rhs)
    {
      TwoVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      return plhs - rhs;
    }

    /* Namespace for utils used by two variable polynomial with integer degs */
    namespace TwoVIDPolyUtil{
      /* Calculate the number of coefficients in a two variable polynomial
       * with max degree, max_deg
       */
      static inline int get_ncoeffs(int max_deg)
      {
        int ncoeffs = 1;

        while(max_deg){
          ncoeffs += (max_deg + 1);
          max_deg--;
        }

        return ncoeffs;
      }

      /* Calculate the index of the coefficient, in the polynomial 
       * representation of a two variable polynomial, with maximum
       * degree, max_deg and the individual variable degrees, v1deg
       * and v2deg
       */
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

        //std::cout << "(" << v1deg << "," << v2deg << ") = " << coeff_idx << "\n";

        return coeff_idx;
      }
    } // namespace TwoVIDPolyUtil

    template<typename CType>
    TwoVIDPoly<CType> operator*(const TwoVIDPoly<CType> &lhs, const TwoVIDPoly<CType> &rhs)
    {
      std::vector<std::string> lhs_vnames = lhs.get_vnames();
      std::vector<std::string> rhs_vnames = rhs.get_vnames();

      assert(lhs_vnames == rhs_vnames);

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
          if(val != 0){
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

      res.set_vnames(lhs_vnames);
      if(lhs.has_cs_info()){
        if(rhs.has_cs_info()){
          assert(lhs.get_cs_infos() == rhs.get_cs_infos());
        }
        res.set_cs_infos(lhs.get_cs_infos());
      }
      else if(rhs.has_cs_info()){
        res.set_cs_infos(rhs.get_cs_infos());
      }

      return res;
    }

    template<typename CLType, typename CRType>
    TwoVIDPoly<CRType> operator*(const CLType &lhs, const TwoVIDPoly<CRType> &rhs)
    {
      TwoVIDPoly<CRType> plhs(static_cast<CRType>(lhs));
      std::vector<std::string> vnames = rhs.get_vnames();
      plhs.set_vnames(vnames);
      return plhs * rhs;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_PolyTwoV_H
