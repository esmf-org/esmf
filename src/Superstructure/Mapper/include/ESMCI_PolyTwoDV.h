#ifndef ESMCI_PolyTwoDV_H
#define ESMCI_PolyTwoDV_H

#include <iostream>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_Poly.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_LPolyMV.h"
//#include "ESMCI_PolyDer.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename CType>
    int FindPDerivative(const TwoVIDPoly<CType>& poly, const std::string &vname,
                      TwoVIDPoly<CType>& dpoly);

    /* Polynomials with variables that have dependent
     * variables
     */
    template<typename CType, typename DType>
    class TwoDVPoly : public GenPoly<CType, DType>{
      public:
        virtual ~TwoDVPoly() = default;
        void set_degs(const std::vector<std::vector<DType> > &degs) { assert(0); }
        std::vector<std::vector<DType> > get_degs(void ) const { assert(0); }
    };

    template<typename CType>
    class TwoDVIDPoly : public TwoDVPoly<CType, int>{
      public:
        TwoDVIDPoly();
        TwoDVIDPoly(const CType &coeff);
        TwoDVIDPoly(const std::vector<CType>& coeffs);
        TwoDVIDPoly(std::initializer_list<CType> coeffs);
        TwoDVIDPoly(const UVIDPoly<CType> &uvpoly);
        TwoDVIDPoly(const UVIDPoly<CType> &uvpoly, const std::vector<std::string> &vnames);
        int get_max_deg(void ) const;
        void set_vnames(const std::vector<std::string>& vnames);
        std::vector<std::string> get_vnames(void ) const;
        std::vector<std::string> get_dvnames(void ) const;
        void set_coeffs(const std::vector<CType>& coeffs);
        void set_coeffs(std::initializer_list<CType> coeffs);
        void set_dfuncs(const std::vector<MVIDLPoly<CType> > &dps);
        std::vector<MVIDLPoly<CType> > get_dfuncs(void ) const;
        MVIDLPoly<CType> get_dfunc(void ) const;
        void set_cs_infos(const std::vector<PolyCSInfo<CType> > &csinfos);
        std::vector<CType> get_coeffs(void ) const;
        std::vector<std::vector<CType> > get_dcoeffs(void ) const;
        /* Evaluate using independent variables */
        CType eval(const std::vector<CType> &vvals) const;
        /* Evaluate using dependent variables */
        CType deval(const std::vector<CType> &vvals) const;
        TwoDVIDPoly<CType> find_pd(const std::string &vname) const;

        template<typename T>
        friend std::ostream &operator<<(std::ostream &ostr, const TwoDVIDPoly<T> &p);

        template<typename T>
        friend TwoDVIDPoly<T> operator+(const TwoDVIDPoly<T> &lhs,
          const TwoDVIDPoly<T> &rhs);
        template<typename T, typename U>
        friend TwoDVIDPoly<U> operator+(const T &lhs,
          const TwoDVIDPoly<U> &rhs);

        template<typename T>
        friend TwoDVIDPoly<T> operator-(const TwoDVIDPoly<T> &lhs,
          const TwoDVIDPoly<T> &rhs);
        template<typename T, typename U>
        friend TwoDVIDPoly<U> operator-(const T &lhs,
          const TwoDVIDPoly<U> &rhs);

        template<typename T>
        friend TwoDVIDPoly<T> operator*(const TwoDVIDPoly<T> &lhs,
          const TwoDVIDPoly<T> &rhs);
        template<typename T, typename U>
        friend TwoDVIDPoly<U> operator*(const T &lhs,
          const TwoDVIDPoly<U> &rhs);

        template<typename T>
        friend bool operator==(const TwoDVIDPoly<T> &lhs,
          const TwoDVIDPoly<T> &rhs);
      private:
        TwoVIDPoly<CType> p_;
        std::vector<MVIDLPoly<CType> > dps_;
        MVIDLPoly<CType> dp_;
        std::vector<std::string> dps_vnames_;
        bool has_dep_vfunc_;
    };  // class TwoDVIDPoly

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly():has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly(const CType &coeff):p_(coeff), has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly(const std::vector<CType>& coeffs)
      :p_(coeffs), has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly(std::initializer_list<CType> coeffs)
      :p_(coeffs), has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly(const UVIDPoly<CType> &uvpoly)
      :p_(uvpoly), has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    TwoDVIDPoly<CType>::TwoDVIDPoly(const UVIDPoly<CType> &uvpoly,
      const std::vector<std::string> &vnames)
        :p_(uvpoly, vnames), has_dep_vfunc_(false)
    {
    }

    template<typename CType>
    int TwoDVIDPoly<CType>::get_max_deg(void ) const
    {
      return p_.get_max_deg();
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_vnames(const std::vector<std::string>& vnames)
    {
      p_.set_vnames(vnames);
    }

    template<typename CType>
    std::vector<std::string> TwoDVIDPoly<CType>::get_vnames(void ) const
    {
      if(!has_dep_vfunc_){
        return p_.get_vnames();
      }
      else{
        return get_dvnames();
      }
    }

    template<typename CType>
    std::vector<std::string> TwoDVIDPoly<CType>::get_dvnames(void ) const
    {
      return dps_vnames_;
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_coeffs(const std::vector<CType>& coeffs)
    {
      p_.set_coeffs(coeffs);
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_coeffs(std::initializer_list<CType> coeffs)
    {
      p_.set_coeffs(coeffs);
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_dfuncs(const std::vector<MVIDLPoly<CType> > &dps)
    {
      dps_ = dps;
      dps_vnames_.clear();
      if(!dps_.empty()){
        std::vector<std::string> all_vnames_uniq;
        typename std::vector<MVIDLPoly<CType> >::const_iterator citer = dps.cbegin();
        //std::cout << "Dep func : " << *citer << "\n";
        MVIDLPoly<CType> dp = *citer;
        ++citer;
        for(;citer != dps.cend(); ++citer){
          //std::cout << "Dep func : " << *citer << "\n";
          dp = dp + (*citer);

          std::vector<std::string> tmp_dp_vnames = (*citer).get_vnames();
          all_vnames_uniq.insert(all_vnames_uniq.end(),
            tmp_dp_vnames.begin(), tmp_dp_vnames.end());
        }
        dp_ = dp;
        //std::cout << "Dep cumulative func : " << dp_ << "\n";
        has_dep_vfunc_ = true;

        /* Find and cache the unique dependent function variable names */
        std::sort(all_vnames_uniq.begin(), all_vnames_uniq.end());
        all_vnames_uniq.erase(std::unique(all_vnames_uniq.begin(), all_vnames_uniq.end()),
                              all_vnames_uniq.end());
        dps_vnames_ = all_vnames_uniq;
      }
    }

    template<typename CType>
    std::vector<MVIDLPoly<CType> > TwoDVIDPoly<CType>::get_dfuncs(void ) const
    {
      return dps_;
    }

    template<typename CType>
    MVIDLPoly<CType> TwoDVIDPoly<CType>::get_dfunc(void ) const
    {
      return dp_;
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_cs_infos(const std::vector<PolyCSInfo<CType> > &csinfos)
    {
      p_.set_cs_infos(csinfos);
    }

    template<typename CType>
    std::vector<CType> TwoDVIDPoly<CType>::get_coeffs(void ) const
    {
      return p_.get_coeffs();
    }

    template<typename CType>
    std::vector<std::vector<CType> > TwoDVIDPoly<CType>::get_dcoeffs(void ) const
    {
      std::vector<std::vector<CType> > dp_coeffs;
      for(typename std::vector<MVIDLPoly<CType> >::const_iterator citer = dps_.cbegin();
          citer != dps_.cend(); ++citer){
        dp_coeffs.push_back((*citer).get_coeffs());
      }
      return dp_coeffs;
    }

    template<typename CType>
    CType TwoDVIDPoly<CType>::eval(const std::vector<CType> &vvals) const
    {
      if(has_dep_vfunc_){
        std::vector<CType> p_vvals;
        p_vvals.reserve(dps_.size());
        for(typename std::vector<MVIDLPoly<CType> >::const_iterator citer = dps_.cbegin();
              citer != dps_.cend(); ++citer){
          p_vvals.push_back((*citer).eval(vvals));
        }
        return p_.eval(p_vvals);
      }
      else{
        return p_.eval(vvals);
      }
    }

    template<typename CType>
    CType TwoDVIDPoly<CType>::deval(const std::vector<CType> &vvals) const
    {
      return p_.eval(vvals);
    }

    template<typename CType>
    TwoDVIDPoly<CType> TwoDVIDPoly<CType>::find_pd(const std::string &vname) const
    {
      int ret = ESMF_SUCCESS;
      bool is_dep_var = false;

      TwoDVIDPoly<CType> res;

      std::vector<std::string> p_vnames = p_.get_vnames();
      assert(p_vnames.size() == 2);
      std::vector<std::string>::const_iterator iter = std::find(p_vnames.cbegin(),
                                                        p_vnames.cend(),
                                                        vname);
      if(iter == p_vnames.cend()){
        is_dep_var = true;
      }

      if(has_dep_vfunc_ && is_dep_var){
        int ret = ESMF_SUCCESS;
        TwoVIDPoly<CType> pd;
        /* x = first var, y = second var */
        TwoVIDPoly<CType> dp_dx;
        TwoVIDPoly<CType> dp_dy;

        ret = FindPDerivative(p_, p_vnames[0], dp_dx);
        assert(ret == ESMF_SUCCESS);

        ret = FindPDerivative(p_, p_vnames[1], dp_dy);
        assert(ret == ESMF_SUCCESS);

        MVIDLPoly<CType> dx_dv;
        CType coeff_dx_dv = static_cast<CType>(0);
        MVIDLPoly<CType> dy_dv;
        CType coeff_dy_dv = static_cast<CType>(0);

        assert(dps_.size() == 2);

        ret = FindPDerivative(dps_[0], vname, dx_dv);
        assert(ret == ESMF_SUCCESS);
        std::vector<CType> dx_dv_coeffs = dx_dv.get_coeffs();
        for(typename std::vector<CType>::const_iterator citer = dx_dv_coeffs.cbegin();
              citer != dx_dv_coeffs.cend(); ++citer){
          if(*citer != static_cast<CType>(0)){
            coeff_dx_dv = *citer;
            break;
          }
        }

        ret = FindPDerivative(dps_[1], vname, dy_dv);
        assert(ret == ESMF_SUCCESS);
        std::vector<CType> dy_dv_coeffs = dy_dv.get_coeffs();
        for(typename std::vector<CType>::const_iterator citer = dy_dv_coeffs.cbegin();
              citer != dy_dv_coeffs.cend(); ++citer){
          if(*citer != static_cast<CType>(0)){
            coeff_dy_dv = *citer;
            break;
          }
        }

        pd = coeff_dx_dv * dp_dx + coeff_dy_dv * dp_dy;
        
        res.p_ = pd;
      }
      else{
        TwoVIDPoly<CType> pd;
        ret = FindPDerivative(p_, vname, pd);
        assert(ret == ESMF_SUCCESS);

        //res.set_coeffs(pd.get_coeffs());
        //res.set_vnames(p_.get_vnames());
        res.p_ = pd;
      }
      res.dps_ = dps_;
      res.dp_ = dp_;
      res.dps_vnames_ = dps_vnames_;
      res.has_dep_vfunc_ = has_dep_vfunc_;
      return res;
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const TwoDVIDPoly<CType> &p)
    {
      ostr << p.p_;
      if(p.has_dep_vfunc_){
        ostr << "( Dependencies : ";
        for(typename std::vector<MVIDLPoly<CType> >::const_iterator citer =
              p.dps_.cbegin();
            citer != p.dps_.cend(); ++citer){
          ostr << *citer << ", ";
        }
        ostr << " ) ";
      }
      return ostr;
    }

    template<typename CType>
    TwoDVIDPoly<CType> operator+(const TwoDVIDPoly<CType> &lhs,
      const TwoDVIDPoly<CType> &rhs)
    {
      TwoDVIDPoly<CType> res;

      res.p_ = lhs.p_ + rhs.p_;
      assert(lhs.dps_.size() == rhs.dps_.size());

      res.dps_ = lhs.dps_;
      res.dp_ = lhs.dp_;
      res.dps_vnames_ = lhs.dps_vnames_;
      res.has_dep_vfunc_ = lhs.has_dep_vfunc_;
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator+(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs + rhs.p_;

      res.dps_ = rhs.dps_;
      res.dp_ = rhs.dp_;
      res.dps_vnames_ = rhs.dps_vnames_;
      res.has_dep_vfunc_ = rhs.has_dep_vfunc_;
      return res;
    }

    template<typename CType>
    TwoDVIDPoly<CType> operator-(const TwoDVIDPoly<CType> &lhs,
      const TwoDVIDPoly<CType> &rhs)
    {
      TwoDVIDPoly<CType> res;

      res.p_ = lhs.p_ - rhs.p_;
      assert(lhs.dps_.size() == rhs.dps_.size());

      res.dps_ = lhs.dps_;
      res.dp_ = lhs.dp_;
      res.dps_vnames_ = lhs.dps_vnames_;
      res.has_dep_vfunc_ = lhs.has_dep_vfunc_;
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator-(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs - rhs.p_;

      res.dps_ = rhs.dps_;
      res.dp_ = rhs.dp_;
      res.dps_vnames_ = rhs.dps_vnames_;
      res.has_dep_vfunc_ = rhs.has_dep_vfunc_;
      return res;
    }

    template<typename CType>
    TwoDVIDPoly<CType> operator*(const TwoDVIDPoly<CType> &lhs,
      const TwoDVIDPoly<CType> &rhs)
    {
      TwoDVIDPoly<CType> res;

      res.p_ = lhs.p_ * rhs.p_;
      assert(lhs.dps_.size() == rhs.dps_.size());

      res.dps_ = lhs.dps_;
      res.dp_ = lhs.dp_;
      res.dps_vnames_ = lhs.dps_vnames_;
      res.has_dep_vfunc_ = lhs.has_dep_vfunc_;
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator*(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs * rhs.p_;

      res.dps_ = rhs.dps_;
      res.dp_ = rhs.dp_;
      res.dps_vnames_ = rhs.dps_vnames_;
      res.has_dep_vfunc_ = rhs.has_dep_vfunc_;
      return res;
    }

    template<typename CType>
    bool operator==(const TwoDVIDPoly<CType> &lhs, const TwoDVIDPoly<CType> &rhs)
    {
      /* Compare only coeffs of the poly */
      std::vector<CType> lhs_p_coeffs = lhs.p_.get_coeffs();
      std::vector<CType> rhs_p_coeffs = rhs.p_.get_coeffs();
      if(lhs_p_coeffs.size() != rhs_p_coeffs.size()){
        return false;
      }

      for(typename std::vector<CType>::const_iterator
            citer_lhs = lhs_p_coeffs.cbegin(), citer_rhs = rhs_p_coeffs.cbegin();
            (citer_lhs != lhs_p_coeffs.cend()) && (citer_rhs != rhs_p_coeffs.cend());
            ++citer_lhs, ++citer_rhs){
        if(*citer_lhs != *citer_rhs){
          return false;
        }
      }

      /* Compare variable and dep variable names */
      std::vector<std::string> lhs_vnames = lhs.get_dvnames();
      std::vector<std::string> rhs_vnames = rhs.get_dvnames();
      if(lhs_vnames.size() != rhs_vnames.size()){
        return false;
      }

      std::sort(lhs_vnames.begin(), lhs_vnames.end());
      std::sort(rhs_vnames.begin(), rhs_vnames.end());
      for(typename std::vector<std::string>::const_iterator
          citer_lhs = lhs_vnames.cbegin(), citer_rhs = rhs_vnames.cbegin();
          (citer_lhs != lhs_vnames.cend()) && (citer_rhs != rhs_vnames.cend());
          ++citer_lhs, ++citer_rhs){
        if(*citer_lhs != *citer_rhs){
          return false;
        }
      }

      return true;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_PolyTwoDV_H
