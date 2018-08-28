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
        std::vector<std::vector<std::string> > get_dvnames(void ) const;
        void set_coeffs(const std::vector<CType>& coeffs);
        void set_coeffs(std::initializer_list<CType> coeffs);
        void set_dfuncs(const std::vector<MVIDLPoly<CType> > &dps);
        std::vector<MVIDLPoly<CType> > get_dfuncs(void ) const;
        void set_cs_info(const PolyCSInfo<CType> &csinfo);
        std::vector<CType> get_coeffs(void ) const;
        std::vector<std::vector<CType> > get_dcoeffs(void ) const;
        CType eval(const std::vector<CType> &vvals) const;
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
      private:
        TwoVIDPoly<CType> p_;
        std::vector<MVIDLPoly<CType> > dps_;
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
      return p_.get_vnames();
    }

    template<typename CType>
    std::vector<std::vector<std::string> > TwoDVIDPoly<CType>::get_dvnames(void ) const
    {
      std::vector<std::vector<std::string> > dp_vnames;
      for(typename std::vector<MVIDLPoly<CType> >::const_iterator citer = dps_.cbegin();
            citer != dps_.cend(); ++citer){
        dp_vnames.push_back((*citer).get_vnames());
      }
      return dp_vnames;
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
      has_dep_vfunc_ = true;
    }

    template<typename CType>
    std::vector<MVIDLPoly<CType> > TwoDVIDPoly<CType>::get_dfuncs(void ) const
    {
      return dps_;
    }

    template<typename CType>
    void TwoDVIDPoly<CType>::set_cs_info(const PolyCSInfo<CType> &csinfo)
    {
      p_.set_cs_info(csinfo);
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
        assert(0);
      }
      else{
        return p_.eval(vvals);
      }
    }

    template<typename CType>
    TwoDVIDPoly<CType> TwoDVIDPoly<CType>::find_pd(const std::string &vname) const
    {
      int ret = ESMF_SUCCESS;
      TwoDVIDPoly<CType> res;
      if(has_dep_vfunc_){
        assert(0);
      }
      else{
        TwoVIDPoly<CType> pd;
        ret = FindPDerivative(p_, vname, pd);
        assert(ret == ESMF_SUCCESS);

        res.set_coeffs(pd.get_coeffs());
        res.set_vnames(p_.get_vnames());
      }
      return res;
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const TwoDVIDPoly<CType> &p)
    {
      ostr << p.p_;
      if(p.has_dep_vfunc_){
        assert(0);
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
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator+(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs + rhs.p_;

      res.dps_ = rhs.dps_;
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
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator-(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs - rhs.p_;

      res.dps_ = rhs.dps_;
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
      return res;
    }

    template<typename CLType, typename CRType>
    TwoDVIDPoly<CRType> operator*(const CLType &lhs,
      const TwoDVIDPoly<CRType> &rhs)
    {
      TwoDVIDPoly<CRType> res;

      res.p_ = lhs * rhs.p_;

      res.dps_ = rhs.dps_;
      return res;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_PolyTwoDV_H
