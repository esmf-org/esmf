#ifndef ESMCI_Poly_H
#define ESMCI_Poly_H

#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>

namespace ESMCI{
  namespace MapperUtil{

    template<typename CType, typename DType>
    class GenPoly{
      public:
        virtual ~GenPoly() = default;
        virtual void set_coeffs(const std::vector<CType>& coeffs) = 0;
        virtual void set_coeffs(std::initializer_list<CType> coeffs) = 0;
        virtual void set_vnames(const std::vector<std::string> &vnames) = 0;
        virtual std::vector<std::string> get_vnames(void ) const = 0;
        virtual void set_degs(const std::vector<std::vector<DType> >& degs) = 0;
        virtual int get_max_deg(void ) const = 0;
        virtual std::vector<CType> get_coeffs(void ) const = 0;
        virtual std::vector<std::vector<DType> > get_degs(void ) const = 0;
        virtual CType eval(const std::vector<CType> &vvals) const = 0;
    }; // class GenPoly

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
    bool operator==(const CType &val, const GenPoly<CType, DType> &poly)
    {
      return (poly == val);
    }
  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_Poly_H
