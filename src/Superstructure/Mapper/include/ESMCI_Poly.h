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
    }; // class GenPoly


  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_Poly_H
