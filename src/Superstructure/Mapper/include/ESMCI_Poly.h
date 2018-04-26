#ifndef ESMCI_Poly_H
#define ESMCI_Poly_H

#include <vector>
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
        virtual void set_degs(const std::vector<std::vector<DType> >& degs) = 0;
        virtual std::vector<CType> get_coeffs(void ) const = 0;
        virtual std::vector<std::vector<DType> > get_degs(void ) const = 0;
    }; // class GenPoly

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

    template<typename CType>
    class UVIDPoly : public UniVPoly<CType, int>{
      public:
        UVIDPoly() = default;
        virtual ~UVIDPoly() = default;
        UVIDPoly(const std::vector<CType>& coeffs);
        UVIDPoly(std::initializer_list<CType> coeffs);
        void set_coeffs(const std::vector<CType>& coeffs); 
        void set_coeffs(std::initializer_list<CType> coeffs);
        std::vector<CType> get_coeffs(void ) const;
      private:
        std::vector<CType> coeffs_;
    }; // class UVIDPoly

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly(const std::vector<CType>& coeffs):coeffs_(coeffs)
    {}

    template<typename CType>
    inline UVIDPoly<CType>::UVIDPoly(std::initializer_list<CType> coeffs):coeffs_(coeffs.begin(), coeffs.end())
    {}

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
    inline std::vector<CType> UVIDPoly<CType>::get_coeffs(void ) const
    {
      return coeffs_;
    }

    template<typename CType>
    std::ostream& operator<<(std::ostream &ostr, const UVIDPoly<CType>& p)
    {
      std::vector<CType> coeffs(p.get_coeffs());
      std::size_t cur_deg = coeffs.size()-1;
      for(typename std::vector<CType>::iterator iter = coeffs.begin();
            iter != coeffs.end(); ++iter){
        if(cur_deg > 1){
          ostr << *iter << " x^" << cur_deg-- << " + ";
        }else if(cur_deg == 1){
          ostr << *iter << " x" << " + ";
          cur_deg--;
        }
        else{
          ostr << *iter;
        }
      }
      return ostr;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_Poly_H
