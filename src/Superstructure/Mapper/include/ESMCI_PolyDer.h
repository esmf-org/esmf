#ifndef ESMCI_PolyDer_H
#define ESMCI_PolyDer_H

#include <vector>
#include "ESMCI_Poly.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename CType, typename DType>
    inline int FindDerivative(const UniVPoly<CType, DType>& poly,
                  UniVPoly<CType, DType>& dpoly)
    {
      std::vector<CType> poly_coeffs = poly.get_coeffs();
      int max_deg = poly_coeffs.size() - 1;
      std::vector<CType> dpoly_coeffs;
      int cur_deg = max_deg;
      for(typename std::vector<CType>::const_iterator citer = poly_coeffs.cbegin();
            (citer != poly_coeffs.cend()) && (cur_deg > 0); ++citer){
        dpoly_coeffs.push_back((*citer) * cur_deg);
        cur_deg--; 
      }

      dpoly.set_coeffs(dpoly_coeffs);
      return 0;
    }

  } // namespace MapperUtil
} //namespace ESMCI


#endif // ESMCI_PolyDer_H
