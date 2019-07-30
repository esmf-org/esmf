#ifndef ESMCI_LFit_H
#define ESMCI_LFit_H

#include "ESMCI_Macros.h"
#include <iostream>
#include <vector>
#include <cassert>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Get a linear fit for values in 2D space defined by xvals and yvals */
    template<typename CType, typename VType>
    int LinearFit(const std::vector<VType> &xvals, const std::vector<VType> &yvals,
                  UVIDPoly<CType> &poly)
    {
      const int MIN_VALS_REQD_FOR_LFIT = 2;
      assert(xvals.size() == yvals.size());
      if(xvals.size() != MIN_VALS_REQD_FOR_LFIT){
        return ESMF_FAILURE;
      }

      /* y = slope * x + y_intercept */
      CType slope = (yvals[1] - yvals[0])/(xvals[1] - xvals[0]);
      CType y_icept = yvals[1] - slope * xvals[1];

      std::vector<CType> coeffs;
      coeffs.push_back(slope);
      coeffs.push_back(y_icept);

      poly.set_coeffs(coeffs);

      return ESMF_SUCCESS;
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_LFit_H
