#ifndef ESMCI_PolyDer_H
#define ESMCI_PolyDer_H

#include "ESMCI_Macros.h"
#include <vector>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Find the derivative of a Univariate polynomial
     * The derivative is stored in (returned via) dpoly
     */
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
      return ESMF_SUCCESS;
    }

    /* Find the partial derivative of a polynomial with two variables
     * The function below computes partial derivative of the polynomial,
     * poly, wrt to the first variable (x) if by_x == true and wrt to
     * the second variable if by_x == false
     * The partial derivative is stored in (returned via) dpoly
     */
    template<typename CType>
    inline int FindPDerivative(const TwoVIDPoly<CType>& poly, bool by_x,
                  TwoVIDPoly<CType>& dpoly)
    {
      std::vector<CType> poly_coeffs = poly.get_coeffs();
      int max_deg = poly.get_max_deg();
      int max_deg_dpoly = max_deg - 1;
      int dpoly_ncoeffs = TwoVIDPolyUtil::get_ncoeffs(max_deg_dpoly);
      std::vector<CType> dpoly_coeffs(dpoly_ncoeffs, 0);
      int cur_deg = max_deg;
      int cur_xdeg = cur_deg;
      int cur_ydeg = 0;
      int nrem_coeffs_in_deg = cur_deg + 1;
      std::vector<std::string> vnames = poly.get_vnames();
      assert(vnames.size() == 2);

      dpoly.set_vnames(vnames);

      /* We process the polynomial coefficients one degree at a time.
       * cur_deg => current degree of the polynomial coefficients being processes
       */
      for(typename std::vector<CType>::const_iterator citer = poly_coeffs.cbegin();
            (citer != poly_coeffs.cend()) && (cur_deg > 0); ++citer){

        //std::cout << "Processing coeff : " << *citer << "\n";
        int has_coeff = (by_x) ? (cur_xdeg > 0) : (cur_ydeg > 0);
        if(has_coeff){
          /* Coefficient in the result */
          CType coeff = ((by_x) ? (cur_xdeg) : (cur_ydeg)) * (*citer);
          //std::cout << "coeff : " << coeff << "\n";
          if(coeff != 0){
            /* Find the index in the result poly for the coeff */
            int coeff_idx = TwoVIDPolyUtil::get_coeff_idx(max_deg_dpoly, (by_x) ? (cur_xdeg - 1) : cur_xdeg, (by_x) ? (cur_ydeg) : (cur_ydeg - 1));
            assert((coeff_idx >= 0) && (coeff_idx < dpoly_ncoeffs));
            //std::cout << "dpoly_coeffs[" << coeff_idx << "] = " << coeff << "\n";
            dpoly_coeffs[coeff_idx] = coeff; 
          }
        }

        nrem_coeffs_in_deg--;
        if(nrem_coeffs_in_deg == 0){
          cur_deg--;
          cur_xdeg = cur_deg;
          cur_ydeg = 0;
          nrem_coeffs_in_deg = cur_deg + 1;
        }
        else{
          cur_xdeg--;
          cur_ydeg++;
        }
      }

      /* Find the first non zero coefficient in the result */
      typename std::vector<CType>::const_iterator citer_first_nz_val = dpoly_coeffs.cbegin();
      while((citer_first_nz_val != dpoly_coeffs.cend()) && (*citer_first_nz_val == 0)){
        ++citer_first_nz_val;
      }
      if(citer_first_nz_val != dpoly_coeffs.cend()){
        dpoly_coeffs.erase(dpoly_coeffs.begin(), citer_first_nz_val);
      }

      dpoly.set_coeffs(dpoly_coeffs);
      return ESMF_SUCCESS;
    }

    /* Find the partial derivative of a polynomial with two variables
     * The function below computes partial derivative of the polynomial,
     * poly, wrt to the variable vname
     * The partial derivative is stored in (returned via) dpoly
     */
    template<typename CType>
    inline int FindPDerivative(const TwoVIDPoly<CType>& poly, const std::string &vname,
                  TwoVIDPoly<CType>& dpoly)
    {
      int ret = ESMF_SUCCESS;
      std::vector<CType> poly_coeffs = poly.get_coeffs();
      int max_deg = poly.get_max_deg();
      int max_deg_dpoly = max_deg - 1;
      int dpoly_ncoeffs = TwoVIDPolyUtil::get_ncoeffs(max_deg_dpoly);
      std::vector<CType> dpoly_coeffs(dpoly_ncoeffs, 0);
      int cur_deg = max_deg;
      int cur_xdeg = cur_deg;
      int cur_ydeg = 0;
      int nrem_coeffs_in_deg = cur_deg + 1;
      std::vector<std::string> vnames = poly.get_vnames();
      assert(vnames.size() == 2);

      if(vname == vnames[0]){
        ret = FindPDerivative(poly, true, dpoly);
      }
      else if(vname == vnames[1]){
        ret = FindPDerivative(poly, false, dpoly);
      }
      else{
        dpoly.set_vnames(vnames);
        dpoly.set_coeffs(dpoly_coeffs);
        ret = ESMF_SUCCESS;
      }

      return ret;
    }
  } // namespace MapperUtil
} //namespace ESMCI


#endif // ESMCI_PolyDer_H
