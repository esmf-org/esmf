#include <iostream>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyFit.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  std::vector<float> xvals = {1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0};
  std::vector<float> yvals = {1.1, 1.3, 1.6, 2.0, 2.7, 3.4, 4.1};
  ESMCI::MapperUtil::UVIDPoly<float> p0;
  int max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p0);
  std::cout << p0 << std::endl;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    std::cout << "p0(" << *citer1 << ") = " << p0.eval(*citer1)
              << " : " << *citer2 << "\n"; 
  }

  ESMCI::MapperUtil::UVIDPoly<float> p1;
  max_deg = 3;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p1);
  std::cout << p1 << std::endl;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    std::cout << "p1(" << *citer1 << ") = " << p1.eval(*citer1)
              << " : " << *citer2 << "\n"; 
  }


  ESMCI::MapperUtil::UVIDPoly<float> p2;
  max_deg = 4;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p2);
  std::cout << p2 << std::endl;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    std::cout << "p2(" << *citer1 << ") = " << p2.eval(*citer1)
              << " : " << *citer2 << "\n"; 
  }


  // The fit below will fail since two yvals for same xval
  //std::vector<float> xvals_p4 = { 0.0, 0.0};
  //std::vector<float> yvals_p4 = { 6.0, 3.0};
  //ESMCI::MapperUtil::UVIDPoly<float> p4;
  //rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK,
  //      max_deg, xvals_p4, yvals_p4, p4);
  //std::cout << p4 << std::endl;

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
