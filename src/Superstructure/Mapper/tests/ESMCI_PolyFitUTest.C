#include <iostream>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyFit.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  std::vector<float> xvals = {1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0};
  std::vector<float> yvals = {1.1, 1.3, 1.6, 2.0, 2.7, 3.4, 4.1};
  ESMCI::MapperUtil::UVIDPoly<float> p0;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_2D_LS_LAPACK, xvals, yvals, p0);
  std::cout << p0 << std::endl;

  // x^2+2x+3
  ESMCI::MapperUtil::UVIDPoly<float> p1 = {1.0, 2.0, 3.0};
  std::cout << p1 << std::endl;

  ESMCI::MapperUtil::UVIDPoly<int> p2 = {1, 2, 3};
  std::cout << p2 << std::endl;

  ESMCI::MapperUtil::UVIDPoly<float> p3;
  p3.set_coeffs({1.0, 2.0, 3.0});
  std::cout << p3 << std::endl;

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
