#include <iostream>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyDer.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];

  ESMC_TestStart(__FILE__, __LINE__, 0);

  // x^2+2x+3
  ESMCI::MapperUtil::UVIDPoly<float> p1 = {1.0, 2.0, 3.0};
  std::cout << p1 << std::endl;

  ESMCI::MapperUtil::UVIDPoly<int> p2 = {1, 2, 3};
  std::cout << p2 << std::endl;

  ESMCI::MapperUtil::UVIDPoly<float> p3;
  p3.set_coeffs({1.0, 2.0, 3.0});
  std::cout << p3 << std::endl;

  strncpy(name, "Polynomial Derivative UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Derivative test failed", ESMF_MAX_STRLEN);
  // 2 * x^2 + 3 * x + 4
  ESMCI::MapperUtil::UVIDPoly<float> p4 = {2.0, 3.0, 4};
  // Derivative of p4
  ESMCI::MapperUtil::UVIDPoly<float> dp4;
  rc = FindDerivative(p4, dp4);
  ESMC_Test((rc == 0), name, failMsg, &result, __FILE__, __LINE__, 0);

  std::cout << "Poly : " << p4 << ", DPoly : " << dp4 << std::endl;

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
