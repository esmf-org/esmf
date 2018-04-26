#include <iostream>
#include "ESMCI_Poly.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0;

  ESMC_TestStart(__FILE__, __LINE__, 0);

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
