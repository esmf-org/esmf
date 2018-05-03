#include <iostream>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyDer.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];

  ESMC_TestStart(__FILE__, __LINE__, 0);

  // x^2+2xy+3y^2+4x+5y+6
  ESMCI::MapperUtil::TwoVIDPoly<float> p1 = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
  std::cout << p1 << std::endl;

  // x^2+2xy+5y+6
  ESMCI::MapperUtil::TwoVIDPoly<float> p2 = {1.0, 2.0, 0.0, 0.0, 5.0, 6.0};
  std::cout << p2 << std::endl;

  ESMCI::MapperUtil::TwoVIDPoly<float> p3 = p1+p2;
  std::cout << p3 << std::endl;

  // x + 2
  ESMCI::MapperUtil::TwoVIDPoly<float> p4 = {1.0, 0.0, 2.0};
  ESMCI::MapperUtil::TwoVIDPoly<float> p5 = p1 * p4;
  std::cout << p5 << std::endl;


  ESMCI::MapperUtil::TwoVIDPoly<float> p6;
  int ret = ESMCI::MapperUtil::FindPDerivative(p1, true, p6);
  assert(ret == 0);
  std::cout << p6 << std::endl;

  ESMCI::MapperUtil::TwoVIDPoly<float> p7, p8;
  std::vector<std::string> p1_vnames = {"p", "q"};
  p1.set_vnames(p1_vnames);
  std::cout << p1 << std::endl;
  rc = ESMCI::MapperUtil::FindPDerivative(p1, std::string("p"), p7);
  assert(rc == 0);
  std::cout << p7 << std::endl;
  rc = ESMCI::MapperUtil::FindPDerivative(p1, std::string("q"), p8);
  assert(rc == 0);
  std::cout << p8 << std::endl;

  std::vector<float> vvals = {2.0, 3.0};
  std::cout << p2.eval(vvals) << std::endl;

  // 2 x^2 + 3x + 4
  ESMCI::MapperUtil::UVIDPoly<float> uvpoly1 = {2.0, 3.0, 4.0};
  ESMCI::MapperUtil::TwoVIDPoly<float> p9(uvpoly1);
  std::cout << uvpoly1 << " == " << p9 << std::endl;

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
