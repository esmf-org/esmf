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
  //std::cout << p1 << std::endl;

  // x^2+2xy+5y+6
  ESMCI::MapperUtil::TwoVIDPoly<float> p2 = {1.0, 2.0, 0.0, 0.0, 5.0, 6.0};
  //std::cout << p2 << std::endl;

  strncpy(name, "Polynomial Addition Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> p3 = p1+p2;
  //std::cout << p3 << std::endl;
  // p1 = x^2+2xy+3y^2+4x+5y+6; p2 = x^2+2xy+5y+6
  ESMCI::MapperUtil::TwoVIDPoly<float> ep3 = { 2, 4, 3, 4, 10, 12};
  ESMC_Test((p3 == ep3), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial Multiplication Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Multiplication Utest failed", ESMF_MAX_STRLEN);
  // x + 2
  ESMCI::MapperUtil::TwoVIDPoly<float> p4 = {1.0, 0.0, 2.0};
  ESMCI::MapperUtil::TwoVIDPoly<float> p5 = p1 * p4;
  //std::cout << p5 << std::endl;
  ESMCI::MapperUtil::TwoVIDPoly<float> ep5 = {1, 2, 3, 0, 6, 9, 6, 14, 10, 12};
  ESMC_Test((p5 == ep5), name, failMsg, &result, __FILE__, __LINE__, 0);


  strncpy(name, "Polynomial derivative (2 deg) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> p6;
  rc = ESMCI::MapperUtil::FindPDerivative(p1, true, p6);
  //std::cout << p6 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "Polynomial derivative (2 deg) Correctness Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) Correctness Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> ep6 = {2, 2, 4};
  ESMC_Test((p6 == ep6), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::TwoVIDPoly<float> p7, p8;
  std::vector<std::string> p1_vnames = {"p", "q"};
  p1.set_vnames(p1_vnames);
  //std::cout << p1 << std::endl;
  strncpy(name, "Polynomial derivative (2 deg) wrt p Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) wrt p Utest failed", ESMF_MAX_STRLEN);
  rc = ESMCI::MapperUtil::FindPDerivative(p1, std::string("p"), p7);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //std::cout << p7 << std::endl;

  const float SCALE_FACTOR = 0.01;
  const double TOL = 0.001;
  //ESMCI::MapperUtil::TwoVIDPoly<float> SCALE_FACTOR = {0.01};
  //SCALE_FACTOR.set_vnames(p1_vnames);
  ESMCI::MapperUtil::TwoVIDPoly<float> sp1 = SCALE_FACTOR * p1;
  ESMCI::MapperUtil::TwoVIDPoly<float> esp1 = {0.01, 0.02, 0.03, 0.04, 0.05, 0.06};
  esp1.set_vnames(p1_vnames);
  strncpy(name, "Polynomial Scaling Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Scaling Utest failed", ESMF_MAX_STRLEN);
  std::cout << sp1 << "\n";
  std::cout << esp1 << "\n";
  ESMC_Test((sp1.equals(esp1, TOL)), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial derivative (2 deg) wrt p Correctness Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) wrt p Correctness Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> ep7 = {2, 2, 4};
  ep7.set_vnames(p1_vnames);
  ESMC_Test((p7 == ep7), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "Polynomial derivative (2 deg) wrt q Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) wrt q Utest failed", ESMF_MAX_STRLEN);
  rc = ESMCI::MapperUtil::FindPDerivative(p1, std::string("q"), p8);
  //std::cout << p8 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "Polynomial derivative (2 deg) wrt q Correctness Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial derivative (2 deg) wrt q Correctness Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> ep8 = {2, 6, 5};
  ep8.set_vnames(p1_vnames);
  ESMC_Test((p8 == ep8), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial evaluation Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial evaluation Utest", ESMF_MAX_STRLEN);
  std::vector<float> vvals = {2.0, 3.0};
  float ep2_eval = 37;
  //std::cout << p2.eval(vvals) << std::endl;
  ESMC_Test((p2.eval(vvals) == ep2_eval), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial create two v poly from univ poly Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial create two v poly from univ poly Utest failed", ESMF_MAX_STRLEN);
  // 2 x^2 + 3x + 4
  ESMCI::MapperUtil::UVIDPoly<float> uvpoly1 = {2.0, 3.0, 4.0};
  ESMCI::MapperUtil::TwoVIDPoly<float> p9(uvpoly1);
  //std::cout << uvpoly1 << " == " << p9 << std::endl;
  ESMCI::MapperUtil::TwoVIDPoly<float> ep9 = {2, 0, 0, 3, 0, 4};
  ESMC_Test((p9 == ep9), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial subtraction Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial subtraction Utest failed", ESMF_MAX_STRLEN);
  std::vector<std::string> uvpoly1_vnames1 = {"p"};
  uvpoly1.set_vnames(uvpoly1_vnames1);
  std::vector<std::string> p10_vnames = {"p", "q"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p10(uvpoly1, p10_vnames);
  //std::cout << "p10 = " << p10 << "\n";

  std::vector<std::string> uvpoly1_vnames2 = {"q"};
  uvpoly1.set_vnames(uvpoly1_vnames2);
  std::vector<std::string> p11_vnames = {"p", "q"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p11(uvpoly1, p11_vnames);
  //std::cout << "p11 = " << p11 << "\n";

  ESMCI::MapperUtil::TwoVIDPoly<float> p12;
  std::vector<std::string> p12_vnames = {"p", "q"};
  p12.set_vnames(p12_vnames);
  p12 = p11 - p10;
  //std::cout << "\"" << p11 << "\" - \"" << p10 << "\" = " << p12 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> ep12 = {-2, 0, 2, -3, 3, 0};
  ep12.set_vnames(p12_vnames);
  ESMC_Test((p12 == ep12), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial square (2 deg) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial square (2 deg) Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> p13 = {1, 0, -4, 2, -5, -3};
  ESMCI::MapperUtil::TwoVIDPoly<float> p13_sq = p13 * p13;
  //std::cout << p13_sq << std::endl;
  ESMCI::MapperUtil::TwoVIDPoly<float> ep13_sq = {1, 0, -8, 0, 16, 4, -10, -16, 40, -2, -20, 49, -12, 30, 9};
  ESMC_Test((p13_sq == ep13_sq), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  strncpy(name, "Polynomial multiplication (1 deg x 1 deg) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial multiplication (1 deg x 1 deg) Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoVIDPoly<float> p14 = {2, 3, -4};
  ESMCI::MapperUtil::TwoVIDPoly<float> p15 = {1, -2, 3};
  ESMCI::MapperUtil::TwoVIDPoly<float> prod_p14_p15 = p14 * p15;
  ESMCI::MapperUtil::TwoVIDPoly<float> eprod_p14_p15 = {2, -1, -6, 2, 17, -12};
  //std::cout << prod_p14_p15 << std::endl;
  ESMC_Test((prod_p14_p15 == eprod_p14_p15), name, failMsg, &result, __FILE__, __LINE__, 0);


  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
