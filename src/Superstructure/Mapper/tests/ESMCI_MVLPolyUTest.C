#include <iostream>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_LPolyMV.h"
#include "ESMCI_PolyDer.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];

  ESMC_TestStart(__FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p1 = {1.0, 2.0, 4.0};
  //std::cout << p1 << std::endl;

  ESMCI::MapperUtil::MVIDLPoly<float> p2 = {1.0, 2.0, 4.0};

  strncpy(name, "Polynomial equality (==) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial equality (==) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p1 == p2), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p3 = {1.0, 1.0, 4.0};

  strncpy(name, "Polynomial equality (!=) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial equality (!=) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p1 != p3), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p4 = p1 + p3;
  ESMCI::MapperUtil::MVIDLPoly<float> p4_exp = {2.0, 3.0, 8.0};

  strncpy(name, "Polynomial Addition (auto vars) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition (auto vars) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p4 == p4_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p5 = {1.0, 2.0, 3.0, 4.0};
  float c = 4.0;
  ESMCI::MapperUtil::MVIDLPoly<float> p5_plus_c = p5 + c;
  ESMCI::MapperUtil::MVIDLPoly<float> p5_plus_c_exp = {1.0, 2.0, 3.0, 8.0};
  strncpy(name, "Polynomial Addition with constant UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition with constant test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p5_plus_c == p5_plus_c_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p5_min_c = p5 - c;
  ESMCI::MapperUtil::MVIDLPoly<float> p5_min_c_exp = {1.0, 2.0, 3.0, 0.0};
  strncpy(name, "Polynomial Subtraction with constant UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Subtraction with constant test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p5_min_c == p5_min_c_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p6 = p1 + p5;
  ESMCI::MapperUtil::MVIDLPoly<float> p6_exp = {2.0, 4.0, 3.0, 8.0};
  //std::cout << "(" << p1 << ")+(" << p5 << ")=" << p6 << "\n";
  //std::cout << "p6_exp = " << p6_exp << "\n";

  strncpy(name, "Polynomial Addition different vars (auto vars) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition different vars (auto vars) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p6 == p6_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p7 = p1 - p3;
  ESMCI::MapperUtil::MVIDLPoly<float> p7_exp = {0.0, 1.0, 0.0};

  strncpy(name, "Polynomial Subtraction (auto vars) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Subtraction (auto vars) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p7 == p7_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p8 = p1 - p5;
  ESMCI::MapperUtil::MVIDLPoly<float> p8_exp = {0.0, 0.0, -3.0, 0.0};
  //std::cout << "(" << p1 << ")-(" << p5 << ")=" << p8 << "\n";
  //std::cout << "p8_exp = " << p8_exp << "\n";

  strncpy(name, "Polynomial Subtraction different vars (auto vars) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Subtraction different vars (auto vars) test failed", ESMF_MAX_STRLEN);

  ESMC_Test((p8 == p8_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p9 = {1.0, 2.0, 3.0, 4.0};
  std::vector<std::string> p9_vnames = {"a", "b", "c"};
  p9.set_vnames(p9_vnames);
  ESMCI::MapperUtil::MVIDLPoly<float> p10 = {1.0, 2.0, 3.0, 4.0};
  std::vector<std::string> p10_vnames = {"a", "c", "d"};
  p10.set_vnames(p10_vnames);

  ESMCI::MapperUtil::MVIDLPoly<float> p11 = p9 + p10;
  ESMCI::MapperUtil::MVIDLPoly<float> p11_exp = {2.0, 2.0, 5.0, 3.0, 8.0};
  std::vector<std::string> p11_exp_vnames = {"a", "b", "c", "d"};
  p11_exp.set_vnames(p11_exp_vnames);

  strncpy(name, "Polynomial Addition different vars (explicit names) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition different vars (explicit names) test failed", ESMF_MAX_STRLEN);
  ESMC_Test((p11 == p11_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p12 = p9 - p10;
  ESMCI::MapperUtil::MVIDLPoly<float> p12_exp = {0.0, 2.0, 1.0, -3.0, 0.0};
  std::vector<std::string> p12_exp_vnames = {"a", "b", "c", "d"};
  p12_exp.set_vnames(p12_exp_vnames);

  strncpy(name, "Polynomial Addition different vars (explicit names) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition different vars (explicit names) test failed", ESMF_MAX_STRLEN);
  ESMC_Test((p12 == p12_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::MVIDLPoly<float> p11_der_by_c;
  ESMCI::MapperUtil::MVIDLPoly<float> p11_der_by_c_exp = {0.0, 0.0, 0.0, 0.0, 5.0};
  p11_der_by_c_exp.set_vnames(p11_exp_vnames);
  rc = FindPDerivative(p11, "c", p11_der_by_c);
  //std::cout << "d(p11)/dc = " << p11_der_by_c << "\n";
  strncpy(name, "Polynomial Derivative UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Derivative test failed", ESMF_MAX_STRLEN);
  ESMC_Test((p11_der_by_c == p11_der_by_c_exp), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
