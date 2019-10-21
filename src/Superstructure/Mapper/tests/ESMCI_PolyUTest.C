#include <iostream>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
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
  //std::cout << p1 << std::endl;

  ESMCI::MapperUtil::UVIDPoly<int> p2 = {1, 2, 3};
  //std::cout << p2 << std::endl;

  strncpy(name, "Polynomial equality (==) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial equality (==) test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p3;
  p3.set_coeffs({1.0, 2.0, 3.0});
  ESMC_Test((p1 == p3), name, failMsg, &result, __FILE__, __LINE__, 0);
  /*
  std::cout << p3 << std::endl;
  if(p1 == p3){
    std::cout << "\"" << p1 << "\" == \"" << p2 << "\"\n";
  }
  else{
    std::cout << "\"" << p1 << "\" != \"" << p2 << "\"\n";
  }
  */

  strncpy(name, "Polynomial equality (!=) UTest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial equality (!=) test failed", ESMF_MAX_STRLEN);
  // 2 * x^2 + 3 * x + 4
  ESMCI::MapperUtil::UVIDPoly<float> p4 = {2.0, 3.0, 4};
  ESMC_Test((p1 != p4), name, failMsg, &result, __FILE__, __LINE__, 0);
  /*
  if(p1 == p4){
    std::cout << "\"" << p1 << "\" == \"" << p4 << "\"\n";
  }
  else{
    std::cout << "\"" << p1 << "\" != \"" << p4 << "\"\n";
  }
  */
  strncpy(name, "Polynomial Derivative Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Derivative Utest failed", ESMF_MAX_STRLEN);
  // Derivative of p4
  ESMCI::MapperUtil::UVIDPoly<float> dp4;
  rc = FindDerivative(p4, dp4);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial Derivative Correctness Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Derivative Correctness Utest failed", ESMF_MAX_STRLEN);
  // p4 = 2 * x^2 + 3 * x + 4 ; edp4 = 4 * x + 3
  ESMCI::MapperUtil::UVIDPoly<float> edp4 = { 4, 3};
  ESMC_Test((dp4 == edp4), name, failMsg, &result, __FILE__, __LINE__, 0);

  //std::cout << "Poly : " << p4 << ", DPoly : " << dp4 << std::endl;

  strncpy(name, "Polynomial Addition Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Addition Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p5 = p1 + p3;
  //std::cout << "(" << p1 << ") + (" << p3 << ") = " << p5 << std::endl;
  // p1 = x^2+2x+3; p3 = x^2+2x+3
  ESMCI::MapperUtil::UVIDPoly<float> ep5 = { 2, 4, 6};
  ESMC_Test((p5 == ep5), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial Subraction Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Subtraction Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p6 = {1.0, 1.0};
  ESMCI::MapperUtil::UVIDPoly<float> p7 = p1 - p6;
  //std::cout << "(" << p1 << ") - (" << p6 << ") = " << p7 << std::endl;
  // p1 = x^2+2x+3
  ESMCI::MapperUtil::UVIDPoly<float> ep7 = { 1, 1, 2};
  ESMC_Test((p7 == ep7), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial Multiplication Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Multiplication Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p8 = p1 * p6;
  //std::cout << "(" << p1 << ") * (" << p6 << ") = " << p8 << std::endl;
  ESMCI::MapperUtil::UVIDPoly<float> ep8 = { 1, 3, 5, 3};
  ESMC_Test((p8 == ep8), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial Scalar Multiplication Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial Scalar Multiplication Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p9 = 2.0 * p6;
  //std::cout << "2.0 * (" << p6 << ") = " << p9 << std::endl;
  ESMCI::MapperUtil::UVIDPoly<float> ep9 = { 2, 2};
  ESMC_Test((p9 == ep9), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial evaluation (vec of vals) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial evaluation (vec of vals) Utest failed", ESMF_MAX_STRLEN);
  std::vector<float> vvals = { 3.0 };
  //std::cout << p1.eval(vvals) << std::endl;
  float ep1_eval = 18.0;
  ESMC_Test((p1.eval(vvals) == ep1_eval), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "Polynomial evaluation (single val) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial evaluation (single val) Utest failed", ESMF_MAX_STRLEN);
  float vval = 3.0;
  //std::cout << p1.eval(vval) << std::endl;
  ESMC_Test((p1.eval(vval) == ep1_eval), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Polynomial comparison (scalar) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial comparison (scalar) Utest", ESMF_MAX_STRLEN);
  const float sval = 2.0;
  ESMCI::MapperUtil::UVIDPoly<float> p10 = {sval, sval, sval};
  /*
  if(p10 == static_cast<float>(2)){
    std::cout << "\"" << p10 << "\" == 2.0\n";
  }
  else{
    std::cout << "\"" << p10 << "\" != 2.0\n";
  }
  */
  ESMC_Test((p10 == sval), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::UVIDPoly<float> p11 = {1.01, 2.009, 3.009999};
  ESMCI::MapperUtil::UVIDPoly<float> p12 = {1, 2, 3};
  double tol = 0.1;
  strncpy(name, "Polynomial comparison (equals, tol=0.1) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial comparison (equals, tol=0.1) Utest", ESMF_MAX_STRLEN);
  /*
  if(p11.equals(p12, tol)){
    std::cout << "\"" << p11 << "\" == " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  else{
    std::cout << "\"" << p11 << "\" != " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  */
  ESMC_Test((p11.equals(p12, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);
  tol = 0.01;
  strncpy(name, "Polynomial comparison (equals, tol=0.01) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial comparison (equals, tol=0.01) Utest", ESMF_MAX_STRLEN);
  /*
  if(p11.equals(p12, tol)){
    std::cout << "\"" << p11 << "\" == " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  else{
    std::cout << "\"" << p11 << "\" != " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  */
  ESMC_Test((p11.equals(p12, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);
  tol = 0.001;
  strncpy(name, "Polynomial comparison (equals, tol=0.001) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Polynomial comparison (equals, tol=0.001) Utest", ESMF_MAX_STRLEN);
  /*
  if(p11.equals(p12, tol)){
    std::cout << "\"" << p11 << "\" == " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  else{
    std::cout << "\"" << p11 << "\" != " << "\"" << p12 << "\" (tol=" << tol << ")\n";
  }
  */
  ESMC_Test((!p11.equals(p12, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
