#include <iostream>
#include <cstring>
#include <cmath>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyFit.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  int rc = 0, result = 0;
  double tol = 0.1;
  bool test_success = true;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  std::vector<float> xvals = {1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0};
  std::vector<float> yvals = {1.1, 1.3, 1.6, 2.0, 2.7, 3.4, 4.1};

  strncpy(name, "2 Degree Polynomial fit Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree Polynomial fit Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p0;
  int max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p0);
  //std::cout << p0 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "2 Degree Polynomial fit Accuracy Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree Polynomial fit Utest Accuracy failed", ESMF_MAX_STRLEN);
  test_success = true;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    //std::cout << "p0(" << *citer1 << ") = " << p0.eval(*citer1)
    //          << " : " << *citer2 << "\n"; 
    if(fabs(*citer2 - p0.eval(*citer1)) > tol){
      std::cout << "Expected value = " << *citer2 << ", Eval value = " << p0.eval(*citer1) << ", tol = " << tol << "\n";
      test_success = false;
      break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 
  

  strncpy(name, "3 Degree Polynomial fit Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "3 Degree Polynomial fit Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p1;
  max_deg = 3;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p1);
  //std::cout << p1 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "3 Degree Polynomial fit Accuracy Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "3 Degree Polynomial fit Utest Accuracy failed", ESMF_MAX_STRLEN);
  test_success = true;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    //std::cout << "p1(" << *citer1 << ") = " << p1.eval(*citer1)
    //          << " : " << *citer2 << "\n"; 
    if(fabs(*citer2 - p1.eval(*citer1)) > tol){
      test_success = false;
      break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 


  strncpy(name, "4 Degree Polynomial fit Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "4 Degree Polynomial fit Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p2;
  max_deg = 4;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals, yvals, p2);
  //std::cout << p2 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "4 Degree Polynomial fit Accuracy Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "4 Degree Polynomial fit Utest Accuracy failed", ESMF_MAX_STRLEN);
  test_success = true;
  for(std::vector<float>::const_iterator citer1 = xvals.cbegin(),
      citer2 = yvals.cbegin();
      (citer1 != xvals.cend()) && (citer2 != yvals.cend()); ++citer1, ++citer2){
    //std::cout << "p2(" << *citer1 << ") = " << p2.eval(*citer1)
    //          << " : " << *citer2 << "\n"; 
    if(fabs(*citer2 - p2.eval(*citer1)) > tol){
      test_success = false;
      break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 


  // The fit below will fail since two yvals for same xval
  //std::vector<float> xvals_p4 = { 0.0, 0.0};
  //std::vector<float> yvals_p4 = { 6.0, 3.0};
  //ESMCI::MapperUtil::UVIDPoly<float> p4;
  //rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK,
  //      max_deg, xvals_p4, yvals_p4, p4);
  //std::cout << p4 << std::endl;

  std::vector<float> xvals_atm = {3600, 7200, 14400, 28800};
  std::vector<float> yvals_atm = {1273.542, 819.708, 426.051, 290.470};

  strncpy(name, "2 Degree Polynomial fit Utest (ATM data)", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree Polynomial fit Utest (ATM data) failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> p5;
  max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, xvals_atm, yvals_atm, p5);
  //std::cout << p5 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "2 Degree Polynomial fit Accuracy (ATM data) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree Polynomial fit Utest Accuracy (ATM data) failed", ESMF_MAX_STRLEN);
  test_success = true;
  tol = 100;
  for(std::vector<float>::const_iterator citer1 = xvals_atm.cbegin(),
      citer2 = yvals_atm.cbegin();
      (citer1 != xvals_atm.cend()) && (citer2 != yvals_atm.cend()); ++citer1, ++citer2){
    //std::cout << "p5(" << *citer1 << ") = " << p5.eval(*citer1)
    //          << " : " << *citer2 << "\n"; 
    if(fabs(*citer2 - p5.eval(*citer1)) > tol){
      std::cout << "Expected value = " << *citer2 << ", Eval value = " << p5.eval(*citer1) << ", tol = " << tol << "\n";
      test_success = false;
      break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 
  
  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
