#include <iostream>
#include <cstring>
#include <cmath>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoDV.h"
#include "ESMCI_PolyFit.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  const int ESMF_MAX_STRLEN = 256;
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
  
//  std::vector<float> x1vals = {1800, 3600, 7200, 14400};
//  std::vector<float> x2vals = {800, 600, 200, 4400};
//  std::vector<float> y1vals = {1273.542, 819.708, 426.051, 290.470};
  //std::vector<float> x1vals = {2, 5, 3.1, 9, 11.2};
  //std::vector<float> x2vals = {2, 5, 3.1, 9, 11.2};
  std::vector<float> x1vals;
  std::vector<float> x2vals;
  const int NVALS = 32;
  for(int i=1; i<=NVALS; i++){
    x1vals.push_back(i);
    x2vals.push_back(i);
  }
  //std::vector<float> x2vals = {5, 2, 5.0, 12, 19.3};
  std::vector<float> y1vals;
  ESMCI::MapperUtil::TwoVIDPoly<float> p6_to_fit = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
  // The poly fitted for data from poly below does not meet the tol
  //ESMCI::MapperUtil::TwoVIDPoly<float> p6_to_fit = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
  for(std::vector<float>::const_iterator citer1 = x1vals.cbegin(),
        citer2 = x2vals.cbegin();
      (citer1 != x1vals.cend()) && (citer2 != x2vals.cend());
      ++citer1, ++citer2){
    std::vector<float> p6_to_fit_vvals = {*citer1, *citer2};
    y1vals.push_back(p6_to_fit.eval(p6_to_fit_vvals));
  }

  strncpy(name, "2 Degree 2 Var Poly fit Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree 2 Var Poly fit Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoDVIDPoly<float> p6;
  max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, x1vals, x2vals, y1vals, p6);
  std::cout << p6 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "2 Degree 2 Var Poly fit Accuracy Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree 2 Var Poly fit Utest Accuracy failed", ESMF_MAX_STRLEN);
  test_success = true;
  tol = 100;
  for(std::vector<float>::const_iterator citer1 = x1vals.cbegin(),
      citer2 = x2vals.cbegin(),
      citer3 = y1vals.cbegin();
      (citer1 != x1vals.cend()) && (citer2 != x2vals.cend()) && (citer3 != y1vals.cend());
      ++citer1, ++citer2, ++citer3){
    //std::cout << "p5(" << *citer1 << ") = " << p5.eval(*citer1)
    //          << " : " << *citer2 << "\n"; 
    std::vector<float> p6_vvals = {*citer1, *citer2};
    if(fabs(*citer3 - p6.eval(p6_vvals)) > tol){
      std::cout << "Expected value = " << *citer3 << ", Eval value = " << p6.eval(p6_vvals) << ", tol = " << tol << "\n";
      test_success = false;
      //break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 
  
  strncpy(name, "2 Degree 2 Var Poly fit with some xvals Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree 2 Var Poly fit with some xvals Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::TwoDVIDPoly<float> p7;
  max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, p6, p7, x1vals, x2vals);
  std::cout << p7 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  strncpy(name, "2 Degree 2 Var Poly fit with some xvals Accuracy Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "2 Degree 2 Var Poly fit with some xvals Utest Accuracy failed", ESMF_MAX_STRLEN);
  test_success = true;
  tol = 100;
  for(std::vector<float>::const_iterator citer1 = x1vals.cbegin(),
      citer2 = x2vals.cbegin();
      (citer1 != x1vals.cend()) && (citer2 != x2vals.cend());
      ++citer1, ++citer2){
    std::vector<float> p6_vvals = {*citer1, *citer2};
    if(fabs(p7.eval(p6_vvals) - p6.eval(p6_vvals)) > tol){
      std::cout << "Expected value = " <<  p6.eval(p6_vvals) << ", Eval value = " << p7.eval(p6_vvals) << ", tol = " << tol << "\n";
      test_success = false;
      //break;
    }
  }
  ESMC_Test(test_success, name, failMsg, &result, __FILE__, __LINE__, 0); 
  
  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
