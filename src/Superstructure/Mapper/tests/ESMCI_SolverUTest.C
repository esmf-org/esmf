#include <iostream>
#include <vector>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  const int SOLVER_MAX_ITERS = 10;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  // Initial value of the variables
  std::vector<float> init_vals = {2.0, 4.0, 6.0};
  // Functions in x, y and z used to create the user
  // constraint functions
  std::vector<std::string> vnames = {"x", "y", "z"};
  ESMCI::MapperUtil::UVIDPoly<float> p1 = {1.0, 2.0, 3.0};
  std::vector<std::string> p1_vnames = {"x"};
  p1.set_vnames(p1_vnames);
  ESMCI::MapperUtil::UVIDPoly<float> p2 = {4.0, 5.0, 6.0};
  std::vector<std::string> p2_vnames = {"y"};
  p2.set_vnames(p2_vnames);
  ESMCI::MapperUtil::UVIDPoly<float> p3 = {7.0, 8.0, 9.0};
  std::vector<std::string> p3_vnames = {"z"};
  p3.set_vnames(p3_vnames);
  // Create two variable polynomials from univariate polynomials
  std::vector<std::string> p2v_xy_vnames = {"x", "y"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xy(p1, p2v_xy_vnames);
  std::cout << p1_2v_xy << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p2_2v(p2, p2v_xy_vnames);
  std::cout << p2_2v << "\n";
  std::vector<std::string> p2v_xz_vnames = {"x", "z"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xz(p1, p2v_xz_vnames);
  std::cout << p1_2v_xz << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p3_2v(p3, p2v_xz_vnames);
  std::cout << p3_2v << "\n";
  // User constraint functions
  // Constraint functions are square of the difference between
  // (pair-wise) the functions above
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs =
    {(p1_2v_xy - p2_2v)* (p1_2v_xy - p2_2v), (p1_2v_xz - p3_2v) * (p1_2v_xz - p3_2v)};

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver(vnames, init_vals, funcs);
  solver.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals = solver.minimize();

  for(std::vector<float>::const_iterator citer = sol_vals.cbegin();
      citer != sol_vals.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  // Second test

  // Initial value of the variables
  std::vector<float> init_vals_test2 = {4096, 2048, 1024};
  // Functions in x, y and z used to create the user
  // constraint functions
  std::vector<std::string> vnames_test2 = {"x", "y", "z"};
  ESMCI::MapperUtil::UVIDPoly<float> p1_test2 = {350.7, -586.6, 439.4};
  std::vector<std::string> p1_vnames_test2 = {"x"};
  p1_test2.set_vnames(p1_vnames_test2);
  ESMCI::MapperUtil::UVIDPoly<float> p2_test2 = {399.3, -569.9, 232.1};
  std::vector<std::string> p2_vnames_test2 = {"y"};
  p2_test2.set_vnames(p2_vnames_test2);
  ESMCI::MapperUtil::UVIDPoly<float> p3_test2 = {161.3, -202.4, 292.6};
  std::vector<std::string> p3_vnames_test2 = {"z"};
  p3_test2.set_vnames(p3_vnames_test2);
  // Create two variable polynomials from univariate polynomials
  std::vector<std::string> p2v_xy_vnames_test2 = {"x", "y"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xy_test2(p1_test2, p2v_xy_vnames_test2);
  std::cout << p1_2v_xy_test2 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p2_2v_test2(p2_test2, p2v_xy_vnames_test2);
  std::cout << p2_2v_test2 << "\n";
  std::vector<std::string> p2v_xz_vnames_test2 = {"x", "z"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xz_test2(p1_test2, p2v_xz_vnames_test2);
  std::cout << p1_2v_xz_test2 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p3_2v_test2(p3_test2, p2v_xz_vnames_test2);
  std::cout << p3_2v_test2 << "\n";
  // User constraint functions
  // Constraint functions are square of the difference between
  // (pair-wise) the functions above
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs_test2 =
    { (p1_2v_xy_test2 - p2_2v_test2)* (p1_2v_xy_test2 - p2_2v_test2),
      (p1_2v_xz_test2 - p3_2v_test2) * (p1_2v_xz_test2 - p3_2v_test2)};

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test2(vnames_test2,
    init_vals_test2, funcs_test2);
  solver_test2.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals_test2 = solver_test2.minimize();

  for(std::vector<float>::const_iterator citer = sol_vals_test2.cbegin();
      citer != sol_vals_test2.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  // Third test

  // Initial value of the variables
  std::vector<float> init_vals_test3 = {4823, 1685, 657};
  // Functions in x, y and z used to create the user
  // constraint functions
  std::vector<std::string> vnames_test3 = {"x", "y", "z"};
  ESMCI::MapperUtil::UVIDPoly<float> p1_test3 = {2.82311e-06, -0.128872, 1664.65};
  std::vector<std::string> p1_vnames_test3 = {"x"};
  p1_test3.set_vnames(p1_vnames_test3);
  ESMCI::MapperUtil::UVIDPoly<float> p2_test3 = {5.55039e-06, -0.166144, 1271.83};
  std::vector<std::string> p2_vnames_test3 = {"y"};
  p2_test3.set_vnames(p2_vnames_test3);
  ESMCI::MapperUtil::UVIDPoly<float> p3_test3 = {1.1893e-05, -0.19465, 1025.47};
  std::vector<std::string> p3_vnames_test3 = {"z"};
  p3_test3.set_vnames(p3_vnames_test3);
  // Create two variable polynomials from univariate polynomials
  std::vector<std::string> p2v_xy_vnames_test3 = {"x", "y"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xy_test3(p1_test3, p2v_xy_vnames_test3);
  std::cout << p1_2v_xy_test3 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p2_2v_test3(p2_test3, p2v_xy_vnames_test3);
  std::cout << p2_2v_test3 << "\n";
  std::vector<std::string> p2v_xz_vnames_test3 = {"x", "z"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1_2v_xz_test3(p1_test3, p2v_xz_vnames_test3);
  std::cout << p1_2v_xz_test3 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p3_2v_test3(p3_test3, p2v_xz_vnames_test3);
  std::cout << p3_2v_test3 << "\n";
  // User constraint functions
  // Constraint functions are square of the difference between
  // (pair-wise) the functions above
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs_test3 =
    { (p1_2v_xy_test3 - p2_2v_test3)* (p1_2v_xy_test3 - p2_2v_test3),
      (p1_2v_xz_test3 - p3_2v_test3) * (p1_2v_xz_test3 - p3_2v_test3)};

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test3(vnames_test3,
    init_vals_test3, funcs_test3);
  solver_test3.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals_test3 = solver_test3.minimize();

  for(std::vector<float>::const_iterator citer = sol_vals_test3.cbegin();
      citer != sol_vals_test3.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
