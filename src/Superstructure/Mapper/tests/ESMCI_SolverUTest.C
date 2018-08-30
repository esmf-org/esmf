#include <iostream>
#include <vector>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_LPolyMV.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"
#include "ESMC_Test.h"

template<typename T>
class ThreeValFromTwoGenerator
  : public ESMCI::MapperUtil::SESolver<T>::UConstraintValGenerator
{
  public:
    ThreeValFromTwoGenerator(T sum);
    void set_sum(T sum);
    std::vector<T> get_vals(const std::vector<T> &available_vals) const;
  private:
    T sum_;
};

template<typename T>
ThreeValFromTwoGenerator<T>::ThreeValFromTwoGenerator(T sum):sum_(sum)
{
}

template<typename T>
void ThreeValFromTwoGenerator<T>::set_sum(T sum)
{
  sum_ = sum;
}

template<typename T>
std::vector<T> ThreeValFromTwoGenerator<T>::get_vals(const std::vector<T> &available_vals) const
{
  std::vector<T> res(available_vals.cbegin(), available_vals.cend());
  T avals_sum = std::accumulate(available_vals.cbegin(), available_vals.cend(), static_cast<T>(0));
  res.push_back(sum_ - avals_sum);
  return res;
}

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

  float init_vals_sum = std::accumulate(init_vals.cbegin(), init_vals.cend(), 0.0);
  ThreeValFromTwoGenerator<float> uc_vgen(init_vals_sum);
  std::vector<ESMCI::MapperUtil::MVIDLPoly<float> > mvid_lpoly_funcs;
  ESMCI::MapperUtil::MVIDLPoly<float> final_constraint =
    {1.0, 1.0, 1.0, static_cast<float>(-1.0) * init_vals_sum};
  final_constraint.set_vnames(vnames);
  mvid_lpoly_funcs.push_back(final_constraint);

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver(vnames, init_vals, funcs, mvid_lpoly_funcs);
  solver.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals = solver.minimize(uc_vgen);

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

  float init_vals_test2_sum = std::accumulate(init_vals_test2.cbegin(),
    init_vals_test2.cend(), 0.0);
  uc_vgen.set_sum(init_vals_test2_sum);

  std::vector<ESMCI::MapperUtil::MVIDLPoly<float> > mvid_lpoly_funcs_test2;
  ESMCI::MapperUtil::MVIDLPoly<float> final_constraint_test2 =
    {1.0, 1.0, 1.0, static_cast<float>(-1.0) * init_vals_test2_sum};
  final_constraint_test2.set_vnames(vnames_test2);
  mvid_lpoly_funcs_test2.push_back(final_constraint_test2);

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test2(vnames_test2,
    init_vals_test2, funcs_test2, mvid_lpoly_funcs_test2);
  solver_test2.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals_test2 = solver_test2.minimize(uc_vgen);

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

  float init_vals_test3_sum = std::accumulate(init_vals_test3.cbegin(),
    init_vals_test3.cend(), 0.0);
  uc_vgen.set_sum(init_vals_test3_sum);

  std::vector<ESMCI::MapperUtil::MVIDLPoly<float> > mvid_lpoly_funcs_test3;
  ESMCI::MapperUtil::MVIDLPoly<float> final_constraint_test3 =
    {1.0, 1.0, 1.0, static_cast<float>(-1.0) * init_vals_test3_sum};
  final_constraint_test3.set_vnames(vnames_test3);
  mvid_lpoly_funcs_test3.push_back(final_constraint_test3);

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test3(vnames_test3,
    init_vals_test3, funcs_test3, mvid_lpoly_funcs_test3);
  solver_test3.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals_test3 = solver_test3.minimize(uc_vgen);

  for(std::vector<float>::const_iterator citer = sol_vals_test3.cbegin();
      citer != sol_vals_test3.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  std::cout << "============= Overconstrained system ==================\n";
  std::vector<std::string> p2v_yz_vnames_test4 = {"y", "z"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p2_2v_yz_test4(p2_test3, p2v_yz_vnames_test4);
  std::cout << p2_2v_yz_test4 << "\n";
  ESMCI::MapperUtil::TwoVIDPoly<float> p3_2v_test4(p3_test3, p2v_yz_vnames_test4);
  std::cout << p3_2v_test4 << "\n";
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs_test4 =
    { (p1_2v_xy_test3 - p2_2v_test3)* (p1_2v_xy_test3 - p2_2v_test3),
      (p1_2v_xz_test3 - p3_2v_test3) * (p1_2v_xz_test3 - p3_2v_test3),
      (p2_2v_yz_test4 - p3_2v_test4) * (p2_2v_yz_test4 - p3_2v_test4)
      };

  uc_vgen.set_sum(init_vals_test3_sum);

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test4(vnames_test3,
    init_vals_test3, funcs_test4, mvid_lpoly_funcs_test3);
  solver_test4.set_niters(SOLVER_MAX_ITERS);
  std::vector<float> sol_vals_test4 = solver_test4.minimize(uc_vgen);

  for(std::vector<float>::const_iterator citer = sol_vals_test4.cbegin();
      citer != sol_vals_test4.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  // Test4 : Perform a poly fit using the solver
  // Initial value of the variables - coefficients of the fit function
  std::vector<float> pcomp1_xvals = {3600.0, 7200.0, 14400.0, 28800.0};
  std::vector<float> pcomp1_yvals = {1273.542, 819.700, 426.051, 290.470};
  
  // Functions in x, y and z used to create the user
  // constraint functions
  std::vector<std::string> pcomp1_vnames = {"x"};

  // Perform a 2deg fit
  // ax^2 + bx + c = y
  // Substitute x and y values to get 4 linear equations
  // Initial values of coeffs a, b, c == 1
  std::vector<float> init_coeff_vals = {1.0, 1.0, 1.0};
  // First user constraint function a * x_1^2 + b * x_1 + c - y_1 = 0

  // User constraint functions
  // Constraint functions are square of the difference between
  // (pair-wise) the functions above
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs_test5;

  // Create a solver instance and optimize init_vals
  ESMCI::MapperUtil::SESolver<float> solver_test5(pcomp1_vnames, init_coeff_vals, funcs_test5);
  solver_test5.set_niters(SOLVER_MAX_ITERS);
  /*
  std::vector<float> sol_vals_test4 = solver_test4.minimize(uc_vgen);

  for(std::vector<float>::const_iterator citer = sol_vals_test4.cbegin();
      citer != sol_vals_test4.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";
  */

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
