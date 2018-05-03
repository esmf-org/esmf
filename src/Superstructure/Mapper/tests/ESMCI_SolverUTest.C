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

  ESMC_TestStart(__FILE__, __LINE__, 0);

  std::vector<float> init_vals = {2.0, 4.0, 6.0};
  std::vector<std::string> vnames = {"x", "y", "z"};
  ESMCI::MapperUtil::TwoVIDPoly<float> p1 = {1.0, 2.0, 3.0};
  std::vector<std::string> p1_vnames = {"x", "y"};
  p1.set_vnames(p1_vnames);
  ESMCI::MapperUtil::TwoVIDPoly<float> p2 = {4.0, 5.0, 6.0};
  std::vector<std::string> p2_vnames = {"y", "z"};
  p2.set_vnames(p2_vnames);
  ESMCI::MapperUtil::TwoVIDPoly<float> p3 = {7.0, 8.0, 9.0};
  std::vector<std::string> p3_vnames = {"x", "z"};
  p3.set_vnames(p3_vnames);
  std::vector<ESMCI::MapperUtil::TwoVIDPoly<float> > funcs = {p1, p2, p3};
  ESMCI::MapperUtil::SESolver<float> solver(vnames, init_vals, funcs);
  std::vector<float> sol_vals = solver.minimize();

  for(std::vector<float>::const_iterator citer = sol_vals.cbegin();
      citer != sol_vals.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
