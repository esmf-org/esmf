#include <iostream>
#include <vector>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"
#include "ESMCI_LoadBalancer.h"
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

  int ncomps = 2;
  std::vector<float> serial_exec_times = {3.0, 5.0, 9.0};
  std::vector<int> npets = {1, 2, 3};
  ESMCI::MapperUtil::LoadBalancer<float> lb(ncomps, serial_exec_times, npets);
  std::vector<int> opt_npets = lb.optimize();

  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
