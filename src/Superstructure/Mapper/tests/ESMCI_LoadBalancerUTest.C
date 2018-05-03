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

  int ncomps = 3;
  std::vector<float> parallel_exec_times = {100.0, 200.0, 300.0};
  std::vector<float> serial_exec_times = {3000.0, 5000.0, 9000.0};
  std::vector<int> npets = {128, 256, 512};
  ESMCI::MapperUtil::LoadBalancer<float> lb(ncomps, parallel_exec_times,
    serial_exec_times, npets);
  std::vector<int> opt_npets = lb.optimize();

  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
