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
  const int MAX_ITER = 5;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  int ncomps = 3;
  ESMCI::MapperUtil::UVIDPoly<float> pcomp1, pcomp2, pcomp3;
  std::vector<float> pcomp1_xvals = {3600.0, 7200.0, 14400.0, 28800.0};
  std::vector<float> pcomp1_yvals = {1273.542, 819.700, 426.051, 290.470};

  std::vector<float> pcomp2_xvals = {2368, 4096, 8064, 21120};
  std::vector<float> pcomp2_yvals = {959.630, 605.206, 324.372, 236.945};

  std::vector<float> pcomp3_xvals = {1600, 4096, 8192, 9600};
  std::vector<float> pcomp3_yvals = {756.506, 399.585, 270.775, 227.472};

  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_2D_LS_LAPACK, pcomp1_xvals, pcomp1_yvals, pcomp1);
  assert(rc == 0);
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_2D_LS_LAPACK, pcomp2_xvals, pcomp2_yvals, pcomp2);
  assert(rc == 0);
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_2D_LS_LAPACK, pcomp3_xvals, pcomp3_yvals, pcomp3);
  assert(rc == 0);
  
  std::vector<int> npets = {128, 256, 512};
  std::vector<float> parallel_exec_times = {pcomp1.eval(std::vector<float>(1,npets[0])), pcomp2.eval(std::vector<float>(1,npets[1])), pcomp3.eval(std::vector<float>(1,npets[2]))};
  std::vector<float> serial_exec_times = {npets[0]*parallel_exec_times[0], npets[1] * parallel_exec_times[1], npets[2] * parallel_exec_times[2]};
  ESMCI::MapperUtil::LoadBalancer<float> lb(ncomps, parallel_exec_times, serial_exec_times, npets);

  for(int i=0; i<MAX_ITER; i++){
    std::vector<int> opt_npets = lb.optimize();
    parallel_exec_times = {pcomp1.eval(std::vector<float>(1,opt_npets[0])), pcomp2.eval(std::vector<float>(1,opt_npets[1])), pcomp3.eval(std::vector<float>(1,opt_npets[2]))};
    serial_exec_times = {opt_npets[0]*parallel_exec_times[0], opt_npets[1] * parallel_exec_times[1], opt_npets[2] * parallel_exec_times[2]};
    lb.set_lb_info(parallel_exec_times, serial_exec_times, opt_npets);

    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      std::cout << *citer << ", ";
    }
    std::cout << "\n";
  }

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
