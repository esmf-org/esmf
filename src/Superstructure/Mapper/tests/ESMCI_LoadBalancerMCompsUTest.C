#include <iostream>
#include <vector>
#include <cstring>
#include <algorithm>
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
  char failMsgNPetsNeg[ESMF_MAX_STRLEN];
  char failMsgSolnDiv[ESMF_MAX_STRLEN];
  const int MAX_ITER = 100;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  int ncomps = 3;
  ESMCI::MapperUtil::UVIDPoly<double> pcomp1, pcomp2, pcomp3;
  std::vector<double> pcomp1_xvals = {3600.0, 7200.0, 14400.0, 28800.0};
  std::vector<double> pcomp1_yvals = {1273.542, 819.700, 426.051, 290.470};

  std::vector<double> pcomp2_xvals = {2368, 4096, 8064, 21120};
  std::vector<double> pcomp2_yvals = {959.630, 605.206, 324.372, 236.945};

  std::vector<double> pcomp3_xvals = {1600, 4096, 8192, 9600};
  std::vector<double> pcomp3_yvals = {756.506, 399.585, 270.775, 227.472};

  int max_deg = 2;
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, pcomp1_xvals, pcomp1_yvals, pcomp1);
  assert(rc == 0);
  std::cout << "pcomp1 = " << pcomp1 << "\n";
  for(std::vector<double>::const_iterator citer1 = pcomp1_xvals.cbegin(),
      citer2 = pcomp1_yvals.cbegin();
      (citer1 != pcomp1_xvals.cend()) && (citer2 != pcomp1_yvals.cend());
      ++citer1, ++citer2){
    std::cout << "pcomp1(" << *citer1 << ") = "
              << pcomp1.eval(*citer1) << " : " << *citer2 << "\n";
  }
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, pcomp2_xvals, pcomp2_yvals, pcomp2);
  assert(rc == 0);
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, pcomp3_xvals, pcomp3_yvals, pcomp3);
  assert(rc == 0);
  
//  std::vector<int> npets = {2048, 4096, 8192};
  std::vector<std::pair<int, int> > comp_pet_ranges = {
                                                        std::pair<int, int>(0, 4095),
                                                        std::pair<int, int>(4096, 6143),
                                                        std::pair<int, int>(6144, 7167)
                                                        };
  std::vector<int> comp_npets = {
    comp_pet_ranges[0].second - comp_pet_ranges[0].first + 1,
    comp_pet_ranges[1].second - comp_pet_ranges[1].first + 1,
    comp_pet_ranges[2].second - comp_pet_ranges[2].first + 1
                            };
  std::vector<std::pair<double, double> > comp_time_intvls = {
    std::pair<double, double>(0, pcomp1.eval(comp_npets[0])),
    std::pair<double, double>(0, pcomp2.eval(comp_npets[1])),
    std::pair<double, double>(0, pcomp3.eval(comp_npets[2]))
                                                            };

  ESMCI::MapperUtil::CompInfo<double> comp0("comp0", "run",
                                      comp_pet_ranges[0], comp_time_intvls[0]);
  ESMCI::MapperUtil::CompInfo<double> comp1("comp1", "run",
                                      comp_pet_ranges[1], comp_time_intvls[1]);
  ESMCI::MapperUtil::CompInfo<double> comp2("comp2", "run",
                                      comp_pet_ranges[2], comp_time_intvls[2]);

  std::vector<ESMCI::MapperUtil::CompInfo<double> > comp_infos = {
                                                                  comp0,
                                                                  comp1,
                                                                  comp2
                                                                };
  ESMCI::MapperUtil::CompInfoStore<double> *comp_info_store =
    ESMCI::MapperUtil::CompInfoStore<double>::get_instance();

  comp_info_store->add_comp_info(comp_infos[0]);
  comp_info_store->add_comp_info(comp_infos[1]);
  comp_info_store->add_comp_info(comp_infos[2]);

  ESMCI::MapperUtil::LoadBalancer<double> lb(comp_infos);

  strncpy(name, "Load Balancer UTest", ESMF_MAX_STRLEN);
  strncpy(failMsgNPetsNeg, "Load Balancer test failed (returned number of PETs < 0)", ESMF_MAX_STRLEN);
  strncpy(failMsgSolnDiv, "Load Balancer test failed (solution diverging, idle time increasing)", ESMF_MAX_STRLEN);
  for(int i=0; i<MAX_ITER; i++){
    std::cout << "Load Balancer iter : " << i << "\n";
    std::vector<int> opt_npets;
    std::vector<std::pair<int, int> > opt_pet_ranges;
    double opt_wtime;
    bool opt_pets_available = lb.optimize(opt_npets, opt_pet_ranges, opt_wtime);
    std::cout << "Optimized time : " << opt_wtime << "\n";
    std::cout << "Optimized pet list : (";
    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      std::cout << *citer << ", ";
    }
    std::cout << " ) = " << std::accumulate(opt_npets.begin(), opt_npets.end(), 0);
    std::cout << " pets\n";
    if(!opt_pets_available){
      /* The solution has converged or solver no longer works for the case */
      break;
    }
    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      ESMC_Test((*citer > 0), name, failMsgNPetsNeg, &result, __FILE__, __LINE__, 0);
    }

    comp_pet_ranges[0].first = 0;
    comp_pet_ranges[0].second = comp_pet_ranges[0].first + opt_npets[0] - 1;

    comp_pet_ranges[1].first = comp_pet_ranges[0].second + 1;
    comp_pet_ranges[1].second = comp_pet_ranges[1].first + opt_npets[1] - 1;

    comp_pet_ranges[2].first = comp_pet_ranges[1].second + 1;
    comp_pet_ranges[2].second = comp_pet_ranges[2].first + opt_npets[2] - 1;

    /* FIXME: Generalize the code below for n comps, put in a for loop */
    comp_npets[0] = comp_pet_ranges[0].second - comp_pet_ranges[0].first + 1;
    comp_npets[1] = comp_pet_ranges[1].second - comp_pet_ranges[1].first + 1;
    comp_npets[2] = comp_pet_ranges[2].second - comp_pet_ranges[2].first + 1;

    comp_time_intvls[0].first = 0;
    comp_time_intvls[0].second = pcomp1.eval(comp_npets[0]);

    comp_time_intvls[1].first = 0;
    comp_time_intvls[1].second = pcomp2.eval(comp_npets[1]);

    comp_time_intvls[2].first = 0;
    comp_time_intvls[2].second = pcomp3.eval(comp_npets[2]);

    comp_infos[0].set_pet_range(comp_pet_ranges[0]);
    comp_infos[0].set_time_interval(comp_time_intvls[0]);

    comp_infos[1].set_pet_range(comp_pet_ranges[1]);
    comp_infos[1].set_time_interval(comp_time_intvls[1]);

    comp_infos[2].set_pet_range(comp_pet_ranges[2]);
    comp_infos[2].set_time_interval(comp_time_intvls[2]);

    comp_info_store->add_comp_info(comp_infos[0]);
    comp_info_store->add_comp_info(comp_infos[1]);
    comp_info_store->add_comp_info(comp_infos[2]);

    lb.set_lb_info(comp_infos);
  }

  std::vector<int> opt_npets;
  std::vector<std::pair<int, int> > opt_pet_ranges;
  double opt_wtime;
  lb.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
  std::cout << "Optimal Optimized time : " << opt_wtime << "\n";
  std::cout << "Optimal Optimized pet list : ";
  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";
  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    ESMC_Test((*citer > 0), name, failMsgNPetsNeg, &result, __FILE__, __LINE__, 0);
  }

  ESMCI::MapperUtil::CompInfoStore<double>::finalize();
  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
