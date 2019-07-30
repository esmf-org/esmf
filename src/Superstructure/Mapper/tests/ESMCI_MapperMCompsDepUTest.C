#include <iostream>
#include <vector>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Mapper.h"
#include "ESMCI_ExecSim.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsgNPetsNeg[ESMF_MAX_STRLEN];
  char failMsgSolnDiv[ESMF_MAX_STRLEN];
  const int MAX_ITER = 10;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  int ncomps = 3;
  ESMCI::MapperUtil::UVIDPoly<double> pcomp1, pcomp2, pcomp3, pcomp4;
  std::vector<double> pcomp1_xvals = {3600.0, 7200.0, 14400.0, 28800.0};
  std::vector<double> pcomp1_yvals = {1273.542, 819.700, 426.051, 290.470};

  std::vector<double> pcomp2_xvals = {2368, 4096, 8064, 21120};
  std::vector<double> pcomp2_yvals = {959.630, 605.206, 324.372, 236.945};

  std::vector<double> pcomp3_xvals = {1600, 4096, 8192, 9600};
  std::vector<double> pcomp3_yvals = {756.506, 399.585, 270.775, 227.472};

  std::vector<double> pcomp4_xvals = {1000, 2000, 3000, 4000};
  std::vector<double> pcomp4_yvals = {700.0, 400.0, 150.0, 100.0};

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
  rc = ESMCI::MapperUtil::PolyFit(ESMCI::MapperUtil::POLY_FIT_LS_LAPACK, max_deg, pcomp4_xvals, pcomp4_yvals, pcomp4);
  assert(rc == 0);
  
  
//  std::vector<int> npets = {2048, 4096, 8192};
  std::vector<std::pair<int, int> > comp_pet_ranges = {
                                                        std::pair<int, int>(0, 1999),
                                                        std::pair<int, int>(0, 1999),
                                                        std::pair<int, int>(0, 1999),
                                                        std::pair<int, int>(0, 1999),
                                                        };
  std::vector<int> comp_npets = {
    comp_pet_ranges[0].second - comp_pet_ranges[0].first + 1,
    comp_pet_ranges[1].second - comp_pet_ranges[1].first + 1,
    comp_pet_ranges[2].second - comp_pet_ranges[2].first + 1,
    comp_pet_ranges[3].second - comp_pet_ranges[3].first + 1
                            };
  double delay = 1.0;
  std::vector<std::pair<double, double> > comp_time_intvls = {
    std::pair<double, double>(0, pcomp1.eval(comp_npets[0])),
    std::pair<double, double>(0, pcomp2.eval(comp_npets[1])),
    std::pair<double, double>(0, pcomp3.eval(comp_npets[2])),
    std::pair<double, double>(0, pcomp4.eval(comp_npets[3]))
                                                            };

  comp_time_intvls[1].first = comp_time_intvls[0].second + delay;
  comp_time_intvls[1].second += comp_time_intvls[1].first;

  comp_time_intvls[2].first = comp_time_intvls[1].second + delay;
  comp_time_intvls[2].second += comp_time_intvls[2].first;

  comp_time_intvls[3].first = comp_time_intvls[2].second + delay;
  comp_time_intvls[3].second += comp_time_intvls[3].first;

  std::cout << "Stacked config, execution time = "
    << comp_time_intvls[3].second << " s\n";
  ESMCI::MapperUtil::CompInfo<double> comp0("comp0", "run",
                                      comp_pet_ranges[0], comp_time_intvls[0]);
  ESMCI::MapperUtil::CompInfo<double> comp1("comp1", "run",
                                      comp_pet_ranges[1], comp_time_intvls[1]);
  ESMCI::MapperUtil::CompInfo<double> comp2("comp2", "run",
                                      comp_pet_ranges[2], comp_time_intvls[2]);
  ESMCI::MapperUtil::CompInfo<double> comp3("comp3", "run",
                                      comp_pet_ranges[3], comp_time_intvls[3]);

  std::vector<ESMCI::MapperUtil::CompInfo<double> > comp_infos = {
                                                                  comp0,
                                                                  comp1,
                                                                  comp2,
                                                                  comp3
                                                                };
  strncpy(name, "Mapper UTest", ESMF_MAX_STRLEN);
  strncpy(failMsgNPetsNeg, "Mapper test failed (returned number of PETs < 0)", ESMF_MAX_STRLEN);
  strncpy(failMsgSolnDiv, "Mapper test failed (solution diverging, idle time increasing)", ESMF_MAX_STRLEN);

  ESMCI::VM vm;
  ESMCI::Mapper mapper(vm, "./FourCompWithDataDepMapperTestRunSeq.txt");

  mapper.add_opt_method(ESMCI::Mapper::MAPPER_OPT_MIN_IDLE_TIME);
  mapper.add_opt_method(ESMCI::Mapper::MAPPER_OPT_USE_RSEQ_CONN_DEP);

  for(int i=0; i<MAX_ITER; i++){
    std::cout << "Load Balancer iter : " << i << "\n";
    std::vector<int> opt_npets;
    int total_opt_npets = 0;
    std::vector<std::pair<int, int> > opt_pet_ranges;
    std::vector<double> comp_wtimes(comp_infos.size(), 0.0);
    double opt_wtime;

    mapper.set_comp_info(comp_infos);

    bool opt_pets_available = mapper.optimize(opt_npets, opt_pet_ranges, opt_wtime);
    std::cout << "Optimized time : " << opt_wtime << "\n";
    std::cout << "Optimized pet list : {";
    std::vector<int>::const_iterator opt_npets_citer = opt_npets.cbegin();
    std::vector<std::pair<int, int> >::const_iterator opt_pet_ranges_citer =
      opt_pet_ranges.cbegin();
    for(;
        (opt_npets_citer != opt_npets.cend()) &&
        (opt_pet_ranges_citer != opt_pet_ranges.cend());
        ++opt_npets_citer, ++opt_pet_ranges_citer){
      std::cout << *opt_npets_citer <<
        " [" << (*opt_pet_ranges_citer).first << " - " <<
        (*opt_pet_ranges_citer).second << "], ";
      total_opt_npets = std::max(total_opt_npets, (*opt_pet_ranges_citer).second);
    }
    std::cout << "} = " << total_opt_npets << "\n";
    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      ESMC_Test((*citer > 0), name, failMsgNPetsNeg, &result, __FILE__, __LINE__, 0);
    }

    assert(comp_npets.size() == comp_pet_ranges.size());
    int j=0;
    for(std::vector<std::pair<int, int> >::iterator 
          iter = comp_pet_ranges.begin(), new_iter = opt_pet_ranges.begin();
          (iter != comp_pet_ranges.end()) && (new_iter != opt_pet_ranges.end());
          ++iter, ++new_iter, j++){
      *iter = *new_iter;
      comp_npets[j] = (*iter).second - (*iter).first + 1;
      assert(comp_npets[j] > 0);
    }
    int k=0;
    for(std::vector<ESMCI::MapperUtil::CompInfo<double> >::iterator iter = comp_infos.begin();
          iter != comp_infos.end(); ++iter, k++){
      (*iter).set_pet_range(opt_pet_ranges[k]);
    }


    comp_wtimes[0] = pcomp1.eval(comp_npets[0]);
    comp_wtimes[1] = pcomp2.eval(comp_npets[1]);
    comp_wtimes[2] = pcomp3.eval(comp_npets[2]);
    comp_wtimes[3] = pcomp4.eval(comp_npets[3]);

    ESMCI::MapperUtil::MapperSimRun(comp_infos, comp_wtimes);

  }
  std::vector<int> opt_npets;
  int total_opt_npets = 0;
  std::vector<std::pair<int, int> > opt_pet_ranges;
  double opt_wtime;

  bool opt_pets_available = mapper.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
  for(std::vector<std::pair<int, int> >::const_iterator
        citer = opt_pet_ranges.cbegin();
        citer != opt_pet_ranges.cend(); ++citer){
    total_opt_npets = std::max(total_opt_npets, (*citer).second);
  }
  std::cout << "Optimal time : " << opt_wtime << "\n";
  std::cout << "Optimal pet list : (";
  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << ") = " << total_opt_npets << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
