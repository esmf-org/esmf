#include <iostream>
#include <vector>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Mapper.h"
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
  
  std::vector<ESMCI::MapperUtil::UVIDPoly<double> > pcomps =
    { pcomp1, pcomp2, pcomp3, pcomp4 };
  
  const int NPETS = 16000;
  const int NCOMPS = 16;
  std::vector<std::pair<int, int> > comp_pet_ranges;
  std::vector<int> comp_npets;
  std::vector<std::pair<double, double> > comp_time_intvls;
  std::vector<ESMCI::MapperUtil::CompInfo<double> > comp_infos;
  int next_start_pet = 0;
  for(int i=0; i<NCOMPS; i++){
    comp_pet_ranges.push_back(
      std::pair<int, int>(next_start_pet, next_start_pet + NPETS/NCOMPS - 1));
    next_start_pet += NPETS/NCOMPS;
    comp_npets.push_back(comp_pet_ranges[i].second - comp_pet_ranges[i].first + 1);
    comp_time_intvls.push_back(
      std::pair<double, double>(0.0, pcomps[i%pcomps.size()].eval(comp_npets[i])));

    const std::string COMP_PHASE_NAME("run");
    const std::string COMP_NAME_PREFIX("comp");
    std::ostringstream ostr;
    ostr << COMP_NAME_PREFIX << i;
    std::string comp_name = ostr.str();

    ESMCI::MapperUtil::CompInfo<double> comp_info(
                    comp_name, COMP_PHASE_NAME,
                    comp_pet_ranges[i], comp_time_intvls[i]);
    comp_infos.push_back(comp_info);
  }

  double app_non_opt_wtime = 0.0;
  for(std::vector<std::pair<double, double> >::const_iterator
        iter = comp_time_intvls.cbegin();
        iter != comp_time_intvls.cend(); ++iter){
    app_non_opt_wtime = std::max(app_non_opt_wtime, (*iter).second);
  }
  std::cout << "Initial (non-optimized) app run time = " <<
    app_non_opt_wtime << " s\n";

  strncpy(name, "Mapper UTest", ESMF_MAX_STRLEN);
  strncpy(failMsgNPetsNeg, "Mapper test failed (returned number of PETs < 0)", ESMF_MAX_STRLEN);
  strncpy(failMsgSolnDiv, "Mapper test failed (solution diverging, idle time increasing)", ESMF_MAX_STRLEN);

  ESMCI::VM vm;
  //ESMCI::Mapper mapper(vm, "./FourCompMapperTestRunSeq.txt");
  ESMCI::Mapper mapper(vm);

  mapper.add_opt_method(ESMCI::Mapper::MAPPER_OPT_MIN_IDLE_TIME);
  //mapper.add_opt_method(ESMCI::Mapper::MAPPER_OPT_USE_RSEQ_CONN_DEP);

  for(int i=0; i<MAX_ITER; i++){
    std::cout << "Load Balancer iter : " << i << "\n";
    std::vector<int> opt_npets;
    std::vector<std::pair<int, int> > opt_pet_ranges;
    double opt_wtime;

    mapper.set_comp_info(comp_infos);

    bool opt_pets_available = mapper.optimize(opt_npets, opt_pet_ranges, opt_wtime);
    std::cout << "Optimized time : " << opt_wtime << "\n";
    std::cout << "Optimized pet list : ";
    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      std::cout << *citer << ", ";
    }
    std::cout << "\n";
    for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
        citer != opt_npets.cend(); ++citer){
      ESMC_Test((*citer > 0), name, failMsgNPetsNeg, &result, __FILE__, __LINE__, 0);
    }

    int next_avail_pet = 0;
    for(int i=0; i<NCOMPS; i++){
      comp_pet_ranges[i].first = next_avail_pet;
      comp_pet_ranges[i].second = comp_pet_ranges[i].first + opt_npets[i] - 1;
      next_avail_pet = comp_pet_ranges[i].second + 1;

      comp_npets[i] = comp_pet_ranges[i].second - comp_pet_ranges[i].first + 1;

      comp_time_intvls[i].first = 0;
      comp_time_intvls[i].second = pcomps[i%pcomps.size()].eval(comp_npets[i]);

      comp_infos[i].set_pet_range(comp_pet_ranges[i]);
      comp_infos[i].set_time_interval(comp_time_intvls[i]);
    }

  }
  std::vector<int> opt_npets;
  std::vector<std::pair<int, int> > opt_pet_ranges;
  double opt_wtime;

  bool opt_pets_available = mapper.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
  assert(opt_pets_available);
  std::cout << "Optimal time : " << opt_wtime << "\n";
  std::cout << "Optimal pet list : ";
  for(std::vector<int>::const_iterator citer = opt_npets.cbegin();
      citer != opt_npets.cend(); ++citer){
    std::cout << *citer << ", ";
  }
  std::cout << "\n";

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
