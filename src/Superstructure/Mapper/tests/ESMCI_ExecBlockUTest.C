#include <iostream>
#include <cstring>
#include <utility>
#include <vector>
#include "ESMCI_CompInfo.h"
#include "ESMCI_ExecBlock.h"
#include "ESMCI_ExecBlockUtils.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  double tol = 0.00000001;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    std::pair<int, int> atm_pet_range(1, 2);
    std::pair<float, float> atm_time_intvl(0.0, 3.0);
    ESMCI::MapperUtil::CompInfo<float> comp1("ATM", "run", atm_pet_range, atm_time_intvl);
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "CompInfo create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "CompInfo create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    std::vector<std::pair<int, int> > pet_ranges = {
                                              std::pair<int, int>(1, 10),
                                              std::pair<int, int>(1, 20),
                                              std::pair<int, int>(1, 25)
                                            };
    std::vector<std::pair<float, float> > time_intvls = {
                                              std::pair<float, float>(0.0, 3.0),
                                              std::pair<float, float>(3.1, 4.1),
                                              std::pair<float, float>(4.2, 4.9)
                                            };
    std::vector<std::pair<int, int> >::const_iterator pet_iter = pet_ranges.cbegin();
    std::vector<std::pair<float, float> >::const_iterator time_iter =time_intvls.cbegin();
    ESMCI::MapperUtil::CompInfoStore<float> *comp_info_store =
      ESMCI::MapperUtil::CompInfoStore<float>::get_instance();
    for(; (pet_iter != pet_ranges.end()) && (time_iter != time_intvls.end());
          ++pet_iter, ++time_iter){
      std::pair<int, int> atm_pet_range(*pet_iter);
      std::pair<float, float> atm_time_intvl(*time_iter);
      int npets = pet_iter->second - pet_iter->first + 1;
      std::pair<int, int> ocn_pet_range(pet_iter->second + 1, pet_iter->second + npets);
      std::pair<float, float> ocn_time_intvl(*time_iter);
      ESMCI::MapperUtil::CompInfo<float> comp1("ATM", "run", atm_pet_range, atm_time_intvl);
      ESMCI::MapperUtil::CompInfo<float> comp2("OCN", "run", ocn_pet_range, ocn_time_intvl);

      comp_info_store->add_comp_info(comp1);
      comp_info_store->add_comp_info(comp2);

    }
    ESMCI::MapperUtil::UVIDPoly<float> sfunc;
    ESMCI::MapperUtil::CompInfo<float> comp1("ATM", "run", pet_ranges[0], time_intvls[0]);
    ESMCI::MapperUtil::CompInfo<float> comp2("OCN", "run", pet_ranges[0], time_intvls[0]);
    bool has_scaling_func = comp_info_store->get_scaling_function(comp1, sfunc);
    if(has_scaling_func){
      std::cout << sfunc << "\n";
    }
    else{
      rc = ESMF_FAILURE;
    }
    has_scaling_func = comp_info_store->get_scaling_function(comp2, sfunc);
    if(has_scaling_func){
      std::cout << sfunc << "\n";
    }
    else{
      rc = ESMF_FAILURE;
    }

    ESMCI::MapperUtil::ExecBlock<float> eblock;
    eblock.add_comp_phase(comp1);
    eblock.add_comp_phase(comp2);

    ESMCI::MapperUtil::MVIDLPoly<float> sfunc_cfunc;
    has_scaling_func = eblock.get_scaling_function(sfunc, sfunc_cfunc);
    if(has_scaling_func){
      std::cout << "Scaling function : " << sfunc << "\n";
      std::cout << "Scaling constraint function : " << sfunc_cfunc << "\n";
    }
    else{
      rc = ESMF_FAILURE;
    }
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Exec Block (two parallel comps) Scaling function test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Exec Blocks (two parallel comps) Scaling function test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    std::vector<ESMCI::MapperUtil::CompInfo<float> > comps;
    std::pair<int, int> atm_pet_range(1, 2);
    std::pair<float, float> atm_time_intvl(0.0, 3.0);
    comps.push_back(ESMCI::MapperUtil::CompInfo<float>("ATM", "run", atm_pet_range, atm_time_intvl));

    std::pair<int, int> ocn_pet_range(3, 4);
    std::pair<float, float> ocn_time_intvl(0.0, 2.0);
    comps.push_back(ESMCI::MapperUtil::CompInfo<float>("OCN", "run", ocn_pet_range, ocn_time_intvl));

    std::vector<std::vector<ESMCI::MapperUtil::ExecBlock<float> > >pexec_blocks;
    bool found_pexec_blocks = 
      ESMCI::MapperUtil::find_parallel_exec_blocks(comps, pexec_blocks);

    if(found_pexec_blocks){
      int pexec_block_i = 0;
      for(std::vector<std::vector<ESMCI::MapperUtil::ExecBlock<float> > >::const_iterator
          citer1 = pexec_blocks.cbegin(); citer1 != pexec_blocks.cend();
          ++citer1, pexec_block_i++){
        std::cout << "Parallel Exec Block " << pexec_block_i << " : \n";
        int exec_block_i = 0;
        for(std::vector<ESMCI::MapperUtil::ExecBlock<float> >::const_iterator
          citer2 = (*citer1).cbegin(); citer2 != (*citer1).cend();
          ++citer2, exec_block_i++){
          std::cout << "\tExec Block " << exec_block_i << " : \n";
          std::cout << *citer2 << "\n";
        }
      }
    }

    if(found_pexec_blocks && (pexec_blocks.size() == 1) && (pexec_blocks[0].size() == 2)){
      rc = ESMF_SUCCESS;
    }
    else{
      rc = ESMF_FAILURE;
    }
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Find Parallel Exec Blocks (two parallel comps) test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Find Parallel Exec Blocks (two parallel comps) test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
