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
