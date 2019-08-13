#include <iostream>
#include <cstring>
#include <vector>
//#include "ESMCI_Poly.h"
//#include "ESMCI_PolyDer.h"
//#include "ESMCI_Mat.h"
//#include "ESMCI_Graph.h"
//#include "ESMCI_GraphUtils.h"
#include "ESMCI_RunSeqDGraph.h"
#include "ESMCI_RunSeqUtils.h"
#include "ESMCI_Macros.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];

  ESMC_TestStart(__FILE__, __LINE__, 0);

  // =============================================================
  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::RunSeqDGraph g1;
    rc = ESMCI::MapperUtil::CreateDGraphFromRSeq("./ThreeCompAndMedRunSeq.txt", g1);
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Create DGraph from Run Sequence test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Create DGraph from Run Sequence test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // =============================================================
  rc = ESMF_SUCCESS;
  bool stconfig_ATM_is_dep_on_OCN = false;
  bool stconfig_OCN_is_dep_on_ICE = false;
  bool stconfig_ATM_is_dep_on_ICE = false;
  try{
    ESMCI::MapperUtil::RunSeqDGraph gstacked;
    rc = ESMCI::MapperUtil::CreateDGraphFromRSeq("./StackedRunSeq.txt", gstacked);
    stconfig_ATM_is_dep_on_OCN = gstacked.has_dependency("ATM", "OCN");
    stconfig_OCN_is_dep_on_ICE = gstacked.has_dependency("OCN", "ICE");
    stconfig_ATM_is_dep_on_ICE = gstacked.has_dependency("ATM", "ICE");

    gstacked.print_to_file("./StackedRunSeq.dot");
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Testing DGraph dependencies Stacked comps (ATM, OCN) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Stacked comps (ATM, OCN) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!stconfig_ATM_is_dep_on_OCN), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Stacked comps (OCN, ICE) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Stacked comps (OCN, ICE) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!stconfig_OCN_is_dep_on_ICE), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Stacked comps (ATM, ICE) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Stacked comps (ATM, ICE) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!stconfig_ATM_is_dep_on_ICE), name, failMsg, &result, __FILE__, __LINE__, 0);

  // =============================================================
  rc = ESMF_SUCCESS;
  bool sconfig_ATM_is_dep_on_OCN = false;
  bool sconfig_OCN_is_dep_on_ICE = false;
  bool sconfig_ATM_is_dep_on_ICE = false;
  bool sconfig_ATM_is_dep_on_MED = false;
  bool sconfig_ICE_is_dep_on_MED = false;
  bool sconfig_OCN_is_dep_on_MED = false;
  try{
    ESMCI::MapperUtil::RunSeqDGraph gstacked;
    rc = ESMCI::MapperUtil::CreateDGraphFromRSeq("./SimpleRunSeq.txt", gstacked);
    sconfig_ATM_is_dep_on_OCN = gstacked.has_dependency("ATM", "OCN");
    sconfig_ATM_is_dep_on_MED = gstacked.has_dependency("ATM", "MED");
    sconfig_OCN_is_dep_on_ICE = gstacked.has_dependency("OCN", "ICE");
    sconfig_OCN_is_dep_on_MED = gstacked.has_dependency("OCN", "MED");
    sconfig_ATM_is_dep_on_ICE = gstacked.has_dependency("ATM", "ICE");
    sconfig_ATM_is_dep_on_MED = gstacked.has_dependency("ATM", "MED");

    gstacked.print_to_file("./SimpleRunSeq.dot");
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Testing DGraph dependencies Simple comps (ATM, OCN) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (ATM, OCN) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!sconfig_ATM_is_dep_on_OCN), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Simple comps (ATM, MED) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (ATM, MED) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!sconfig_ATM_is_dep_on_MED), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Simple comps (OCN, ICE) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (OCN, ICE) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (sconfig_OCN_is_dep_on_ICE), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Simple comps (OCN, MED) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (OCN, MED) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (sconfig_OCN_is_dep_on_MED), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Simple comps (ATM, ICE) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (ATM, ICE) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!sconfig_ATM_is_dep_on_ICE), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies Simple comps (ICE, MED) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies Simple comps (ICE, MED) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!sconfig_ICE_is_dep_on_MED), name, failMsg, &result, __FILE__, __LINE__, 0);

  // =============================================================
  rc = ESMF_SUCCESS;
  bool ATM_is_dep_on_MED = false, OCN_is_dep_on_ATM = false;
  bool ATM_is_dep_on_ICE = false;
  try{
    ESMCI::MapperUtil::RunSeqDGraph g1;
    rc = ESMCI::MapperUtil::CreateDGraphFromRSeq("./ThreeCompAndMedRunSeq.txt", g1);
    ATM_is_dep_on_MED = g1.has_dependency("ATM", "MED");
    OCN_is_dep_on_ATM = g1.has_dependency("OCN", "ATM");
    ATM_is_dep_on_ICE = g1.has_dependency("ATM", "ICE");

    g1.print_to_file("./ThreeCompAndMedRunSeq.dot");
    g1.fuse_merge_phases();
    g1.print_to_file("./ThreeCompAndMedRunSeq_fused.dot");
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Testing DGraph dependencies (ATM, MED) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies (ATM, MED) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (ATM_is_dep_on_MED), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies (OCN, ATM) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies (OCN, ATM) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!OCN_is_dep_on_ATM), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Testing DGraph dependencies (ATM, ICE) from Run Sequence", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Testing DGraph dependencies (ATM, ICE) from Run Sequence failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS) && (!ATM_is_dep_on_ICE), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
