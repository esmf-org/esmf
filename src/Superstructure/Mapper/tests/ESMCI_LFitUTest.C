#include <iostream>
#include <cstring>
#include <cmath>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyFit.h"
#include "ESMCI_LFit.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  int rc = 0, result = 0;
  double tol = 0.1;
  bool test_success = true;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  std::vector<float> xvals = {1.0, 1.5};
  std::vector<float> yvals = {1.1, 1.3};

  strncpy(name, "Linear fit Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Linear fit Utest failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::UVIDPoly<float> l0;
  rc = ESMCI::MapperUtil::LinearFit(xvals, yvals, l0);
  std::cout << l0 << std::endl;
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
