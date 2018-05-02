#include <iostream>
#include <cstring>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];

  ESMC_TestStart(__FILE__, __LINE__, 0);

  ESMCI::MapperUtil::Matrix<float> m1 = {{1, 2}, {5, 6}};
  std::cout << m1 << std::endl;


  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
