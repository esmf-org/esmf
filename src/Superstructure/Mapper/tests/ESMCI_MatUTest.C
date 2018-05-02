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

  ESMCI::MapperUtil::Matrix<float> m2 = {{2, 3}, {5, 6, 7, 8, 9, 10}};
  std::cout << m2 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m3 = {{3, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
  std::cout << m3 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m4 = {{3, 2, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
  std::cout << m4 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m5 = {{1, 2}, {6, 5}};
  ESMCI::MapperUtil::Matrix<float> m6 = m1 + m5;
  std::cout << m6 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m7 = m1 - m5;
  std::cout << m7 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m8 = {{2, 1}, {5, 6}};
  ESMCI::MapperUtil::Matrix<float> m9 = m1 * m8;
  std::cout << m9 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m10 = {{2, 2}, {1, 2, 3, 4}};
  ESMCI::MapperUtil::Matrix<float> m11 = m10.inv();
  std::cout << m11 << std::endl;

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
