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
  double tol = 0.00000001;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  ESMCI::MapperUtil::Matrix<float> m1 = {{1, 2}, {5, 6}};
  //std::cout << m1 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m1_1 = {{1, 2}, {5, 6}};
  //std::cout << m1_1 << std::endl;

  strncpy(name, "Matrix comparison (==) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix comparison (==) test failed", ESMF_MAX_STRLEN);
  ESMC_Test((m1 == m1_1), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix comparison (equals) Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix comparison (equals) test failed", ESMF_MAX_STRLEN);
  ESMC_Test((m1.equals(m1_1, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::MapperUtil::Matrix<float> m2 = {{2, 3}, {5, 6, 7, 8, 9, 10}};
  //std::cout << m2 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m3 = {{3, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
  //std::cout << m3 << std::endl;

  ESMCI::MapperUtil::Matrix<float> m4 = {{3, 2, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
  //std::cout << m4 << std::endl;

  strncpy(name, "Matrix (1x2) addition Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (1x2) addition test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m5 = {{1, 2}, {6, 5}};
  ESMCI::MapperUtil::Matrix<float> m6 = m1 + m5;
  //std::cout << m6 << std::endl;
  ESMCI::MapperUtil::Matrix<float> em1_plus_m5 = {{1, 2}, {11, 11}};
  ESMC_Test((m6 == em1_plus_m5), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (1x2) subtraction Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (1x2) subtraction test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m7 = m1 - m5;
  //std::cout << m7 << std::endl;
  ESMCI::MapperUtil::Matrix<float> em1_minus_m5 = {{1, 2}, {-1, 1}};
  ESMC_Test((m7 == em1_minus_m5), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (1x2 * 2x1) multiplication Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (1x2 * 2x1) multiplication test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m8 = {{2, 1}, {5, 6}};
  ESMCI::MapperUtil::Matrix<float> m9 = m1 * m8;
  //std::cout << m9 << std::endl;
  ESMCI::MapperUtil::Matrix<float> em1_mult_m8 = {{1, 1}, {61}};
  ESMC_Test((m9 == em1_mult_m8), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (2x2) inverse Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (2x2) inverse test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m10 = {{2, 2}, {1, 2, 3, 4}};
  ESMCI::MapperUtil::Matrix<float> m11 = m10.inv();
  //std::cout << m11 << std::endl;
  tol = 0.1;
  ESMCI::MapperUtil::Matrix<float> em10_inv = {{2, 2}, {-2, 1, 1.5, -0.5}};
  ESMC_Test((m11.equals(em10_inv, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (2x2) Pseudo inverse Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (2x2) Pseudo inverse test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m10_pinv = m10.pinv();
  //std::cout << m10_pinv << "\n";
  ESMC_Test((m10_pinv.equals(em10_inv, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (3x3) inverse Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (3x3) inverse test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m12 = {{3, 3}, {7, 2, 1, 0, 3, -1, -3, 4, -2}};
  ESMCI::MapperUtil::Matrix<float> m13 = m12.inv();
  //std::cout << m13 << std::endl;
  tol = 0.1;
  ESMCI::MapperUtil::Matrix<float> em12_inv = {{3, 3}, {-2, 8, -5, 3, -11, 7, 9, -34, 21}};
  ESMC_Test((m13.equals(em12_inv, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (3x3) Pseudo inverse Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (3x3) Pseudo inverse test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m12_pinv = m12.pinv();
  ESMC_Test((m12_pinv.equals(em12_inv, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Matrix (2x4) Pseudo inverse Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (2x4) Pseudo inverse test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m14 = {{2, 4}, {1, 1, 1, 1, 5, 7, 7, 9}};
  ESMCI::MapperUtil::Matrix<float> em14_pinv = {{4, 2}, {2, -0.25, 0.25, 0, 0.25, 0, -1.5, 0.25}};
  ESMCI::MapperUtil::Matrix<float> m14_pinv = m14.pinv();
  std::cout << m14_pinv << "\n";
  ESMC_Test((m14_pinv.equals(em14_pinv, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);
  // Singular matrix
  // ESMCI::MapperUtil::Matrix<float> m14 = {{2, 2}, {3, 4, 6, 8}};
  // ESMCI::MapperUtil::Matrix<float> m15 = m14.inv();
  // std::cout << m15 << std::endl;

  //ESMCI::MapperUtil::Matrix<float> m3 = {{3, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
  strncpy(name, "Matrix (3x4) transpose Utest", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Matrix (3x4) transpose test failed", ESMF_MAX_STRLEN);
  ESMCI::MapperUtil::Matrix<float> m3_trans = m3.transpose();
  ESMCI::MapperUtil::Matrix<float> em3_trans = {{4, 3}, {1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12}};
  std::cout << m3_trans << "\n";
  ESMC_Test((m3_trans.equals(em3_trans, tol)), name, failMsg, &result, __FILE__, __LINE__, 0);
  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
