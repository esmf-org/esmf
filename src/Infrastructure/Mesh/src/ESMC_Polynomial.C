// $Id: ESMC_Polynomial.C,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Polynomial.h>

#include <iterator>
#include <iostream>

#ifdef ESMC_LAPACK
extern "C" void FTN(dgelsy)(int *,int *,int*,double*,int*,double*,int*,int*,double*,int*,double*,int*,int*);
#endif

namespace ESMCI {
namespace MESH {

void generate_ILkernel(std::vector<double> &coef) {
  UInt k = coef.size() - 3;

  // Long divide out l0*l1;
  std::vector<double> ocoef;ocoef.reserve(k+3);
  std::copy(coef.begin(), coef.end(), std::back_inserter(ocoef));
  coef.resize(k+1);

 // -l0*l1=0.25x^2-0.25
std::cout << "ker:" << k << " :";
  for (int i = k; i >= 0; --i) {
    coef[i] = ocoef[i+2]/0.25;
    // subtract off
    ocoef[i+2] = 0; ocoef[i] -= coef[i]*(-0.25);
std::cout << coef[i] << " ";
  }
std::cout << std::endl;
}

void set_legendre_coef(UInt m, std::vector<double> &coef) {
  coef.resize(m+1, 0);
  std::vector<double> Lk_1(m+1,0);
  std::vector<double> Lk(m+1,0);
  
  coef[0] = 1;  if (m == 0) return;
  coef[0] = 0; coef[1] = 1;  if (m == 1) return;

  // The recursion Lk = 1/(k+1)*((2*k+1)Lk-1 - k Lk)
  // Hence the kth coeff of Lk is (2k+1)/(k+1)i-1th Lk - k/(k+1) ith of Lk-1
  
  // Now compute the remaining coeffs
  Lk[0] = 0; Lk[1] = 1;
  Lk_1[0] = 1; 
  for (UInt k = 1; k < m; k++) {
    double twok = ((double)(2*k+1)) / (k+1);
    double kopk = (double) k/(k+1);

    // Recurse downward. lk_1 is left filled with zeros, so first term ok
    for (int j = k+1; j >= 1; j--) {
      coef[j] = twok*Lk[j-1] - kopk*Lk_1[j];
    }
    coef[0] = -kopk*Lk_1[0]; // last one
    // Save off results
    std::copy(&Lk[0], &Lk[k+1], Lk_1.begin());
    std::copy(&coef[0], &coef[k+2], Lk.begin());
  }

/*
  std::cout << "Legendre, k=" << m << std::endl;
  std::copy(coef.begin(), coef.end(), std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;
*/
}

void set_Ilegendre_coef(UInt K, std::vector<double> &coef) {
  if (K == 0) {
    coef.resize(2,0);
    coef[0] = 0.5; 
    coef[1] = -0.5;
    K = 1; // reset degree
    return;
  } else if (K == 1) {
    coef.resize(2,0);
    coef[0] = 0.5; 
    coef[1] = 0.5;
    K = 1; // reset degree
    return;
  }

  // Else
  coef.resize(K+1);
  Legendre<double> lg(K-1);
  const double *lk_1 = lg.GetCoefficients();
  
  for (UInt j = 1; j <= K; j++) {
    double one_j = 1.0/j;
//std::cout << "j=" << j << (j& 0x01 ? " true" : " false") << std::endl;
    coef[0] -= one_j*( j & 0x01 ? -1.0 : 1.0)*lk_1[j-1];
    coef[j] = one_j*lk_1[j-1];
  }
/*
  std::cout << "Integrated Legendre, k=" << K << std::endl;
  std::copy(coef.begin(), coef.end(), std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;
*/
}

template<typename POLY>
void PolyFit1D(UInt nsamples, const double coord[], const double vals[], const std::vector<POLY*> &poly, double coef[])
{
#ifdef ESMC_LAPACK
  UInt ncoef = poly.size();
  int m = nsamples, n = ncoef, nrhs = 1, info = 0, rank, ldb;
  ldb = std::max(std::max(m,n),1);
  std::vector<double> mat(nsamples*ncoef);
  std::vector<double> rhs(ldb);

  for (UInt i = 0; i < nsamples; i++) {
    rhs[i] = vals[i]; // sizing might not be right, so copy
    for (UInt j = 0; j < ncoef; j++) {
      mat[j*nsamples + i] = EvalPoly<POLY>()(*poly[j], coord[i]);
    }
  }

  std::vector<int> jpvt(ncoef, 0);
  //int lwork = std::max(std::min(m,n)+2*n+1, 2*std::min(m,n)+nrhs);
  // TODO figure this out
  int lwork = 4028;
  std::vector<double> work(lwork, 0);
  double rcond=0.0000000000001;

  FTN(dgelsy)(
    &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);

  for (UInt i = 0; i < ncoef; i++) coef[i] = rhs[i];

#endif
}

template void PolyFit1D(UInt, const double*, const double*, const std::vector<Legendre<double>*> &, double *);
template void PolyFit1D(UInt, const double*, const double*, const std::vector<ILegendre<double>*> &, double *);
template void PolyFit1D(UInt, const double*, const double*, const std::vector<Monomial<double>*> &, double *);

} // namespace
} // namespace
