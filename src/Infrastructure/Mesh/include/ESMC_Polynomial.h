// $Id: ESMC_Polynomial.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Polynomial_h
#define ESMC_Polynomial_h

#include <ESMC_MeshTypes.h>
#include <ESMC_Exception.h>

#include <vector>

namespace ESMCI {
namespace MESH {

template<typename POLY, typename Real=typename POLY::real_type>
struct EvalPoly {
Real operator()(const POLY &p, Real x) {
  // Horners
  UInt m = p.Degree();
  const double *c = p.GetCoefficients();
  Real res = c[m];
  for (int k = m-1; k >= 0; --k) {
    res = x*res + c[k]; 
  }
  return res;
}
};


template<typename POLY, typename Real=typename POLY::real_type>
struct EvalPolyDeriv {
Real operator()(const POLY &p, Real x) {
  // Horners
  UInt m = p.Degree();
  const double *c = p.GetCoefficients();
  Real res = c[m]*m;
  for (int k = m-1; k >= 1; --k) {
    res = x*res + c[k]*k; 
  }
  return res;
}
};

// TPOLY overloads operator[i] to return a reference for the ith 1d poly
template<typename TPOLY>
inline typename TPOLY::real_type EvalTensorPoly(UInt dim, const typename TPOLY::real_type x[], const TPOLY &poly) {
  typename TPOLY::real_type res = 1.;
  for (UInt i = 0; i < dim; i++) {
    res *= EvalPoly<typename TPOLY::poly_type>()(poly[i], x[i]);
  }
 return res;
}

template<typename TPOLY>
inline void EvalTensorPolyDeriv(UInt dim, const typename TPOLY::real_type x[], const TPOLY &poly, typename TPOLY::real_type res[]) {
  // First eval the polys and derivs at point
  std::vector<typename TPOLY::real_type> f(dim, 0);
  std::vector<typename TPOLY::real_type> df(dim, 0);
  for (UInt i = 0; i < dim; i++) {
    f[i] = EvalPoly<typename TPOLY::poly_type>()(poly[i], x[i]);
    df[i] = EvalPolyDeriv<typename TPOLY::poly_type>()(poly[i], x[i]);
  }
  for (UInt i = 0; i < dim; i++) {
    res[i] = df[i];
    for (UInt j = 0; j < dim; j++) {
      if (j == i) continue;
      res[i] *= f[j];
    }
  }
}

// Legendre coefficients on [-1,1]
void set_legendre_coef(UInt k, std::vector<double> &coef);

// A 1d polynomial class
template <typename Real=double>
class Legendre {
public:
typedef Real real_type;
Legendre(UInt _k) : k(_k) { set_legendre_coef(k, coef); }

const double *GetCoefficients() const { return &coef[0];}
double *GetCoefficients() { return &coef[0];}
UInt Degree() const { return k;}
private:
std::vector<double> coef;
UInt k;
};

// Integrated Legendre coefficients on [-1,1]
void set_Ilegendre_coef(UInt k, std::vector<double> &coef);
// Integrate legendre
template <typename Real=double>
class ILegendre {
public:
typedef Real real_type;
ILegendre(UInt _k) : k(_k) { set_Ilegendre_coef(k, coef); }
// or create, then init
ILegendre() : k(0), coef() {}
void Init(UInt _k) {k = _k; set_Ilegendre_coef(k, coef); }

const double *GetCoefficients() const { return &coef[0];}
double *GetCoefficients() { return &coef[0];}
UInt Degree() const { if (k <= 1) return 1; return k;} // l0,l1 first, lk=k
private:
UInt k;
std::vector<double> coef;
};

void generate_ILkernel(std::vector<double> &);
// IL Kernels phi_k = -l_k+2 / l0*l1
template <typename Real=double>
class ILKernel {
public:
typedef Real real_type;
ILKernel(UInt _k) : k(_k) { set_Ilegendre_coef(k+2, coef); generate_ILkernel(coef); }
// or create, then init
ILKernel() : k(0), coef() {}
void Init(UInt _k) {k = _k; set_Ilegendre_coef(k+2, coef); generate_ILkernel(coef); }

const double *GetCoefficients() const { return &coef[0];}
double *GetCoefficients() { return &coef[0];}
UInt Degree() const { return k;} // degree is k
private:
UInt k;
std::vector<double> coef;
};

template<typename Real=double>
class Monomial {
public:
typedef Real real_type;
Monomial(UInt _deg) : deg(_deg) {}
UInt GetDegree() const {return deg;}
private:
UInt deg;
};

// Faster specializations for monomials
template<typename Real>
struct EvalPoly<Monomial<Real>, Real> {
Real operator()(const Monomial<Real> &p, Real x) {
  Real res = 1.;
  int ord = p.GetDegree();
  while (ord > 0) {
    res *= x; ord--;
  }
  return res;
}
};

template<typename Real>
struct EvalPolyDeriv<Monomial<Real>, Real> {
Real operator()(const Monomial<Real> &p, Real x) {
  Real res = 1.;
  int ord = p.GetDegree();
  while (ord > 0) {
    res *= x; ord--;
  }
  return res;
}
};

// A tensor polynomial built from 1d polys (up to 3d)
template <typename POLY>
class TensorPolynomial {
public:
typedef POLY poly_type;
typedef typename POLY::real_type real_type;
TensorPolynomial(const POLY *p1, const POLY *p2, const POLY *p3 = NULL) {
  poly[0] = p1; poly[1] = p2; poly[2] = p3;
}
const POLY &operator[](UInt dim) const {
  ThrowAssert(dim < 3 && poly[dim] != NULL);
  return *poly[dim];
}
private:
  const POLY *poly[3];
};

// Return the coefficients that best fit the given set of polynomials through the given values (at the given locs)
template<typename POLY>
void PolyFit1D(UInt nsamples, const double coord[], const double vals[], const std::vector<POLY*> &poly, double coef[]);

} // namespace
} // namespace

#endif
