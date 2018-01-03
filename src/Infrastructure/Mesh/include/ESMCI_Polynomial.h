// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Polynomial_h
#define ESMCI_Polynomial_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Exception.h>

#include <vector>

namespace ESMCI {

/**
 * @defgroup polynomials
 * A suite of polynomial function evaluators, representros, and generators.
 * Templated for generic programming. 
 */

/**
 * A polynomial evaluator, using horner's method.  The poly is of
 * generic type, but must provide 
 * p.Degree() and p.GetCoefficients()
 * The coefficients are double, so, for instance, to represent
 * 1 + x + 3x*x*x, we would have degree == 3, and coefficients
 * would return an array {1, 1, 0, 3}.
 */
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

/**
 * Evaluate a general polynomial derivative.  Polynomial supports
 * p.Degree() and p.GetCoefficients()
 */
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

/** 
 * A tensor polynomial evaluator.  The TPOLY must return a reference to 
 * a POLY of type used above, i.e. 
 * TPOLY overloads operator[i] to return a reference for the ith 1d poly
 */
template<typename TPOLY>
inline typename TPOLY::real_type EvalTensorPoly(UInt dim, const typename TPOLY::real_type x[], const TPOLY &poly) {
  typename TPOLY::real_type res = 1.;
  for (UInt i = 0; i < dim; i++) {
    res *= EvalPoly<typename TPOLY::poly_type>()(poly[i], x[i]);
  }
 return res;
}
/**
 * Evaluate the gradient of a tensor polynomial.  Assumes that each tensor
 * entry is a different variable and returns the gradient under this assumption.
 * @param res of size tensor dim, each gradient 
 */
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

/**
 * Generatre the Legendre coefficients on [-1,1]
 */
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

/**
 * Gerneates the Integrated Legendre coefficients on [-1,1]
 */
void set_Ilegendre_coef(UInt k, std::vector<double> &coef);


/**
 * An object supporting the POLY interface, and constructed from the
 * legendre polynomials of a desired order.
 */
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

/**
 * Coefficient of the integrated legendre kernels, see below
 */
void generate_ILkernel(std::vector<double> &coef);

/**
 * The integrated legendre kernel functions, used in triangular and
 * tetarahedral hierarchical shape functions.  Class supports the generic interface
 * of a POLY
 */
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

/**
 * A monomial polynomial class (optimized muliplies)
 */
template<typename Real=double>
class Monomial {
public:
typedef Real real_type;
Monomial(UInt _deg) : deg(_deg) {}
UInt GetDegree() const {return deg;}
private:
UInt deg;
};

/** 
 * A specialization of EvalPoly that works quickly for monomials
 */
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

/**
 * Very fast monomial derivative evaluator
 */
template<typename Real>
struct EvalPolyDeriv<Monomial<Real>, Real> {
Real operator()(const Monomial<Real> &p, Real x) {
  Real res = p.GetDegree();
  int ord = p.GetDegree()-1;
  while (ord > 0) {
    res *= x; ord--;
  }
  return res;
}
};

/**
 * A class that allows one to build a tensor polynomial easily from a sequence of
 * 1d polynomials
 */
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

/**
 * A function to return the coefficients that best fit the given set of polynomials
 * through the given values (at the given locs)
 */
template<typename POLY>
void PolyFit1D(UInt nsamples, const double coord[], const double vals[], const std::vector<POLY*> &poly, double coef[]);

} // namespace

#endif
