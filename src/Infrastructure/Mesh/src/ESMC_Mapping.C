// $Id: ESMC_Mapping.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Mapping.h>
#include <ESMC_ShapeFunc.h>
#include <iostream>
#include <limits>

#include <sacado/Sacado.hpp>

#include <ESMC_Exception.h>

namespace ESMCI {
namespace MESH {

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
const std::string POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::name("POLY_MAPPING:s=d:" + SFUNC_TYPE::name);

template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::classInstance = NULL;
template<class SFUNC_TYPE,typename MPTRAITS>
const std::string POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::name("POLY_MAPPING:3,1:" + SFUNC_TYPE::name);

template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::classInstance = NULL;
template<class SFUNC_TYPE,typename MPTRAITS>
const std::string POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::name("POLY_MAPPING:3,2:" + SFUNC_TYPE::name);


template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>();

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>();

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>();

  return classInstance;
}

template<class SFUNC_TYPE,typename MPTRAITS>
bool POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::is_in_cell(const double *mdata,
                             const double *point,
                             double *pcoord,
                             double *dist) const
{
  Throw() << "is_in_cell not implemented for 3,1";
}

template<class SFUNC_TYPE,typename MPTRAITS>
bool POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::is_in_cell(const double *mdata,
                             const double *point,
                             double *pcoord,
                             double *dist) const
{
//std::cout << "in 3 2 is_in_cell" << std::endl;
  // Newton's method
  const double ctol = 1e-11;
  const int max_iter = 20;

  fad_type s[sdim];
  fad_type normal[sdim];
  double delta_s[sdim];
  fad_type res[sdim];
  double jac[sdim][sdim];
  double jac_inv[sdim][sdim];
  double dnorm = 0.0, rnorm = 0.0;
  int niters = 0;

  for (unsigned int i = 0; i < sdim; i++) {
    s[i] = 0.0;
    s[i].diff(i, sdim);
    delta_s[i] = 0.0;
    res[i] = 0.0;
    normal[i] = 0;
  }

  POLY_Mapping<SFUNC_TYPE,MPTraits<double,fad_type>,sdim,pdim> *mp_psens =
    trade<MPTraits<double,fad_type> >();
  bool converged = false;
  do {
    // Calculate residual.  Also use loop to start jacobian
    mp_psens->forward(1, mdata, s, res); // F($)

    // Now compute the normals and add n(eta,xi)*d
    mp_psens->normal(1,mdata,s,normal);
//    std::cout << "normal:(" << normal[0] << ", " << normal[1] << ", " <<  normal[2] <<")" << std::endl;

    for (UInt i = 0; i < sdim; i++)
      res[i] += normal[i]*s[sdim-1];
    
    rnorm = 0.0;
    for (unsigned int i = 0; i < sdim; i++) {
      res[i] = res[i]-point[i];  // F(x) - x = R(x)

      rnorm += res[i].val()*res[i].val();

      // Form jacobian at the same time
      double *row = &(res[i].fastAccessDx(0));
      for (UInt j = 0; j < sdim; j++) {
        jac[i][j] = row[j];
      }
    } // for sdim
/*
std::cout << "res:(" << res[0].val() << ", " << res[1].val() << ", " <<  res[2].val() <<")" << std::endl;
std::cout << "jacobian:" << std::endl;;
for (unsigned int i = 0; i < sdim; i++) {
for (unsigned int j = 0; j < sdim; j++) {
  std::cout << jac[i][j] << ", ";
}
std::cout << std::endl;
}
*/

/* Rnorm can be tiny when dnorm is huge.  Dnorm seems to be more reliable.
    if (rnorm < ctol) {
      converged = true;
      break;
    }
*/

    // So now res holds -F(x) and jac has jacobian.  We must invert the jacobian
    POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], &jac_inv[0][0]);
/*
std::cout << "jacobian inv:" << std::endl;
for (unsigned int i = 0; i < sdim; i++) {
for (unsigned int j = 0; j < sdim; j++) {
  std::cout << jac_inv[i][j] << ", ";
}
std::cout << std::endl;
}
*/

    // delta_s = jac_inv*res
    dnorm = 0;
    for (unsigned int i = 0; i < sdim; i++) {
      delta_s[i] = 0.0;
      for (unsigned int j = 0; j < sdim; j++) {
        delta_s[i] += jac_inv[i][j]*(-res[j].val());
// TODO Use fad derivs

      }

      // snew = sold+delta
      s[i] = s[i] + delta_s[i];
      dnorm += delta_s[i]*delta_s[i];
    } // i

    if (dnorm <= ctol) converged = true;
    niters++;

    if (niters >= max_iter) break; // stop loop uncoverged.
  } while(!converged);
//std::cout << "Newton iters:" << niters << ", d=" << s[sdim-1].val() << std::endl;

  // Throw out last coord
  for (unsigned int i = 0; i < pdim; i++) 
    pcoord[i] = s[i].val();

  if (!converged) {
//    std::cout << "Not converged, dnorm=" << dnorm << ", rnorm=" << rnorm<< std::endl;
    if (dist) *dist = std::numeric_limits<double>::max();
    return false;
  } //else std::cout << "Converged, dnorm=" << dnorm << " rnorm=" << rnorm<< std::endl;

  if (dist) *dist = std::sqrt(
       s[sdim-1].val()*(
       normal[0].val()*normal[0].val()
       +normal[1].val()*normal[1].val()
       +normal[2].val()*normal[2].val()));

  // check parametric bounds.
  double sdist(0);
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  if(dist) *dist += sdist;
  return resu;
}

template<class SFUNC_TYPE,typename MPTRAITS,int SPATIAL_DIM, int PARAMETRIC_DIM>
bool POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::is_in_cell(const double *mdata,
                             const double *point,
                             double *pcoord,
                             double *dist) const
{
  // Newton's method
  const double ctol = 1e-10;
  const int max_iter = 15;

  double s[pdim];
  double delta_s[pdim];
  double res[pdim];
  double jac[sdim][sdim];
  double jac_inv[sdim][sdim];
  double sgrads[SFUNC_TYPE::ndofs][sdim];
  double dnorm = 0.0, rnorm = 0.0;
  int niters = 0;

  for (unsigned int i = 0; i < pdim; i++) {
    s[i] = delta_s[i] = res[i] = 0.0;
  }

  POLY_Mapping<SFUNC_TYPE,MPTraits<>,SPATIAL_DIM,PARAMETRIC_DIM> *mpstd =
    trade<MPTraits<> >();
  bool converged = false;
  do {
    SFUNC_TYPE::shape_grads(1, s, &sgrads[0][0]);
    // Calculate residual.  Also use loop to start jacobian
    mpstd->forward(1, mdata, s, res); // F($)
    rnorm = 0.0;
    for (unsigned int i = 0; i < sdim; i++) {
      res[i] = point[i] - res[i];  // x - F(x) = -R(x)
      rnorm += res[i]*res[i];
      // Load forward jacobian
      for (unsigned int j = 0; j < sdim; j++) {
        jac[i][j] = 0.0;
        for (unsigned int k = 0; k < SFUNC_TYPE::ndofs; k++) {
          jac[i][j] += sgrads[k][j]*mdata[k*sdim+i];
        }
      }
    } // for sdim

    /*  rnorm is small even when very bad.
    if (rnorm < ctol) {
      converged = true;
      break;
    }
    */

    // So now res holds -F(x) and jac has jacobian.  We must invert the jacobian
    POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], &jac_inv[0][0]);

    // delta_s = jac_inv*res
    dnorm = 0;
    for (unsigned int i = 0; i < sdim; i++) {
      delta_s[i] = 0.0;
      for (unsigned int j = 0; j < sdim; j++) {
        delta_s[i] += jac_inv[i][j]*res[j];
      }

      // snew = sold+delta
      s[i] = s[i] + delta_s[i];
      dnorm += delta_s[i]*delta_s[i];
    } // i

    if (dnorm <= ctol) converged = true;
    niters++;

    if (niters >= max_iter) break; // stop loop uncoverged.
  } while(!converged);

  for (unsigned int i = 0; i < sdim; i++) 
    pcoord[i] = s[i];

  if (dist) *dist = 0.0;

  if (!converged) {
    if (dist) *dist = std::numeric_limits<double>::max();
    return false;
  }

  // check parametric bounds.
  double sdist(0);
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  if(dist) *dist += sdist;
  return resu;
}

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::forward(const unsigned int npts,
                          const mdata_type mdata[],
                          const pcoord_type points[],
                          typename richest_type<mdata_type,pcoord_type>::value  results[]) const
{
  // We get the shape function values at points
  std::vector<pcoord_type> svals(npts*SFUNC_TYPE::ndofs);

  SFUNC_TYPE::shape(npts, points, &svals[0]);

/*
std::cout << "shape:";
for (unsigned int i = 0; i < npts; i++) {
for (unsigned int j = 0; j < SFUNC_TYPE::ndofs; j++) {
  std::cout << svals[i][j] << ", ";
}
std::cout << std::endl;
}
*/

  for (unsigned int j = 0; j < npts; j++) {
    for (unsigned int i = 0; i < sdim; i++) {
    results[j*sdim+i] = 0.0;
      for (unsigned int ncf = 0; ncf < SFUNC_TYPE::ndofs; ncf++) {
        results[j*sdim+i] += mdata[ncf*sdim+i]*svals[j*SFUNC_TYPE::ndofs+ncf];
      } // ncf
    } // sdim
  } // npts
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::forward(const unsigned int npts,
                          const mdata_type mdata[],
                          const pcoord_type points[],
                          typename richest_type<mdata_type,pcoord_type>::value results[]) const
{
  // We get the shape function values at points
  std::vector<pcoord_type> svals(npts*SFUNC_TYPE::ndofs);

  SFUNC_TYPE::shape(npts, points, &svals[0]);

  for (unsigned int j = 0; j < npts; j++) {
    for (unsigned int i = 0; i < sdim; i++) {
    results[j*sdim+i] = 0.0;
      for (unsigned int ncf = 0; ncf < SFUNC_TYPE::ndofs; ncf++) {
        results[j*sdim+i] += mdata[ncf*sdim+i]*svals[j*SFUNC_TYPE::ndofs+ncf];
      } // ncf
    } // sdim
  } // npts
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::forward(const unsigned int npts,
                          const mdata_type mdata[],
                          const pcoord_type points[],
                          typename richest_type<mdata_type,pcoord_type>::value results[]) const
{
  // We get the shape function values at points
  std::vector<pcoord_type> svals(npts*SFUNC_TYPE::ndofs);

  SFUNC_TYPE::shape(npts, points, &svals[0]);

/*
std::cout << "shape:";
for (unsigned int i = 0; i < npts; i++) {
for (unsigned int j = 0; j < SFUNC_TYPE::ndofs; j++) {
  std::cout << svals[i][j] << ", ";
}
std::cout << std::endl;
}
*/

  for (unsigned int j = 0; j < npts; j++) {
    for (unsigned int i = 0; i < sdim; i++) {
    results[j*sdim+i] = 0.0;
      for (unsigned int ncf = 0; ncf < SFUNC_TYPE::ndofs; ncf++) {
        results[j*sdim+i] += mdata[ncf*sdim+i]*svals[j*SFUNC_TYPE::ndofs+ncf];
      } // ncf
    } // sdim
  } // npts
}

template<class SFUNC_TYPE, typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::Jx(
  UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
  typename richest_type<mdata_type,pcoord_type>::value result[]) const
{
  typename richest_type<mdata_type,pcoord_type>::value jac[sdim][sdim];
  pcoord_type sgrads[SFUNC_TYPE::ndofs][sdim];
  for (UInt n = 0; n < npts; n++) {
    SFUNC_TYPE::shape_grads(1, &pcoord[n*pdim], &sgrads[0][0]);
    for (unsigned int i = 0; i < sdim; i++) {
      for (unsigned int j = 0; j < sdim; j++) {
        jac[i][j] = 0.0;
        for (unsigned int k = 0; k < SFUNC_TYPE::ndofs; k++) {
          jac[i][j] += sgrads[k][j]*mdata[k*sdim+i];
        }
      }
    } // for sdim

    result[n] = POLY_Mapping_determinant<sdim>(&jac[0][0]);
  }

}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::Jx(
  UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
   typename richest_type<mdata_type,pcoord_type>::value result[]) const
{

  // Acually, the answer is simply the norm of the normal
  
  std::vector<typename richest_type<mdata_type,pcoord_type>::value> nn(npts*sdim);
  normal(npts, mdata, pcoord, &nn[0]);
  for (UInt i = 0; i < npts; i++) {
    typename richest_type<mdata_type,pcoord_type>::value *n = &nn[i*sdim];
    result[i] = std::sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2]);
  }
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::Jx(
  UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
   typename richest_type<mdata_type,pcoord_type>::value result[]) const
{

  // Acually, the answer is simply the norm of the tanget
  pcoord_type sgrads[SFUNC_TYPE::ndofs][pdim];

  for (UInt n = 0; n < npts; n++) {
    SFUNC_TYPE::shape_grads(1,&pcoord[n*pdim], &sgrads[0][0]);

    typename richest_type<mdata_type,pcoord_type>::value Fxi[sdim];
  
    for (UInt i = 0; i < sdim; i++) {
      Fxi[i] = 0.0;
      for (UInt n = 0; n < SFUNC_TYPE::ndofs; n++) {
        Fxi[i] += mdata[n*sdim+i]*sgrads[n][0];
      }
    }
    result[n] = std::sqrt(Fxi[0]*Fxi[0]+Fxi[1]*Fxi[1]+Fxi[2]*Fxi[2]);
  }
}

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {
  typename richest_type<mdata_type,pcoord_type>::value jac[sdim][sdim];
  std::vector<pcoord_type> sgrads(SFUNC_TYPE::ndofs*sdim);

  SFUNC_TYPE::shape_grads(1, pcoord, &sgrads[0]);
  for (unsigned int i = 0; i < sdim; i++) {
    for (unsigned int j = 0; j < sdim; j++) {
      jac[i][j] = 0.0;
      for (unsigned int k = 0; k < SFUNC_TYPE::ndofs; k++) {
        jac[i][j] += sgrads[k*sdim+j]*mdata[k*sdim+i];
      }
    }
  } // for sdim

  POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], result);

}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {
  Throw() << "No jacobian invert for 3,1";
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {
  typename richest_type<mdata_type,pcoord_type>::value jac[sdim][sdim];
  std::vector<pcoord_type> sgrads(SFUNC_TYPE::ndofs*pdim);

  SFUNC_TYPE::shape_grads(1, pcoord, &sgrads[0]);
  for (unsigned int i = 0; i < sdim; i++) {
    for (unsigned int j = 0; j < pdim; j++) {
      jac[i][j] = 0.0;
      for (unsigned int k = 0; k < SFUNC_TYPE::ndofs; k++) {
        jac[i][j] += sgrads[k*pdim+j]*mdata[k*sdim+i];
      }
    }
  } // for sdim

  // Now put the normal in the far right column
  typename richest_type<mdata_type,pcoord_type>::value n[sdim];
  normal(1, mdata, pcoord, &n[0]);

  for (UInt i = 0; i < sdim; i++) jac[i][2] = n[i];

  POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], result);
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::normal(UInt npts, const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {

  Throw() << "No normal for 3,1";

}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::normal(UInt npts, const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {

  std::vector<pcoord_type> sgrads(npts*SFUNC_TYPE::ndofs*pdim);
  // First, calculate \partial_eta F, \partial_xi F

  SFUNC_TYPE::shape_grads(npts,pcoord, &sgrads[0]);

  for (UInt p = 0; p < npts; p++) {
    typename richest_type<mdata_type,pcoord_type>::value Feta[sdim];
    typename richest_type<mdata_type,pcoord_type>::value Fxi[sdim];
  
    for (UInt i = 0; i < sdim; i++) {
      Feta[i] = 0.0; Fxi[i] = 0.0;
      for (UInt n = 0; n < SFUNC_TYPE::ndofs; n++) {
        Feta[i] += mdata[n*sdim+i]*sgrads[(p*SFUNC_TYPE::ndofs+n)*pdim+0];
        Fxi[i] += mdata[n*sdim+i]*sgrads[(p*SFUNC_TYPE::ndofs+n)*pdim+1];
      }
    }
  
  /*
std::cout << "Feta:" << Feta[0] << ", " << Feta[1] << "," << Feta[2] << std::endl;
std::cout << "Fxi:" << Fxi[0] << ", " << Fxi[1] << "," << Fxi[2] << std::endl;
*/

  // Cross product
    result[p*sdim +0] = Feta[1]*Fxi[2] -Feta[2]*Fxi[1];
    result[p*sdim+1] = Feta[2]*Fxi[0] - Feta[0]*Fxi[2];
    result[p*sdim+2] = Feta[0]*Fxi[1] - Feta[1]*Fxi[0];
  }

/*
ScalarT dot= result[0]*Feta[0]+result[1]*Feta[1]+result[2]*Feta[2];
std::cout << "dot=" << dot << std::endl;
*/
}

// Invert 1d jacobian.
template<>
void POLY_Mapping_jacobian_invert<1>(const double jac[], double jac_inv[]) {

  jac_inv[0] = 1.0/jac[0];
}
template<>
void POLY_Mapping_jacobian_invert<1>(const fad_type jac[], fad_type jac_inv[]) {

  jac_inv[0] = 1.0/jac[0];
}

template<>
double POLY_Mapping_determinant<1>(const double jac[])
{
  return jac[0];
}
template<>
fad_type POLY_Mapping_determinant<1>(const fad_type jac[])
{
  return jac[0];
}
// Invert a 2d jacobian.
template<>
void POLY_Mapping_jacobian_invert<2>(const double jac[], double jac_inv[]) {
  double deti = 1.0/(jac[0]*jac[3] - jac[1]*jac[2]);

  jac_inv[0] = deti*jac[3];
  jac_inv[1] = -deti*jac[1];
  jac_inv[2] = -deti*jac[2];
  jac_inv[3] = deti*jac[0];
}
template<>
void POLY_Mapping_jacobian_invert<2>(const fad_type jac[], fad_type jac_inv[]) {
  fad_type deti = 1.0/(jac[0]*jac[3] - jac[1]*jac[2]);

  jac_inv[0] = deti*jac[3];
  jac_inv[1] = -deti*jac[1];
  jac_inv[2] = -deti*jac[2];
  jac_inv[3] = deti*jac[0];
}
template<>
double POLY_Mapping_determinant<2>(const double jac[])
{
  return (jac[0]*jac[3] - jac[1]*jac[2]);
}
template<>
fad_type POLY_Mapping_determinant<2>(const fad_type jac[])
{
  return (jac[0]*jac[3] - jac[1]*jac[2]);
}

template<>
void POLY_Mapping_jacobian_invert<3>(const double jac[], double jac_inv[]) {
  const double deti = 1.0 / ( +jac[0] * (jac[4]*jac[8] - jac[5]*jac[7])
                                -jac[1] * (jac[3]*jac[8] - jac[5]*jac[6])
                                +jac[2] * (jac[3]*jac[7] - jac[4]*jac[6]) );

  jac_inv[0] = (jac[4]*jac[8] - jac[5]*jac[7]) * deti;
  jac_inv[1] = (jac[2]*jac[7] - jac[1]*jac[8]) * deti;
  jac_inv[2] = (jac[1]*jac[5] - jac[2]*jac[4]) * deti;

  jac_inv[3] = (jac[5]*jac[6] - jac[3]*jac[8]) * deti;
  jac_inv[4] = (jac[0]*jac[8] - jac[2]*jac[6]) * deti;
  jac_inv[5] = (jac[2]*jac[3] - jac[0]*jac[5]) * deti;

  jac_inv[6] = (jac[3]*jac[7] - jac[4]*jac[6]) * deti;
  jac_inv[7] = (jac[1]*jac[6] - jac[0]*jac[7]) * deti;
  jac_inv[8] = (jac[0]*jac[4] - jac[1]*jac[3]) * deti;
 
}
template<>
void POLY_Mapping_jacobian_invert<3>(const fad_type jac[], fad_type jac_inv[]) {
  const fad_type deti = 1.0 / ( +jac[0] * (jac[4]*jac[8] - jac[5]*jac[7])
                                -jac[1] * (jac[3]*jac[8] - jac[5]*jac[6])
                                +jac[2] * (jac[3]*jac[7] - jac[4]*jac[6]) );

  jac_inv[0] = (jac[4]*jac[8] - jac[5]*jac[7]) * deti;
  jac_inv[1] = (jac[2]*jac[7] - jac[1]*jac[8]) * deti;
  jac_inv[2] = (jac[1]*jac[5] - jac[2]*jac[4]) * deti;

  jac_inv[3] = (jac[5]*jac[6] - jac[3]*jac[8]) * deti;
  jac_inv[4] = (jac[0]*jac[8] - jac[2]*jac[6]) * deti;
  jac_inv[5] = (jac[2]*jac[3] - jac[0]*jac[5]) * deti;

  jac_inv[6] = (jac[3]*jac[7] - jac[4]*jac[6]) * deti;
  jac_inv[7] = (jac[1]*jac[6] - jac[0]*jac[7]) * deti;
  jac_inv[8] = (jac[0]*jac[4] - jac[1]*jac[3]) * deti;
 
}

template<>
double POLY_Mapping_determinant<3>(const double jac[]) {
  return (jac[0] * (jac[4]*jac[8] - jac[5]*jac[7])
                                -jac[1] * (jac[3]*jac[8] - jac[5]*jac[6])
                                +jac[2] * (jac[3]*jac[7] - jac[4]*jac[6]) );
}
template<>
fad_type POLY_Mapping_determinant<3>(const fad_type jac[]) {
  return (jac[0] * (jac[4]*jac[8] - jac[5]*jac[7])
                                -jac[1] * (jac[3]*jac[8] - jac[5]*jac[6])
                                +jac[2] * (jac[3]*jac[7] - jac[4]*jac[6]) );
}


// Also instantiates
MappingBase *Topo2Map::operator()(const std::string &toponame) {
  const std::string &name = toponame;

  if (name == "BAR" || name == "BAR2") {
    return POLY_Mapping<bar_shape_func,MPTraits<> >::instance();
  }
  else if (name == "BAR2_3D") {
    return POLY_Mapping<bar_shape_func,MPTraits<>,3,1>::instance();
  } else if (name == "BAR3_3D") {
    return POLY_Mapping<bar3_shape_func,MPTraits<>,3,1>::instance();
  } else if (name == "BAR2_2D") {
    return POLY_Mapping<bar_shape_func,MPTraits<>,3,2>::instance();
  } else if (name == "QUAD" || name == "QUAD4") {
    return POLY_Mapping<quad_shape_func,MPTraits<> >::instance();
  } else if (name == "QUAD_3D") {
    return POLY_Mapping<quad_shape_func,MPTraits<>,3,2>::instance();
  } else if (name == "TRI" || name == "TRI3") {
    return POLY_Mapping<tri_shape_func,MPTraits<> >::instance();
  } else if (name == "TRI3_3D" || name == "TRI_3D") {
    return POLY_Mapping<tri_shape_func,MPTraits<>,3,2>::instance();
  } else if (name == "QUAD9") {
    return POLY_Mapping<quad9_shape_func,MPTraits<> >::instance();
  }
  else if (name == "SHELL" || name == "SHELL4") {
    return POLY_Mapping<quad_shape_func,MPTraits<> ,3,2>::instance();
  }
  else if (name == "SHELL3") {
    return POLY_Mapping<tri_shape_func,MPTraits<> ,3,2>::instance();
  }
  else if (name == "SHELL9") {
    return POLY_Mapping<quad9_shape_func,MPTraits<> ,3,2>::instance();
  }
  else if (name == "HEX" || name == "HEX8") {
    return POLY_Mapping<hex_shape_func,MPTraits<> >::instance();
  }
  else if (name == "TETRA" || name == "TETRA4") {
    return POLY_Mapping<tet_shape_func,MPTraits<> >::instance();
  }
  else {
    std::string err = "Cant find mapping for topo:" + name + "!!!";
    throw(err.c_str());
  }
}



// BAR
template class POLY_Mapping<bar_shape_func,MPTraits<> >;
template class POLY_Mapping<bar_shape_func,MPTraits<>, 3,1>;
template class POLY_Mapping<bar_shape_func,MPTraits<>, 2,1>;
template class POLY_Mapping<bar_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<bar_shape_func,MPTraits<fad_type,double>, 3,1>;
template class POLY_Mapping<bar_shape_func,MPTraits<fad_type,double>, 2,1>;
template class POLY_Mapping<bar_shape_func,MPTraits<double,fad_type> >;
template class POLY_Mapping<bar_shape_func,MPTraits<double,fad_type>, 3,1>;
template class POLY_Mapping<bar_shape_func,MPTraits<double,fad_type>, 2,1>;



// BAR3
template class POLY_Mapping<bar3_shape_func,MPTraits<> >;
template class POLY_Mapping<bar3_shape_func,MPTraits<>, 3,1>;
template class POLY_Mapping<bar3_shape_func,MPTraits<>, 2,1>;
template class POLY_Mapping<bar3_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<bar3_shape_func,MPTraits<fad_type,double>, 3,1>;
template class POLY_Mapping<bar3_shape_func,MPTraits<fad_type,double>, 2,1>;
template class POLY_Mapping<bar3_shape_func,MPTraits<double,fad_type> >;
template class POLY_Mapping<bar3_shape_func,MPTraits<double,fad_type>, 3,1>;
template class POLY_Mapping<bar3_shape_func,MPTraits<double,fad_type>, 2,1>;

// TRI
template class POLY_Mapping<tri_shape_func,MPTraits<> >;
template class POLY_Mapping<tri_shape_func,MPTraits<>, 3,2>;
template class POLY_Mapping<tri_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<tri_shape_func,MPTraits<fad_type,double>, 3,2>;
template class POLY_Mapping<tri_shape_func,MPTraits<double,fad_type> >;
template class POLY_Mapping<tri_shape_func,MPTraits<double,fad_type>, 3,2>;
// QUAD
template class POLY_Mapping<quad_shape_func,MPTraits<> >;
template class POLY_Mapping<quad_shape_func,MPTraits<>, 3,2>;
template class POLY_Mapping<quad_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<quad_shape_func,MPTraits<fad_type,double>, 3,2>;
template class POLY_Mapping<quad_shape_func,MPTraits<double,fad_type> >;
template class POLY_Mapping<quad_shape_func,MPTraits<double,fad_type>, 3,2>;

// QUAD9
template class POLY_Mapping<quad9_shape_func,MPTraits<> >;
template class POLY_Mapping<quad9_shape_func,MPTraits<>, 3,2>;
template class POLY_Mapping<quad9_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<quad9_shape_func,MPTraits<fad_type,double>, 3,2>;
template class POLY_Mapping<quad9_shape_func,MPTraits<double,fad_type> >;
template class POLY_Mapping<quad9_shape_func,MPTraits<double,fad_type>, 3,2>;

// HEX
template class POLY_Mapping<hex_shape_func,MPTraits<> >;
template class POLY_Mapping<hex_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<hex_shape_func,MPTraits<double,fad_type> >;

// TET
template class POLY_Mapping<tet_shape_func,MPTraits<> >;
template class POLY_Mapping<tet_shape_func,MPTraits<fad_type,double> >;
template class POLY_Mapping<tet_shape_func,MPTraits<double,fad_type> >;


} //namespace
} //namespace
