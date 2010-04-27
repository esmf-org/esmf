// $Id: ESMCI_Mapping.C,v 1.6 2010/04/27 21:41:53 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_ShapeFunc.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <iostream>
#include <limits>

#include <Mesh/include/sacado/Sacado.hpp>

#include <Mesh/include/ESMCI_Exception.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Mapping.C,v 1.6 2010/04/27 21:41:53 oehmke Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {


template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::classInstance = NULL;


template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>("mapdd" + std::string(SFUNC_TYPE::name));

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>("map32," + std::string(SFUNC_TYPE::name));

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>("map31," + std::string(SFUNC_TYPE::name));

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::instance() {
  if (classInstance == NULL)
    classInstance = new POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>("map21," + std::string(SFUNC_TYPE::name));

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
bool POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::is_in_cell(const double *mdata,
                             const double *point,
                             double *pcoord,
                             double *dist) const
{
  Throw() << "is_in_cell not implemented for 2,1";
}

#if 1
//// Intersect a line and a quad 
//// These should eventually be moved elsewhere (perhaps into ESMCI_ShapeFunc.C??) 
void mult(double m[], double v[], double out_v[]) {

  out_v[0]=m[0]*v[0]+m[1]*v[1]+m[2]*v[2];
  out_v[1]=m[3]*v[0]+m[4]*v[1]+m[5]*v[2];
  out_v[2]=m[6]*v[0]+m[7]*v[1]+m[8]*v[2];

}

// returns true if matrix is inverted, false otherwise
bool invert(double m[], double m_inv[]) {

  const double det =  m[0] * (m[4]*m[8] - m[5]*m[7])
                     -m[1] * (m[3]*m[8] - m[5]*m[6])
                     +m[2] * (m[3]*m[7] - m[4]*m[6]);
  
  // If det == 0.0 we can't invert
  if (det == 0.0) return false;

  const double deti = 1.0/det;

  m_inv[0] = (m[4]*m[8] - m[5]*m[7]) * deti;
  m_inv[1] = (m[2]*m[7] - m[1]*m[8]) * deti;
  m_inv[2] = (m[1]*m[5] - m[2]*m[4]) * deti;

  m_inv[3] = (m[5]*m[6] - m[3]*m[8]) * deti;
  m_inv[4] = (m[0]*m[8] - m[2]*m[6]) * deti;
  m_inv[5] = (m[2]*m[3] - m[0]*m[5]) * deti;

  m_inv[6] = (m[3]*m[7] - m[4]*m[6]) * deti;
  m_inv[7] = (m[1]*m[6] - m[0]*m[7]) * deti;
  m_inv[8] = (m[0]*m[4] - m[1]*m[3]) * deti;

  return true;
}


// Intersects between the quad q (entries in counterclockwise order)                                                    
// and the line determined by the endpoints l1 and l2                                                                   
// returns true if the two intersect and the output variables are valid                                                 
// outputs p containing the coordinates in the quad and t the coordinate in the line                                    
// of the intersection.                                                                                                 
// NOTE: the intersection doesn't have to be inside the quad or line for this to return true                            
bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
			      double *t) {

  double A[3], B[3], C[3], D[3], E[3], F[3];
  double J[3*3], inv_J[3*3];
  double X[3], delta_X[3];

  const double *q0=q;
  const double *q1=q+3;
  const double *q2=q+6;
  const double *q3=q+9;


  // Set some convient variables                                                                                        
  A[0]=q0[0]-q1[0]+q2[0]-q3[0];
  A[1]=q0[1]-q1[1]+q2[1]-q3[1];
  A[2]=q0[2]-q1[2]+q2[2]-q3[2];

  B[0]=q1[0]-q0[0];
  B[1]=q1[1]-q0[1];
  B[2]=q1[2]-q0[2];

  C[0]=q3[0]-q0[0];
  C[1]=q3[1]-q0[1];
  C[2]=q3[2]-q0[2];

  D[0]=l1[0]-l2[0];
  D[1]=l1[1]-l2[1];
  D[2]=l1[2]-l2[2];

  E[0]=q0[0]-l1[0];
  E[1]=q0[1]-l1[1];
  E[2]=q0[2]-l1[2];

  // Initialize answer                                                                                                  
  X[0]=0.0;
  X[1]=0.0;
  X[2]=0.0;

  // Do multiple iterations, exiting inside loop if solution is good enough
  for (int i=0; i<20; i++) {

    // Calculate Value of function at X                                                                                   
    F[0]=X[0]*X[1]*A[0]+X[0]*B[0]+X[1]*C[0]+X[2]*D[0]+E[0];
    F[1]=X[0]*X[1]*A[1]+X[0]*B[1]+X[1]*C[1]+X[2]*D[1]+E[1];
    F[2]=X[0]*X[1]*A[2]+X[0]*B[2]+X[1]*C[2]+X[2]*D[2]+E[2];

    // If we're close enough to 0.0 then exit                                                                             
    if (F[0]*F[0]+F[1]*F[1]+F[2]*F[2] < 1.0E-10) break;

    // Construct Jacobian                                                                                                 
    J[0]=A[0]*X[1]+B[0]; J[1]=A[0]*X[0]+C[0]; J[2]=D[0];
    J[3]=A[1]*X[1]+B[1]; J[4]=A[1]*X[0]+C[1]; J[5]=D[1];
    J[6]=A[2]*X[1]+B[2]; J[7]=A[2]*X[0]+C[2]; J[8]=D[2];

    // Invert Jacobian                                                                                                    
    if (!invert(J,inv_J)) return false;

    // Calculate change in X                                                                                              
    mult(inv_J, F, delta_X);

    // Move to next approximation of X                                                                                    
    X[0] = X[0] - delta_X[0];
    X[1] = X[1] - delta_X[1];
    X[2] = X[2] - delta_X[2];

  }

  // Get answer out                                                                                                     
  p[0]=X[0];
  p[1]=X[1];
  *t=X[2];

  return true;
}

//// Intersect a line and a tri 
//// These should eventually be moved elsewhere (perhaps into ESMCI_ShapeFunc.C??) 
// Intersects between the tri t (entries in counterclockwise order)                                              
// and the line determined by the endpoints l1 and l2                                                            
// returns true if the two intersect and the output variables are valid                                          
// outputs p containing the coordinates in the tri and t the coordinate in the line                              
// of the intersection.                                                                                          
// NOTE: the intersection doesn't have to be inside the tri or line for this to return true                      
bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
			     double *t) {

  double M[3*3], inv_M[3*3];
  double V[3];
  double X[3];

  const double *tri0=tri;
  const double *tri1=tri+3;
  const double *tri2=tri+6;

  // To do intersection just solve the set of linear equations for both                                          

  // Setup M                                                                                                     
  M[0]=l1[0]-l2[0]; M[1]=tri1[0]-tri0[0]; M[2]=tri2[0]-tri0[0];
  M[3]=l1[1]-l2[1]; M[4]=tri1[1]-tri0[1]; M[5]=tri2[1]-tri0[1];
  M[6]=l1[2]-l2[2]; M[7]=tri1[2]-tri0[2]; M[8]=tri2[2]-tri0[2];


  // Invert M                                                                                                     
  if (!invert(M,inv_M)) return false;

  // Set variable holding vector                                                                                 
  V[0]=l1[0]-tri0[0];
  V[1]=l1[1]-tri0[1];
  V[2]=l1[2]-tri0[2];

  // Calculate solution                                                                                         
  mult(inv_M, V, X);

  // Get answer out                                                                                              
  *t=X[0];
  p[0]=X[1];
  p[1]=X[2];

  return true;
}


template<class SFUNC_TYPE,typename MPTRAITS>
bool POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::is_in_cell(const double *mdata,
                             const double *point,
                             double *pcoord,
                             double *dist) const
{

  // Eventually need to reorganize mapping/shape_func system
  // so that the switch to different mapping types happens
  // automatically (perhaps via a curved set of shapes as suggested
  // by Ryan).

  // if this is a quad then solve it particular to that
  if (SFUNC_TYPE::ndofs==4) {
    double center[3]={0.0,0.0,0.0}; // center of sphere
    double p[2]; 
    double t;

    // Intersect quad with line from point to center of sphere
    if (!intersect_quad_with_line(mdata, point, center, p, &t)) {
      if (dist) *dist = std::numeric_limits<double>::max();
      pcoord[0]=0.0; pcoord[1]=0.0;
      return false;
    }

    // Transform quad parametric coords from [0,1] to [-1,1] for consistancy
    pcoord[0]=2*p[0]-1.0;
    pcoord[1]=2*p[1]-1.0;


    // Calculate distance
    if (dist) {
      double ipnt[3]; // intersection point

      // calculate intersection point
      ipnt[0]=t*(center[0]-point[0])+point[0];
      ipnt[1]=t*(center[1]-point[1])+point[1];
      ipnt[2]=t*(center[2]-point[2])+point[2];

      // calculate dist from point to intersection point
      *dist=std::sqrt((point[0]-ipnt[0])*(point[0]-ipnt[0]) +
		      (point[1]-ipnt[1])*(point[1]-ipnt[1]) +
		      (point[2]-ipnt[2])*(point[2]-ipnt[2]));
    }

    // do is in
    double sdist;
    bool in_quad = SFUNC_TYPE::is_in(pcoord, &sdist);

    return in_quad;
  }


  // if this is a tri then solve it particular to that
  if (SFUNC_TYPE::ndofs==3) {
    double center[3]={0.0,0.0,0.0}; // center of sphere
    double p[2]; 
    double t;

    // Intersect quad with line from point to center of sphere
    if (!intersect_tri_with_line(mdata, point, center, p, &t)) {
      if (dist) *dist = std::numeric_limits<double>::max();
      pcoord[0]=0.0; pcoord[1]=0.0;
      return false;
    }

    // Don't need to transform tri parametric coords because tri shape func seems to use [0,1] 

    // Calculate distance
    if (dist) {
      double ipnt[3]; // intersection point

      // calculate intersection point
      ipnt[0]=t*(center[0]-point[0])+point[0];
      ipnt[1]=t*(center[1]-point[1])+point[1];
      ipnt[2]=t*(center[2]-point[2])+point[2];

      // calculate dist from point to intersection point
      *dist=std::sqrt((point[0]-ipnt[0])*(point[0]-ipnt[0]) +
		      (point[1]-ipnt[1])*(point[1]-ipnt[1]) +
		      (point[2]-ipnt[2])*(point[2]-ipnt[2]));
    }

    // do is in
    double sdist;
    bool in_tri = SFUNC_TYPE::is_in(pcoord, &sdist);

    return in_tri;
  }


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
       std::abs(s[sdim-1].val())*(
       normal[0].val()*normal[0].val()
       +normal[1].val()*normal[1].val()
       +normal[2].val()*normal[2].val()));

  // check parametric bounds.
  double sdist(0);
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  //  if(dist) *dist += sdist; Dont' do this for now because is_in doesn't set this consistently
  return resu;
}
#endif

#if 0
// Original

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
       std::abs(s[sdim-1].val())*(
       normal[0].val()*normal[0].val()
       +normal[1].val()*normal[1].val()
       +normal[2].val()*normal[2].val()));

  // check parametric bounds.
  double sdist(0);
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  if(dist) *dist += sdist;
  return resu;
}
#endif

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

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::forward(const unsigned int npts,
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

    result[n] = std::abs(POLY_Mapping_determinant<sdim>(&jac[0][0]));
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

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::Jx(
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
    result[n] = std::sqrt(Fxi[0]*Fxi[0]+Fxi[1]*Fxi[1]);
//Par::Out() << "Jx="  << result[n] << std::endl;
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
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {
  Throw() << "No jacobian invert for 2,1";
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

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
const Mapping<MPTRAITS> *POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::side_mapping(UInt side_num) const
{
  Throw() << "side mapping not implemented";
}

template<>
const Mapping<MPTraits<> > *POLY_Mapping<quad_shape_func,MPTraits<>,2, 2>::side_mapping(UInt side_num) const
{
  return POLY_Mapping<bar_shape_func,MPTraits<>,2,1>::instance();
}

template<>
const Mapping<MPTraits<> > *POLY_Mapping<tri_shape_func,MPTraits<>,2, 2>::side_mapping(UInt side_num) const
{
  return POLY_Mapping<bar_shape_func,MPTraits<>,2,1>::instance();
}

template<>
const Mapping<MPTraits<> > *POLY_Mapping<hex_shape_func,MPTraits<>,3, 3>::side_mapping(UInt side_num) const
{
  return POLY_Mapping<quad_shape_func,MPTraits<>,3,2>::instance();
}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,1>::normal(UInt npts, const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {

  Throw() << "No normal for 3,1";

}

template<class SFUNC_TYPE,typename MPTRAITS>
void POLY_Mapping<SFUNC_TYPE,MPTRAITS,2,1>::normal(UInt npts, const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename richest_type<mdata_type,pcoord_type>::value result[]) const {

  Throw() << "No normal for 2,1";

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
    return POLY_Mapping<bar_shape_func,MPTraits<>,2,1>::instance();
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


} // namespace
