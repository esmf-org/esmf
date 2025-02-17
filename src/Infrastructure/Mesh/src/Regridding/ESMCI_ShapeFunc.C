// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_ShapeFunc.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <Mesh/include/sacado/Sacado_No_Kokkos.hpp>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

ShapeFunc::ShapeFunc() {
}

ShapeFunc::~ShapeFunc() {
}

// DG0

template<int PDIM>
const char *dg0_shape_func<PDIM>::name = "dg0";

template<int PDIM>
const int dg0_shape_func<PDIM>::dof_description[1][4] = {
{DOF_ELEM, 0, 0, 1}
};

template<int PDIM>
template<typename ScalarT>
void dg0_shape_func<PDIM>::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    results[j] = 1;
  }
}

template<int PDIM>
template<typename ScalarT>
void dg0_shape_func<PDIM>::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  Throw() << "No grads for dg0";
}

template<int PDIM>
bool dg0_shape_func<PDIM>::is_in(const double pcoord[], double *dist) {
  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false
  if (dist) *dist=0.0;

  return false;
}

template<int PDIM>
const double dg0_shape_func<PDIM>::ipoints[1] = 
{
0
};

// Bar

const char *bar_shape_func::name = "bar";

const int bar_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1}
};

template<typename ScalarT>
void bar_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j];
    results[ndofs*j] = 0.5*(1.0-xi);
    results[ndofs*j+1] = 0.5*(1.0+xi);
  }
}

template<typename ScalarT>
void bar_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {

    // dof 1
    results[ndofs*j*pdim] = -0.5;

    // dof 2
    results[ndofs*j*pdim+1] = 0.5;

  }  // for j
}


bool bar_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false
  if (dist) *dist=0.0;   // Init to 0.0

  if (pcoord[0] < -1.0-in_tol || pcoord[0] > 1.0+in_tol) return false;
  return true;
}

const double bar_shape_func::ipoints[ndofs*pdim] = {
 -1,
  1
};

// Bar3

const char *bar3_shape_func::name="bar3";

const int bar3_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1}
};

template<typename ScalarT>
void bar3_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j];
    results[ndofs*j] = -0.5*xi*(1.0-xi);
    results[ndofs*j+1] = 0.5*xi*(1.0+xi);
    results[ndofs*j+2] = (1.0-xi)*(1.0+xi);
  }
}

template<typename ScalarT>
void bar3_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j];

    // dof 1
    results[ndofs*j*pdim] = xi-0.5;

    // dof 2
    results[ndofs*j*pdim+1] = xi+0.5;

    // dof 3
    results[ndofs*j*pdim+1] = -2*xi;

  }  // for j
}

bool bar3_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false
  if (dist) *dist=0.0;   // Init to 0.0
  
  if (pcoord[0] < -1.0-in_tol || pcoord[0] > 1.0+in_tol) return false;
  return true;
}

const double bar3_shape_func::ipoints[ndofs*pdim] = {
-1,1,0
};

// TRI 
const char *tri_shape_func::name="tri";
const int tri_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1}
};


template<typename ScalarT>
void tri_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    results[ndofs*j] = 1 - xi - eta;
    results[ndofs*j+1] = xi;
    results[ndofs*j+2] = eta;
  }
}

template<typename ScalarT>
void tri_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {

    // dof 1
    results[ndofs*j*pdim] = -1;
    results[ndofs*j*pdim+1] = -1;

    // dof 2
    results[ndofs*j*pdim+2] = 1;
    results[ndofs*j*pdim+3] = 0;

    // dof 3
    results[ndofs*j*pdim+4] = 0;
    results[ndofs*j*pdim+5] = 1;

  }  // for j
}



bool tri_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // Init to is_in = true case
  bool in=true;
  if (dist) *dist=0.0; 

  // Figure out if it's actually not in
  if ((pcoord[0] <-in_tol) || (pcoord[1] <-in_tol)) {
    double out_dist[2]={0.0,0.0};

     if (pcoord[0] < -in_tol) {
        out_dist[0] = -pcoord[0];
        in=false;
     } else if (pcoord[0] > 1.0+in_tol) {
        out_dist[0] = pcoord[0]-1.0;
        // no setting of in, this is just to calculate dist
     }

     if (pcoord[1] < -in_tol) {
        out_dist[1] = -pcoord[1];
        in=false;
     } else if (pcoord[1] > 1.0+in_tol) {
        out_dist[1] = pcoord[1]-1.0;
        // no setting of in, this is just to calculate dist
     }

     if (dist) *dist=std::sqrt(out_dist[0]*out_dist[0]+out_dist[1]*out_dist[1]);

  } else if ((pcoord[0] + pcoord[1]) > 1.0+in_tol) {
    if (dist) *dist = std::abs((pcoord[0] + pcoord[1]) - 1.0);
    in=false;
  }

  return in;
}


const double tri_shape_func::ipoints[ndofs*pdim] = {
 0,  0,
 1,  0,
 0,  1
};

// QUAD
const char *quad_shape_func::name="quad";
const double quad_shape_func::one4th = 1.0/4.0;
const int quad_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1},
{DOF_NODE, 3, 0, 1}
/* bizare test case 
{DOF_NODE, 0, 0},
{DOF_NODE, 1, 0},
{DOF_NODE, 2, 0},
{DOF_NODE, 3, 0},
{DOF_EDGE, 0, 0},
{DOF_EDGE, 0, 1},
{DOF_EDGE, 1, 0},
{DOF_EDGE, 1, 1},
{DOF_EDGE, 2, 0},
{DOF_EDGE, 2, 1},
{DOF_EDGE, 3, 0},
{DOF_EDGE, 3, 1},
{DOF_FACE, 0, 0},
{DOF_FACE, 0, 1},
{DOF_FACE, 0, 2},
{DOF_FACE, 1, 0},
{DOF_FACE, 1, 1},
{DOF_FACE, 1, 2},
{DOF_FACE, 2, 0},
{DOF_FACE, 2, 1},
{DOF_FACE, 2, 2},
{DOF_FACE, 3, 0},
{DOF_FACE, 3, 1},
{DOF_ELEM, 0, 0}
*/
};


template<typename ScalarT>
void quad_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    results[ndofs*j] = one4th*(1.0-xi)*(1.0-eta);
    results[ndofs*j+1] = one4th*(1.0+xi)*(1.0-eta);
    results[ndofs*j+2] = one4th*(1.0+xi)*(1.0+eta);
    results[ndofs*j+3] = one4th*(1.0-xi)*(1.0+eta);
  }
}

template<typename ScalarT>
void quad_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];

    // dof 1
    results[ndofs*j*pdim] = -one4th*(1.0-eta);
    results[ndofs*j*pdim+1] = -one4th*(1.0-xi);

    // dof 2
    results[ndofs*j*pdim+2] = one4th*(1.0-eta);
    results[ndofs*j*pdim+3] = -one4th*(1.0+xi);

    // dof 3
    results[ndofs*j*pdim+4] = one4th*(1.0+eta);
    results[ndofs*j*pdim+5] = one4th*(1.0+xi);

    // dof 4
    results[ndofs*j*pdim+6] = -one4th*(1.0+eta);
    results[ndofs*j*pdim+7] = one4th*(1.0-xi);

  }  // for j
}

  
bool quad_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // Init to is_in = true case
  bool in=true;
  double max_out[2]={0.0,0.0};
  if (dist) *dist=0.0; 

  // Figure out if it's actually not in
  if (pcoord[0] < -1.0-in_tol) {
    max_out[0]=-1.0 - pcoord[0];
    in= false;
  } else if (pcoord[0] > 1.0+in_tol) {
    max_out[0]=pcoord[0] - 1.0;
    in= false;
  }

  if (pcoord[1] < -1.0-in_tol) {
    max_out[1]=-1.0 - pcoord[1];
    in= false;
  } else if (pcoord[1] > 1.0+in_tol) {
    max_out[1]=pcoord[1] - 1.0;
    in= false;
  }

  if (dist) *dist=std::sqrt(max_out[0]*max_out[0]+max_out[1]*max_out[1]);

  return in;
}





const double quad_shape_func::ipoints[ndofs*pdim] = {
 -1, -1,  
  1, -1,  
  1,  1,
 -1,  1
};

// Quad9
const char *quad9_shape_func::name="quad9";
const int quad9_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1},
{DOF_NODE, 3, 0, 1},
{DOF_NODE, 4, 0, 1},
{DOF_NODE, 5, 0, 1},
{DOF_NODE, 6, 0, 1},
{DOF_NODE, 7, 0, 1},
{DOF_NODE, 8, 0, 1}
/*
{DOF_EDGE, 0, 0},
{DOF_EDGE, 1, 0},
{DOF_EDGE, 2, 0},
{DOF_EDGE, 3, 0},
{DOF_ELEM, 0, 0}
*/
};


template<typename ScalarT>
void quad9_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    results[ndofs*j] = 0.25*xi*eta*(1.0-xi)*(1.0-eta);
    results[ndofs*j+1] = -0.25*xi*eta*(1.0+xi)*(1.0-eta);
    results[ndofs*j+2] = 0.25*xi*eta*(1.0+xi)*(1.0+eta);
    results[ndofs*j+3] = -0.25*xi*eta*(1.0-xi)*(1.0+eta);

    results[ndofs*j+4] = -0.5*eta*(1.0+xi)*(1.0-xi)*(1.0-eta);
    results[ndofs*j+5] = 0.5*xi*(1.0+eta)*(1.0-eta)*(1.0+xi);
    results[ndofs*j+6] = 0.5*eta*(1.0+xi)*(1.0-xi)*(1.0+eta);
    results[ndofs*j+7] = -0.5*xi*(1.0+eta)*(1.0-eta)*(1.0-xi);
    results[ndofs*j+8] = (1.0-xi*xi)*(1.0-eta*eta);
  }
}

template<typename ScalarT>
void quad9_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    ScalarT xi2 = xi*xi; ScalarT eta2 = eta*eta;

    // dof 1
    results[ndofs*j*pdim] = 0.25*(2*xi*eta2 - 2*xi*eta-eta2+eta);
    results[ndofs*j*pdim+1] = 0.25*(2*xi2*eta - 2*xi*eta-xi2+xi);

    // dof 2
    results[ndofs*j*pdim+2] = 0.25*(2*xi*eta2 - 2*xi*eta+eta2-eta);
    results[ndofs*j*pdim+3] = 0.25*(2*xi2*eta + 2*xi*eta-xi2-xi);

    // dof 3
    results[ndofs*j*pdim+4] = 0.25*(2*xi*eta2 + 2*xi*eta+eta2+eta);
    results[ndofs*j*pdim+5] = 0.25*(2*xi2*eta + 2*xi*eta+xi2+xi);

    // dof 4
    results[ndofs*j*pdim+6] = 0.25*(2*xi*eta2 + 2*xi*eta-eta2-eta);
    results[ndofs*j*pdim+7] = 0.25*(2*xi2*eta - 2*xi*eta+xi2-xi);

    // dof 5
    results[ndofs*j*pdim+8] = -0.5*(2*xi*eta2 - 2*xi*eta);
    results[ndofs*j*pdim+9] = -0.5*(2*xi2*eta -xi2 - 2*eta+1);

    // dof 6
    results[ndofs*j*pdim+10] = -0.5*(2*xi*eta2 + eta2 - 2*xi-1);
    results[ndofs*j*pdim+11] = -0.5*(2*xi2*eta + 2*xi*eta);

    // dof 7
    results[ndofs*j*pdim+12] = -0.5*(2*xi*eta2 + 2*xi*eta);
    results[ndofs*j*pdim+13] = -0.5*(2*xi2*eta + xi2-2*eta-1);

    // dof 8
    results[ndofs*j*pdim+14] = -0.5*(2*xi*eta2 - eta2 - 2*xi+1);
    results[ndofs*j*pdim+15] = -0.5*(2*xi2*eta - 2*xi*eta);

    // dof 9
    results[ndofs*j*pdim+17] = 2*xi2*eta - 2*eta;
    results[ndofs*j*pdim+16] = 2*xi*eta2 - 2*xi;

  }  // for j
}

bool quad9_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false
  if (dist) *dist=0.0;   // Init to 0.0
  
  if (pcoord[0] < -1.0-in_tol || pcoord[0] > 1.0+in_tol || pcoord[1] < -1.0-in_tol || pcoord[1] > 1.0+in_tol) return false;
  return true;
}

const double quad9_shape_func::ipoints[ndofs*pdim] = {
 -1, -1,  
  1, -1,  
  1,  1,
 -1,  1,
  0, -1,
  1,  0,
  0,  1,
 -1,  0,
  0,  0
};

// HEX

const char *hex_shape_func::name="hex";
const double hex_shape_func::one8th = 1.0/8.0;
const int hex_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1},
{DOF_NODE, 3, 0, 1},
{DOF_NODE, 4, 0, 1},
{DOF_NODE, 5, 0, 1},
{DOF_NODE, 6, 0, 1},
{DOF_NODE, 7, 0, 1}
};


template<typename ScalarT>
void hex_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1], zeta = pcoord[pdim*j+2];
    ScalarT one_m_xi = 1.0 - xi;
    ScalarT one_p_xi = 1.0 + xi;
    ScalarT one_m_eta = 1.0 - eta;
    ScalarT one_p_eta = 1.0 + eta;
    ScalarT one_m_zeta = 1.0 - zeta;
    ScalarT one_p_zeta = 1.0 + zeta;

    results[ndofs*j]   = one8th*one_m_xi*one_m_eta*one_m_zeta;
    results[ndofs*j+1] = one8th*one_p_xi*one_m_eta*one_m_zeta;
    results[ndofs*j+2] = one8th*one_p_xi*one_p_eta*one_m_zeta;
    results[ndofs*j+3] = one8th*one_m_xi*one_p_eta*one_m_zeta;
    results[ndofs*j+4] = one8th*one_m_xi*one_m_eta*one_p_zeta;
    results[ndofs*j+5] = one8th*one_p_xi*one_m_eta*one_p_zeta;
    results[ndofs*j+6] = one8th*one_p_xi*one_p_eta*one_p_zeta;
    results[ndofs*j+7] = one8th*one_m_xi*one_p_eta*one_p_zeta;
  }
}

template<typename ScalarT>
void hex_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1], zeta = pcoord[pdim*j+2];
    ScalarT one_m_xi   = 1.0 - xi, one_p_xi   = 1.0 + xi,
                   one_m_eta  = 1.0 - eta, one_p_eta  = 1.0 + eta, one_m_zeta = 1.0 - zeta, one_p_zeta = 1.0 + zeta;
       
    // xi direction -

    results[ndofs*j*pdim] = - one8th * one_m_eta * one_m_zeta;
    results[ndofs*j*pdim+3] =   one8th * one_m_eta * one_m_zeta;
    results[ndofs*j*pdim+6] =   one8th * one_p_eta * one_m_zeta;
    results[ndofs*j*pdim+9] = - one8th * one_p_eta * one_m_zeta;
    results[ndofs*j*pdim+12] = - one8th * one_m_eta * one_p_zeta;
    results[ndofs*j*pdim+15] =   one8th * one_m_eta * one_p_zeta;
    results[ndofs*j*pdim+18] =   one8th * one_p_eta * one_p_zeta;
    results[ndofs*j*pdim+21] = - one8th * one_p_eta * one_p_zeta;
    //eta direction -

    results[ndofs*j*pdim+1] = - one8th * one_m_xi  * one_m_zeta;
    results[ndofs*j*pdim+4] = - one8th * one_p_xi  * one_m_zeta;
    results[ndofs*j*pdim+7] =   one8th * one_p_xi  * one_m_zeta;
    results[ndofs*j*pdim+10] =   one8th * one_m_xi  * one_m_zeta;
    results[ndofs*j*pdim+13] = - one8th * one_m_xi  * one_p_zeta;
    results[ndofs*j*pdim+16] = - one8th * one_p_xi  * one_p_zeta;
    results[ndofs*j*pdim+19] =   one8th * one_p_xi  * one_p_zeta;
    results[ndofs*j*pdim+22] =   one8th * one_m_xi  * one_p_zeta;
    // zeta direction -

    results[ndofs*j*pdim+2] = - one8th * one_m_xi  * one_m_eta;
    results[ndofs*j*pdim+5] = - one8th * one_p_xi  * one_m_eta;
    results[ndofs*j*pdim+8] = - one8th * one_p_xi  * one_p_eta;
    results[ndofs*j*pdim+11] = - one8th * one_m_xi  * one_p_eta;
    results[ndofs*j*pdim+14] =   one8th * one_m_xi  * one_m_eta;
    results[ndofs*j*pdim+17] =   one8th * one_p_xi  * one_m_eta;
    results[ndofs*j*pdim+20] =   one8th * one_p_xi  * one_p_eta;
    results[ndofs*j*pdim+23] =   one8th * one_m_xi  * one_p_eta;

  }  // for j
}


bool hex_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // Init to is_in = true case
  bool in=true;
  double max_out[3]={0.0,0.0,0.0};
  if (dist) *dist=0.0; 

  // Figure out if it's actually not in  
  if (pcoord[0] < -1.0-in_tol) {
    max_out[0]=-1.0 - pcoord[0];
    in= false;
  } else if (pcoord[0] > 1.0+in_tol) {
    max_out[0]=pcoord[0] - 1.0;
    in= false;
  }

  if (pcoord[1] < -1.0-in_tol) {
    max_out[1]=-1.0 - pcoord[1];
    in= false;
  } else if (pcoord[1] > 1.0+in_tol) {
    max_out[1]=pcoord[1] - 1.0;
    in= false;
  }

  if (pcoord[2] < -1.0-in_tol) {
    max_out[2]=-1.0 - pcoord[2];
    in= false;
  } else if (pcoord[2] > 1.0+in_tol) {
    max_out[2]=pcoord[2] - 1.0;
    in= false;
  }

  // Compute point distance from hex
  if (dist) *dist=std::sqrt(max_out[0]*max_out[0]+max_out[1]*max_out[1]+max_out[2]*max_out[2]);

  return in;
}



const double hex_shape_func::ipoints[ndofs*pdim] = {
-1,  -1,  -1,
 1,  -1,  -1,
 1, 1, -1,
 -1, 1, -1, 
-1,-1,1,
1, -1, 1,
1, 1,  1,
-1, 1, 1
};

// Tet
const char *tet_shape_func::name="tet";
const int tet_shape_func::dof_description[ndofs][4] = {
{DOF_NODE, 0, 0, 1},
{DOF_NODE, 1, 0, 1},
{DOF_NODE, 2, 0, 1},
{DOF_NODE, 3, 0, 1}
};

template<typename ScalarT>
void tet_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1], zeta = pcoord[pdim*j+2];
    results[ndofs*j]   = 1.0 - xi - eta - zeta;
    results[ndofs*j+1] = xi;
    results[ndofs*j+2] = eta;
    results[ndofs*j+3] = zeta;
  }
}

template<typename ScalarT>
void tet_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    //ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1], zeta = pcoord[pdim*j+2];

    // dof 1
    results[ndofs*j*pdim] = -1.0;
    results[ndofs*j*pdim+1] = -1.0;
    results[ndofs*j*pdim+2] = -1.0;

    // dof 2
    results[ndofs*j*pdim+3] = 1.0;
    results[ndofs*j*pdim+4] = 0.0;
    results[ndofs*j*pdim+5] = 0.0;

    // dof 3
    results[ndofs*j*pdim+6] = 0.0;
    results[ndofs*j*pdim+7] = 1.0;
    results[ndofs*j*pdim+8] = 0.0;

    // dof 4
    results[ndofs*j*pdim+9] = 0.0;
    results[ndofs*j*pdim+10] = 0.0;
    results[ndofs*j*pdim+11] = 1.0;

  }
}

bool tet_shape_func::is_in(const double pcoord[],double *dist) {
  const double in_tol = 1e-10;

  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false  
  if (dist) *dist=0.0;   // Init to 0.0
  
  if (pcoord[0] < 0-in_tol || pcoord[1] < -in_tol || pcoord[2] < -in_tol) return false;
  if ((pcoord[0] + pcoord[1] + pcoord[2]) > 1+in_tol) return false;

  return true;
}

const double tet_shape_func::ipoints[ndofs*pdim] = {
  0, 0, 0,
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
};

// QUAD_ZERODERIV
const char *quad_zeroderiv_shape_func::name="quad_zero_deriv";
const double quad_zeroderiv_shape_func::one16th = 1.0/16.0;


template<typename ScalarT>
void quad_zeroderiv_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    ScalarT onemx_sq = (1.0-xi)*(1.0-xi);
    ScalarT onemy_sq = (1.0-eta)*(1.0-eta);
    ScalarT onepx_sq = (1.0+xi)*(1.0+xi);
    ScalarT onepy_sq = (1.0+eta)*(1.0+eta);
    results[ndofs*j] = one16th*onemx_sq*onemy_sq;
    results[ndofs*j+1] = one16th*onepx_sq*onemy_sq;
    results[ndofs*j+2] = one16th*onepx_sq*onepy_sq;
    results[ndofs*j+3] = one16th*onemx_sq*onepy_sq;
  }
}

template<typename ScalarT>
void quad_zeroderiv_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  Throw() << "quad_zeroderiv shape_grads not implemented";
}

bool quad_zeroderiv_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;

  // WARNING:
  // This method isn't used right now, but
  // init dist to 0.0, just so something consistent is
  // returned. However, this will be WRONG if is_in=false.
  // TODO: If this is used, fix dist value to be correct when is_in=false
  if (dist) *dist=0.0;   // Init to 0.0
  
  if (pcoord[0] < -1.0-in_tol || pcoord[0] > 1.0+in_tol || pcoord[1] < -1.0-in_tol || pcoord[1] > 1.0+in_tol) return false;
  return true;
}

const double quad_zeroderiv_shape_func::ipoints[ndofs*pdim] = {
 -1, -1,  
  1, -1,  
  1,  1,
 -1,  1
};

// explicit instantiation.  The size in Sacado is the size of the sensitivity; it may be more appropriate to use the variable size
// fad
// DGO
template class dg0_shape_func<1>;
template class dg0_shape_func<2>;
template class dg0_shape_func<3>;

template void dg0_shape_func<1>::shape(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<1>::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void dg0_shape_func<1>::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<1>::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

template void dg0_shape_func<2>::shape(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<2>::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void dg0_shape_func<2>::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<2>::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

template void dg0_shape_func<3>::shape(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<3>::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void dg0_shape_func<3>::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void dg0_shape_func<3>::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);
// BAR
template void bar_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void bar_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void bar_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void bar_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);
// BAR3
template void bar3_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void bar3_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void bar3_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void bar3_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

// TRI
template void tri_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void tri_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void tri_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void tri_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

// QUAD
template void quad_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void quad_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void quad_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void quad_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

// QUAD9
template void quad9_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void quad9_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void quad9_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void quad9_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);
// QUAD_ZERODERIV
template void quad_zeroderiv_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void quad_zeroderiv_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void quad_zeroderiv_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void quad_zeroderiv_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

// HEX
template void hex_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void hex_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void hex_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void hex_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);
// TET
template void tet_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void tet_shape_func::shape(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[], Sacado::Fad::DFad<double> results[]);
template void tet_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
template void tet_shape_func::shape_grads(unsigned int npts, const Sacado::Fad::DFad<double> pcoord[],
                                  Sacado::Fad::DFad<double> results[]);

} //namespace
