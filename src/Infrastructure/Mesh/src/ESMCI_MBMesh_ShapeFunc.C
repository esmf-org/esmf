// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_MBMesh_ShapeFunc.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
//
// The contents of this file were migrated from the legacy mesh file 
// ESMCI_ShapeFunc.C in June of 2018.
//
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

MBShapeFunc::MBShapeFunc() {
}

MBShapeFunc::~MBShapeFunc() {
}

// TRI 
const char *mb_tri_shape_func::name="tri";
const int mb_tri_shape_func::dof_description[ndofs][4] = {
{MB_DOF_NODE, 0, 0, 1},
{MB_DOF_NODE, 1, 0, 1},
{MB_DOF_NODE, 2, 0, 1}
};


template<typename ScalarT>
void mb_tri_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    results[ndofs*j] = 1 - xi - eta;
    results[ndofs*j+1] = xi;
    results[ndofs*j+2] = eta;
  }
}

template<typename ScalarT>
void mb_tri_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
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


#if 0
bool mb_tri_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  if (pcoord[0] < -in_tol) {
    if (dist) *dist = -pcoord[0];
    return false;
  } else if (pcoord[1] < -in_tol) {
    if (dist) *dist = -pcoord[1];
    return false;
  } else if ((pcoord[0] + pcoord[1]) > 1.0+in_tol) {
    if (dist) *dist = std::abs((pcoord[0] + pcoord[1]) - 1.0);
    return false;
  }
  return true;
}
#endif

bool mb_tri_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  bool in=true;

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


const double mb_tri_shape_func::ipoints[ndofs*pdim] = {
 0,  0,
 1,  0,
 0,  1
};

// QUAD
const char *mb_quad_shape_func::name="quad";
const double mb_quad_shape_func::one4th = 1.0/4.0;
const int mb_quad_shape_func::dof_description[ndofs][4] = {
{MB_DOF_NODE, 0, 0, 1},
{MB_DOF_NODE, 1, 0, 1},
{MB_DOF_NODE, 2, 0, 1},
{MB_DOF_NODE, 3, 0, 1}
/* bizare test case 
{MB_DOF_NODE, 0, 0},
{MB_DOF_NODE, 1, 0},
{MB_DOF_NODE, 2, 0},
{MB_DOF_NODE, 3, 0},
{MB_DOF_EDGE, 0, 0},
{MB_DOF_EDGE, 0, 1},
{MB_DOF_EDGE, 1, 0},
{MB_DOF_EDGE, 1, 1},
{MB_DOF_EDGE, 2, 0},
{MB_DOF_EDGE, 2, 1},
{MB_DOF_EDGE, 3, 0},
{MB_DOF_EDGE, 3, 1},
{MB_DOF_FACE, 0, 0},
{MB_DOF_FACE, 0, 1},
{MB_DOF_FACE, 0, 2},
{MB_DOF_FACE, 1, 0},
{MB_DOF_FACE, 1, 1},
{MB_DOF_FACE, 1, 2},
{MB_DOF_FACE, 2, 0},
{MB_DOF_FACE, 2, 1},
{MB_DOF_FACE, 2, 2},
{MB_DOF_FACE, 3, 0},
{MB_DOF_FACE, 3, 1},
{MB_DOF_ELEM, 0, 0}
*/
};


template<typename ScalarT>
void mb_quad_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1];
    results[ndofs*j] = one4th*(1.0-xi)*(1.0-eta);
    results[ndofs*j+1] = one4th*(1.0+xi)*(1.0-eta);
    results[ndofs*j+2] = one4th*(1.0+xi)*(1.0+eta);
    results[ndofs*j+3] = one4th*(1.0-xi)*(1.0+eta);
  }
}

template<typename ScalarT>
void mb_quad_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
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

#if 0
bool mb_quad_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  if (pcoord[0] < -1.0-in_tol) {
    if(dist) *dist = -1.0 - pcoord[0];
    return false;
  } else if (pcoord[0] > 1.0+in_tol) {
    return false;
  } else if  (pcoord[1] < -1.0-in_tol) {
    return false;
  } else if(pcoord[1] > 1.0+in_tol) {
    return false;
  }
  return true;
}
#endif


bool mb_quad_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  bool in=true;
  double max_out[2]={0.0,0.0};

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

const double mb_quad_shape_func::ipoints[ndofs*pdim] = {
 -1, -1,  
  1, -1,  
  1,  1,
 -1,  1
};

// HEX

const char *mb_hex_shape_func::name="hex";
const double mb_hex_shape_func::one8th = 1.0/8.0;
const int mb_hex_shape_func::dof_description[ndofs][4] = {
{MB_DOF_NODE, 0, 0, 1},
{MB_DOF_NODE, 1, 0, 1},
{MB_DOF_NODE, 2, 0, 1},
{MB_DOF_NODE, 3, 0, 1},
{MB_DOF_NODE, 4, 0, 1},
{MB_DOF_NODE, 5, 0, 1},
{MB_DOF_NODE, 6, 0, 1},
{MB_DOF_NODE, 7, 0, 1}
};


template<typename ScalarT>
void mb_hex_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
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
void mb_hex_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
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

#if 1
bool mb_hex_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  bool in=true;
  double max_out[3]={0.0,0.0,0.0};

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

#else

bool mb_hex_shape_func::is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  if (pcoord[0] < -1.0-in_tol || pcoord[0] > 1.0+in_tol || pcoord[1] < -1.0-in_tol || pcoord[1] > 1.0+in_tol
   || pcoord[2] < -1.0-in_tol || pcoord[2] > 1.0+in_tol) return false;
  return true;
}

#endif

const double mb_hex_shape_func::ipoints[ndofs*pdim] = {
-1,  -1,  -1,
 1,  -1,  -1,
 1, 1, -1,
 -1, 1, -1, 
-1,-1,1,
1, -1, 1,
1, 1,  1,
-1, 1, 1
};

/*
// Tet
const char *mb_tet_shape_func::name="tet";
const int mb_tet_shape_func::dof_description[ndofs][4] = {
{MB_DOF_NODE, 0, 0, 1},
{MB_DOF_NODE, 1, 0, 1},
{MB_DOF_NODE, 2, 0, 1},
{MB_DOF_NODE, 3, 0, 1}
};

template<typename ScalarT>
void mb_tet_shape_func::shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
  for (unsigned int j = 0; j < npts; j++) {
    ScalarT xi = pcoord[pdim*j], eta = pcoord[pdim*j+1], zeta = pcoord[pdim*j+2];
    results[ndofs*j]   = 1.0 - xi - eta - zeta;
    results[ndofs*j+1] = xi;
    results[ndofs*j+2] = eta;
    results[ndofs*j+3] = zeta;
  }
}

template<typename ScalarT>
void mb_tet_shape_func::shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]) {
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

bool mb_tet_shape_func::is_in(const double pcoord[],double *dist) {
  const double in_tol = 1e-10;
  if (pcoord[0] < 0-in_tol || pcoord[1] < -in_tol || pcoord[2] < -in_tol) return false;

  if ((pcoord[0] + pcoord[1] + pcoord[2]) > 1+in_tol) return false;
  return true;
}

const double mb_tet_shape_func::ipoints[ndofs*pdim] = {
  0, 0, 0,
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
};
*/

// explicit instantiation.  The size in Sacado is the size of the sensitivity; it may be more appropriate to use the variable size

// TRI
template void mb_tri_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void mb_tri_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);

// QUAD
template void mb_quad_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void mb_quad_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);

// HEX
template void mb_hex_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void mb_hex_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
// TET
/*
template void mb_tet_shape_func::shape(unsigned int npts, const double pcoord[], double results[]);
template void mb_tet_shape_func::shape_grads(unsigned int npts, const double pcoord[], double results[]);
*/
} //namespace
