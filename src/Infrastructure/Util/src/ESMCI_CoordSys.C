// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_CoordSys.C"
//==============================================================================

// single blank line to make protex happy.
//BOP

//EOP
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Util methods declared
// in the companion file ESMCI_Util.h
//
//-----------------------------------------------------------------------------

// associated class definition file and others
#include "ESMCI_CoordSys.h"

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMC_Util.h"

#include <cmath>

using namespace std;

// Some xlf compilers don't define this
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

// Constants for converting between radians and degrees
// (These need to match the ones in ESMF_UtilTypes.F90) 
const double ESMC_CoordSys_Deg2Rad= 0.01745329251994329547437;
const double ESMC_CoordSys_Rad2Deg=57.29577951308232286464772;

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_CoordSys_CalcCartDim"
//BOPI
// !IROUTINE: ESMCI_CoordSys_CalcCartDim
//
// !INTERFACE:
    int ESMCI_CoordSys_CalcCartDim(
//
// !RETURN VALUE:
//  return code
// 
// !ARGUMENTS:
                                  ESMC_CoordSys_Flag coordSys,   // Input coordSys
                                  int in_dim,        // Dimension to convert to Cart dimension
                                  int *cart_dim        // Corresponding Cart dimension
 ) {
//EOPI

  if (coordSys==ESMC_COORDSYS_CART) {
    *cart_dim=in_dim;
  } else if ((coordSys==ESMC_COORDSYS_SPH_DEG) || (coordSys==ESMC_COORDSYS_SPH_RAD)) {
    if ((in_dim==2)||(in_dim==3)) {
      *cart_dim=3;
    } else {
      int rc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- ESMF_COORDSYS_SPH currently only works with 2D or 3D coordinates",
        ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // return success
  return ESMF_SUCCESS;
}


//// STOPPED HERE!!!!! ////

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_CoordSys_ConvertToCart"
//BOPI
// !IROUTINE: ESMCI_CoordSys_ConvertToCart
//
// !INTERFACE:
template <class TYPE>
    int ESMCI_CoordSys_ConvertToCart(
//
// !RETURN VALUE:
//  return code
// 
// !ARGUMENTS:

// QUESTION: Do I convert coordinates in place or have an input and output???????
//  - probably doesn't matter awhole lot except for a small efficiency difference in certain circumstances



                                    ESMC_CoordSys_Flag cs,   // Input coordSys
                                    int in_dim,        // Dimension of input coordinates
                                                       // output dim can be found by ESMC_CoordSys_CalcCartDim 
                                    TYPE *in_coord,  // Input coordinates 
                                    TYPE *cart_coord  // Output cartesian coordinates 
 ) {
//EOPI
  int localrc;

  // transform if necessary to cartesian
  if (cs==ESMC_COORDSYS_CART) {
    for (int i=0; i<in_dim; i++) {
      cart_coord[i]=in_coord[i];
    }               
  } else if (cs==ESMC_COORDSYS_SPH_DEG) {
    double lon = in_coord[0];
    double lat = in_coord[1];

    const double ninety = 90.0;
    double theta = lon*ESMC_CoordSys_Deg2Rad;
    double phi   = (ninety-lat)*ESMC_CoordSys_Deg2Rad;
    cart_coord[0] = std::cos(theta)*std::sin(phi);
    cart_coord[1] = std::sin(theta)*std::sin(phi);
    cart_coord[2] = std::cos(phi);   

    // If 3D Sph then multiply through by radius
    if (in_dim==3) {
      cart_coord[0] *= in_coord[2];
      cart_coord[1] *= in_coord[2];
      cart_coord[2] *= in_coord[2];
    }
    
  } else if (cs==ESMC_COORDSYS_SPH_RAD) {
    const double half_pi = 0.5*M_PI;
    double theta = in_coord[0];
    double phi = half_pi-in_coord[1];
    cart_coord[0] = std::cos(theta)*std::sin(phi);
    cart_coord[1] = std::sin(theta)*std::sin(phi);
    cart_coord[2] = std::cos(phi);    

    // If 3D Sph then multiply through by radius
    if (in_dim==3) {
      cart_coord[0] *= in_coord[2];
      cart_coord[1] *= in_coord[2];
      cart_coord[2] *= in_coord[2];
    }
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
              "Unknown CoordSys", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  
  // return success
  return ESMF_SUCCESS;
}

// Add more types here if necessary
template int ESMCI_CoordSys_ConvertToCart(
                                          ESMC_CoordSys_Flag cs,   // Input coordSys
                                          int in_dim,        // Dimension of input coordinates
                                          // output dim can be found by ESMC_CoordSys_CalcCartDim 
                                          ESMC_R8 *in_coord,  // Input coordinates 
                                          ESMC_R8 *cart_coord  // Output cartesian coordinates 
                                          );
