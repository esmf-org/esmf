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
#include <Mesh/include/Legacy/ESMCI_MCoord.h>
#include <cmath>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

MCoord::MCoord(const double c[], const double n[]) {
  for (UInt i = 0; i < SpaceDim(); i++) {
    ct[i] = c[i];
  }
  double tmp = std::sqrt(n[1]*n[1] + n[2]*n[2]);

  if (std::abs(tmp) > 1e-8) { // vector could be in x direction
    // An orthogonal basis for the cospace of the vector n;
    u[0] = 0; u[1] = n[2]/tmp; u[2] = -n[1]/tmp;
    u[3] = tmp; u[4] = -n[0]*n[1]/tmp; u[5] = -n[0]*n[2]/tmp;
  } else {
    tmp = std::sqrt(n[0]*n[0] + n[2]*n[2]);
    u[0] = n[2]/tmp; u[1] = 0; u[2] = -n[0]/tmp;
    u[3] = -n[1]*n[0]/tmp; u[4] = tmp; u[5] = -n[1]*n[2]/tmp;
  }
}

MCoord::MCoord() {
  for (UInt i = 0; i < 3; i++) {
    ct[i] = 0;
    u[2*i] = 0;
    u[2*i+1] = 0;
  }
}

MCoord &MCoord::operator=(const MCoord &rhs) {
  if (this == &rhs) return *this;
  
  for (UInt i = 0; i < 3; i++) {
    ct[i] = rhs.ct[i];
    u[2*i] = rhs.u[2*i];
    u[2*i+1] = rhs.u[2*i+1];
  }

  return *this;
}

MCoord::MCoord(const MCoord &rhs) {

  *this = rhs;
  
}

void MCoord::Transform(const double in[], double out[]) const {
  for (UInt i = 0; i < ManifoldDim(); i++) {
    out[i] = 0;
    for (UInt j = 0; j < SpaceDim(); j++) {
      out[i] += (in[j] - ct[j])*u[i*SpaceDim() + j];
    }
  }
}

} // namespace
