// $Id: ESMC_MCoord.C,v 1.1.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MCoord.h>
#include <cmath>

namespace ESMCI {
namespace MESH {

MCoord::MCoord(const double c[], const double n[]) {
  for (UInt i = 0; i < SpaceDim(); i++) {
    ct[i] = c[i];
  }
  double tmp = std::sqrt(n[1]*n[1] + n[2]*n[2]);
  // An orthogonal basis for the cospace of the vector n;
  u[0] = 0; u[1] = n[2]/tmp; u[2] = -n[1]/tmp;
  u[3] = tmp; u[4] = -n[0]*n[1]/tmp; u[5] = -n[0]*n[2]/tmp;
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
} // namespace
