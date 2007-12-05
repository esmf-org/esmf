//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MESHPNC_H_
#define ESMC_MESHPNC_H_

#include <string>

namespace ESMC {

class Mesh;

/* Load the dual mesh of a given netcdf file.  Note: this loads the file on
 * processor zero only.  It should, however, be called from all processors.
 * Will generate a bilinear mesh unless use_quad = true, then a quadratic.
 */
void LoadNCDualMeshPar(Mesh &mesh, const std::string fname, bool use_quad=false);

} // namespace

#endif /*ESMC_MESHPNC_H_*/
