// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Pgon.h"
#include "Mesh/include/ESMCI_MathUtil.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

 // Dump to vtk file for debugging
template<> void Pgon<GEOM_CART2D>::write_to_vtk(const char *filename) {
  write_2D_poly_woid_to_vtk(filename, get_num_pnts(), pnt_coords.data());
}

// Dump to vtk file for debugging
template<> void Pgon<GEOM_SPH2D3D>::write_to_vtk(const char *filename) {
  write_3D_poly_woid_to_vtk(filename, get_num_pnts(), pnt_coords.data());
}

// Method for subtracting polygon clipper from polygon subject to yield the results
// list of new polygons
// TODO:
//  - Since the implementation will be so big, maybe put this in a separate file
//  - Think about having a diff that takes away from the current polygon, so you're not
//    always building new classes when you diff
template<class GEOM>
void Pgon<GEOM>::difference(Pgon<GEOM> subject, Pgon<GEOM> clipper,
                            std::vector< Pgon<GEOM> > reults) {

}
