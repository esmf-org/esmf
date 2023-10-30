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

using namespace ESMCI;


// Constructors for 2D Cart
template<>
Vert<GEOM_CART2D>::  Vert(double x, double y): Vert() {
    pnt[0]=x;
    pnt[1]=y;
}

template<>
Vert<GEOM_CART2D>::  Vert(double x, double y, double z): Vert() {
  Throw() << "Using 3D constructor for Cart 2D Vertex.";
}


// Constructors for Sph
template<>
Vert<GEOM_SPH2D3D>::  Vert(double x, double y): Vert() {
    Throw() << "Using 3D constructor for Cart 2D Vertex.";
}

template<>
Vert<GEOM_SPH2D3D>::  Vert(double x, double y, double z): Vert() {
    pnt[0]=x;
    pnt[1]=y;
    pnt[2]=z;
}



// Dump to vtk file for debugging
template<> void Pgon<GEOM_CART2D>::write_to_vtk(const char *filename) {
  pack_coords_into_buff();
  write_2D_poly_woid_to_vtk(filename, get_num_pnts(), coord_buff.data());
}

// Dump to vtk file for debugging
template<> void Pgon<GEOM_SPH2D3D>::write_to_vtk(const char *filename) {
  pack_coords_into_buff();
  write_3D_poly_woid_to_vtk(filename, get_num_pnts(), coord_buff.data());
}
