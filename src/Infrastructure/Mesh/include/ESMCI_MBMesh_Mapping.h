// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_MBMesh_Mapping_h
#define ESMCI_MBMesh_Mapping_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


class MBElemMap {
private:

  bool tri_is_in(const double pcoord[], double *dist);
  bool quad_is_in(const double pcoord[], double *dist);

public:

  bool cartesian_eval(const double *mdata, const double *point, int num_pnts, double *pcoord, double *dist);
  bool spherical_eval(const double *mdata, const double *point, int num_pnts, double *pcoord, double *dist);
};

#endif
#endif
