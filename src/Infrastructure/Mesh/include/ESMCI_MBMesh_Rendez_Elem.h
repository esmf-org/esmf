// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_MBMesh_Rendez_Elem_h
#define ESMCI_MBMesh_Rendez_Elem_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void create_rendez_mbmesh_elem(MBMesh *srcmesh, MBMesh *dstmesh, MBMesh **_srcmesh_rendez, MBMesh **_dstmesh_rendez);

#endif // ESMF_MOAB

#endif
