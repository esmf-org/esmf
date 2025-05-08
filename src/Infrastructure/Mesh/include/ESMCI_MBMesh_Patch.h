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

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_MBMesh_Patch_h
#define ESMCI_MBMesh_Patch_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/Regridding/ESMCI_WMat.h"

// for IWeights?
#include <Mesh/include/Regridding/ESMCI_Interp.h>


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void calc_patch_regrid_wgts(MBMesh *mbmp, PointList *dplp, IWeights &wts, int *map_type, bool set_dst_status, WMat &dst_status);

#endif
#endif
