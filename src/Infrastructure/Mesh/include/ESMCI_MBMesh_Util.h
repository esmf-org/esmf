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

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_MBMesh_Util_h
#define ESMCI_MBMesh_Util_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include "ESMCI_PointList.h"
#include "Util/include/ESMC_Util.h"
#include "Util/include/ESMCI_F90Interface.h"

#include <vector>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void MBMesh_get_gid(MBMesh *mbmp, EntityHandle eh, int *gid);

void MBMesh_get_elem_coords_3D_ccw(MBMesh *mbmp, EntityHandle elem, 
                                   int max_num_nodes, double *tmp_coords, 
                                   int *num_nodes, double *coords);
void MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords);

void MBMesh_get_elem_centroid(MBMesh *mbmp, EntityHandle elem, double *centroid);

void MBMesh_get_local_elem_gids(MBMesh *mbmp, std::vector<UInt> &egids);

// expects pcoords in domain [-1,1] and translates to [0,1]
// useful for translating pcoords from MOAB to ESMF domain
void translate(double *pcoords);

ESMCI::PointList *MBMesh_to_PointList(MBMesh *mesh, ESMC_MeshLoc_Flag meshLoc, ESMCI::InterArray<int> *maskValuesArg, int *rc);
#endif // ESMF_MOAB

#endif
