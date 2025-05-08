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

#ifndef ESMCI_MBMesh_Regrid_Glue_h
#define ESMCI_MBMesh_Regrid_Glue_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_Regrid_Nearest.h"

// for temp weights, this file should be merged into something else
#include "Mesh/include/Regridding/ESMCI_Regrid_Helper.h"

// #include "Mesh/include/ESMCI_Mesh.h"
// #include "Mesh/include/ESMCI_MathUtil.h"

// #include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
// #include "Mesh/include/Regridding/ESMCI_Integrate.h"
// #include "Mesh/include/Regridding/ESMCI_Interp.h"
// #include "Mesh/include/Regridding/ESMCI_ExtrapolationPoleLGC.h"
// #include "Mesh/include/Regridding/ESMCI_Regrid_Helper.h"

#include "Mesh/include/Legacy/ESMCI_Exception.h"
// #include "Mesh/include/Legacy/ESMCI_MeshRead.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

 
void MBMesh_regrid_create(MBMesh **meshsrcpp, ESMCI::Array **arraysrcpp, 
                          ESMCI::PointList **plsrcpp,
                          MBMesh **meshdstpp, ESMCI::Array **arraydstpp, 
                          ESMCI::PointList **pldstpp,
                          int *regridMethod,
                          int *map_type,
                          int *norm_type,
                          int *regridPoleType, int *regridPoleNPnts,  
                          int *extrapMethod,
                          int *extrapNumSrcPnts,
                          ESMC_R8 *extrapDistExponent,
                          int *extrapNumLevels,
                          int *extrapNumInputLevels, 
                          int *unmappedaction, int *_ignoreDegenerate,
                          int *srcTermProcessing, int *pipelineDepth, 
                          ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                          int *nentries, ESMCI::TempWeights **tweights,
                          int *has_udl, int *_num_udl, ESMCI::TempUDL **_tudl, 
                          int *_has_statusArray, ESMCI::Array **_statusArray,
                          int*rc);


#endif // ESMF_MOAB

#endif // ESMCI_Mesh_Regrid_Glue_h
