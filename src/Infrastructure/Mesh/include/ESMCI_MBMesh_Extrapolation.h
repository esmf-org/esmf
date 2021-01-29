// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#ifndef ESMCI_MBMesh_Extrapolation_h
#define ESMCI_MBMesh_Extrapolation_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

#include "Mesh/include/ESMCI_MBMesh.h"

#include "Mesh/include/Regridding/ESMCI_WMat.h"

#include "ESMCI_PointList.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

void MBMesh_Extrapolate(MBMesh *srcmesh, PointList *srcpointlist, 
                        MBMesh *dstmesh, PointList *dstpointlist,
                        WMat &wts,
                        // int *mtype, // RLO: not sure if this is needed
                        UInt *pole_constraint_id, // Only valid with srcmesh
                        int *extrapMethod,
                        int *extrapNumSrcPnts,
                        ESMC_R8 *extrapDistExponent,
                        int *extrapNumLevels,
                        int *extrapNumInputLevels, 
                        bool set_dst_status, WMat &dst_status,
                        int *rc);

#endif // ESMF_MOAB

#endif // ESMCI_Mesh_Extrapolation_h
