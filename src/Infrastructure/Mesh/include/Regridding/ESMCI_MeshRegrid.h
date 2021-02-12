// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshRegrid_h
#define ESMCI_MeshRegrid_h

#include <Mesh/include/ESMCI_Mesh.h>
#include <PointList/include/ESMCI_PointList.h>
#include <Mesh/include/Regridding/ESMCI_ExtrapolationPoleLGC.h>
#include <Mesh/include/Regridding/ESMCI_Integrate.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Legacy/ESMCI_WriteWeightsPar.h>

#include <Mesh/include/ESMCI_RegridConstants.h>

#ifdef REGRIDTIMING
#include <mpi.h>
#endif

namespace ESMCI {

 int regrid(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
            Mesh *midmesh, IWeights &wts,
            int *regridMethod, int *regridScheme,
            int *regridPoleType, int *regridPoleNPnts,
            int *map_type,
            int *extrapMethod,
            int *extrapNumSrcPnts,
            ESMC_R8 *extrapDistExponent,
            int *extrapNumLevels,
            int *extrapNumInputLevels, 
            int *unmappedaction,
            bool set_dst_status, WMat &dst_status, bool checkFlag);

 int get_iwts(Mesh &mesh, MEField<> *iwts, int *regridScheme);

} // namespace

#endif /*ESMC_MESHREGRID_H_*/
