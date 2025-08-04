// $Id$
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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
            int *regridMethod, 
            int *regridPoleType, int *regridPoleNPnts,
            int *map_type,
            int *extrapMethod,
            int *extrapNumSrcPnts,
            ESMC_R8 *extrapDistExponent,
            int *extrapNumLevels,
            int *extrapNumInputLevels, 
            int *unmappedaction,
            bool set_dst_status, WMat &dst_status, bool checkFlag);

 void translate_split_src_elems_in_wts(Mesh *srcmesh, int num_entries,
                                      int *iientries);
 void translate_split_dst_elems_in_wts(Mesh *dstmesh, int num_entries,
                                      int *iientries, double *factors);
  
 int get_iwts(Mesh &mesh, MEField<> *iwts);

} // namespace

#endif /*ESMC_MESHREGRID_H_*/
