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
#ifndef ESMCI_Extrap_h
#define ESMCI_Extrap_h

#include <Mesh/include/ESMCI_Mesh.h>
#include <PointList/include/ESMCI_PointList.h>
#include <Mesh/include/Regridding/ESMCI_ExtrapolationPoleLGC.h>
#include <Mesh/include/Regridding/ESMCI_Integrate.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Legacy/ESMCI_WriteWeightsPar.h>
#include <Mesh/include/ESMCI_RegridConstants.h>

namespace ESMCI {

  void extrap(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
              IWeights &wts,
              MAP_TYPE mtype,
              UInt pole_constraint_id, // Only valid when srcmesh exists
              int extrapMethod,
              int extrapNumSrcPnts,
              ESMC_R8 extrapDistExponent,
              int extrapNumLevels,
              int extrapNumInputLevels, 
              bool set_dst_status, WMat &dst_status);


} // namespace

#endif /* ESMCI_Extrap_H_*/
