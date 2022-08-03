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
#define ESMC_FILENAME "ESMCI_MBMesh_Pole_Extrap.C"
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMC_Util.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_RegridConstants.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"
#include "Mesh/include/Regridding/ESMCI_WMat.h"


using namespace ESMCI;





void MBMesh_Pole_Extrap(MBMesh *srcmesh, PointList *srcpointlist, 
                        MBMesh *dstmesh, PointList *dstpointlist,
                        int poleType, int poleNPnts,
                        WMat &wts,
                        bool set_dst_status, WMat &dst_status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_Pole_Extrap()"

  int localrc;

    // Branch to each type of pole extrapolation
    if (poleType == ESMC_REGRID_POLETYPE_NONE) {

      // Don't do anything

    } else if (poleType == ESMC_REGRID_POLETYPE_ALL) {
      printf(" poleMethod=ESMF_POLEMETHOD_ALLAVG\n");
    } else if (poleType == ESMC_REGRID_POLETYPE_NPNT) {
      Throw() << " ESMF_POLEMETHOD_NPNTAVG currently not supported when using MOAB internal mesh representation.";
    } else if (poleType == ESMC_REGRID_POLETYPE_TEETH) {
      Throw() << " ESMF_POLEMETHOD_TEETH currently not supported when using MOAB internal mesh representation.";
    } else {
      Throw() << " Unrecognized pole method.";
    }
}

#undef  ESMC_METHOD

#endif // ESMF_MOAB
