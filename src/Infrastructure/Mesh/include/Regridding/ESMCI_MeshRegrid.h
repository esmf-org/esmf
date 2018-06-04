// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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
#include <Mesh/include/Regridding/ESMCI_Extrapolation.h>
#include <Mesh/include/Regridding/ESMCI_Integrate.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Legacy/ESMCI_WriteWeightsPar.h>

#ifdef REGRIDTIMING
#include <mpi.h>
#endif

namespace ESMCI {

#ifdef REGRIDTIMING
class regridTimer
{
public:
  double start;
  double gridsInput;
  double regridComplete;
  double weightsOutput;
private:
} ;
#endif

enum {ESMC_NORM_TYPE_DSTAREA = 0, ESMC_NORM_TYPE_FRACAREA = 1};

enum {ESMC_REGRID_SCHEME_FULL3D = 0,
      ESMC_REGRID_SCHEME_NATIVE,
      ESMC_REGRID_SCHEME_REGION3D,
      ESMC_REGRID_SCHEME_FULLTOREG3D,
      ESMC_REGRID_SCHEME_REGTOFULL3D,
      ESMC_REGRID_SCHEME_DCON3D,
      ESMC_REGRID_SCHEME_DCON3DWPOLE};


#define ESMC_REGRID_STATUS_DST_MASKED 0
#define ESMC_REGRID_STATUS_SRC_MASKED 1
#define ESMC_REGRID_STATUS_OUTSIDE 2
#define ESMC_REGRID_STATUS_MAPPED 4
#define ESMC_REGRID_STATUS_EXTRAP_MAPPED 8

#define ESMC_EXTRAPMETHOD_NONE 0
#define ESMC_EXTRAPMETHOD_NEAREST_STOD 1
#define ESMC_EXTRAPMETHOD_NEAREST_IDAVG 2


enum {ESMC_REGRID_METHOD_BILINEAR = 0, ESMC_REGRID_METHOD_PATCH,
      ESMC_REGRID_METHOD_CONSERVE, ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST, ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC,
      ESMC_REGRID_METHOD_CONSERVE_2ND, ESMC_REGRID_METHOD_NEAREST_IDAVG};
enum {ESMC_REGRID_CONSERVE_OFF = 0, ESMC_REGRID_CONSERVE_ON = 1};
enum {ESMC_REGRID_POLETYPE_NONE = 0, ESMC_REGRID_POLETYPE_ALL = 1, ESMC_REGRID_POLETYPE_NPNT = 2, ESMC_REGRID_POLETYPE_TEETH = 3};

// offline
 int regrid(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
            Mesh *midmesh, IWeights &wts,
            int *regridMethod, int *regridScheme,
            int *regridPoleType, int *regridPoleNPnts,
            int *map_type,
            int *extrapMethod,
            int *extrapNumSrcPnts,
            ESMC_R8 *extrapDistExponent,
            int *unmappedaction,
            bool set_dst_status, WMat &dst_status);

int csrv(Mesh &, Mesh &, IWeights &, MEField<> *, MEField<> *,
         int *, int *, int *, int *, int *);

// online
#ifdef REGRIDTIMING
int offline_regrid(Mesh &, Mesh &, Mesh &, int *, int *, int *, int *, char *, char *, char *, regridTimer &rT);
#else
int offline_regrid(Mesh &, Mesh &, Mesh &, int *, int *, int *, int *, char *, char *, char *);
#endif
  int online_regrid(Mesh *srcmesh, PointList *srcpointlist,
                    Mesh *dstmesh, PointList *dstpointlist,
                    IWeights &wts,
                    int *regridConserve, int *regridMethod,
                    int *regridPoleType, int *regridPoleNPnts,
                    int *regridScheme,
                    int *map_type,
                    int *extrapMethod,
                    int *extrapNumSrcPnts,
                    ESMC_R8 *extrapDistExponent,
                    int *unmappedaction,
                    bool set_dst_status, WMat &dst_status);

// get the integration weights for one mesh
int get_iwts(Mesh &, MEField<> *, int *);

} // namespace

#endif /*ESMC_MESHREGRID_H_*/
