// $Id: ESMCI_MeshRegrid.h,v 1.11 2011/04/26 19:48:22 feiliu Exp $
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
#include <Mesh/include/ESMCI_Extrapolation.h>
#include <Mesh/include/ESMCI_Integrate.h>
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_WriteWeightsPar.h>

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

enum {ESMC_REGRID_SCHEME_FULL3D = 0, 
      ESMC_REGRID_SCHEME_NATIVE,
      ESMC_REGRID_SCHEME_REGION3D,
      ESMC_REGRID_SCHEME_FULLTOREG3D,
      ESMC_REGRID_SCHEME_REGTOFULL3D,
      ESMC_REGRID_SCHEME_DCON3D,
      ESMC_REGRID_SCHEME_DCON3DWPOLE};



enum {ESMC_REGRID_METHOD_BILINEAR = 0, ESMC_REGRID_METHOD_PATCH, 
      ESMC_REGRID_METHOD_CONSERVE};
enum {ESMC_REGRID_CONSERVE_OFF = 0, ESMC_REGRID_CONSERVE_ON = 1};
enum {ESMC_REGRID_POLETYPE_NONE = 0, ESMC_REGRID_POLETYPE_ALL = 1, ESMC_REGRID_POLETYPE_NPNT = 2, ESMC_REGRID_POLETYPE_TEETH = 3};

// offline
int regrid(Mesh &, Mesh &, Mesh *, IWeights &, int *, int *, int *, int *, int *);
int csrv(Mesh &, Mesh &, IWeights &, MEField<> *, MEField<> *, 
         int *, int *, int *, int *, int *);

// online
#ifdef REGRIDTIMING
int offline_regrid(Mesh &, Mesh &, Mesh &, int *, int *, int *, int *, char *, char *, char *, regridTimer &rT);
#else
int offline_regrid(Mesh &, Mesh &, Mesh &, int *, int *, int *, int *, char *, char *, char *);
#endif
int online_regrid(Mesh &srcmesh, Mesh &dstmesh, IWeights &wts,
                  int *regridConserve, int *regridMethod,
                  int *regridPoleType, int *regridPoleNPnts, 
                  int *regridScheme, int *unmappedaction);

// get the integration weights for one mesh
int get_iwts(Mesh &, MEField<> *, int *);

} // namespace

#endif /*ESMC_MESHREGRID_H_*/
