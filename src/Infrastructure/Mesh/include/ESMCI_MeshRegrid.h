//
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


namespace ESMCI {

enum {ESMC_REGRID_SCHEME_FULL3D = 0, ESMC_REGRID_SCHEME_NATIVE = 1};
enum {ESMC_REGRID_METHOD_BILINEAR = 0, ESMC_REGRID_METHOD_PATCH = 1};
enum {ESMC_REGRID_CONSERVE_OFF = 0, ESMC_REGRID_CONSERVE_ON = 1};
enum {ESMC_REGRID_POLETYPE_NONE = 0, ESMC_REGRID_POLETYPE_ALL = 1, ESMC_REGRID_POLETYPE_NPNT = 2, ESMC_REGRID_POLETYPE_TEETH = 3};

// offline
int regrid(Mesh &, Mesh &, IWeights &, int *, int *, int *, int *, int *);
int csrv(Mesh &, Mesh &, IWeights &, MEField<> *, MEField<> *, 
         int *, int *, int *, int *, int *);

// online
int offline_regrid(Mesh &, Mesh &, int *, int *, int *, int *, char *, char *, char *);
int online_regrid(Mesh &, Mesh &, IWeights &, int *, int *, int *, int *);

} // namespace

#endif /*ESMC_MESHREGRID_H_*/
