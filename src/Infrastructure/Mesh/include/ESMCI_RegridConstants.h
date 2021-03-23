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
#ifndef ESMCI_RegridConstants_h
#define ESMCI_RegridConstants_h

enum {ESMC_NORM_TYPE_DSTAREA = 0,
      ESMC_NORM_TYPE_FRACAREA};

enum {ESMC_REGRID_CONSERVE_OFF = 0,
      ESMC_REGRID_CONSERVE_ON};

enum {ESMC_REGRID_METHOD_BILINEAR = 0,
      ESMC_REGRID_METHOD_PATCH,
      ESMC_REGRID_METHOD_CONSERVE,
      ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST,
      ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC,
      ESMC_REGRID_METHOD_CONSERVE_2ND,
      ESMC_REGRID_METHOD_NEAREST_IDAVG};

enum {ESMC_REGRID_POLETYPE_NONE = 0,
      ESMC_REGRID_POLETYPE_ALL,
      ESMC_REGRID_POLETYPE_NPNT,
      ESMC_REGRID_POLETYPE_TEETH};

enum {ESMC_REGRID_SCHEME_FULL3D = 0,
      ESMC_REGRID_SCHEME_NATIVE,
      ESMC_REGRID_SCHEME_REGION3D,
      ESMC_REGRID_SCHEME_FULLTOREG3D,
      ESMC_REGRID_SCHEME_REGTOFULL3D,
      ESMC_REGRID_SCHEME_DCON3D,
      ESMC_REGRID_SCHEME_DCON3DWPOLE};

enum {ESMC_REGRID_STATUS_DST_MASKED = 0,
      ESMC_REGRID_STATUS_SRC_MASKED = 1,
      ESMC_REGRID_STATUS_OUTSIDE = 2,
      ESMC_REGRID_STATUS_MAPPED = 4,
      ESMC_REGRID_STATUS_EXTRAP_MAPPED = 8};

#endif
