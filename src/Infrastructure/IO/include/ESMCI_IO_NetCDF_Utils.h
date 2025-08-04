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
// ESMCI NetCDF utility functions header file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_IO_NetCDF_Utils_H
#define ESMCI_IO_NetCDF_Utils_H

#include "ESMCI_Macros.h"
#include "ESMC_Util.h"

#if defined(ESMF_PNETCDF)
#include <pnetcdf.h>
#elif defined(ESMF_NETCDF)
#include <netcdf.h>
#else
// Some definitions needed to allow things to work without either ESMF_PNETCDF or
// ESMF_NETCDF (to avoid needing an overly-complex set of ifdefs elsewhere).
typedef int nc_type;
#define NC_NOERR 0
#endif

#define NC_UNSPECIFIED ((nc_type)0)

namespace ESMCI{
   // Convert a NetCDF error code to an ESMC error code
   int ncerrToEsmcRc(int ncerr);

   // Convert a NetCDF data type code to an ESMC data type code
   ESMC_TypeKind_Flag ncToEsmcType(nc_type ncTypeVal);

   // Convert an ESMC data type code to a NetCDF data type code
   nc_type esmcToNcType(ESMC_TypeKind_Flag esmcTypeVal);
}

#endif // ESMCI_IO_NetCDF_Utils_H
