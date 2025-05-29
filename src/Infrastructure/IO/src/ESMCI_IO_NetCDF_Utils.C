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
//==============================================================================
#define ESMC_FILENAME "ESMCI_IO_NetCDF_Utils.C"
//==============================================================================

// single blank line to make protex happy.
// BOP

// EOP
//-----------------------------------------------------------------------------
//
//  !DESCRIPTION:
//
//  The code in this file implements utility functions for NetCDF.
//
//-----------------------------------------------------------------------------

// associated header file
#include "ESMCI_IO_NetCDF_Utils.h"

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::ncerrToEsmcRc"
//BOPI
// !IROUTINE:  ESMCI::ncerrToEsmcRc
//
// !INTERFACE:
int ESMCI::ncerrToEsmcRc(
//
// !RETURN VALUE:
//    int ESMC error code
//
// !ARGUMENTS:
   int ncerror  // (in) NetCDF error code
   ){
// !DESCRIPTION:
//    Convert a NetCDF error code to an ESMC error code.
//EOPI
//-----------------------------------------------------------------------------
   if (ncerror == NC_NOERR)
      return ESMF_SUCCESS;
   else
      return ESMF_FAILURE;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::ncToEsmcType"
//BOPI
// !IROUTINE:  ESMCI::ncToEsmcType
//
// !INTERFACE:
ESMC_TypeKind_Flag ESMCI::ncToEsmcType(
//
// !RETURN VALUE:
//    ESMC_TypeKind_Flag ESMC data type code
//
// !ARGUMENTS:
   nc_type ncTypeVal  // (in) NetCDF data type code
   ){
// !DESCRIPTION:
//    Convert a NetCDF data type code to an ESMC data type code.
//EOPI
//-----------------------------------------------------------------------------
   ESMC_TypeKind_Flag esmcTypeVal = ESMF_NOKIND;

#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   switch (ncTypeVal)
   {
   case NC_BYTE:
      esmcTypeVal = ESMC_TYPEKIND_I1;
      break;
   case NC_CHAR:
      esmcTypeVal = ESMC_TYPEKIND_CHARACTER;
      break;
   case NC_SHORT:
      esmcTypeVal = ESMC_TYPEKIND_I2;
      break;
   case NC_INT:
      esmcTypeVal = ESMC_TYPEKIND_I4;
      break;
   // case NC_LONG:  // TODO?: deprecated in netCDF - same as NC_INT
   //  esmcTypeVal = ESMC_TYPEKIND_I8;
   // break;
   case NC_FLOAT:
      esmcTypeVal = ESMC_TYPEKIND_R4;
      break;
   case NC_DOUBLE:
      esmcTypeVal = ESMC_TYPEKIND_R8;
      break;
   default:
      break;
   }
#endif

   return esmcTypeVal;
}

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::esmcToNcType"
//BOPI
// !IROUTINE:  ESMCI::esmcToNcType
//
// !INTERFACE:
nc_type ESMCI::esmcToNcType(
//
// !RETURN VALUE:
//    nc_type NetCDF data type code
//
// !ARGUMENTS:
   ESMC_TypeKind_Flag esmcTypeVal  // (in) ESMC data type code
   ){
// !DESCRIPTION:
//    Convert an ESMC data type code to a NetCDF data type code.
//EOPI
//-----------------------------------------------------------------------------
   nc_type ncTypeVal = NC_UNSPECIFIED;

#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   switch (esmcTypeVal)
   {
   case ESMC_TYPEKIND_I1:
      ncTypeVal = NC_BYTE;
      break;
   case ESMC_TYPEKIND_I2:
      ncTypeVal = NC_SHORT;
      break;
   case ESMC_TYPEKIND_I4:
      ncTypeVal = NC_INT;
      break;
   case ESMC_TYPEKIND_I8:
      ncTypeVal = NC_LONG; // TODO?: deprecated in netCDF - same ncInt
      break;
   case ESMC_TYPEKIND_R4:
      ncTypeVal = NC_FLOAT;
      break;
   case ESMC_TYPEKIND_R8:
      ncTypeVal = NC_DOUBLE;
      break;
   case ESMF_C8:
      ncTypeVal = NC_UNSPECIFIED;
      // TODO:  ncTypeVal = netCDF 8 byte complex type?
      break;
   case ESMF_C16:
      ncTypeVal = NC_UNSPECIFIED;
      // TODO:  ncTypeVal = netCDF 16 byte complex type?
      break;
   case ESMC_TYPEKIND_LOGICAL:
      ncTypeVal = NC_BYTE;
      break;
   case ESMC_TYPEKIND_CHARACTER:
      ncTypeVal = NC_CHAR;
      break;
   default:
      break;
   }
#endif

   return ncTypeVal;
}
