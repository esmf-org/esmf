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

#include <string.h>

// ESMF header
#include "ESMC.h"
#include "ESMCI_IO_NetCDF_Utils.h"

// ESMF Test header
#include "ESMCI_Test.h"

using namespace ESMCI;

//==============================================================================
//BOP
// !PROGRAM: ESMCI_IO_NetCDF_UtilsUTest - Check ESMCI_IO_NetCDF_Utils functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void)
{
   char name[128];
   char failMsg[128];
   int result = 0;
   int rc;
   bool allCorrect = true;
   ESMC_TypeKind_Flag esmcTypeVal;
   nc_type ncTypeVal;

   //----------------------------------------------------------------------------
   TestStart(__FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "ncerrToEsmcRc with non-error");
   strcpy(failMsg, "Did not return ESMF_SUCCESS");
   rc = NetCDFUtils::ncerrToEsmcRc(NC_NOERR);
   Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "ncerrToEsmcRc with error");
   strcpy(failMsg, "Did not return ESMF_FAILURE");
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   rc = NetCDFUtils::ncerrToEsmcRc(NC_EBADID);
#else
   // This is a dummy NetCDF error code, since we don't have a real NetCDF library
   rc = NetCDFUtils::ncerrToEsmcRc(NC_NOERR+1);
#endif
   Test((rc == ESMF_FAILURE), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "ncToEsmcType with a variety of valid NetCDF types");
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   strcpy(failMsg, "Did not return correct ESMC type for one or more NetCDF types");
#else
   strcpy(failMsg, "Did not return ESMF_NOKIND for one or more NetCDF types");
#endif
   const int num_vals = 6;
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   const nc_type ncTypeVals[num_vals] = {NC_BYTE, NC_CHAR, NC_SHORT, NC_INT, NC_FLOAT, NC_DOUBLE};
   const ESMC_TypeKind_Flag expectedEsmcTypeVals[num_vals] = {
      ESMC_TYPEKIND_I1, ESMC_TYPEKIND_CHARACTER, ESMC_TYPEKIND_I2,
      ESMC_TYPEKIND_I4, ESMC_TYPEKIND_R4, ESMC_TYPEKIND_R8
   };
#else
   const nc_type ncTypeVals[num_vals] = {NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED,
      NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED};
   const ESMC_TypeKind_Flag expectedEsmcTypeVals[num_vals] = {
      ESMF_NOKIND, ESMF_NOKIND, ESMF_NOKIND,
      ESMF_NOKIND, ESMF_NOKIND, ESMF_NOKIND
   };
#endif
   allCorrect = true;
   for (int i = 0; i < num_vals; ++i) {
      esmcTypeVal = NetCDFUtils::ncToEsmcType(ncTypeVals[i]);
      if (esmcTypeVal != expectedEsmcTypeVals[i]) {
         allCorrect = false;
      }
   }
   Test(allCorrect, name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "ncToEsmcType with an invalid NetCDF type");
   strcpy(failMsg, "Did not return ESMF_NOKIND");
   esmcTypeVal = NetCDFUtils::ncToEsmcType(NC_UNSPECIFIED);
   Test((esmcTypeVal == ESMF_NOKIND), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "esmcToNcType with a variety of valid ESMC types");
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   strcpy(failMsg, "Did not return correct NetCDF type for one or more ESMC types");
#else
   strcpy(failMsg, "Did not return NC_UNSPECIFIED for one or more ESMC types");
#endif
   const int num_vals2 = 10;
   const ESMC_TypeKind_Flag esmcTypeVals[num_vals2] = {
      ESMC_TYPEKIND_I1, ESMC_TYPEKIND_I2, ESMC_TYPEKIND_I4,
      ESMC_TYPEKIND_I8, ESMC_TYPEKIND_R4, ESMC_TYPEKIND_R8,
      ESMF_C8, ESMF_C16, ESMC_TYPEKIND_LOGICAL, ESMC_TYPEKIND_CHARACTER
   };
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
   const nc_type expectedNcTypeVals[num_vals2] = {
      NC_BYTE, NC_SHORT, NC_INT,
      NC_LONG, NC_FLOAT, NC_DOUBLE,
      NC_UNSPECIFIED, NC_UNSPECIFIED, NC_BYTE, NC_CHAR
   };
#else
   const nc_type expectedNcTypeVals[num_vals2] = {
      NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED,
      NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED,
      NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED, NC_UNSPECIFIED
   };
#endif
   allCorrect = true;
   for (int i = 0; i < num_vals2; ++i) {
      ncTypeVal = NetCDFUtils::esmcToNcType(esmcTypeVals[i]);
      if (ncTypeVal != expectedNcTypeVals[i]) {
         allCorrect = false;
      }
   }
   Test(allCorrect, name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   //NEX_UTest
   strcpy(name, "esmcToNcType with an invalid ESMC type");
   strcpy(failMsg, "Did not return NC_UNSPECIFIED");
   ncTypeVal = NetCDFUtils::esmcToNcType(ESMF_NOKIND);
   Test((ncTypeVal == NC_UNSPECIFIED), name, failMsg, &result, __FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   //----------------------------------------------------------------------------
   TestEnd(__FILE__, __LINE__, 0);
   //----------------------------------------------------------------------------

   return 0;
}