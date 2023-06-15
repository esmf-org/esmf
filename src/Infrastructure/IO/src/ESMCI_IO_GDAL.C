// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_NetCDF method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_GDAL} methods declared
// in the companion file {\tt ESMCI\_IO\_GDAL.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_GDAL.C"

// associated class definition file
#include "ESMCI_IO_GDAL.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <string>
#include <sstream>

#include "ESMC_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_Array.h"

using namespace std;

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI
{
//-------------------------------------------------------------------------

  ESMC_TypeKind_Flag  IO_GDAL::gdalToEsmcType(OGRFieldType gdalTypeVal)
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_GDAL::gdalToEsmcType"

    ESMC_TypeKind_Flag  esmcTypeVal = ESMF_NOKIND;

    // Currently only integer and double (real*8) are implemented

    switch (gdalTypeVal)
    {
    case OFTInteger:
      esmcTypeVal = ESMC_TYPEKIND_I4;
      break;
    case OFTIntegerList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTReal:
      esmcTypeVal = ESMC_TYPEKIND_R8;
      break;
    case OFTRealList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTString:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTStringList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTBinary:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTDate:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTTime:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTDateTime:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTInteger64:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTInteger64List:
      esmcTypeVal = ESMF_NOKIND;
      break;
    default:
      break;
    }

    return esmcTypeVal;
  }


//-------------------------------------------------------------------------

}  // end namespace ESMCI
