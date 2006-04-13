// $Id: ESMC_F90Interface_F.C,v 1.1 2006/04/13 23:20:01 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_F90Interface_F.C"
//==============================================================================

#include "ESMC_F90Interface.h"
#include "ESMC_Start.h"

// the interface subroutine names MUST be in lower case by ESMF convention

extern "C" {

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_InterfaceIntArray interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_interfaceintarraycreate1d)(ESMC_InterfaceIntArray **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintarraycreate1d()"
    *array = new ESMC_InterfaceIntArray(farray, 1, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintarraycreate2d)(ESMC_InterfaceIntArray **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintarraycreate2d()"
    *array = new ESMC_InterfaceIntArray(farray, 2, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintarraycreate3d)(ESMC_InterfaceIntArray **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintarraycreate3d()"
    *array = new ESMC_InterfaceIntArray(farray, 3, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintarraydestroy)(ESMC_InterfaceIntArray **array,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintarraydestroy()"
    delete *array;
    *rc = ESMF_SUCCESS;
  }

};
