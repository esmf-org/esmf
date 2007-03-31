// $Id: ESMC_F90Interface_F.C,v 1.4 2007/03/31 05:51:27 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_F90Interface_F.C"
//==============================================================================

#include "ESMC_F90Interface.h"
#include "ESMC_Start.h"

// the interface subroutine names MUST be in lower case by ESMF convention

extern "C" {

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_InterfaceInt interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_interfaceintcreate1d)(ESMC_InterfaceInt **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintcreate1d()"
    *array = new ESMC_InterfaceInt(farray, 1, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintcreate2d)(ESMC_InterfaceInt **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintcreate2d()"
    *array = new ESMC_InterfaceInt(farray, 2, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintcreate3d)(ESMC_InterfaceInt **array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintcreate3d()"
    *array = new ESMC_InterfaceInt(farray, 3, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_interfaceintdestroy)(ESMC_InterfaceInt **array,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interfaceintdestroy()"
    delete *array;
    *rc = ESMF_SUCCESS;
  }

};
