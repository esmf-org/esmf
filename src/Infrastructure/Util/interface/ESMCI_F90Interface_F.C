// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_F90Interface_F.C"
//==============================================================================

#include "ESMC_Util.h"
#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"

// the interface subroutine names MUST be in lower case by ESMF convention

extern "C" {

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_InterArray interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN_X(c_esmc_interarraysetinvalid)(ESMCI::InterArray<int> *array,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraysetinvalid()"
    array->set();
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate1d)(ESMCI::InterArray<int> *array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate1d()"
    array->set(farray, 1, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate2d)(ESMCI::InterArray<int> *array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate2d()"
    array->set(farray, 2, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate3d)(ESMCI::InterArray<int> *array, 
    int *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate3d()"
    array->set(farray, 3, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraysetinvalidi8)(ESMCI::InterArray<ESMC_I8> *array,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraysetinvalid()"
    array->set();
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate1di8)(ESMCI::InterArray<ESMC_I8> *array, 
    ESMC_I8 *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate1di8()"
    array->set(farray, 1, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate2di8)(ESMCI::InterArray<ESMC_I8> *array, 
    ESMC_I8 *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate2di8()"
    array->set(farray, 2, len);
    *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_interarraycreate3di8)(ESMCI::InterArray<ESMC_I8> *array, 
    ESMC_I8 *farray, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_interarraycreate3di8()"
    array->set(farray, 3, len);
    *rc = ESMF_SUCCESS;
  }

};
