// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include "ESMC_Mapper.h"
#include "ESMCI_Mapper.h"

namespace ESMCI{
extern "C"{

  void FTN_X(c_esmc_mappercreate)(Mapper **ptr,
                                  ESMC_VM vm,
                                  int *config_fname_len,
                                  const char *config_fname,
                                  int *status,
                                  ESMCI_FortranStrLenArg config_fname_l)
  {
    ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);
    *ptr = ESMCI_MapperCreate(vmp, *config_fname_len,
            config_fname, status);
  }

  void FTN_X(c_esmc_mapperdestroy)(Mapper **ptr,
                                    int *status)
  {
    int local_status;
    local_status = ESMCI_MapperDestroy(*ptr);
    if(status){
      *status = local_status;
    }
  }

} // extern "C"
} // namespace ESMCI
