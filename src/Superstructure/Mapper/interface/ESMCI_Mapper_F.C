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

  void FTN_X(c_esmc_mappersetconstraints)(Mapper **ptr, int *status)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mappersetcompconstraints)(Mapper **ptr,
    int *comp_name_len,
    const char *comp_name,
    int *phase_name_len,
    const char *phase_name,
    int *status,
    ESMCI_FortranStrLenArg comp_name_l,
    ESMCI_FortranStrLenArg phase_name_l)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mapperoptimize)(Mapper **ptr,
              int *opt_threshold_reached, int *status)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    int lopt_threshold_reached;
    lstatus = ESMCI_MapperOptimize(*ptr, &lopt_threshold_reached);
    if(opt_threshold_reached){
      *opt_threshold_reached = lopt_threshold_reached;
    }
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mapperprint)(Mapper **ptr, int *status)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mappersetcompinfo)(Mapper **ptr,
    int *comp_name_len,
    const char *comp_name,
    int *phase_name_len,
    const char *phase_name,
    int *comp_pet_range_start,
    int *comp_pet_range_end,
    double *comp_time_intvl_start,
    double *comp_time_intvl_end,
    int *status,
    ESMCI_FortranStrLenArg comp_name_l,
    ESMCI_FortranStrLenArg phase_name_l)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    lstatus = ESMCI_MapperSetCompInfo(*ptr,
                    *comp_name_len, comp_name,
                    *phase_name_len, phase_name,
                    *comp_pet_range_start,
                    *comp_pet_range_end,
                    *comp_time_intvl_start,
                    *comp_time_intvl_end);
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mappergetcompinfo)(Mapper **ptr,
    int *comp_name_len,
    const char *comp_name,
    int *phase_name_len,
    const char *phase_name,
    int *comp_pet_range_start,
    int *comp_pet_range_end,
    int *status,
    ESMCI_FortranStrLenArg comp_name_l,
    ESMCI_FortranStrLenArg phase_name_l)
  {
    int lstatus = ESMC_RC_NOT_IMPL;
    int lcomp_pet_range_start, lcomp_pet_range_end;

    lstatus = ESMCI_MapperGetCompInfo(*ptr,
                *comp_name_len, comp_name,
                *phase_name_len, phase_name,
                &lcomp_pet_range_start, &lcomp_pet_range_end);
    if(comp_pet_range_start){
      *comp_pet_range_start = lcomp_pet_range_start;
    }
    if(comp_pet_range_end){
      *comp_pet_range_end = lcomp_pet_range_end;
    }
    if(status){
      *status = lstatus;
    }
  }

  void FTN_X(c_esmc_mapperdestroy)(Mapper **ptr,
                                    int *status)
  {
    int lstatus;
    lstatus = ESMCI_MapperDestroy(*ptr);
    if(status){
      *status = lstatus;
    }
  }

} // extern "C"
} // namespace ESMCI
