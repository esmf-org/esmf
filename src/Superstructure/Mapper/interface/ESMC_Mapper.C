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
#define ESMC_FILENAME "ESMC_Mapper.C"

//==============================================================================
//
// ESMC Mapper method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Mapper methods declared
// in the companion file ESMC_Mapper.h
//
//-----------------------------------------------------------------------------

// include associated header file

#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMC_Mapper.h"
#include "ESMCI_Mapper.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id$";

ESMC_Mapper ESMC_MapperCreate(ESMC_VM vm,
              int config_fname_len, const char *config_fname, int *rc)
{
  int localrc = ESMC_RC_NOT_IMPL;

  ESMC_Mapper mapper;
  ESMCI::VM *vmp = (ESMCI::VM *) (vm.ptr);
  mapper.ptr = (ESMCI::Mapper *) ESMCI_MapperCreate(vmp,
                                  config_fname_len,
                                  config_fname,
                                  rc);

  if(rc != NULL){
    *rc = ESMF_SUCCESS;
  }

  return mapper;
}

int ESMC_MapperSetConstraints(ESMC_Mapper mapper)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperSetCompConstraints(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperOptimize(ESMC_Mapper mapper, int *opt_threshold_reached)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperPrint(ESMC_Mapper mapper)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperSetCompInfo(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name,
      int comp_pet_range_start, int comp_pet_range_end,
      double comp_time_intvl_start, double comp_time_intvl_end)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperGetCompInfo(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name,
      int *comp_pet_range_start, int *comp_pet_range_end)
{
  return ESMC_RC_NOT_IMPL;
}

int ESMC_MapperDestroy(ESMC_Mapper mapper)
{
  if(mapper.ptr != NULL){
    ESMCI::Mapper *mptr = (ESMCI::Mapper *)(mapper.ptr);
    ESMCI_MapperDestroy(mptr);
    mapper.ptr = NULL;
  }
  return ESMF_SUCCESS;
}
