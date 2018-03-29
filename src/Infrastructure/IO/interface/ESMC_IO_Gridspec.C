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
// Retrieve information from a GRIDSPEC-type NetCDF file.

//--------------------------------------------------------------------------
// Function to return the rank and dimensions from a GRIDSPEC NetCDF file.

#include <stdlib.h>
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_IO_Gridspec.h"

extern "C" {
  void ESMC_GridspecInq (const char *, int *, int *, int *);
}
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_GridspecInq"
void ESMC_GridspecInq (const char *filename, int *ndims, int *grid_dims, int *rc)
{
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
  ESMCI::IO_Gridspec::inq(filename, ndims, grid_dims, &localrc);
  /////////////////////////start test
  //*ndims = 42;
  //grid_dims[0] = 4;
  //grid_dims[1] = 2;
  //////////////////////////end test
  localrc = ESMF_SUCCESS;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    rc)) return; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
