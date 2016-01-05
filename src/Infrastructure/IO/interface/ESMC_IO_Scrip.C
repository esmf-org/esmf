// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
#include "ESMCI_IO_Scrip.h"

extern "C" {
  void ESMC_ScripInq (const char *, int *, int *, int *);
}
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ScripInq"
void ESMC_ScripInq (const char *filename, int *grid_dims, int *grid_rank,
                    int *rc)
{
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
  ESMCI::IO_Scrip::inq(filename, grid_dims, grid_rank, &localrc);
  localrc = ESMF_SUCCESS;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
				    rc)) return; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}  
