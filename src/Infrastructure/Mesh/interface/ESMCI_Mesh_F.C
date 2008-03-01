// $Id: ESMCI_Mesh_F.C,v 1.2 2008/03/01 01:07:24 theurich Exp $
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
#define ESMC_FILENAME "ESMC_Mesh_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMC_RHandle.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMC_Mesh.h"


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Mesh} class functions.
//
//EOP
//-------------------------------------------------------------------------
extern "C" void FTN(c_esmc_meshcreate)(ESMC::Mesh **meshpp,
                         int *pdim, int *sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreate()"

    int localrc;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    *meshpp = new ESMC::Mesh();

    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(*sdim);

    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));



} // meshcreate





