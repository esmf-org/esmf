// $Id: ESMC_Mach_F.C,v 1.2 2003/10/16 22:35:26 nscollins Exp $
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
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Machine.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Machine} class functions.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_machineinitialize)(int *language, int *status) {
           *status = ESMC_MachineInitialize((ESMC_MainLanguage) *language);
       }

       void FTN(c_esmc_machinefinalize)(int *status) {
           *status = ESMC_MachineFinalize();
       }

       void FTN(c_esmc_machineprint)(char *opts, int *status, int olen) {
           *status = Machine.ESMC_MachinePrint();
       }

}









