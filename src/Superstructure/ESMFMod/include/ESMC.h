// $Id: ESMC.h,v 1.19 2008/07/10 15:48:03 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//

// main include file which includes all others

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_H
#define ESMC_H

// optional arguments in ESMC interfaces
#include "ESMC_Arg.h"

// interface types
#include "ESMC_Interface.h"

// Infrastructure headers
#include "ESMC_Config.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_DistGrid.h"
#include "ESMC_Array.h"
#include "ESMC_Calendar.h"
#include "ESMC_Time.h"

// Superstructure headers
#include "ESMC_State.h"

// framework-wide initialization and finalization
#include "ESMC_Init.h"


#endif  // ESMC_H

