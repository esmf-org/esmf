// $Id: ESMC.h,v 1.16 2008/02/29 23:10:48 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research,
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

// Infrastructure headears
#include "ESMC_Config.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_Array.h"

// Superstructure headers
#include "ESMC_State.h"

// framework-wide initialization and finalization
#include "ESMC_Init.h"


#endif  // ESMC_H

