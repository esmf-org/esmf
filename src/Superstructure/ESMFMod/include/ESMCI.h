// $Id: ESMCI.h,v 1.8.2.7 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//

// main include file which includes all others

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_H
#define ESMCI_H


// common macros and constants, #defines
#include "ESMC_Start.h"

// base class, I/O, and error
#include "ESMC_Base.h"
#include "ESMC_IOSpec.h"
#include "ESMC_LogErr.h"

// optional arguments in ESMC interfaces
#include "ESMCI_Arg.h"

// time manager
#include "ESMC_Fraction.h"
#include "ESMC_BaseTime.h"
#include "ESMC_Calendar.h"
#include "ESMC_TimeInterval.h"
#include "ESMC_Time.h"
#include "ESMC_Alarm.h"
#include "ESMC_Clock.h"

// local array, datamap
#include "ESMCI_ArraySpec.h"
#include "ESMC_LocalArray.h"
#include "ESMC_InternArrayDataMap.h"

// vm and layout
#include "ESMC_VMKernel.h"
#include "ESMC_VM.h"

// array, igrid, field, bundle, comms
#include "ESMC_InternArray.h"
#include "ESMC_Grid.h"

// config
#include "ESMCI_Config.h"

// components and related items
#include "ESMC_State.h"
#include "ESMC_FTable.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"

// framework-wide initialization and finalization
#include "ESMCI_Init.h"


#endif  // ESMCI_H

