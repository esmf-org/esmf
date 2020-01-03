// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
//
// Main include file of the pure C public ESMC API
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_H
#define ESMC_H

// platform specific configuration details
#include "ESMC_Conf.h"

// optional arguments in ESMC interfaces
#include "ESMC_Arg.h"

// interface types
#include "ESMC_Interface.h"

// Infrastructure headers
#include "ESMC_Util.h"
#include "ESMC_VM.h"
#include "ESMC_RHandle.h"
#include "ESMC_DistGrid.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_Array.h"
#include "ESMC_Config.h"
#include "ESMC_Calendar.h"
#include "ESMC_Time.h"
#include "ESMC_TimeInterval.h"
#include "ESMC_Clock.h"
#include "ESMC_Mesh.h"
#include "ESMC_Grid.h"
#include "ESMC_XGrid.h"
#include "ESMC_Field.h"
#include "ESMC_LocStream.h"

// Superstructure headers
#include "ESMC_State.h"
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"
#include "ESMC_SciComp.h"

// framework-wide initialization and finalization
#include "ESMC_Init.h"

// --- include these for now, but need re-consideration and clean-up ---
#include "ESMC_LogErr.h"
#include "ESMC_Macros.h"
// ---------------------------------------------------------------------

#endif  // ESMC_H

