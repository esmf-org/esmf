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

// main include file which includes all others

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_H
#define ESMCI_H


// common macros and constants, #defines
#include "ESMCI_Macros.h"

// base class, I/O, and error
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"

// optional arguments in ESMC interfaces
#include "ESMCI_Arg.h"

// time manager
#include "ESMCI_Fraction.h"
#include "ESMCI_BaseTime.h"
#include "ESMCI_Calendar.h"
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Time.h"
#include "ESMCI_Alarm.h"
#include "ESMCI_Clock.h"

// attribute
#include "ESMCI_Attribute.h"

// local array, datamap
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"

// array, igrid, field, bundle, comms
#include "ESMCI_Grid.h"

// components and related items
#include "ESMCI_State.h"

// framework-wide initialization and finalization
#include "ESMCI_Init.h"


#endif  // ESMCI_H

