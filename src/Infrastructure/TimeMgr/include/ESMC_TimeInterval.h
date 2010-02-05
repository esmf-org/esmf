// $Id: ESMC_TimeInterval.h,v 1.55.2.1 2010/02/05 20:00:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_TimeInterval_H
#define ESMC_TimeInterval_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_TimeInterval - Public C interface to the ESMF TimeInterval class
//
// !DESCRIPTION:
//
// The code in this file defines the public C TimeInterval interfaces and
// declares method signatures (prototypes).  The companion file
// {\tt ESMC\_TimeInterval.C} contains the definitions (full code bodies) for
// the TimeInterval methods.
//
//EOPI
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"
#include "ESMC_Time.h"
#include "ESMC_Calendar.h"

// TODO: these definitions need different home (shared with ESMCI_TimeInterval)
enum ESMC_ComparisonType {ESMC_EQ, ESMC_NE,
                          ESMC_LT, ESMC_GT,
                          ESMC_LE, ESMC_GE};
enum ESMC_AbsValueType {ESMC_POSITIVE_ABS, ESMC_NEGATIVE_ABS};

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct { 
  // private:  // Members opaque on C side, philosophically.
    // Allocate enough memory to store members on the C side.
    // Adjust if members are added, rounding up to multiples of
    // 8 bytes (64 bits - largest machine word size).  Add 8 bytes extra
    // padding; match 'type ESMF_TimeInterval' in ESMF_TimeIntervalType.F90.
    // TODO:  implement isInit initialization like in F90 API?
    char shallowMem[152];  // 18 8-byte members + 1 8-bytes extra = 19 * 8
} ESMC_TimeInterval;

// Class API
int ESMC_TimeIntervalSet(ESMC_TimeInterval *timeInterval,       
                         ESMC_I4 h_I4);

int ESMC_TimeIntervalGet(ESMC_TimeInterval timeInterval,
                         ESMC_I8 *s_I8,
                         ESMC_R8 *h_R8);

int ESMC_TimeIntervalPrint(ESMC_TimeInterval timeInterval);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_TimeInterval_H
