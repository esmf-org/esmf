// $Id: ESMC_TimeInterval.h,v 1.53 2008/09/03 23:32:30 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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

#include "ESMC_Util.h"

enum ESMC_ComparisonType {ESMC_EQ, ESMC_NE,
                          ESMC_LT, ESMC_GT,
                          ESMC_LE, ESMC_GE};

enum ESMC_AbsValueType {ESMC_POSITIVE_ABS, ESMC_NEGATIVE_ABS};

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_TimeInterval - Public C interface to the ESMF TimeInterval class
//
// !DESCRIPTION:
//
// The code in this file defines the public C TimeInterval class and declares 
// method signatures (prototypes).  The companion file {\tt ESMC\_TimeInterval.C}
// contains the definitions (full code bodies) for the TimeInterval methods.
//
//EOPI
//-----------------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_TimeInterval;

// Class API

int ESMC_TimeIntervalSet(ESMC_TimeInterval*, ESMC_I4);

int ESMC_TimeIntervalGet(ESMC_TimeInterval, ESMC_I8*, ESMC_R8*);

int ESMC_TimeIntervalPrint(ESMC_TimeInterval);

#ifdef __cplusplus
} //extern "C"
#endif

#endif // ESMC_TimeInterval_H
