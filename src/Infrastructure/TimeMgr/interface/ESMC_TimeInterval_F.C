// $Id: ESMC_TimeInterval_F.C,v 1.3 2003/03/24 17:41:38 eschwab Exp $
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
#include "ESMC.h"
#include "ESMC_TimeInterval.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt TimeInterval} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

#if 0
       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_timeintervalinit)(ESMC_TimeInterval **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalInit(*arg1, *arg2, *arg3);
       }

       void FTN(c_esmc_timeintervalget)(ESMC_TimeInterval **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeIntervalGet(&value);
       }

       void FTN(c_esmc_timeintervalset)(ESMC_TimeInterval **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_TimeIntervalSet(value);
       }

       void FTN(c_esmc_timeintervalvalidate)(ESMC_TimeInterval **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalValidate(opts);
       }

       void FTN(c_esmc_timeintervalprint)(ESMC_TimeInterval **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_TimeIntervalPrint(opts);
       }
#endif

};
