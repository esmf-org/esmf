// $Id: ESMC_Calendar_F.C,v 1.2 2003/03/22 05:46:04 eschwab Exp $
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
#include "ESMC_Calendar.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Calendar} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_calendarinit)(ESMC_Calendar **ptr,
                                     ESMC_CalendarType_e *type, 
                                     int *status) {
           *status = (*ptr)->ESMC_CalendarInit(*type);
       }

#if 0
       void FTN(c_esmc_calendarget)(ESMC_Calendar **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_CalendarGet(&value);
       }

       void FTN(c_esmc_calendarset)(ESMC_Calendar **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_CalendarSet(value);
       }

       void FTN(c_esmc_calendarvalidate)(ESMC_Calendar **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CalendarValidate(opts);
       }

       void FTN(c_esmc_calendarprint)(ESMC_Calendar **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CalendarPrint(opts);
       }
#endif

};


