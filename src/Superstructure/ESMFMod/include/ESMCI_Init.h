// $Id: ESMCI_Init.h,v 1.1.2.4 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Init include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_Init_H
#define ESMCI_Init_H

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the Init Code.
//
// 
//

// !USES:
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.
#include "ESMC_Calendar.h"

// public globals, filled in by ESMC_Initialize()
//  and used by MPI_Init().   set once, treat as read-only!

extern int globalargc;
extern char **globalargv;


// keep in sync with Fortran
enum ESMCI_MainLanguage { ESMF_MAIN_C=1, ESMF_MAIN_F90 };


// prototypes for C routines
int ESMCI_Initialize(char *defaultConfigFilename,
  ESMC_CalendarType defaultCalendar=ESMC_CAL_NOCALENDAR,
  char *defaultLogFilename=NULL, ESMC_LogType defaultLogType=ESMC_LOG_SINGLE);

int ESMCI_Initialize(ESMC_CalendarType defaultCalendar=ESMC_CAL_NOCALENDAR);

int ESMCI_Initialize(int argc, char **argv, 
  ESMC_CalendarType defaultCalendar=ESMC_CAL_NOCALENDAR);

int ESMCI_Finalize(void);


// prototypes for fortran interface routines
extern "C" {
   void FTN(f_esmf_frameworkinitialize)(int *language, 
                                        char *defaultConfigFileName,
                                        ESMC_CalendarType *defaultCalendar,
                                        char *defaultLogFileName,
                                        ESMC_LogType *defaultLogType,
                                        int *rc, ESMCI_FortranStrLenArg count1,
					ESMCI_FortranStrLenArg count2);
   void FTN(f_esmf_frameworkfinalize)(int *rc);
};


#endif  // ESMCI_Init_H
