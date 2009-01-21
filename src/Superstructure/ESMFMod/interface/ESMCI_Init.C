// $Id: ESMCI_Init.C,v 1.1.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Component method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Initialization and Finalization
// interfaces to the functions in the companion file {\tt ESMF\_Init.F90}.  
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <string.h>
#include <stdio.h>
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"
#include "ESMCI_Init.h"

// public globals, to be filled in by ESMC_Initialize()
//  and used by MPI_Init().   set once, treat as read-only!
int globalargc;
char **globalargv;


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMCI_Initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *defaultConfigFilename,         // in - default config filename
      ESMC_CalendarType defaultCalendar,   // in - optional time manager
                                           //      default calendar type
      char *defaultLogFilename,            // in - default log filename
      ESMC_LogType defaultLogType) {       // in - default log type (single/multi)
//  
// !DESCRIPTION:
//
//EOP

    int rc;
    ESMCI_MainLanguage l = ESMF_MAIN_C;

    globalargc = 0;
    globalargv = NULL;

    FTN(f_esmf_frameworkinitialize)((int*)&l, defaultConfigFilename, 
                                    &defaultCalendar, defaultLogFilename, 
                                    &defaultLogType, &rc,
                                    strlen (defaultConfigFilename),
                                    strlen (defaultLogFilename));

    return rc;

 } // end ESMCI_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMCI_Initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType defaultCalendar) { // in - optional time manager
                                           //      default calendar type
//
// !DESCRIPTION:
//
//EOP

    int rc;
    ESMCI_MainLanguage l = ESMF_MAIN_C;
    ESMC_LogType lt = ESMC_LOG_SINGLE;

    globalargc = 0;
    globalargv = NULL;

    FTN(f_esmf_frameworkinitialize)((int*)&l, NULL, &defaultCalendar, NULL,
                                    &lt, &rc, 0, 0);

    return rc;

 } // end ESMCI_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMCI_Initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int argc, char **argv,       // in - the arguments to the program
      ESMC_CalendarType defaultCalendar) {  // in - optional time manager
                                            //      default calendar type
//
// !DESCRIPTION:
//
//EOP

    int rc;
    ESMCI_MainLanguage l = ESMF_MAIN_C;
    ESMC_LogType lt = ESMC_LOG_SINGLE;

    // make this public so the mpi init code in Machine can grab them.
    globalargc = argc;
    globalargv = argv;

    FTN(f_esmf_frameworkinitialize)((int*)&l, NULL, &defaultCalendar, NULL, 
                                    &lt, &rc, 0, 0);

    return rc;

 } // end ESMCI_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_Finalize - Finalize the ESMF Framework
//
// !INTERFACE:
      int ESMCI_Finalize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP

    int rc;

    FTN(f_esmf_frameworkfinalize)(&rc);

    return rc;

 } // end ESMCI_FrameworkFinallize

