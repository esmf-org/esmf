// $Id: ESMC_Init.C,v 1.3 2004/04/23 20:22:58 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
//#include "ESMC_Machine.h"
#include "ESMC_Init.h"

// public globals, to be filled in by ESMC_Initialize()
//  and used by MPI_Init().   set once, treat as read-only!
int globalargc;
char **globalargv;


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMC_Initialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *defaultConfigFilename,         // in - default config filename
      ESMC_CalendarType defaultCalendar,   // in - optional time manager
                                           //      default calendar type
      char *defaultLogFilename) {          // in - default log filename
//  
// !DESCRIPTION:
//
//EOP

    int rc;
    ESMC_MainLanguage l = ESMF_MAIN_C;

    globalargc = 0;
    globalargv = NULL;

    FTN(f_esmf_frameworkinitialize)((int*)&l, defaultConfigFilename, 
                                    &defaultCalendar, defaultLogFilename, &rc);

    return rc;

 } // end ESMC_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMC_Initialize(
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
    ESMC_MainLanguage l = ESMF_MAIN_C;

    globalargc = 0;
    globalargv = NULL;

    FTN(f_esmf_frameworkinitialize)((int*)&l, NULL, &defaultCalendar, NULL, &rc);

    return rc;

 } // end ESMC_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
      int ESMC_Initialize(
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
    ESMC_MainLanguage l = ESMF_MAIN_C;

    // make this public so the mpi init code in Machine can grab them.
    globalargc = argc;
    globalargv = argv;

    FTN(f_esmf_frameworkinitialize)((int*)&l, NULL, &defaultCalendar, NULL, &rc);

    return rc;

 } // end ESMC_Initialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Finalize - Finalize the ESMF Framework
//
// !INTERFACE:
      int ESMC_Finalize(
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

 } // end ESMC_FrameworkFinallize

