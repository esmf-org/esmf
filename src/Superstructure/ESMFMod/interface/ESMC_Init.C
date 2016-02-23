// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC Init implementation
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the C Init Code.
//
// !USES:
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// include higher level, 3rd party or system headers
#include <string.h>
#include <stdio.h>

// include ESMF headers
#include "ESMC.h"
#include "ESMCI.h"

// include associated header file
#include "ESMC_Init.h"

extern "C" {


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
  int ESMC_Initialize(
//
// !RETURN VALUE:
//  int return code
//
// !ARGUMENTS:
    int *rc,        // return code
    ...){           // optional arguments
//  
// !DESCRIPTION:
//
//EOP

    int localrc;
    ESMCI_ArgList   argPtr;
    ESMCI_ArgID     argID;

    ESMC_CalKind_Flag defaultCalendar       = ESMC_CALKIND_NOCALENDAR;
    char             *defaultConfigFilename = NULL;
    char             *defaultLogFilename    = NULL;
    ESMC_LogKind_Flag logtype               = ESMC_LOGKIND_MULTI;

    // check the optional argument list
    ESMCI_ArgStart(argPtr, rc);
    while ( (argID=ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
      switch ( argID ) {
        case ESMCI_InitArgDefaultCalKindID:
          ESMCI_ArgGetInt(argPtr);
          break;
        case ESMCI_InitArgDefaultConfigFilenameID:
          ESMCI_ArgGetString(argPtr);
          break;
        case ESMCI_InitArgLogFilenameID:
          ESMCI_ArgGetString(argPtr);
          break;
        case ESMCI_InitArgLogKindFlagID:
          ESMCI_ArgGetInt(argPtr);
          break;
        default:
          printf("ESMC_Initialize: Improperly specified optional argument list\n");
          return ESMC_RC_OPTARG_BAD;
      }
    }

    // parse the optional argument list
    ESMCI_ArgStart(argPtr, rc);
    while ( (argID=ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
      switch ( argID ) {
        case ESMCI_InitArgDefaultCalKindID:
          defaultCalendar = (ESMC_CalKind_Flag)ESMCI_ArgGetInt(argPtr);
          break;
        case ESMCI_InitArgDefaultConfigFilenameID:
          defaultConfigFilename = ESMCI_ArgGetString(argPtr);
          break;
        case ESMCI_InitArgLogFilenameID:
          defaultLogFilename = ESMCI_ArgGetString(argPtr);
          break;
        case ESMCI_InitArgLogKindFlagID:
          logtype = (ESMC_LogKind_Flag)ESMCI_ArgGetInt(argPtr);
          break;
        default:
          printf("ESMC_Initialize: Improperly specified optional argument list\n");
          return ESMC_RC_OPTARG_BAD;
      }
    }
    
    // todo: it may be better to go directly into F90 instead of using C++
    // todo: if this was implemented right it were to use the defaultConfigFile.
    localrc = ESMCI_Initialize(
        defaultConfigFilename,
        defaultCalendar,
        defaultLogFilename,
        logtype);
    
    // todo: use LogErr to do error handling for localrc
    if (rc != NULL) *rc = localrc;
    return localrc;

  } // end ESMC_Initialize
//-----------------------------------------------------------------------------

  //-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Finalize - Finalize the ESMF Framework
//
// !INTERFACE:
  int ESMC_Finalize(
//
// !RETURN VALUE:
//  int return code
//
// !ARGUMENTS:
    void){
//  
// !DESCRIPTION:
//
//EOP

    int localrc;
    
    // todo: it may be better to go directly into F90 instead of using C++
    localrc = ESMCI_Finalize();
    
    // todo: use LogErr to do error handling for localrc

    return localrc;

  } // end ESMC_Finalize
//-----------------------------------------------------------------------------

}; // end extern "C"
