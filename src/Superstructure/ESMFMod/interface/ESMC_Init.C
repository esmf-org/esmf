// $Id: ESMC_Init.C,v 1.9 2007/04/12 23:09:57 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
#include "ESMCI.h"      // todo: remove this C++ interf. dependency

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
//  int error return code
//
// !ARGUMENTS:
    void){
//  
// !DESCRIPTION:
//
//EOP

    int localrc;
    
    // todo: it may be better to go directly into F90 instead of using C++
    localrc = ESMCI_Initialize();
    
    // todo: use LogErr to do error handling for localrc

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
//  int error return code
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
