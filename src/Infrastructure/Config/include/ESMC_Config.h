// $Id: ESMC_Config.h,v 1.8.2.2 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Config class public include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Config_H
#define ESMC_Config_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Config - C++ interface to the F90 Config object
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Config members and declares method 
// signatures (prototypes).  The companion file ESMC\_Config.C contains
// the definitions (full code bodies) for the Config methods.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMCI_Util.h"
#include "ESMC_F90Interface.h"
#include "ESMC_Arg.h"

// Optional argument identifier list for the ESMC_Config API.
enum {
  ESMCI_ConfigArgLabelID   = ESMCI_ArgBaseID,  // char*
  ESMCI_ConfigArgDvalueID,                     // type depends on TypeKind
  ESMCI_ConfigArgCountID,                      // int
  ESMCI_ConfigArgUniqueID,                     // int
  ESMCI_ConfigArgTableEndID,                   // int*
  ESMCI_ConfigArgOptionsID,                    // char*
};

// Argument expansion macros for the ESMC_Config API.
#define ESMC_ConfigArgLabel(ARG)      ESMCI_Arg(ESMCI_ConfigArgLabelID,ARG)
#define ESMC_ConfigArgDvalue(ARG)     ESMCI_Arg(ESMCI_ConfigArgDvalueID,ARG)
#define ESMC_ConfigArgCount(ARG)      ESMCI_Arg(ESMCI_ConfigArgCountID,ARG)
#define ESMC_ConfigArgUnique(ARG)     ESMCI_Arg(ESMCI_ConfigArgUniqueID,ARG)
#define ESMC_ConfigArgTableEnd(ARG)   ESMCI_Arg(ESMCI_ConfigArgTableEndID,ARG)
#define ESMC_ConfigArgOptions(ARG)    ESMCI_Arg(ESMCI_ConfigArgOptionsID,ARG)


extern "C" {

// class declaration type
typedef struct {

    // pointer to fortran derived type
    ESMC_F90ClassHolder* f90this;


//
//EOP
//-----------------------------------------------------------------------------

} ESMC_Config ; // end class ESMC_Config


// prototypes for the ESMC_Config API

ESMC_Config* ESMC_ConfigCreate(int* rc);

int ESMC_ConfigDestroy(ESMC_Config* config);

int ESMC_ConfigLoadFile(ESMC_Config* config, char* fname, ...);

int ESMC_ConfigFindLabel(ESMC_Config* config, char* label);

int ESMC_ConfigNextLine(ESMC_Config* config, ...);

int ESMC_ConfigGetChar(ESMC_Config* config, char* value, ...);

int ESMC_ConfigGetLen(ESMC_Config* config, int* wordCount, ...);

int ESMC_ConfigGetDim(ESMC_Config* config, int* lineCount, int* columnCount, ...);

int ESMC_ConfigValidate(ESMC_Config* config, ...);

int ESMC_ConfigGetAttribute(ESMC_Config* config, void* value, ESMC_TypeKind tk, ...);

int ESMC_ConfigSetAttribute(ESMC_Config* config, void* value, ESMC_TypeKind tk, ...);


}; // end extern "C"


#endif  // ESMC_Config_H

