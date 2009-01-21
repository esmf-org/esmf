// $Id: ESMCI_Config.h,v 1.4.2.3 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Config class internal include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_Config_H
#define ESMCI_Config_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Config - C++ interface to the F90 Config object
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC Config class prototypes for the
// fortran interface routines.  The companion file ESMF\_Config_C.F90 contains
// the definitions (full code bodies) for the interface routines.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Config.h"
#include "ESMCI_Util.h"
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
extern "C" {

  void FTN(f_esmf_configcreate)(ESMC_Config* config, int* rc);

  void FTN(f_esmf_configdestroy)(ESMC_Config* config, int* rc);

  void FTN(f_esmf_configloadfile)(ESMC_Config* config, char* fname, int* unique,
                            //    int* rc          );
                                  int* rc, ESMCI_FortranStrLenArg flen);

  void FTN(f_esmf_configfindlabel)(ESMC_Config* config, char* label, int* rc,
				ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_confignextline)(ESMC_Config* config, int* ltable, int* rc);

  void FTN(f_esmf_configgetchar)(ESMC_Config* config, char* value, char* label,
                                 char* dvalue, int* rc, ESMCI_FortranStrLenArg,
				ESMCI_FortranStrLenArg llen, ESMCI_FortranStrLenArg);

  void FTN(f_esmf_configgetlen)(ESMC_Config* config, int* wordCount, char* label,
                                int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetdim)(ESMC_Config* config, int* lineCount, int* columnCount,
                                char* label, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configvalidate)(ESMC_Config* config, char* options, int* rc,
				ESMCI_FortranStrLenArg olen);

  //
  // Functions for ConfigGetAttribute interface
  //
  void FTN(f_esmf_configgetstring)(ESMC_Config* config, char* value, char* label,
                                   char* dvalue, int* rc, ESMCI_FortranStrLenArg vlen,
				ESMCI_FortranStrLenArg llen, ESMCI_FortranStrLenArg dlen);

  void FTN(f_esmf_configgetinti4)(ESMC_Config* config, ESMC_I4* value, char* label,
                                  ESMC_I4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetinti8)(ESMC_Config* config, ESMC_I8* value, char* label,
                                  ESMC_I8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetfloatr4)(ESMC_Config* config, ESMC_R4* value, char* label,
                                    ESMC_R4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetfloatr8)(ESMC_Config* config, ESMC_R8* value, char* label,
                                    ESMC_R8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetlogical)(ESMC_Config* config, int* value, char* label,
                                    int* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetintsi4)(ESMC_Config* config, int* count,
    ESMC_I4* value, char* label, ESMC_I4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetintsi8)(ESMC_Config* config, int* count,
    ESMC_I8* value, char* label, ESMC_I8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetfloatsr4)(ESMC_Config* config, int* count,
    ESMC_R4* value, char* label, ESMC_R4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetfloatsr8)(ESMC_Config* config, int* count,
    ESMC_R8* value, char* label, ESMC_R8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN(f_esmf_configgetlogicals)(ESMC_Config* config, int* count,
    int* value, char* label, int* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

}; // end prototypes for fortran interface

#endif  // ESMCI_Config_H

