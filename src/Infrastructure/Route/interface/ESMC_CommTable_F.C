// $Id: ESMC_CommTable_F.C,v 1.1 2003/03/10 23:20:21 nscollins Exp $
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
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_CommTable.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt CommTable} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_commtablecreate)(ESMC_CommTable **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_CommTableCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_commtabledestroy)(ESMC_CommTable **ptr, int *status) {
           *status = ESMC_CommTableDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_commtableinit)(ESMC_CommTable **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_CommTableInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_commtablegetconfig)(ESMC_CommTable **ptr, 
                                         ESMC_CommTableConfig *config, int *status} {
           *status = (*ptr)->ESMC_CommTableGetConfig(&config);
       }

       void FTN(c_esmc_commtablesetconfig)(ESMC_CommTable **ptr, 
                                         ESMC_CommTableConfig *config, int *status} {
           *status = (*ptr)->ESMC_CommTableSetConfig(config);
       }

       void FTN(c_esmc_commtableget)(ESMC_CommTable **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_CommTableGet(&value);
       }

       void FTN(c_esmc_commtableset)(ESMC_CommTable **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_CommTableSet(value);
       }

       void FTN(c_esmc_commtablevalidate)(ESMC_CommTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CommTableValidate(opts);
       }

       void FTN(c_esmc_commtableprint)(ESMC_CommTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CommTablePrint(opts);
       }

};


