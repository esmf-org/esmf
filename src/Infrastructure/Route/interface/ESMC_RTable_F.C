// $Id: ESMC_RTable_F.C,v 1.1 2003/03/10 23:20:22 nscollins Exp $
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
#include "ESMC_RTable.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt RTable} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_rtablecreate)(ESMC_RTable **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_RTableCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_rtabledestroy)(ESMC_RTable **ptr, int *status) {
           *status = ESMC_RTableDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_rtableinit)(ESMC_RTable **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_RTableInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_rtablegetconfig)(ESMC_RTable **ptr, 
                                         ESMC_RTableConfig *config, int *status} {
           *status = (*ptr)->ESMC_RTableGetConfig(&config);
       }

       void FTN(c_esmc_rtablesetconfig)(ESMC_RTable **ptr, 
                                         ESMC_RTableConfig *config, int *status} {
           *status = (*ptr)->ESMC_RTableSetConfig(config);
       }

       void FTN(c_esmc_rtableget)(ESMC_RTable **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_RTableGet(&value);
       }

       void FTN(c_esmc_rtableset)(ESMC_RTable **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_RTableSet(value);
       }

       void FTN(c_esmc_rtablevalidate)(ESMC_RTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RTableValidate(opts);
       }

       void FTN(c_esmc_rtableprint)(ESMC_RTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RTablePrint(opts);
       }

};


