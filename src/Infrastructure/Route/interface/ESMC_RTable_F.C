// $Id: ESMC_RTable_F.C,v 1.8.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
#include "ESMC_Start.h"
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
       void FTN(c_esmc_rtablecreate)(ESMC_RTable **ptr, int *myde, 
                                            int *decount, int *status) {
           *ptr = ESMC_RTableCreate(*myde, *decount, status);
       }

       void FTN(c_esmc_rtabledestroy)(ESMC_RTable **ptr, int *status) {
           *status = ESMC_RTableDestroy(*ptr);
       }

       void FTN(c_esmc_rtableget)(ESMC_RTable **ptr, 
                                         int *value, int *status) {
       //    *status = (*ptr)->ESMC_RTableGet(&value);
             *status = ESMC_RC_NOT_IMPL;
       }

       void FTN(c_esmc_rtableset)(ESMC_RTable **ptr, 
                                         int *value, int *status) {
       //    *status = (*ptr)->ESMC_RTableSet(value);
             *status = ESMC_RC_NOT_IMPL;
       }

       void FTN(c_esmc_rtablevalidate)(ESMC_RTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RTableValidate(opts);
       }

       void FTN(c_esmc_rtableprint)(ESMC_RTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_RTablePrint(opts);
       }

};


