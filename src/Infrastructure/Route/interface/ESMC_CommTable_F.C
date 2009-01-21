// $Id: ESMC_CommTable_F.C,v 1.9.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
       void FTN(c_esmc_commtablecreate)(ESMC_CommTable **ptr, int *myvmid,
                                                   int *cnt, int *status) {
           *ptr = ESMC_CommTableCreate(*myvmid, *cnt, status);
       }

       void FTN(c_esmc_commtabledestroy)(ESMC_CommTable **ptr, int *status) {
           *status = ESMC_CommTableDestroy(*ptr);
       }

       void FTN(c_esmc_commtableget)(ESMC_CommTable **ptr, 
                                         int *value, int *status) {
       //    *status = (*ptr)->ESMC_CommTableGet(&value);
             *status = ESMC_RC_NOT_IMPL;
       }

       void FTN(c_esmc_commtableset)(ESMC_CommTable **ptr, 
                                         int *value, int *status) {
       //    *status = (*ptr)->ESMC_CommTableSet(value);
             *status = ESMC_RC_NOT_IMPL;
       }

       void FTN(c_esmc_commtablevalidate)(ESMC_CommTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CommTableValidate(opts);
       }

       void FTN(c_esmc_commtableprint)(ESMC_CommTable **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_CommTablePrint(opts);
       }

};


