// $Id: ESMC_Fraction_F.C,v 1.2 2003/03/22 05:46:04 eschwab Exp $
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
#include "ESMC.h"
#include "ESMC_Fraction.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Fraction} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_fractioncreate)(ESMC_Fraction **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_FractionCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_fractiondestroy)(ESMC_Fraction **ptr, int *status) {
           *status = ESMC_FractionDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_fractioninit)(ESMC_Fraction **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_FractionInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_fractiongetconfig)(ESMC_Fraction **ptr, 
                                         ESMC_FractionConfig *config, int *status) {
           *status = (*ptr)->ESMC_FractionGetConfig(&config);
       }

       void FTN(c_esmc_fractionsetconfig)(ESMC_Fraction **ptr, 
                                         ESMC_FractionConfig *config, int *status) {
           *status = (*ptr)->ESMC_FractionSetConfig(config);
       }

       void FTN(c_esmc_fractionget)(ESMC_Fraction **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_FractionGet(&value);
       }

       void FTN(c_esmc_fractionset)(ESMC_Fraction **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_FractionSet(value);
       }

       void FTN(c_esmc_fractionvalidate)(ESMC_Fraction **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_FractionValidate(opts);
       }

       void FTN(c_esmc_fractionprint)(ESMC_Fraction **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_FractionPrint(opts);
       }

};


