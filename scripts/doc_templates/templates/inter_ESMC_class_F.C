// $Id: inter_ESMC_class_F.C,v 1.9.2.3 2010/02/01 20:48:49 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include "ESMC_<Class>.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt <Class>} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_<class>create)(ESMC_<Class> **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_<Class>Create(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_<class>destroy)(ESMC_<Class> **ptr, int *status) {
           *status = ESMC_<Class>Destroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_<class>init)(ESMC_<Class> *ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (ptr)->ESMC_<Class>Init(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       // for deep classes, use **ptr/*ptr as in create/destroy above;
       // for shallow ones use *ptr/ptr as in init above
       void FTN(c_esmc_<class>getconfig)(ESMC_<Class> *ptr, 
                                         ESMC_<Class>Config *config, int *status) {
           *status = (ptr)->ESMC_<Class>GetConfig(&config);
       }

       void FTN(c_esmc_<class>setconfig)(ESMC_<Class> *ptr, 
                                         ESMC_<Class>Config *config, int *status) {
           *status = (ptr)->ESMC_<Class>SetConfig(config);
       }

       void FTN(c_esmc_<class>get)(ESMC_<Class> *ptr, 
                                         <value> *value, int *status) {
           *status = (ptr)->ESMC_<Class>Get(&value);
       }

       void FTN(c_esmc_<class>set)(ESMC_<Class> *ptr, 
                                         <value> *value, int *status) {
           *status = (ptr)->ESMC_<Class>Set(value);
       }

       void FTN(c_esmc_<class>validate)(ESMC_<Class> *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_BaseValidate(opts);
       }

       void FTN(c_esmc_<class>print)(ESMC_<Class> *ptr, char *opts, int *status) {
           *status = (ptr)->ESMC_BasePrint(opts);
       }

};
