// $Id: ESMC_Comp_F.C,v 1.1 2003/01/23 22:27:19 nscollins Exp $
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
#include "ESMC_Comp.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Component} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_compcreate)(ESMC_Comp **ptr, char *name,
                     ESMC_Layout *layout, ESMC_CompType *ctype,
                     ESMC_ModelType *mtype, char *filepath, int *status) {
         *ptr = ESMC_CompCreate(name, layout, *ctype, *mtype, filepath, status);
     }

     void FTN(c_esmc_compdestroy)(ESMC_Comp **ptr, int *status) {
         *status = ESMC_CompDestroy(*ptr);
     }

     void FTN(c_esmc_compinit)(ESMC_Comp **ptr, int *status) {
         *status = (*ptr)->ESMC_CompInit();
     }

     void FTN(c_esmc_comprun)(ESMC_Comp **ptr, int *timesteps, int *status) {
         *status = (*ptr)->ESMC_CompRun(*timesteps);
     }

     void FTN(c_esmc_compfinalize)(ESMC_Comp **ptr, int *status) {
         *status = (*ptr)->ESMC_CompFinalize();
     }

     void FTN(c_esmc_compvalidate)(ESMC_Comp **ptr, char *opts, int *status) {
         *status = (*ptr)->ESMC_CompValidate(opts);
     }

     void FTN(c_esmc_compprint)(ESMC_Comp **ptr, char *opts, int *status) {
         *status = (*ptr)->ESMC_CompPrint(opts);
     }

};



