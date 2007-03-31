// $Id: ESMC_F90Interface.C,v 1.4 2007/03/31 05:51:27 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_F90Interface.C"
//==============================================================================

#include "ESMC_Start.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMC_F90Interface.h"

ESMC_InterfaceInt::ESMC_InterfaceInt(void){
  // native constructor
  array = NULL;
  dimCount = 0;
}

ESMC_InterfaceInt::ESMC_InterfaceInt(int *arrayArg, int dimArg,
  int *lenArg){
  // native constructor
  array = arrayArg;
  dimCount = dimArg;
  for (int i=0; i<dimCount; i++)
    extent[i]=lenArg[i];
}

ESMC_InterfaceInt::~ESMC_InterfaceInt(void){
  // native destructor
  array = NULL;
  dimCount = 0;
}
