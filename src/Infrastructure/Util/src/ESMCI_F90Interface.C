// $Id: ESMCI_F90Interface.C,v 1.11 2010/06/24 07:42:58 theurich Exp $
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
#define ESMC_FILENAME "ESMCI_F90Interface.C"
//==============================================================================

//-----------------------------------------------------------------------------

#include "ESMCI_F90Interface.h"

//-----------------------------------------------------------------------------

namespace ESMCI {

InterfaceInt::InterfaceInt(void){
  // native constructor
  array = NULL;
  dimCount = 0;
}

InterfaceInt::InterfaceInt(int *arrayArg, int lenArg){
  // native constructor, special case 1d
  array = arrayArg;
  dimCount = 1;
  extent[0]=lenArg;
}

InterfaceInt::InterfaceInt(std::vector<int> &arrayArg){
  // native constructor, special case 1d
  array = &(arrayArg[0]);
  dimCount = 1;
  extent[0]=arrayArg.size();
}

InterfaceInt::InterfaceInt(int *arrayArg, int dimArg, const int *lenArg){
  // native constructor
  array = arrayArg;
  dimCount = dimArg;
  for (int i=0; i<dimCount; i++)
    extent[i]=lenArg[i];
}

InterfaceInt::~InterfaceInt(void){
  // native destructor
  array = NULL;
  dimCount = 0;
}

} // namespace ESMCI
