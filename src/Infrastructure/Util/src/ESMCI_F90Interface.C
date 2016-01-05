// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_F90Interface.h"

//-----------------------------------------------------------------------------

//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN_X(f_esmf_fortranudtpointersize)(int *size);
  void FTN_X(f_esmf_fortranudtpointercopy)(void *dst, void *src);
}
//==============================================================================

namespace ESMCI {
  
  F90ClassHolder::F90ClassHolder(void **udtPtr){
    // constructor that stores a user derived type (UDT) inside F90ClassHolder
#undef  ESMC_METHOD
#define ESMC_METHOD "F90ClassHolder()"
    int udtSize;
    FTN_X(f_esmf_fortranudtpointersize)(&udtSize);
    if (sizeof(ESMCI::F90ClassHolder) < udtSize){
      int localrc = ESMC_RC_NOT_IMPL;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
        "- hardcoded ESMCI::F90ClassHolder size smaller than UDT size"
        " determined at runtime", ESMC_CONTEXT, &localrc);
      throw localrc;  // bail out with exception
    }
    FTN_X(f_esmf_fortranudtpointercopy)((void *)this, (void *)udtPtr); 
  }
  
  int F90ClassHolder::castToFortranUDT(void **udtPtr){
    int rc=ESMC_RC_NOT_IMPL;
    FTN_X(f_esmf_fortranudtpointercopy)((void *)udtPtr, (void *)this);
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
  //--------------------------------------------------------------------------

  InterfaceInt::InterfaceInt(void){
    // constructor
    array = NULL;
    dimCount = 0;
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  InterfaceInt::InterfaceInt(int *arrayArg, int lenArg){
    // constructor, special case 1d
    array = arrayArg;
    dimCount = 1;
    extent[0]=lenArg;
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  InterfaceInt::InterfaceInt(std::vector<int> &arrayArg){
    // constructor, special case 1d
    array = &(arrayArg[0]);
    dimCount = 1;
    extent[0]=arrayArg.size();
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  InterfaceInt::InterfaceInt(int *arrayArg, int dimArg, const int *lenArg){
    // constructor
    array = arrayArg;
    dimCount = dimArg;
    for (int i=0; i<dimCount; i++)
      extent[i]=lenArg[i];
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  InterfaceInt::~InterfaceInt(void){
    // destructor
    array = NULL;
    dimCount = 0;
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }
  
  void InterfaceInt::set(void){
    // set NULL
    array = NULL;
    dimCount = 0;
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  void InterfaceInt::set(int *arrayArg, int lenArg){
    // set special case 1d
    array = arrayArg;
    dimCount = 1;
    extent[0]=lenArg;
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  void InterfaceInt::set(std::vector<int> &arrayArg){
    // set special case 1d
    array = &(arrayArg[0]);
    dimCount = 1;
    extent[0]=arrayArg.size();
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }

  void InterfaceInt::set(int *arrayArg, int dimArg, const int *lenArg){
    // set
    array = arrayArg;
    dimCount = dimArg;
    for (int i=0; i<dimCount; i++)
      extent[i]=lenArg[i];
    for (int i=dimCount; i<7; i++)
      extent[i]=0;
  }
  
  bool present(InterfaceInt *ptr){
    return ( (ptr != NULL) && (ptr->array != NULL) );
  }

} // namespace ESMCI
