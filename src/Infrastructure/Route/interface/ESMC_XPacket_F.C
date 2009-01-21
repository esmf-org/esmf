// $Id: ESMC_XPacket_F.C,v 1.8.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include "ESMC_XPacket.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt XPacket} class functions.
//
//EOP

// TOD0: once we comment in the real code, remove this.  it just shuts up
// ranlib which moans if this file contains no symbols.
static int foo;

// the interface subroutine names MUST be in lower case
extern "C" {

       // keep this for shallow classes, get rid of create/destroy above
       //void FTN(c_esmc_xpacketinit)(ESMC_XPacket **ptr, int *arg1, int *arg2,
       //                                            int *arg3, int *status) {
       //    *status = (*ptr)->ESMC_XPacketSetDefault(*arg1, *arg2, *arg3);
       //}

       //void FTN(c_esmc_xpacketget)(ESMC_XPacket **ptr, 
       //                                  int *value, int *status) {
       //    *status = (*ptr)->ESMC_XPacketGet(&value);
       //}

       //void FTN(c_esmc_xpacketset)(ESMC_XPacket **ptr, 
       //                                  int *value, int *status) {
       //    *status = (*ptr)->ESMC_XPacketSet(value);
       //}


};


