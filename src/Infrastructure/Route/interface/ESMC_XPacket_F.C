// $Id: ESMC_XPacket_F.C,v 1.1 2003/03/05 17:04:53 nscollins Exp $
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
#include "ESMC_XPacket.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt XPacket} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       // keep these for deep classes, or see init below for shallow
       void FTN(c_esmc_xpacketcreate)(ESMC_XPacket **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *ptr = ESMC_XPacketCreate(*arg1, *arg2, *arg3, status);
       }

       void FTN(c_esmc_xpacketdestroy)(ESMC_XPacket **ptr, int *status) {
           *status = ESMC_XPacketDestroy(*ptr);
       }

       // keep this for shallow classes, get rid of create/destroy above
       void FTN(c_esmc_xpacketinit)(ESMC_XPacket **ptr, int *arg1, int *arg2,
                                                   int *arg3, int *status) {
           *status = (*ptr)->ESMC_XPacketInit(*arg1, *arg2, *arg3);
       }

       // for either shallow or deep classes, the following are needed. 
       void FTN(c_esmc_xpacketgetconfig)(ESMC_XPacket **ptr, 
                                         ESMC_XPacketConfig *config, int *status} {
           *status = (*ptr)->ESMC_XPacketGetConfig(&config);
       }

       void FTN(c_esmc_xpacketsetconfig)(ESMC_XPacket **ptr, 
                                         ESMC_XPacketConfig *config, int *status} {
           *status = (*ptr)->ESMC_XPacketSetConfig(config);
       }

       void FTN(c_esmc_xpacketget)(ESMC_XPacket **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_XPacketGet(&value);
       }

       void FTN(c_esmc_xpacketset)(ESMC_XPacket **ptr, 
                                         <value> *value, int *status} {
           *status = (*ptr)->ESMC_XPacketSet(value);
       }

       void FTN(c_esmc_xpacketvalidate)(ESMC_XPacket **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_XPacketValidate(opts);
       }

       void FTN(c_esmc_xpacketprint)(ESMC_XPacket **ptr, char *opts, int *status) {
           *status = (*ptr)->ESMC_XPacketPrint(opts);
       }

};


