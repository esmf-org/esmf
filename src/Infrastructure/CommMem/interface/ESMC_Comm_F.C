// $Id: ESMC_Comm_F.C,v 1.2 2003/03/25 22:09:31 nscollins Exp $
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
#include "ESMC_Comm.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt DELayout} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_commsendrecv)(ESMC_Comm **ptr, 
  				     void *sbuf, void *rbuf, 
                                     int *snum, int *rnum, 
                                     int *sde, int *rde, int *status) {
      *status = (*ptr)->ESMC_CommSendRecv(sbuf, rbuf, *snum, *rnum, *sde, *rde);
       }

  //         void FTN(c_esmc_commsendrecv)(ESMC_Comm **ptr, ESMC_DELayout *delayout, 
  //				     void *sbuf, void *rbuf, int *num, int *sde, 
  //				     int *rde, int *status) {
  //         *status = (*ptr)->ESMC_CommSendRecv(delayout, sbuf, rbuf, *num, *sde, *rde);
  //       }

  //       void FTN(c_esmc_commbcast)(ESMC_Comm **ptr, ESMC_DELayout *delayout, 
  //			  void *buf, int *count, int *rootde, int *status) {
  //     *status = (*ptr)->ESMC_CommBcast(delayout, buf, *count, *rootde);
  //   }


};









