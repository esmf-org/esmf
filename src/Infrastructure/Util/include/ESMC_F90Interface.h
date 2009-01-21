// $Id: ESMC_F90Interface.h,v 1.7.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Interface C definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_F90INTERFACE_H
#define ESMC_F90INTERFACE_H

typedef struct{
  void *memoryHolder[8];  // reserve 8 times the space of a void pointer
                          // this value has been determined empirically to work
                          // on the supported platforms.
}ESMC_F90ClassHolder;

#endif // ESMC_INTERFACE_H
