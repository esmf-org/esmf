// $Id: ESMC_Array.h,v 1.95 2008/02/16 06:04:37 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Array_H
#define ESMC_Array_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Array - Public C interface to the ESMF Array class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Array class and declares method 
// signatures (prototypes).  The companion file {\tt ESMC\_Array.C} contains
// the definitions (full code bodies) for the Array methods.
//
//EOPI
//-----------------------------------------------------------------------------


extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Array;

// Class API
ESMC_Array ESMC_ArrayCreate(int *rc); // dummy ArrayCreate() to be replaced
int ESMC_ArrayPrint(ESMC_Array array);


}; // extern "C"


#endif  // ESMC_Array_H
