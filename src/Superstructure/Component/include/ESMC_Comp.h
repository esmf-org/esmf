// $Id: ESMC_Comp.h,v 1.36 2008/08/26 05:15:11 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Comp_H
#define ESMC_Comp_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Comp - Public C interface to the ESMF Comp class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Comp class and declares global
// variables to be used in user code written in C.
//
//EOPI
//-----------------------------------------------------------------------------

enum ESMC_GridCompType { ESMF_ATM=1, ESMF_LAND, ESMF_OCEAN, ESMF_SEAICE, 
                      ESMF_RIVER, ESMF_GRIDCOMPTYPE_UNKNOWN };

extern const char *ESMC_SetInit;
extern const char *ESMC_SetRun;
extern const char *ESMC_SetFinal;
extern const char *ESMC_SetWriteRestart;
extern const char *ESMC_SetReadRestart;


// Class declaration type
typedef struct{
  void *fortranclass;
    
}ESMC_CompPublic;

#endif  // ESMC_Comp_H
