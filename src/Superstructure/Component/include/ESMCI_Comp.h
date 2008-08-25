// $Id: ESMCI_Comp.h,v 1.1 2008/08/25 22:03:52 theurich Exp $
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

#ifndef ESMCI_Comp_H
#define ESMCI_Comp_H

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

#define ESMF_INIT 1
#define ESMF_RUN 2
#define ESMF_FINAL 3
#define ESMF_WRITERESTART 4
#define ESMF_READRESTART 5
#define ESMF_SINGLEPHASE 0

enum ESMC_CompType { ESMF_COMPTYPE_GRID=1, ESMF_COMPTYPE_CPL, 
                     ESMF_COMPTYPE_UNKNOWN };
enum ESMC_GridCompType { ESMF_ATM=1, ESMF_LAND, ESMF_OCEAN, ESMF_SEAICE, 
                      ESMF_RIVER, ESMF_GRIDCOMPTYPE_UNKNOWN };

extern const char *ESMC_SetInit;
extern const char *ESMC_SetRun;
extern const char *ESMC_SetFinal;
extern const char *ESMC_SetWriteRestart;
extern const char *ESMC_SetReadRestart;


// Class declaration type
class ESMC_Comp{
  void *fortranclass;
};

#endif  // ESMCI_Comp_H
