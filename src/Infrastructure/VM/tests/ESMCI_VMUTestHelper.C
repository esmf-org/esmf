// $Id: ESMCI_VMUTestHelper.C,v 1.1 2011/09/20 01:43:49 w6ws Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================


//==============================================================================
//
// !DESCRIPTION:
//
// The code in this file assists with the unit testing of VMIds, by providing
// a way to insert data into a VMId without using the Component interfaces.
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_VMUTestHelper.C,v 1.1 2011/09/20 01:43:49 w6ws Exp $";
//-----------------------------------------------------------------------------

//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {

namespace ESMCI {

void FTN(c_esmci_vmidinsert)(ESMCI::VMId **vmid, int *localID, char *key) {
  VMId *localvmid = *vmid;
  localvmid->localID = *localID;
  *localvmid->vmKey  = *key;
}

void FTN(c_esmci_vmidextract)(ESMCI::VMId **vmid, int *localID, char *key) {
  VMId *localvmid = *vmid;
  *localID = localvmid->localID;
  *key     = *localvmid->vmKey;
}

}

}
