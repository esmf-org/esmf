// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC ArraySpec method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C interfaces to ArraySpec methods 
// declared in the companion file ESMC_LocalArray.h.  
//
// The {\tt ESMF\_Array} object allows C++ to emulate the richer
// Fortran language Array operations.  It allows strided access, 
// subsetting operations, known dimension sizes, and typed access 
// to arrays instead of just a starting address to a block of memory.  
//
//-----------------------------------------------------------------------------
//

// for printf
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_LogErr.h" 

// in this case, no body for file, all in class
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_ArraySpec.C,v 1.7 2008/02/29 17:44:01 rosalind Exp $";
//-----------------------------------------------------------------------------

//
// Class Constructor
extern "C" {


  int ESMC_ArraySpecSet(ESMC_ArraySpec *arrayspec_,
                        int rank_,
                        ESMC_TypeKind typekind_){
    
    int localrc;
   // Initialize return code. Assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL;

    FTN(f_esmf_arrayspecset)(arrayspec_, &rank_, &typekind_, &localrc);

    return localrc;

  } // end ESMC_ArraySpecSet

//-----------------------------------------------------------------

  int ESMC_ArraySpecGet(ESMC_ArraySpec arrayspec_,
                        int *rank_,
                        ESMC_TypeKind *typekind_){

    int localrc;
   // Initialize return code. Assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL;

    FTN(f_esmf_arrayspecget)(&arrayspec_, rank_, typekind_, &localrc);


    return localrc;

  } // end ESMC_ArraySpecGet

} //extern "C"
