// $Id: ESMC_ArrayComm.h,v 1.1 2003/10/07 22:29:46 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF ArrayComm C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_ArrayComm_H
 #define ESMC_ArrayComm_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_ArrayComm - Distributed Data associated with a regular grid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Array members and declares method 
// signatures (prototypes).  The companion file ESMC\_Array.C contains
// the definitions (full code bodies) for the Array methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
#include <ESMC_DELayout.h>    // communications code
#include <ESMC_LocalArray.h>  // functions to interoperate F90/C++ arrays
#include <ESMC_Array.h>  // functions to interoperate F90/C++ arrays
#include <ESMC_Grid.h>        // grid info

// !PUBLIC TYPES:


// !PRIVATE TYPES:

  public:

    // most important array methods
    int ESMC_ArrayRedist(ESMC_DELayout *layout, int global_start[], 
                         int global_dimlengths[], int rank_trans[], 
                         int size_rank_trans, int olddecompids[], 
                         int decompids[], int size_decomp,
                          ESMC_Array *RedistArray);
    int ESMC_ArrayHalo(ESMC_DELayout *layout,
                       ESMC_AxisIndex *ai_global, int global_dimlengths[],
                       int decompids[], int size_decomp, ESMC_Logical periodic[]);
    int ESMC_ArrayAllGather(ESMC_DELayout *layout, int decompids[], 
                            int size_decomp, int global_dimlengths[],
                            ESMC_Array **Array_out);
    int ESMC_ArrayGather(ESMC_DELayout *layout, int decompids[], 
                            int size_decomp, int global_dimlengths[],
                            int deid, ESMC_Array **Array_out);
    int ESMC_ArrayScatter(ESMC_DELayout *layout,
                            int decompids[], int size_decomp,
                            int deid, ESMC_Array **Array_out);
    
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Array


 #endif  // ESMC_ArrayComm_H

