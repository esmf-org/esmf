// $Id: ESMC_DistArray.h,v 1.1 2003/07/10 14:55:58 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Distributed Array C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_DistArray_H
 #define ESMC_DistArray_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Array - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines additional {\tt ESMC\_Array} methods.
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Array.h>  // inherit from the Array class

// !PUBLIC TYPES:

// !PRIVATE TYPES:

// class declaration type
class ESMC_Array : public ESMC_Base {    // inherits from ESMC_Base class

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
    // distributed array methods
    int ESMC_ArrayRedist(ESMC_DELayout *layout,
                         int rank_trans[], int size_rank_trans,
                         int olddecompids[], int decompids[], int size_decomp,
                         ESMC_Array *RedistArray);
    int ESMC_ArrayHalo(ESMC_DELayout *layout,
                       int decompids[], int size_decomp,
                       ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot);
    int ESMC_ArrayAllGather(ESMC_DELayout *layout,
                            int decompids[], int size_decomp,
                            ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                            ESMC_Array **Array_out);
    int ESMC_ArrayGather(ESMC_DELayout *layout,
                         int decompids[], int size_decomp,
                         ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                         int deid, ESMC_Array **Array_out);
    int ESMC_ArrayScatter(ESMC_DELayout *layout,
                          int decompids[], int size_decomp,
                          ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
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


 #endif  // ESMC_DistArray_H
