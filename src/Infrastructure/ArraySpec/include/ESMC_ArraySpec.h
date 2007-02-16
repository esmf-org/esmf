// $Id: ESMC_ArraySpec.h,v 1.4 2007/02/16 05:27:42 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF ArraySpec C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_ArraySpec_H
 #define ESMC_ArraySpec_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_ArraySpec - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Array members and declares method 
// signatures (prototypes).  The companion file ESMC\_ArraySpec.C contains
// the definitions (full code bodies) for the Array methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_ArraySpec;

// THIS MUST MATCH F90 DECLARATION in ../interface file
class ESMC_ArraySpec {   // NOT inherited from Base class
 private:
    int rank;
    ESMC_DataType type;
    ESMC_TypeKind kind;    

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  
 // get/set methods for internal data
    int ESMC_ArraySpecSetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_ArraySpecGetRank(void) { return this->rank; }

    int ESMC_ArraySpecSetType(ESMC_DataType type) { this->type = type; 
                                                     return ESMF_SUCCESS;}
    ESMC_DataType ESMC_ArraySpecGetType(void) { return this->type; }

    int ESMC_ArraySpecSetTypeKind(ESMC_TypeKind kind) { this->kind = kind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind ESMC_ArraySpecGetTypeKind(void) { return this->kind; }

    int ESMC_ArraySpecSetRank(int rank, ESMC_DataType type, ESMC_TypeKind kind)
    { if ((rank >= 1) && (rank <= 7)) 
          this->rank = rank; 
      else {
         printf("bad rank %d, must be between 1 and 7\n", rank);
         return ESMF_FAILURE;
      }
      this->type = type, 
      this->kind = kind;
      return ESMF_SUCCESS; 
    }
    int ESMC_ArraySpecGetRank(int *rank, ESMC_DataType *type, ESMC_TypeKind *kind)
    { if (rank) *rank = this->rank;
      if (type) *type = this->type;
      if (kind) *kind = this->kind;
      return ESMF_SUCCESS;  
    }

// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_ArraySpec


 #endif  // ESMC_ArraySpec_H
