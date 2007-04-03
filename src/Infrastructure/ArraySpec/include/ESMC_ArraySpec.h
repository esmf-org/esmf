// $Id: ESMC_ArraySpec.h,v 1.7 2007/04/03 16:36:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
    ESMC_TypeKind typekind;    

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  
 // get/set methods for internal data
    int ESMC_ArraySpecSetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_ArraySpecGetRank(void) { return this->rank; }

    int ESMC_ArraySpecSetTypeKind(ESMC_TypeKind typekind) { this->typekind = typekind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind ESMC_ArraySpecGetTypeKind(void) { return this->typekind; }

    int ESMC_ArraySpecSetRank(int rank, ESMC_TypeKind typekind)
    { if ((rank >= 1) && (rank <= 7)) 
          this->rank = rank; 
      else {
         printf("bad rank %d, must be between 1 and 7\n", rank);
         return ESMF_FAILURE;
      }
      this->typekind = typekind;
      return ESMF_SUCCESS; 
    }
    int ESMC_ArraySpecGetRank(int *rank, ESMC_TypeKind *typekind)
    { if (rank) *rank = this->rank;
      if (typekind) *typekind = this->typekind;
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
