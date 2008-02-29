// $Id: ESMCI_ArraySpec.h,v 1.1.2.2 2008/02/29 23:20:36 theurich Exp $
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

 #ifndef ESMCI_ArraySpec_H
 #define ESMCI_ArraySpec_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMCI_ArraySpec - uniform access to arrays from F90 and C++
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
 class ESMCI_ArraySpec;

// THIS MUST MATCH F90 DECLARATION in ../interface file
class ESMCI_ArraySpec {   // NOT inherited from Base class
 private:
    int rank;
    ESMC_TypeKind typekind;    

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  
    ESMCI_ArraySpec(){}
    ESMCI_ArraySpec(int itsRank, ESMC_TypeKind itsTypeKind);
 // get/set methods for internal data
    int ESMCI_ArraySpecSetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMCI_ArraySpecGetRank(void) { return this->rank; }

    int ESMCI_ArraySpecSetTypeKind(ESMC_TypeKind typekind) { this->typekind = typekind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind ESMCI_ArraySpecGetTypeKind(void) { return this->typekind; }

    int ESMCI_ArraySpecSetRank(int rank, ESMC_TypeKind typekind)
    { if ((rank >= 1) && (rank <= 7)) 
          this->rank = rank; 
      else {
         printf("bad rank %d, must be between 1 and 7\n", rank);
         return ESMF_FAILURE;
      }
      this->typekind = typekind;
      return ESMF_SUCCESS; 
    }
    int ESMCI_ArraySpecGetRank(int *rank, ESMC_TypeKind *typekind)
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

 };   // end class ESMCI_ArraySpec


 #endif  // ESMCI_ArraySpec_H
