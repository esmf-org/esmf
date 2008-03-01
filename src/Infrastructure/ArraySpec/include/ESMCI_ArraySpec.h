// $Id: ESMCI_ArraySpec.h,v 1.1.2.3 2008/03/01 04:12:50 theurich Exp $
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

#ifndef ESMCI_ArraySpec_H
#define ESMCI_ArraySpec_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::ArraySpec - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------

namespace ESMCI {

// classes and structs

class ArraySpec;

// THIS MUST MATCH F90 DECLARATION in ../interface file
// TODO: this is bad bad code -> must be replaced!!!!

class ArraySpec {   // NOT inherited from Base class
 private:
    int rank;
    ESMC_TypeKind typekind;    

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  
    ArraySpec(){}
    ArraySpec(int itsRank, ESMC_TypeKind itsTypeKind);
 // get/set methods for internal data
    int setRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int getRank(void) { return this->rank; }

    int setTypeKind(ESMC_TypeKind typekind) { this->typekind = typekind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind getTypeKind(void) { return this->typekind; }

    int setRank(int rank, ESMC_TypeKind typekind)
    { if ((rank >= 1) && (rank <= 7)) 
          this->rank = rank; 
      else {
         printf("bad rank %d, must be between 1 and 7\n", rank);
         return ESMF_FAILURE;
      }
      this->typekind = typekind;
      return ESMF_SUCCESS; 
    }
    int getRank(int *rank, ESMC_TypeKind *typekind)
    { if (rank) *rank = this->rank;
      if (typekind) *typekind = this->typekind;
      return ESMF_SUCCESS;  
    }

};  //  class ArraySpec

} // namespace ESMCI

#endif  // ESMCI_ArraySpec_H
