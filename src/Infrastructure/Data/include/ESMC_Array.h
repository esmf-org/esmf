// $Id: ESMC_Array.h,v 1.1 2002/11/04 22:16:08 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Array C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_Array_H
 #define ESMC_Array_H

//-----------------------------------------------------------------------------

#include <ESMC_Data.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Array - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Array members and declares method 
// signatures (prototypes).  The companion file ESMC_Array.C contains
// the definitions (full code bodies) for the Array methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_ArrayConfig;
 class ESMC_Array;

// !PRIVATE TYPES:

// class configuration type
class ESMC_ArrayConfig {
   private:
//   < insert resource items here >
};

// class declaration type
class ESMC_Array : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    int rank;
    enum ESMC_DataType type;
    enum ESMC_DataKind kind;
    void *ESMC_base_addr;
    int ESMC_offset[ESMF_MAXDIM];
    int ESMC_length[ESMF_MAXDIM];
    int ESMC_stride[ESMF_MAXDIM];
    enum ESMC_Logical iscontig;
    void *ESMF_f90array;
    
// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
    ESMC_Array *ESMC_ArrayCreate(int rank, 
                           enum ESMC_DataType dt, enum ESMC_DataKind dk,
                           void *base, int *offsets, int *lengths, int *strides, 
                           void *f90ptr, int *rc);
    int ESMC_ArrayDestroy(void);
    void ESMC_ArrayConstruct(ESMC_Array *a, int rank, 
                           enum ESMC_DataType dt, enum ESMC_DataKind dk,
                           void *base, int *offsets, int *lengths, int *strides, 
                           void *f90ptr, int *rc);
    int ESMC_ArrayDestruct(void);

 // optional configuration methods
    int ESMC_ArrayGetConfig(ESMC_ArrayConfig *config) const;
    int ESMC_ArraySetConfig(const ESMC_ArrayConfig *config);

 // accessor methods for class members
    //int ESMC_ArrayGet<Value>(<value type> *value) const;
    //int ESMC_ArraySet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_ArrayValidate(const char *options) const;
    int ESMC_ArrayPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Array(void);
	~ESMC_Array(void);
  
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

 #endif  // ESMC_Array_H
