// $Id: ESMC_CommTable.h,v 1.2 2003/03/11 03:01:02 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF CommTable C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_CommTable_H
 #define ESMC_CommTable_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_Route.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_CommTable - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ CommTable members and declares method 
// signatures (prototypes).  The companion file ESMC_CommTable.C contains
// the definitions (full code bodies) for the CommTable methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_CommTableConfig;
 class ESMC_CommTable;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_CommTableConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_CommTable : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_CommTable members
 //                                 in F90 modules

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
 // ESMC_CommTableCreate and ESMC_CommTableDestroy are declared below,
 // outside the ESMC_CommTable declaration
    int ESMC_CommTableConstruct(args);          // internal only, deep class
    int ESMC_CommTableDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_CommTableInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_CommTableGetConfig(ESMC_CommTableConfig *config) const;
    int ESMC_CommTableSetConfig(const ESMC_CommTableConfig *config);

 // accessor methods for class members
    int ESMC_CommTableGet<Value>(<value type> *value) const;
    int ESMC_CommTableSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_CommTableValidate(const char *options) const;
    int ESMC_CommTablePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_CommTable(args);
	~ESMC_CommTable(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_CommTable

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_CommTable object itself. E.g. if Create
// were a method, the ESMC_CommTable object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_CommTable object.

 ESMC_CommTable *ESMC_CommTableCreate(args, int *rc);// interface only, deep class
 int ESMC_CommTableDestroy(ESMC_CommTable *commtable); // interface only, deep class

 #endif  // ESMC_CommTable_H
