// $Id: ESMC_PEList.h,v 1.1 2002/10/25 19:21:33 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF PEList C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_PEList_H
 #define ESMC_PEList_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_PEList - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ PEList members and declares method 
// signatures (prototypes).  The companion file ESMC_PEList.C contains
// the definitions (full code bodies) for the PEList methods.
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
 class ESMC_PEListConfig;
 class ESMC_PEList;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_PEListConfig {
   private:
 //   < insert resource items here >
 };

 // class definition type
 class ESMC_PEList : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_PEList members
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
    ESMC_PEList *ESMC_PEListCreate(args, int rc);// interface only, deep class
    int ESMC_PEListDestroy(void);            // interface only, deep class
    int ESMC_PEListConstruct(args);          // internal only, deep class
    int ESMC_PEListDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_PEListInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_PEListGetConfig(ESMC_PEListConfig *config) const;
    int ESMC_PEListSetConfig(const ESMC_PEListConfig *config);

 // accessor methods for class members
    int ESMC_PEListGet<Value>(<value type> *value) const;
    int ESMC_PEListSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_PEListValidate(const char *options) const;
    int ESMC_PEListPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_PEList(args);
	~ESMC_PEList(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_PEList

 #endif  // ESMC_PEList_H
