// $Id: ESMC_ArrayMap.h,v 1.1 2002/11/04 22:16:08 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF ArrayMap C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_ArrayMap_H
 #define ESMC_ArrayMap_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_Data.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_ArrayMap - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ArrayMap members and declares method 
// signatures (prototypes).  The companion file ESMC_ArrayMap.C contains
// the definitions (full code bodies) for the ArrayMap methods.
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
 class ESMC_ArrayMapConfig;
 class ESMC_ArrayMap;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_ArrayMapConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_ArrayMap : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_ArrayMap members
 //                                 in F90 modules

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following method applies to a shallow class
    int ESMC_ArrayMapInit(int *rc);         // shallow class only

 // optional configuration methods
    int ESMC_ArrayMapGetConfig(ESMC_ArrayMapConfig *config) const;
    int ESMC_ArrayMapSetConfig(const ESMC_ArrayMapConfig *config);

 // accessor methods for class members
    //int ESMC_ArrayMapGet<Value>(<value type> *value) const;
    //int ESMC_ArrayMapSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_ArrayMapValidate(const char *options) const;
    int ESMC_ArrayMapPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_ArrayMap(void);
	~ESMC_ArrayMap(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_ArrayMap

 #endif  // ESMC_ArrayMap_H
