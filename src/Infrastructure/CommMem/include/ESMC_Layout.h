// $Id: ESMC_Layout.h,v 1.1 2002/10/25 19:21:33 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Layout C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Layout_H
 #define ESMC_Layout_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Layout - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Layout members and declares method 
// signatures (prototypes).  The companion file ESMC_Layout.C contains
// the definitions (full code bodies) for the Layout methods.
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
 class ESMC_LayoutConfig;
 class ESMC_Layout;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_LayoutConfig {
   private:
 //   < insert resource items here >
 };

 // class definition type
 class ESMC_Layout : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Layout members
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
    ESMC_Layout *ESMC_LayoutCreate(args, int rc);// interface only, deep class
    int ESMC_LayoutDestroy(void);            // interface only, deep class
    int ESMC_LayoutConstruct(args);          // internal only, deep class
    int ESMC_LayoutDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_LayoutInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_LayoutGetConfig(ESMC_LayoutConfig *config) const;
    int ESMC_LayoutSetConfig(const ESMC_LayoutConfig *config);

 // accessor methods for class members
    int ESMC_LayoutGet<Value>(<value type> *value) const;
    int ESMC_LayoutSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_LayoutValidate(const char *options) const;
    int ESMC_LayoutPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Layout(args);
	~ESMC_Layout(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Layout

 #endif  // ESMC_Layout_H













