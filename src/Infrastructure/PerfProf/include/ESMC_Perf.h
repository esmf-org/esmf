// $Id: ESMC_Perf.h,v 1.1 2002/11/14 18:14:36 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Perf C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Perf_H
 #define ESMC_Perf_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_PerfProf.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Perf - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Perf members and declares method 
// signatures (prototypes).  The companion file ESMC_Perf.C contains
// the definitions (full code bodies) for the Perf methods.
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
 class ESMC_PerfConfig;
 class ESMC_Perf;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_PerfConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Perf : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Perf members
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
 // ESMC_PerfCreate and ESMC_PerfDestroy are declared below,
 // outside the ESMC_Perf declaration
    int ESMC_PerfConstruct(args);          // internal only, deep class
    int ESMC_PerfDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_PerfInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_PerfGetConfig(ESMC_PerfConfig *config) const;
    int ESMC_PerfSetConfig(const ESMC_PerfConfig *config);

 // accessor methods for class members
    int ESMC_PerfGet<Value>(<value type> *value) const;
    int ESMC_PerfSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_PerfValidate(const char *options) const;
    int ESMC_PerfPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Perf(args);
	~ESMC_Perf(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Perf

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Perf object itself. E.g. if Create
// were a method, the ESMC_Perf object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Perf object.

 ESMC_Perf *ESMC_PerfCreate(args, int *rc);// interface only, deep class
 int ESMC_PerfDestroy(ESMC_Perf *<class>); // interface only, deep class

 #endif  // ESMC_Perf_H
