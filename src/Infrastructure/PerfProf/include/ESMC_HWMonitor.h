// $Id: ESMC_HWMonitor.h,v 1.1 2002/11/14 18:14:36 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF HW_Monitor C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_HW_Monitor_H
 #define ESMC_HW_Monitor_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_PerfProf.h> 

//-------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_HW_Monitor - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ HW_Monitor members and declares method 
// signatures (prototypes).  The companion file ESMC_HW_Monitor.C contains
// the definitions (full code bodies) for the HW_Monitor methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_HW_MonitorConfig;
 class ESMC_HW_Monitor;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_HW_MonitorConfig {
   private:
 //   < insert resource items here >
 };

 // class definition type
 class ESMC_HW_Monitor : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_HW_Monitor members
 //                                 in F90 modules
    int flo;             // Floating point operations
    int L1_cache_util;   // Level 1 cache utilization
    int L1_cache_miss;   // Level 1 cache miss
    int L2_cache_util;   // Level 2 cache utilization
    int L2_cache_miss;   // Level 2 cache miss
    int fpu_util;        // Floating point utilization
    int loads;           // Number of load/store operations
    int instructions;    // Number of instructions
    int fp_stalled;      // Number of cycles floating point unit is stalled
    int cycle;           // Number of cycles


// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
    ESMC_HW_Monitor *ESMC_HW_MonitorCreate(args, int rc);// interface only, deep class
    int ESMC_HW_MonitorDestroy(void);            // interface only, deep class
    int ESMC_HW_MonitorConstruct(args);          // internal only, deep class
    int ESMC_HW_MonitorDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_HW_MonitorInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_HW_MonitorGetConfig(ESMC_HW_MonitorConfig *config) const;
    int ESMC_HW_MonitorSetConfig(const ESMC_HW_MonitorConfig *config);

 // accessor methods for class members
    int ESMC_HW_MonitorGet<Value>(<value type> *value) const;
    int ESMC_HW_MonitorSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_HW_MonitorValidate(const char *options) const;
    int ESMC_HW_MonitorPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_HW_Monitor(args);
	~ESMC_HW_Monitor(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

 };   // end class ESMC_HW_Monitor

 #endif  // ESMC_HW_Monitor_H
