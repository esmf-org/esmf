// $Id: ESMC_HWMonitor.h,v 1.4.14.3 2007/10/18 02:43:04 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF HWMonitor C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_HWMonitor_H
 #define ESMC_HWMonitor_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_PerfProf.h> 
#if ( defined HAS_PCL )
 #include <pcl.h> 
#elif ( defined HAS_PAPI )
#else
#endif


//-------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_HWMonitor - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ HWMonitor members and declares method 
// signatures (prototypes).  The companion file ESMC\_HWMonitor.C contains
// the definitions (full code bodies) for the HWMonitor methods.
//
// 
//
//-------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_HWMonitorConfig;
 class ESMC_HWMonitor;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_HWMonitorConfig {
   private:
 //   < insert resource items here >
 };

 // class definition type
 class ESMC_HWMonitor : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_HWMonitor members
 //                                 in F90 modules
#define MAX_COUNTER 10
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

    typedef enum {
        hw_flo           = 1,
        hw_L1_cache_util = 2,
        hw_L1_cache_miss = 3,
        hw_L2_cache_util = 4,
        hw_L2_cache_miss = 5,
        hw_fpu_util      = 6,
        hw_loads         = 7,
        hw_instructions  = 8,
        hw_fp_stalled    = 9,
        hw_cycle         = 10
    } hw_option_names;

    int counter[MAX_COUNTER]; // Events to do hardware counting on
    int ncounter;             // Number of events to count
    int initial[MAX_COUNTER]; // Initial results after Begin
    int accum[MAX_COUNTER];   // Accumulated results after End
#if ( defined HAS_PCL )
    PCL_DESC_TYPE *desc;      // PCL description type
#endif

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
    ESMC_HWMonitor *ESMC_HWMonitorCreate(args, int rc);// interface only, deep class
    int ESMC_HWMonitorDestroy(void);            // interface only, deep class
    int ESMC_HWMonitorConstruct(args);          // internal only, deep class
    int ESMC_HWMonitorDestruct(void);           // internal only, deep class

 // or
 // the following method applies to a shallow class
    int ESMC_HWMonitorInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_HWMonitorGetConfig(ESMC_HWMonitorConfig *config) const;
    int ESMC_HWMonitorSetConfig(const ESMC_HWMonitorConfig *config);

 // accessor methods for class members
    int ESMC_HWMonitorGet<Value>(<value type> *value) const;
    int ESMC_HWMonitorSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_HWMonitorValidate(const char *options) const;
    int ESMC_HWMonitorPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_HWMonitor(args);
	~ESMC_HWMonitor(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

 };   // end class ESMC_HWMonitor

 #endif  // ESMC_HWMonitor_H
