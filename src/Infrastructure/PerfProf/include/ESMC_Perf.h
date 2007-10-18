// $Id: ESMC_Perf.h,v 1.4.14.3 2007/10/18 02:43:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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
 //#include <ESMC_PerfProf.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Perf - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Perf members and declares method 
// signatures (prototypes).  The companion file ESMC\_Perf.C contains
// the definitions (full code bodies) for the Perf methods.
//
// The performance class allows the user to time and measure hardware
// performance of their code.  To begin the section of code to be
// monitored, call StartSegment.  End the section by calling EndSegment.
// For example,
//            ESMC\_PerfStart('name',...;
//            - code -
//            ESMC\_PerfEnd( 'name')
//
// Each segment of code to be instrumented is stored in an array of instrumented
// segments, a type defined in perf.h.
//
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:

 class ESMC_Perf;

// !PRIVATE TYPES:

  // class declaration type
 class ESMC_Perf : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     bool monitoring; 
     Segment *aSegment;
     int currentSegment;
     int maxSegments;
     bool initialized = false;
     LogErr perfData;
     ofstream logFile;


// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_PerfConstruct(args);          // internal only, deep class
    int ESMC_PerfDestruct(void);           // internal only, deep class

 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_PerfValidate(const char *options) const;
    int ESMC_PerfPrint() const;

 // native C++ constructors/destructors
	ESMC_Perf();
	~ESMC_Perf();
  
 // < declare the rest of the public interface methods here >
  
    void ESMC_PerfStart(string someName, bool doHardware );
    void ESMC_PerfEnd( string someName);

// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
     void ESMC_PerfPrintTimes(double time[], int bound);
     int ESMC_PerfFindSegment(string someName );
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

 ESMC_Perf *ESMC_PerfCreate();// interface only, deep class
 int ESMC_PerfDestroy(ESMC_Perf *<class>); // interface only, deep class

 #endif  // ESMC_Perf_H
