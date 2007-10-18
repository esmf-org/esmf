// $Id: ESMC_Segment.h,v 1.3.14.3 2007/10/18 02:43:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Segment C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Segment_H
 #define ESMC_Segment_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 //#include <ESMC_PerfProf.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Segment - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Segment members and declares method 
// signatures (prototypes).  The companion file ESMC\_Segment.C contains
// the definitions (full code bodies) for the Segment methods.
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
 class ESMC_SegmentConfig;
 class ESMC_Segment;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_SegmentConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Segment : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    bool doHW;
    Timer aTimer[];
    HWMonitor aCounter[];
    int thread_state[];
// Following are valid values for thread_state...
//    NOT_USED   = -1
//    ACTIVE = 0
//    THREAD_WAS_ACTIVE = 1
    string name;
    int MaxThreads;
    double usrTime;
    double sysTime;
    double elapsedTime;
    double avgUsrTime;
    double avgSysTime;
    double avgElapsedTime;
    double minUsrTime;
    double minSysTime;
    double minElapsedTime;
    double maxUsrTime;
    double maxSysTime;
    double maxElapsedTime;


// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
 // ESMC_SegmentCreate and ESMC_SegmentDestroy are declared below,
 // outside the ESMC_Segment declaration
    int ESMC_SegmentConstruct(args);          // internal only, deep class
    int ESMC_SegmentDestruct(void);           // internal only, deep class


 // optional configuration methods
    int ESMC_SegmentGetConfig(ESMC_SegmentConfig *config) const;
    int ESMC_SegmentSetConfig(const ESMC_SegmentConfig *config);

 // accessor methods for class members
    int ESMC_SegmentGet<Value>(<value type> *value) const;
    int ESMC_SegmentSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_SegmentValidate(const char *options) const;
    int ESMC_SegmentPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Segment(args);
	~ESMC_Segment(args);
  
 // < declare the rest of the public interface methods here >
    void ESMC_SegmentBegin();
    void ESMC_SegmentEnd();
    void ESMC_SegmentComputeTime();
    bool ESMC_SegmentActive() const;
    string ESMC_SegmentGetName() const;
    void ESMC_SegmentGetTime(double time[], int bound); 

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_SegmentCreate - Create a new Segment
//
// !INTERFACE:
inline void ESMC_Segment::ESMC_SegmentCreate( bool doHardware ) {
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Segment
//
// !ARGUMENTS:
      bool doHardware ) {           // Flag to turn on/off hardware profiling
//
// !DESCRIPTION:
//      Create a new Segment from ... Allocates memory for a new Segment
//      object and uses the internal routine ESMC\_SegmentContruct to
//      initialize it.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

  doHW=doHardware;
  maxThreads = omp_Get_Max_Threads;   // TODO:: Use machine model
  // TODO:: Allocate memory for aTimer array for maxThreads elements
  // TODO:: Allocate thread_state array for maxThreads elements and set all to NOT_
USED
  if ( doHW ) {
    // TODO:: Allocate memory for aHWMonitor array for maxThreads elements
    // TODO:: Need to call HWMonitorInit for each thread
  }
 } // end ESMC_SegmentCreate

  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Segment

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_SegmentDestroy - free a Segment created with Create
//
// !INTERFACE:
      inline void ESMC_SegmentDestroy(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Segment object previously allocated
//      via an ESMC\_SegmentCreate routine.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  maxThreads = 0;
  // TODO:: Dealllocate memory for aTimer array for maxThreads elements

 } // end ESMC_SegmentDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentBegin - begin timing of a code segment
//
// !INTERFACE:
      void ESMC_Segment::ESMC_SegmentBegin(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      string someName,      // name of segment to time
      bool HWcounter ) {    // optional Hardware counter flag
//
// !DESCRIPTION:
//      This method begins instrumenting a region.  It calls a clock's a
//      clock's SetTime method (to record initial time) and the corresponding 
//      methods if hardware monitoring is enabled.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  mythread = omp_get_thread_num;  // TODO: Use Machine MODEL
  if ( mythread > maxThreads ) {
    // TODO:: Re-allocate aTimer aHWMonitor arrays bigger ....
  }
  if ( thread_state[mythread] != WAS_ACTIVE ||  thread_state[mythread] != NOT_USED 
) {
    // TODO:: Issue warning if this thread has been activated twice
  }
  thread_state[mythread] = ACTIVE;
  aTimer[mythread].TimerSet();
  if (doHW) {
     aHWMonitor[mythread].MonitorBegin();
  }

 } // end ESMC_SegmentBegin

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentEnd - end timing of a code segment
//
// !INTERFACE:
      void ESMC_Segment::ESMC_SegmentEnd( void ) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      This routines ends the timing of a region.  It calls a
//      Timers ComputeTime() to record the final times. This routine
//      then subtracts the initial time from the
//      final time to get the timer intervals for the current region.
//      If hardware monitoring is enabled, the hardware counters are also called.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  mythread = omp_get_thread_num;  // TODO: Use Machine MODEL
  if ( mythread > maxThreads ) {
    // TODO:: Allocate array bigger -- or die....
  }
  // TODO: Use SegmentActive method
  if ( thread_state[mythread] != ACTIVE ) {
    // TODO:: Issue warning if this thread didn't have a SegmentStart that went wit
h it
  }
  thread_state[mythread] = WAS_ACTIVE;
  aTimer[mythread].TimerCompute();
  if (doHW) {
     aHWMonitor[mythread].MonitorEnd();
  }

 } // end ESMC_SegmentEnd;



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentActive - check is Segment is active or not
//
// !INTERFACE:
       inline bool ESMC_Segment::ESMC_SegmentActive() const {
//
// !RETURN VALUE:
//    none// Boundary
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Checks if a segment is active or not.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
  mythread = omp_get_thread_num;  // TODO: Use Machine MODEL
  if ( thread_state[mythread] == ACTIVE ) {
    return true;
  } else {
    return false;
  }
}  // end ESMC_SegmentActive


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentGetTime - Get times
//
// !INTERFACE:
         inline void ESMC_Segment::ESMC_SegmentGetTime(
         double time[],    // Time
         int bound)        // Boundary
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Checks if a segment is active or not.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

{

     SegmentComputeTime();
     time[0]=avgUsrTime;
     time[1]=avgSysTime;
     time[2]=avgElapsedTime;
     time[3]=minUsrTime;
     time[4]=minSysTime;
     time[5]=minElapsedTime;
     time[6]=maxUsrTime;
     time[7]=maxSysTime;
     time[8]=maxElapsedTime;
     time[9]=usrTime;
     time[10]=sysTime;
     time[11]=elapsedTime;
}  //  end ESMC_SegmentGetTime

}

 #endif  // ESMC_Segment_H
