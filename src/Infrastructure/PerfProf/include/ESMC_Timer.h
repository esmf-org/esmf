// $Id: ESMC_Timer.h,v 1.5.14.3 2007/10/18 02:43:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Timer C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Timer_H
 #define ESMC_Timer_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 //#include <ESMC_PerfProf.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Timer - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Timer members and declares method 
// signatures (prototypes).  The companion file ESMC\_Timer.C contains
// the definitions (full code bodies) for the Timer methods.
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
 class ESMC_TimerConfig;
 class ESMC_Timer;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_TimerConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Timer : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     double usrTimeStart; 
     double sysTimeStart; 
     double usrTime; 
     double sysTime; 
     double elapsedTimeStart; 
     double elapsedTime; 


// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_TimerInit(args);         // shallow class only

 // optional configuration methods
    int ESMC_TimerGetConfig(ESMC_TimerConfig *config) const;
    int ESMC_TimerSetConfig(const ESMC_TimerConfig *config);

 // accessor methods for class members
    int ESMC_TimerGet<Value>(<value type> *value) const;
    int ESMC_TimerSet<Value>(<value type>  value);

    inline double ESMC_TimerGetUsr();
    inline double ESMC_TimerGetSys();
    inline double  ESMC_TimerGetElapsed(); 
    double ESMC_TimerGetHpcWall();

    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_TimerValidate(const char *options) const;
    int ESMC_TimerPrint(const char *options) const;
    void ESMC_TimerStart ();
    void ESMC_TimerComputeElapsed ();

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerInit - initializes a Timer object
//
// !INTERFACE:
      inline void ESMC_Timer::ESMC_TimerInit( void ) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which only initializes Timer values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC\_TimerCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  usrTimeStart=0;
  sysTimeStart=0;
  usrTime=0;
  sysTime=0;
  elapsedTimeStart=0;
  elapsedTime=0;

 } // end ESMC_TimerInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetSys - get System time for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGetSys( void ) {
//
// !RETURN VALUE:
//    double system time
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Returns the value of Timer member sysTime.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    return sysTime;

 } // end ESMC_TimerGetSys

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetUsr - get User time for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGetUsr( void ) {
//
// !RETURN VALUE:
//    double user time
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Returns the value of Timer member usrTime.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    return usrTime;

 } // end ESMC_TimerGetUsr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetElapsed - get Elapsed time for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGetElapsed( void ) {
//
// !RETURN VALUE:
//    double elapsed time
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Returns the value of Timer member elapsedTime.
//
//EOP
// !REQUIREMENTS:

    return elapsedTime;

 } // end ESMC_TimerGetElapsed

 // native C++ constructors/destructors
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Timer - native C++ constructor
//
// !INTERFACE:
    inline ESMC_Timer::ESMC_Timer(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF shallow methods for initialization.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

   usrTimeStart=0;
   sysTimeStart=0;
   usrTime=0;
   sysTime=0;
   elapsedTimeStart=0;
   elapsedTime=0;

}

	~ESMC_Timer();
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Timer

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Timer object itself. E.g. if Create
// were a method, the ESMC_Timer object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Timer object.

 ESMC_Timer *ESMC_TimerCreate(args, int *rc);// interface only, deep class
 int ESMC_TimerDestroy(ESMC_Timer *<class>); // interface only, deep class

 #endif  // ESMC_Timer_H
