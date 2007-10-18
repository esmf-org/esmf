// $Id: ESMC_Timer.C,v 1.7.8.3 2007/10/18 02:43:06 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Timer method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Timer} methods declared
// in the companion file ESMC\_Timer.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_Timer.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Timer.C,v 1.7.8.3 2007/10/18 02:43:06 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Timer routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_TimerCreate - Create a new Timer
//
// !INTERFACE:
      ESMC_Timer *ESMC_TimerCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Timer
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3     // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new {\tt ESMC\_Timer} from ... Allocates memory for a new {\tt ESMC\_Timer}
//      object and uses the internal routine {\tt ESMC\_TimerConstruct} to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use {\tt ESMC\_TimerInit}.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Timer.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_TimerCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_TimerDestroy - free a Timer created with Create
//
// !INTERFACE:
      int ESMC_TimerDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Timer *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a {\tt ESMC\_Timer} object previously allocated
//      via an {\tt ESMC\_TimerCreate} routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Timer.h)
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_TimerDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerConstruct - fill in an already allocated Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated {\tt ESMC\_Timer} object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_TimerDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_TimerCreate}, which calls
//      {\tt ESMC\_TimerConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_TimerConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerDestruct - release resources associated w/a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMF\_TimerConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_Timer} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_TimerDestroy}, which calls
//      {\tt ESMC\_TimerDestruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_TimerDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetConfig - get configuration info from a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimerConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the {\tt ESMC\_Timer} object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_TimerGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerSetConfig - set configuration info for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_TimerConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the {\tt ESMC\_Timer} object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_TimerSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerValidate - internal consistency check for a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_Timer} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_TimerValidate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerStart - start a timer for a code segment
//
// !INTERFACE:
      void ESMC_Timer::ESMC_TimerStart(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Start a {\tt ESMC\_Timer} for a code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  struct tms local;

  times(&local);
  usrTimeStart=(double) local.tms_utime/TICKS;
  sysTimeStart= (double) local.tms_stime/TICKS;
  elapsedTimeStart=TimerHpcWall();

 } // end ESMC_TimerStart


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerPrint - print contents of a Timer
//
// !INTERFACE:
      int ESMC_Timer::ESMC_TimerPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_Timer}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_TimerPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerGetHpcWall - Get wall clock time
//
// !INTERFACE:
      double ESMC_Timer::ESMC_TimerGetHpcWall( void ) {
//
// !RETURN VALUE:
//    double Wall clock time
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Get the wall clock time.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  static long zsec = 0;
  static long zusec = 0;
  double esec;
  struct timeval tp;
  struct timezone tzp;
 
  gettimeofday(&tp, &tzp);
 
  if ( zsec == 0 ) zsec = tp.tv_sec;
  if ( zusec == 0 ) zusec = tp.tv_usec;
 
  return (tp.tv_sec - zsec) + (tp.tv_usec - zusec ) * 0.000001 ;

 } // end ESMC_TimerGetHpcWall


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimerComputeElapsed - Compute elapsed times
//
// !INTERFACE:
      void ESMC_Timer::ESMC_TimerComputeElapsed( void ) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Compute the elapsed time for later retrieval.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  static long zsec = 0;
  static long zusec = 0;
  double esec;
  struct timeval tp;
  struct timezone tzp;
 
  gettimeofday(&tp, &tzp);
 
  if ( zsec == 0 ) zsec = tp.tv_sec;
  if ( zusec == 0 ) zusec = tp.tv_usec;
 
  return (tp.tv_sec - zsec) + (tp.tv_usec - zusec ) * 0.000001 ;

 } // end ESMC_TimerGetElapsed


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Timer - native C++ destructor
//
// !INTERFACE:
      ESMC_Timer::~ESMC_Timer(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ~ESMC_Timer
