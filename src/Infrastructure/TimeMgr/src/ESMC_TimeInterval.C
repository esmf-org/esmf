// $Id: ESMC_TimeInterval.C,v 1.9 2003/04/02 20:15:22 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC TimeInterval method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt TimeInterval} methods declared
// in the companion file {\tt ESMC_TimeInterval.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.9 2003/04/02 20:15:22 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_TimeInterval routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_TimeInterval Methods
//-------------------------------------------------------------------------

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal) { // in - optional associated calendar for
                            //      calendar intervals
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with given values
//
//EOP
// !REQUIREMENTS:

    ESMC_BaseTime::ESMC_BaseTimeInit(S, Sn, Sd);

    return(ESMF_SUCCESS);

}  // end ESMC_TimeIntervalInit
#endif

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalInit - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *YY,                  // in - integer number of interval years
      int *MO,                  // in - integer number of interval months
      int *D,                   // in - integer number of interval days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - long integer seconds 
      int *MS,                  // in - integer milliseconds
      int *US,                  // in - integer microseconds
      int *NS,                  // in - integer nanoseconds
      double *d_,               // in - floating point days
      double *h_,               // in - floating point hours
      double *m_,               // in - floating point minutes
      double *s_,               // in - floating point seconds
      double *ms_,              // in - floating point milliseconds
      double *us_,              // in - floating point microseconds
      double *ns_,              // in - floating point nanoseconds
      int *Sn,                  // in - fractional seconds numerator
      int *Sd,                  // in - fractional seconds denominator
      ESMC_Calendar *cal) {     // in - associated calendar
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with values given in variable arg list
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    this->S  = 0;
    this->Sn = 0;
    this->Sd = 1;
    Calendar = ESMC_NULL_POINTER;
    
    // TODO: validate inputs (individual and combos), set basetime values

    if (cal != ESMC_NULL_POINTER) {
      Calendar = cal;
    }
    if (D != ESMC_NULL_POINTER) {
      this->S += *D * 86400;
    }
    if (H != ESMC_NULL_POINTER) {
      this->S += *H * 3600;
    }
    if (M != ESMC_NULL_POINTER) {
      this->S += *M * 60;
    }
    if (S != ESMC_NULL_POINTER) {
      this->S += *S;
    }

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Read - restore TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_Read(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,       // in - integer seconds
      int Sn,                // in - fractional seconds, numerator
      int Sd,                // in - fractional seconds, denominator
      ESMC_Calendar *cal) {  // in - associated calendar
//
// !DESCRIPTION:
//      restore {\tt TimeInterval} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    if (cal == ESMC_NULL_POINTER) {
      cout << "ESMC_TimeInterval::ESMC_Read(): null pointer passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Read() first
    rc = ESMC_BaseTime::ESMC_Read(S, Sn, Sd);

    Calendar = cal;  // TODO?: this only restores calendar pointer; component
                     // must be sure to restore corresponding calendar first
  
    return(rc);

 }  // end ESMC_Read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Write - return TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_Write(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,            // out - integer seconds
      int *Sn,                     // out - fractional seconds, numerator
      int *Sd,                     // out - fractional seconds, denominator
      ESMC_Calendar *cal) const {  // out - associated calendar
//
// !DESCRIPTION:
//      return {\tt TimeInterval} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    if (S  == ESMC_NULL_POINTER || Sn  == ESMC_NULL_POINTER ||
        Sd == ESMC_NULL_POINTER || cal == ESMC_NULL_POINTER) {
      cout << "ESMC_TimeInterval::ESMC_Write(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    // use base class Write() first
    rc = ESMC_BaseTime::ESMC_Write(S, Sn, Sd);

    cal = Calendar;  // TODO?: this only saves calendar pointer; component
                     // must be sure to save corresponding calendar
  
    return(rc);

 }  // end ESMC_Write

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Validate - validate TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_Validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      validate {\tt TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_Validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Print - print Time state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_Print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    cout << "TimeInterval ---------------------------" << endl;
    ESMC_BaseTime::ESMC_Print(options);
    if (Calendar != ESMC_NULL_POINTER) {
      Calendar->ESMC_Calendar::ESMC_Print(options);
              //^^^^^^^^^^^^^^^TODO: override virtual function
              // mechanism to support F90 interface ?
    }
    cout << "end TimeInterval -----------------------" << endl << endl;

    return(ESMF_SUCCESS);

 }  // end ESMC_Print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native default C++ constructor
//
// !INTERFACE:
      ESMC_TimeInterval::ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes an {\tt ESMC\_TimeInterval} with defaults via
//      {\tt ESMC\_BaseTime} base class
//
//EOP
// !REQUIREMENTS:

//   ESMC_BaseTime(0, 0, 1) {  // use base class constructor
   S  = 0;
   Sn = 0;
   Sd = 0;
   Calendar = ESMC_NULL_POINTER;

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native C++ constructor
//
// !INTERFACE:
     ESMC_TimeInterval::ESMC_TimeInterval(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,       // in - integer seconds
      int Sn,                // in - fractional seconds, numerator
      int Sd,                // in - fractional seconds, denominator
      ESMC_Calendar *Cal) :  // in - optional calendar
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInterval} via {\tt ESMC\_BaseTime}
//      base class
//
//EOP
// !REQUIREMENTS:

    ESMC_BaseTime(S, Sn, Sd) {  // pass to base class constructor
    Calendar = Cal;

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TimeInterval - native default C++ destructor
//
// !INTERFACE:
      ESMC_TimeInterval::~ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_TimeInterval} destructor
//
//EOP
// !REQUIREMENTS:

}  // end ~ESMC_TimeInterval
