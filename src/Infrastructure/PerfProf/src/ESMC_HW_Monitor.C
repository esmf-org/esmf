// $Id: ESMC_HW_Monitor.C,v 1.1 2002/11/13 22:08:47 ekluz Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC HW_Monitor method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ HW_Monitor methods declared
// in the companion file ESMC_HW_Monitor.h
//
// Hardware monitor provides performance profiling of hardware
// specifics for sections of user code (cache misses, floating point
// utilization and operation counts etcetera).
//
// This type is implimented in C++ and a corresponding Fortran 90 
// interface is provided for access.
//
//-------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_HW_Monitor.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_HW_Monitor.C,v 1.1 2002/11/13 22:08:47 ekluz Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the HW_Monitor routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorInit - initializes a HW_Monitor object
//
// !INTERFACE:
      int ESMC_HW_Monitor::ESMC_HW_MonitorInit( void ) {
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//      ESMF routine which only initializes HW_Monitor values; it does not
//      allocate any resources.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
#if ( defined HAS_PCL )
#elif ( defined HAS_PAPI )
#else
#endif

 } // end ESMC_HW_MonitorInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorValidate - internal consistency check for a HW_Monitor
//
// !INTERFACE:
      int ESMC_HW_Monitor::ESMC_HW_MonitorValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a HW_Monitor is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_HW_MonitorValidate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorPrint - print contents of a HW_Monitor
//
// !INTERFACE:
      int ESMC_HW_Monitor::ESMC_HW_MonitorPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a HW_Monitor.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HW_MonitorPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorBegin - Begin hardware monitoring
//
// !INTERFACE:
      ESMC_HW_Monitor::ESMC_HW_MonitorBegin(void) {
//
// !RETURN VALUE:
//    none
//
// !DESCRIPTION:
//      Begins hardware monitoring for a code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HW_MonitorBegin


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorEnd - End hardware monitoring
//
// !INTERFACE:
      ESMC_HW_Monitor::ESMC_HW_MonitorEnd(void) {
//
// !RETURN VALUE:
//    none
//
// !DESCRIPTION:
//      Ends hardware monitoring for a code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HW_MonitorEnd


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_MonitorReport - Report hardware monitoring to log.
//
// !INTERFACE:
      ESMC_HW_Monitor::ESMC_HW_MonitorReport(void) {
//
// !RETURN VALUE:
//    none
//
// !DESCRIPTION:
//      Reports hardware monitoring for code segments timed.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HW_MonitorReport

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HW_Monitor - native C++ constructor
//
// !INTERFACE:
      ESMC_HW_Monitor::ESMC_HW_Monitor(void) {
//
// !RETURN VALUE:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HW_Monitor

