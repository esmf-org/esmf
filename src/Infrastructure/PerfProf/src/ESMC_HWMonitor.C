// $Id: ESMC_HWMonitor.C,v 1.3 2002/11/18 20:06:53 ekluz Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC HWMonitor method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ HWMonitor methods declared
// in the companion file ESMC_HWMonitor.h
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
 #include <ESMC_HWMonitor.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_HWMonitor.C,v 1.3 2002/11/18 20:06:53 ekluz Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the HWMonitor routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorInit - initializes a HWMonitor object
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorInit( void ) {
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//      ESMF routine which only initializes HWMonitor values; it does not
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

 } // end ESMC_HWMonitorInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorValidate - internal consistency check for a HWMonitor
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a HWMonitor is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorValidate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorPrint - print contents of a HWMonitor
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a HWMonitor.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorBegin - Begin hardware monitoring
//
// !INTERFACE:
      ESMC_HWMonitor::ESMC_HWMonitorBegin(void) {
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

 } // end ESMC_HWMonitorBegin


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorEnd - End hardware monitoring
//
// !INTERFACE:
      ESMC_HWMonitor::ESMC_HWMonitorEnd(void) {
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

 } // end ESMC_HWMonitorEnd


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorReport - Report hardware monitoring to log.
//
// !INTERFACE:
      ESMC_HWMonitor::ESMC_HWMonitorReport(void) {
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

 } // end ESMC_HWMonitorReport

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitor - native C++ constructor
//
// !INTERFACE:
      ESMC_HWMonitor::ESMC_HWMonitor(void) {
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

 } // end ESMC_HWMonitor

