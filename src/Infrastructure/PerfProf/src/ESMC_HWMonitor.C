// $Id: ESMC_HWMonitor.C,v 1.8.8.3 2007/10/18 02:43:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC HWMonitor method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_HWMonitor} methods declared
// in the companion file ESMC\_HWMonitor.h
//
// Hardware monitor provides performance profiling of hardware
// specifics for sections of user code (cache misses, floating point
// utilization and operation counts et cetera).
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
 static const char *const version = "$Id: ESMC_HWMonitor.C,v 1.8.8.3 2007/10/18 02:43:05 cdeluca Exp $";
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
//      ESMF routine which only initializes {\tt ESMC\_HWMonitor} values; it does not
//      allocate any resources.
//
//EOP
// !REQUIREMENTS:  

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
//      Validates that a {\tt ESMC\_HWMonitor} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
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
//      Print information about a {\tt ESMC\_HWMonitor}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorPrint


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetFLOPS - return Floating point Operations per 
// second.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetFLOPS(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the floating point operations per second for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetFLOPS

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetMemoryOpsPercent- return percentage of load-store 
// operations.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetMemoryOpsPercent(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the percentage of load-store operations for all instructions for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetFLOPS


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetL1CacheUtilRate - return L1-cache utilization
// rate.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetL1CacheUtilRate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the level 1 cache utilization rate for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetL1CacheUtilRate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetL1CacheMissRate - return cache miss
// rate.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetL1CacheMissRate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the level-1 cache miss rate for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetL1CacheMissRate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetL2CacheUtilRate - return L2-cache utilization
// rate.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetL2CacheUtilRate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the level 2 cache utilization rate for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetL2CacheUtilRate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetL2CacheMissRate - return cache miss
// rate.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetL2CacheMissRate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the level-2 cache miss rate for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetL2CacheMissRate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetFPUUtilRate - return FPU Utililization miss
// rate.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetFPUUtilRate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the FPU utilization rate for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetFPUUtilRate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_HWMonitorGetCycles - return number of CPU cycles in
// a code segment.
//
// !INTERFACE:
      int ESMC_HWMonitor::ESMC_HWMonitorGetCycles(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      Get the number of code cycles for this code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_HWMonitorGetCycles


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

