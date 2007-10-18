// $Id: ESMC_Perf.C,v 1.6.8.3 2007/10/18 02:43:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Perf method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Perf methods declared
// in the companion file ESMC_Perf.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_Perf.h>
 #include <ESMC_Segment.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Perf.C,v 1.6.8.3 2007/10/18 02:43:05 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Perf routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_PerfCreate - Create a new Perf object
//
// !INTERFACE:
      ESMC_Perf *ESMC_PerfCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Perf
//
// !ARGUMENTS:
      bool doHW;    // in, Flag to turn on/off hardware performance monitoring
      bool monitor; // in, Flag to turn on/off monitoring
      PEList sync_pe_list ) { // in - PE-list to synchronize with
//
// !DESCRIPTION:
//      Create a new Perf object. Allocates memory for a new Perf
//      object and uses the internal routine ESMC\_PerfConstruct to
//      initialize it.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_PerfCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_PerfDestroy - free a Perf created with Create
//
// !INTERFACE:
      int ESMC_PerfDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Perf *<class>) {    // in - <class> object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Perf object previously allocated
//      via an ESMC\_PerfCreate routine.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfConstruct - fill in an already allocated Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfConstruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    None
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Perf object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_PerfDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_PerfCreate, which calls
//      ESMC\_PerfConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfDestruct - release resources associated w/a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF\_PerfConstruct, does any additional cleanup before the
//      original Perf object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC\_PerfDestroy, which calls
//      ESMC\_PerfDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfInit - initializes a Perf object
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfInit(
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
//      ESMF routine which only initializes Perf values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC\_PerfCreate.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfGetConfig - get configuration info from a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PerfConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Perf object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfSetConfig - set configuration info for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_PerfConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Perf object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PerfSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfValidate - internal consistency check for a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfValidate(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Validates that a Perf is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_PerfValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfPrint - print contents of a Perf
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Perf.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_PerfPrint


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfStart - start performance monitoring on a code segment.
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfStart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *name) {     //  in - code segment name
//
// !DESCRIPTION:
//        Start performance monitoring of a given code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    string aName;
    int segment;
    bool newSegment;
    bool HWcounter;

    if ( ! monitoring ) {
       return;
    }
    if ( ! initialized ) {
       perfData.LogErrPrintErrFile(ESMC_ERR_FILE_ACTIVE,ESMC_WARNING,
                                __LINE__,__FILE__, __DIR__,__FUNC__);
       return;
    }
   //
    // Check to see if this is a segment that's already being timed or not
    //
    segment = ESMC_PerfFindSegment( someName );
    if ( segment == -1 ) { 
       newSegment = true; 
    } else {
       newSegment = false; 
    }
    //
    //
    //
    if ( doHardware.empty ) {
      HWcounter = doHW;
    } else {
      HWcounter = doHardware;
    }
    //
    // If this is a new section to time
    //
    if ( newSegment ) {
       currentSegment++;
       if ( currentSegment > MaxSegments ) {
         // TODO:: Allocate array bigger
       }
       aSegment[currentSegment].SegmentBegin( someName, HWcounter );
    //
    // If this is a section already timed once
    //
    } else {
       if ( aSegment[segment].SegmentActive ) {
          // If already active somethings wrong...
          // Segments being timed should be initially activated here
          // and then unactivated in a PerfEnd call.
          perfData.LogErrPrintErrFile(ESMC_ERR_FILE_ACTIVE,ESMC_FATAL,
                                __LINE__,__FILE__, __DIR__,__FUNC__);
       } else {
          aSegment[segment].SegmentBegin( someName, HWcounter );
       }
    }

 } // end ESMC_PerfStart


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfFindSegment - Find a segment
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfFindSegment(
//
// !RETURN VALUE:
//    int segment array index
//
// !ARGUMENTS:
      string someName ) {     //  in - code segment name
//
// !DESCRIPTION:
//        Find index number of given segment name.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    bool newSegment;
    int  Segment;

    newSegment = true;
    for (segment=0; segment <= currentSegment; segment++ ) {
       // TODO:: Do a real string comparision
       if ( aSegment[segment].SegmentGetName == someName ) {
         newSegment = false;
         break;
       }
    }
    if ( newSegment ) {
      segment = -1;
    }
    return( segment );
 } // end ESMC_PerfFindSegment

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PerfEnd- end performance monitoring on a code segment.
//
// !INTERFACE:
      int ESMC_Perf::ESMC_PerfEnd(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *name) {     //  in - code segment name
//
// !DESCRIPTION:
//        End performance monitoring of a given code segment.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_PerfEnd
