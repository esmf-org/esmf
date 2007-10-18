// $Id: ESMC_Segment.C,v 1.4.8.3 2007/10/18 02:43:06 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Segment method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Segment methods declared
// in the companion file ESMC\_Segment.h
//
// ESMF object to handle the code timing of a code segment.
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_Segment.h>
 #include <ESMC_Timer.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Segment.C,v 1.4.8.3 2007/10/18 02:43:06 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Segment routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentDestruct - release resources associated w/a Segment
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMF\_SegmentConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_Segment} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_SegmentDestroy}, which calls
//      {\tt ESMC\_SegmentDestruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_SegmentDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentInit - initializes a Segment object
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentInit(
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
//      ESMF routine which only initializes {\tt ESMC\_Segment} values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like {\tt ESMC\_SegmentCreate}.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_SegmentInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentGetConfig - get configuration info from a Segment
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_SegmentConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the {\tt ESMC\_Segment} object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_SegmentGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentSetConfig - set configuration info for a Segment
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_SegmentConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the {\tt ESMC\_Segment} object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_SegmentSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentValidate - internal consistency check for a Segment
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_Segment} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_SegmentValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentPrint - print contents of a Segment
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_Segment}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_SegmentPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_SegmentComputeTime - Compute time for this segment.
//
// !INTERFACE:
      int ESMC_Segment::ESMC_SegmentComputeTime(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

   int rank,numProc;

   for( thread=0; thread<=omp_Get_Max_Threads(); thread++ ) {
      if ( thread_state[thread] == THREAD_WAS_ACTIVE ) {
        sysTime=aTimer[thread].TimerGetSys();
        usrTime=aTimer[thread].TimerGetUsr();
        elapsedTime=aTimer[thread].TimerGetElapsed();

        numProc=MPI::COMM_WORLD.Get_size();
        rank=MPI::COMM_WORLD.Get_rank();

        MPI::COMM_WORLD.Reduce(&usrTime,&avgUsrTime,1,MPI::DOUBLE,MPI::SUM,0);
        MPI::COMM_WORLD.Reduce(&sysTime,&avgSysTime,1,MPI::DOUBLE,MPI::SUM,0);
        MPI::COMM_WORLD.Reduce(&elapsedTime,&avgElapsedTime,
                                 1,MPI::DOUBLE,MPI::SUM,0);
        if (rank ==0) {
          avgUsrTime /= numProc;
          avgSysTime /= numProc;
          avgElapsedTime /= numProc;
        }

        MPI::COMM_WORLD.Reduce(&usrTime,&minUsrTime,1,MPI::DOUBLE,MPI::MIN,0);
        MPI::COMM_WORLD.Reduce(&sysTime,&minSysTime,1,MPI::DOUBLE,MPI::MIN,0);
        MPI::COMM_WORLD.Reduce(&elapsedTime,&minElapsedTime,
                        1,MPI::DOUBLE,MPI::MIN,0);

        MPI::COMM_WORLD.Reduce(&usrTime,&maxUsrTime,1,MPI::DOUBLE,MPI::MAX,0);
        MPI::COMM_WORLD.Reduce(&sysTime,&maxSysTime,1,MPI::DOUBLE,MPI::MAX,0);
        MPI::COMM_WORLD.Reduce(&elapsedTime,&maxElapsedTime,
                        1,MPI::DOUBLE,MPI::MAX,0);
     }
  }


 } // end ESMC_SegmentComputeTime


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Segment - native C++ constructor
//
// !INTERFACE:
      ESMC_Segment::ESMC_Segment(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
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

 } // end ESMC_Segment

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Segment - native C++ destructor
//
// !INTERFACE:
      ESMC_Segment::~ESMC_Segment(void) {
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

 } // end ~ESMC_Segment
