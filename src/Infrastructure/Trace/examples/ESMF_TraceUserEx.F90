! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_TraceUserEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

!BOE
! \subsubsection{Profiling/Tracing User-defined Code Regions} \label{ex:TraceUserEx}
!
! This example illustrates how to manually instrument code with
! entry and exit points for user-defined code regions. Note that the
! API calls {\tt ESMF\_TraceRegionEnter} and {\tt ESMF\_TraceRegionExit}
! should always appear in pairs, wrapping a particular section
! of code. The environment variable {\tt ESMF\_RUNTIME\_TRACE} 
! or {\tt ESMF\_RUNTIME\_PROFILE} must be set to {\tt ON} to enable these
! regions. If not at least one is set, the calls to
! {\tt ESMF\_TraceRegionEnter} and {\tt ESMF\_TraceRegionExit}
! will simply return immediately. For this reason, it is safe to
! leave this instrumentation in application code, even when not being profiled.  
!EOE


!  !PROGRAM: ESMF\_UserTrace - Examples of Trace usage.
!
!  !DESCRIPTION: 
!
! This program shows examples of Trace usage


!BOC
      ! Use ESMF framework module
      use ESMF
!EOC
      use ESMF_TestMod
!BOC
      implicit none

      ! Local variables  
      integer :: rc, finalrc
      integer :: i, j, tmp                 
!EOC
      integer                 :: petCount, localPet, &
                                 itemCount, count, result
      type(ESMF_VM)           :: vm

      
      type(ESMF_GridComp)     :: gridcomp
      character(ESMF_MAXSTR)  :: name
      character(ESMF_MAXSTR)  :: testname
      character(ESMF_MAXSTR)  :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_TraceUserEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOC     
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="TraceUserEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)        
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_TraceUser Example"
        print *, "--------------------------------------- "
      endif
!BOC
      ! record entrance into "outer_region"
      call ESMF_TraceRegionEnter("outer_region", rc=rc)

      tmp = 0
      do i=1, 10
         
         ! record entrance into "inner_region_1"
         call ESMF_TraceRegionEnter("inner_region_1", rc=rc)
         ! arbitrary computation
         do j=1,10000
            tmp=tmp+j+i
         enddo
         ! record exit from "inner_region_1"
         call ESMF_TraceRegionExit("inner_region_1", rc=rc)

         tmp = 0
         
         ! record entrance into "inner_region_2"
         call ESMF_TraceRegionEnter("inner_region_2", rc=rc)
         ! arbitrary computation
         do j=1,5000
            tmp=tmp+j+i
         enddo
         ! record exit from "inner_region_2"
         call ESMF_TraceRegionExit("inner_region_2", rc=rc)
      enddo

      ! record exit from "outer_region"
      call ESMF_TraceRegionExit("outer_region", rc=rc)
!EOC
      
      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)    

!BOC      
      call ESMF_Finalize(rc=rc)
!EOC
      
      if (localPet==0) then
         print *, "--------------------------------------- "
         print *, "End of ESMF_TraceUser Example"
         print *, "--------------------------------------- "
      endif

      if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
      if (finalrc==ESMF_SUCCESS) then
         print *, "PASS: ESMF_TraceUserEx.F90"
      else
         print *, "FAIL: ESMF_TraceUserEx.F90"
      endif

end program ESMF_TraceUserEx

