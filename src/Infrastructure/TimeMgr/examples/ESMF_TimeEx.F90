! $Id: ESMF_TimeEx.F90,v 1.27 2004/06/02 01:42:21 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_TimeEx

!------------------------------------------------------------------------------
!EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_TimeEx - Time initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time initialization and manipulation
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate two times
      type(ESMF_Time) :: time1, time2

      ! instantiate a time interval
      type(ESMF_TimeInterval) :: timeinterval1

      ! local variables for Get functions
      integer :: D, H, M, S

      ! return code
      integer:: rc
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\subsubsection{Example 1.  Time Initialization}

! This example shows how to initialize two {\tt ESMF\_Time}s.
!EOP

!BOC
      ! initialize time1 to 2/28/2000 2:24:45
      call ESMF_TimeSet(time1, yy=2000, mm=2, dd=28, h=2, m=24, s=45, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Time1 = "
      call ESMF_TimePrint(time1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! initialize time2 to 3/1/2000 3:26:01
      call ESMF_TimeSet(time2, yy=2000, mm=3, dd=1, h=3, m=26, s=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Time2 = "
      call ESMF_TimePrint(time2, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\subsubsection{Example 2.  Difference Between Two Times}

! This example shows how to determine the difference between two
! {\tt ESMF\_Time}s.
!EOP

!BOC
      ! calculate difference between time2 and time1
      timeinterval1 = time2 - time1

      call ESMF_TimeIntervalGet(timeinterval1, d=D, h=H, m=M, s=S, rc=rc)
      print *, "Difference between time2 and time1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_TimeEx.F90"
     else
        print *, "FAIL: ESMF_TimeEx.F90"
     end if

!BOC
      end program ESMF_TimeEx
!EOC
