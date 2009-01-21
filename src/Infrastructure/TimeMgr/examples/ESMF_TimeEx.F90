! $Id: ESMF_TimeEx.F90,v 1.34.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_TimeEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE	String used by test script to count examples.
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

      ! local variables for Get methods
      integer :: YY, MM, DD, H, M, S

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

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Initialization}

! This example shows how to initialize an {\tt ESMF\_Time}.
!EOE

!BOC
      ! initialize time1 to 2/28/2000 2:24:45
      call ESMF_TimeSet(time1, yy=2000, mm=2, dd=28, h=2, m=24, s=45, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Time1 = "
      call ESMF_TimePrint(time1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Increment}

! This example shows how to increment an {\tt ESMF\_Time} by
! an {\tt ESMF\_TimeInterval}.
!EOE

!BOC
      ! initialize a time interval to 2 days, 8 hours, 36 minutes, 15 seconds
      call ESMF_TimeIntervalSet(timeinterval1, d=2, h=8, m=36, s=15, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Timeinterval1 = "
      call ESMF_TimeIntervalPrint(timeinterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! increment time1 with timeinterval1
      time2 = time1 + timeinterval1

      call ESMF_TimeGet(time2, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      print *, "time2 = time1 + timeinterval1 = ", YY, "/", MM, "/", DD, " ", &
               H, ":", M, ":", S
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Comparison}

! This example shows how to compare two {\tt ESMF\_Times}.
!EOE

!BOC
      if (time2 > time1) then
        print *, "time2 is larger than time1"
      else
        print *, "time1 is smaller than or equal to time2"
      endif

      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_TimeEx.F90"
      else
         print *, "FAIL: ESMF_TimeEx.F90"
      end if

!BOC
      end program ESMF_TimeEx
!EOC
