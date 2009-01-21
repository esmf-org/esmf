! $Id: ESMF_TimeIntervalEx.F90,v 1.28.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_TimeIntervalEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_TimeIntervalEx - Time Interval initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time Interval initialization and manipulation
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate some time intervals
      type(ESMF_TimeInterval) :: timeinterval1, timeinterval2, timeinterval3

      ! local variables
      integer :: d, h, m, s

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
!\subsubsection{Time Interval Initialization}

! This example shows how to initialize two {\tt ESMF\_TimeIntervals}.
!EOE

!BOC
      ! initialize time interval1 to 1 day
      call ESMF_TimeIntervalSet(timeinterval1, d=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalPrint(timeinterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize time interval2 to 4 days, 1 hour, 30 minutes, 10 seconds
      call ESMF_TimeIntervalSet(timeinterval2, d=4, h=1, m=30, s=10, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalPrint(timeinterval2, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Interval Conversion}

! This example shows how to convert {\tt ESMF\_TimeIntervals} into 
! different units.
!EOE

!BOC
      call ESMF_TimeIntervalGet(timeinterval1, s=s, rc=rc)
      print *, "Time Interval1 = ", s, " seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalGet(timeinterval2, h=h, m=m, s=s, rc=rc)
      print *, "Time Interval2 = ", h, " hours, ", m, " minutes, ", &
                                    s, " seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Interval Difference}

! This example shows how to calculate the difference between two 
! {\tt ESMF\_TimeIntervals}. 
!EOE

!BOC
      ! difference between two time intervals
      timeinterval3 = timeinterval2 - timeinterval1
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
      print *, "Difference between TimeInterval2 and TimeInterval1 = ", &
               d, " days, ", h, " hours, ", m, " minutes, ", s, " seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Interval Multiplication}

! This example shows how to multiply an {\tt ESMF\_TimeInterval}. 
!EOE

!BOC
      ! multiply time interval by an integer
      timeinterval3 = timeinterval2 * 3
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
      print *, "TimeInterval2 multiplied by 3 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Interval Comparison}

! This example shows how to compare two {\tt ESMF\_TimeIntervals}. 
!EOE

!BOC
      ! comparison
      if (timeinterval1 < timeinterval2) then
        print *, "TimeInterval1 is smaller than TimeInterval2"
      else 
        print *, "TimeInterval1 is larger than or equal to TimeInterval2"
      end if

      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_TimeIntervalEx.F90"
      else
         print *, "FAIL: ESMF_TimeIntervalEx.F90"
      end if

!BOC
      end program ESMF_TimeIntervalEx
!EOC
