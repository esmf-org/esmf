! $Id: CoupledFlowApp.F90,v 1.2 2003/05/02 20:53:17 nscollins Exp $
!
! ESMF Coupled Application Wrapper
!

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! ESMF Application Wrapper for Coupled Flow Demo
!
!
!\begin{verbatim}

    program ESMF_ApplicationWrapper

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! Flow Component registration routines
    use CoupledFlowMod, only : CoupledFlow_register

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_AppComp) :: compApp
    type(ESMF_GridComp) :: compGridded

    ! States and Layouts
    type(ESMF_DELayout) :: layoutApp
    type(ESMF_State) :: flowstate

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Return codes for error checks
    integer :: rc
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "Coupled Flow Demo Application Start"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

    ! Create the top level application component.  This initializes the
    ! ESMF Framework as well.
    compApp = ESMF_AppCompCreate("Coupled Flow Application", rc=rc)

    ! Query application for default layout.
    call ESMF_AppCompGet(compApp, layout=layoutApp, rc=rc)

   
    ! Create the Gridded component, passing in the default layout.
    compGridded = ESMF_GridCompCreate("Coupled Flow Demo", layout=layoutApp, rc=rc)


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compGridded, CoupledFlow_register, rc)
      print *, "Comp SetServices finished, rc= ", rc


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 2 seconds
      call ESMF_TimeIntervalInit(timeStep, S=int(2,kind=ESMF_IKIND_I8), rc=rc)

      ! initialize start time to 12May2003, 9:00 am
      call ESMF_TimeInit(startTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=12, H=9, M=0, S=int(0,kind=ESMF_IKIND_I8), &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 15May2003, 9:00 am
      call ESMF_TimeInit(stopTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=15, H=9, M=0, S=int(0,kind=ESMF_IKIND_I8), &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a State to use for both import and export.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      flowstate = ESMF_StateCreate("Coupled Flow State", rc=rc)
     
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init, Run, and Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      call ESMF_GridCompInitialize(compGridded, flowstate, flowstate, &
                                                                 clock, rc=rc)
      print *, "Coupled Flow Component Initialize finished, rc =", rc
 


      call ESMF_GridCompRun(compGridded, flowstate, flowstate, clock, rc=rc)
      print *, "Coupled Flow Component Run finished, rc =", rc
 


      call ESMF_GridCompFinalize(compGridded, flowstate, flowstate, clock, rc=rc)
      print *, "Coupled Flow Component Finalize finished, rc =", rc
 
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(flowstate, rc)

      call ESMF_GridCompDestroy(compGridded, rc)

      call ESMF_DELayoutDestroy(layoutApp, rc)

      call ESMF_AppCompDestroy(compApp, rc)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
10    print *, "Coupled Flow Application Demo complete!"

      end program ESMF_ApplicationWrapper
    
!\end{verbatim}
    
