! $Id: ESMF_CplEx.F90,v 1.9 2004/01/14 17:10:24 nscollins Exp $
!
! Example/test code which shows Coupler Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  One of many possible examples of a Coupler component.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example module showing Coupler calls to the Component routines.
    module ESMF_CouplerEx
    
!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none

    public CPL_SetServices
    
    contains

        
!-------------------------------------------------------------------------
!   !  The SetServices routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine CPL_SetServices(comp, rc)
        type(ESMF_CplComp) :: comp
        integer :: rc

        ! SetServices the callback routines.
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, CPL_Init, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, CPL_Run, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, CPL_Final, 0, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CplCompSetInternalState(comp, mydatablock, rc)

    end subroutine


!-------------------------------------------------------------------------
!   !  Coupler Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine CPL_Init(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Init starting"
    
        ! Add whatever code here needed
        ! Precompute any needed values, fill in any inital values
        !  needed in Import States

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine CPL_Run(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Run starting"

        ! Add whatever code needed here to transform Export state data
        !  into Import states for the next timestep.  

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Run returning"

    end subroutine CPL_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine CPL_Final(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Final starting"
    
        ! Add whatever code needed here to compute final values and
        !  finish the computation.

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Final returning"
   
    end subroutine CPL_Final


    end module ESMF_CouplerEx
    


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


    program ESMF_AppMainEx
    
!   ! The ESMF Framework module
    use ESMF_Mod
    
    ! User supplied modules
    use ESMF_CouplerEx, only: CPL_SetServices
    implicit none
    
!   ! Local variables
    integer :: x, y, i, rc
    logical :: finished
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    integer :: delistall(4), delist1(4), delist2(4), delist3(4)
    character(ESMF_MAXSTR) :: cname
    type(ESMF_DELayout) :: layoutall, layout1, layout2, layout3
    type(ESMF_State) :: states(2)
    type(ESMF_CplComp) :: cpl
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    delist1 = (/ (i, i=0,3) /)
    layout1 = ESMF_DELayoutCreate(delist1, 2, (/ 1, 4 /), (/ 0, 0 /), rc)

    cname = "Atmosphere Model Coupler"
    cpl = ESMF_CplCompCreate(cname, layout1, configfile="setup.rc", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(cpl, CPL_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    states(1) = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc=rc)
    states(2) = ESMF_StateCreate(cname, ESMF_STATEIMPORT, rc=rc)

    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    ! initialize calendar to be Gregorian type
    call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    ! initialize start time to 5/1/2004
    call ESMF_TimeSet(startTime, yr=2004, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize stop time to 5/2/2004
    call ESMF_TimeSet(stopTime, yr=2004, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate("top clock", timeStep, startTime, stopTime, rc=rc)
     
    ! Call the Init routine.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_CplCompInitialize(cpl, statelist=states, clock=tclock, rc=rc)
    print *, "Comp Initialize complete"


    ! Main run loop.
    finished = .false.
    do while (.not. finished)

        call ESMF_CplCompRun(cpl, statelist=states, clock=tclock, rc=rc)
        call ESMF_ClockAdvance(tclock, timestep)

        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.

    enddo
    print *, "Comp Run complete"


    ! Give the component a chance to write out final results, clean up.
    call ESMF_CplCompFinalize(cpl, statelist=states, clock=tclock, rc=rc)
    print *, "Comp Finalize complete"


    ! Destroy components.
    call ESMF_CplCompDestroy(cpl, rc)
    print *, "Comp Destroy returned"


    print *, "Application Example 1 finished"

    call ESMF_Finalize(rc=rc)

    end program ESMF_AppMainEx
    
!\end{verbatim}
    
