! $Id: ESMF_CplEx.F90,v 1.17 2004/04/26 15:34:19 nscollins Exp $
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
    
    subroutine CPL_Init(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer :: rc
        type(ESMF_State) :: nestedstate

        print *, "Coupler Init starting"
    
        ! Add whatever code here needed
        ! Precompute any needed values, fill in any inital values
        !  needed in Import States

        call ESMF_StateGetState(importState, "substate 1", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 2", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 3", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init

!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine CPL_Run(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        type(ESMF_State) :: nestedstate

        print *, "Coupler Run starting"

        ! Add whatever code needed here to transform Export state data
        !  into Import states for the next timestep.  

        call ESMF_StateGetState(importState, "substate 1", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 2", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 3", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)

        print *, "Coupler Run returning"

    end subroutine CPL_Run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine CPL_Final(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        type(ESMF_State) :: nestedstate

        print *, "Coupler Final starting"
    
        ! Add whatever code needed here to compute final values and
        !  finish the computation.

        call ESMF_StateGetState(importState, "substate 1", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 2", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)
        call ESMF_StateGetState(importState, "substate 3", nestedstate, rc)
        call ESMF_StatePrint(nestedstate, rc=rc)

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
    type(ESMF_VM) :: vm
    type(ESMF_State) :: importState, exportState
    type(ESMF_CplComp) :: cpl
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework and get the default VM
    call ESMF_Initialize(rc=rc)
    call ESMF_VMGetGlobal(vm, rc)
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    cname = "Atmosphere Model Coupler"
    cpl = ESMF_CplCompCreate(cname, configFile="setup.rc", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(cpl, CPL_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    exportState = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc=rc)
    importState = ESMF_StateCreate(cname, ESMF_STATEIMPORT, rc=rc)

    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    ! initialize start time to 5/1/2004
    call ESMF_TimeSet(startTime, yy=2004, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize stop time to 5/2/2004
    call ESMF_TimeSet(stopTime, yy=2004, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate("top clock", timeStep, startTime, stopTime, rc=rc)
     
    ! Call the Init routine.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_CplCompInitialize(cpl, exportState, importState, tclock, rc=rc)
    print *, "Comp Initialize complete"

    ! Main run loop.
    finished = .false.
    do while (.not. finished)
        call ESMF_CplCompRun(cpl, exportState, importState, tclock, rc=rc)
        call ESMF_ClockAdvance(tclock, timestep)
        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.
    enddo
    print *, "Comp Run complete"

    ! Give the component a chance to write out final results, clean up.
    call ESMF_CplCompFinalize(cpl, exportState, importState, tclock, rc=rc)
    print *, "Comp Finalize complete"

    ! Destroy components.
    call ESMF_ClockDestroy(tclock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)
    call ESMF_CplCompDestroy(cpl, rc)
    print *, "Comp Destroy returned"

    print *, "Application Example 1 finished"

    call ESMF_Finalize(rc=rc)

    end program ESMF_AppMainEx
    
!\end{verbatim}
    
