! $Id: ESMF_CplEx.F90,v 1.27.4.2 2008/04/01 00:26:07 theurich Exp $
!
! Example/test code which shows Coupler Component calls.

!-------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_CplEx.F90 - Coupler Component example
! 
! !DESCRIPTION:
!
!   The skeleton of one of many possible Coupler component models.
!
!-----------------------------------------------------------------------------

!EOC

!BOP
!\subsubsection{Specifying a User-Code SetServices Routine}
!
! Every {\tt ESMF\_CplComp} is required to provide and document
! a set services routine.  It can have any name, but must
! follow the declaration below: a subroutine which takes an 
! {\tt ESMF\_CplComp} as the first argument, and 
! an integer return code as the second.
! Both arguments are required and must {\em not} be declared as 
! {\tt optional}. If an intent is specified in the interface it must be 
! {\tt intent(inout)} for the first and {\tt intent(out)} for the 
! second argument.
!
! The set services routine must call the ESMF method 
! {\tt ESMF\_CplCompSetEntryPoint()} to
! register with the framework what user-code subroutines should be called
! to initialize, run, and finalize the component.  There are
! additional routines which can be registered as well, for checkpoint
! and restart functions.
!
! Note that the actual subroutines being registered do not have to be
! public to this module; only the set services routine itself must
! be available to be used by other code.
!EOP

!BOC
    ! Example Coupler Component
    module ESMF_CouplerEx
    
    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    public CPL_SetServices

    contains

    subroutine CPL_SetServices(comp, rc)
        type(ESMF_CplComp) :: comp
        integer, intent(out) :: rc

        ! SetServices the callback routines.
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, CPL_Init, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, CPL_Run, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, CPL_Final, 0, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CplCompSetInternalState(comp, mydatablock, rc)

        rc = ESMF_SUCCESS
    end subroutine
!EOC


!BOP
!\subsubsection{Specifying a User-Code Initialize Routine}
!
! When a higher level component is ready to begin using an 
! {\tt ESMF\_CplComp},
! it will call its initialize routine.  The component writer must
! supply a subroutine with the exact calling sequence below; no arguments
! can be optional, and the types and order must match.
!
! At initialization time the component can allocate data space, open
! data files, set up initial conditions; anything it needs to do to
! prepare to run.
!
! The {\tt rc} return code should be set if an error occurs, otherwise
! the value {\tt ESMF\_SUCCESS} should be returned.
!EOP
    
!BOC
    subroutine CPL_Init(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        print *, "Coupler Init starting"
    
        ! Add whatever code here needed
        ! Precompute any needed values, fill in any inital values
        !  needed in Import States

        rc = ESMF_SUCCESS

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init
!EOC

!BOP
!\subsubsection{Specifying a User-Code Run Routine}
!
! During the execution loop, the run routine may be called many times.
! Each time it should read data from the {\tt importState}, use the
! {\tt clock} to determine what the current time is in the calling
! component, compute new values or process the data, 
! and produce any output and place it in the {\tt exportState}.
!
! When a higher level component is ready to use the {\tt ESMF\_CplComp}
! it will call its run routine.  The component writer must
! supply a subroutine with the exact calling sequence below; 
! no arguments can be optional, and the types and order must match.
!
! It is expected that this is where the bulk of the model computation
! or data analysis will occur.
!
! The {\tt rc} return code should be set if an error occurs, otherwise
! the value {\tt ESMF\_SUCCESS} should be returned.
!EOP
    
!BOC
    subroutine CPL_Run(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc


        print *, "Coupler Run starting"

        ! Add whatever code needed here to transform Export state data
        !  into Import states for the next timestep.  

        rc = ESMF_SUCCESS

        print *, "Coupler Run returning"

    end subroutine CPL_Run
!EOC

!BOP
!\subsubsection{Specifying a User-Code Finalize Routine}
!
! At the end of application execution, each {\tt ESMF\_CplComp} should
! deallocate data space, close open files, and flush final results.
! These functions should be placed in a finalize routine.
!
! The {\tt rc} return code should be set if an error occurs, otherwise
! the value {\tt ESMF\_SUCCESS} should be returned.
!
!EOP
    
!BOC
    subroutine CPL_Final(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc


        print *, "Coupler Final starting"
    
        ! Add whatever code needed here to compute final values and
        !  finish the computation.

        rc = ESMF_SUCCESS

        print *, "Coupler Final returning"
   
    end subroutine CPL_Final

    end module ESMF_CouplerEx
!EOC

!-------------------------------------------------------------------------
! Note - the program below is here only to make this an executable
! program.  It should not appear in the documentation for a Coupler Component.
!-------------------------------------------------------------------------

    program ESMF_AppMainEx
    
!   ! The ESMF Framework module
    use ESMF_Mod
    
    ! User supplied modules
    use ESMF_CouplerEx, only: CPL_SetServices
    implicit none
!   ! Local variables
    integer :: rc
    logical :: finished
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    character(ESMF_MAXSTR) :: cname
    type(ESMF_VM) :: vm
    type(ESMF_State) :: importState, exportState
    type(ESMF_CplComp) :: cpl
    integer :: finalrc
    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework and get the default VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
        print *, "Unable to initialize ESMF Framework"
        print *, "FAIL: ESMF_CplEx.F90"
        stop
    endif
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    cname = "Atmosphere Model Coupler"
    cpl = ESMF_CplCompCreate(name=cname, configFile="setup.rc", rc=rc)  

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(cpl, CPL_SetServices, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    exportState = ESMF_StateCreate(cname, ESMF_STATE_EXPORT, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    importState = ESMF_StateCreate(cname, ESMF_STATE_IMPORT, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    


    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", ESMF_CAL_GREGORIAN, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    


    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    


    ! initialize start time to 5/1/2004
    call ESMF_TimeSet(startTime, yy=2004, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    


    ! initialize stop time to 5/2/2004
    call ESMF_TimeSet(stopTime, yy=2004, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    


    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate("top clock", timeStep, startTime, stopTime, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

     
    ! Call the Init routine.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_CplCompInitialize(cpl, exportState, importState, tclock, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    print *, "Comp Initialize complete"

    ! Main run loop.
    finished = .false.
    do while (.not. finished)
        call ESMF_CplCompRun(cpl, exportState, importState, tclock, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if    

        call ESMF_ClockAdvance(tclock, timestep)
        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.
    enddo
    print *, "Comp Run complete"

    ! Give the component a chance to write out final results, clean up.
    call ESMF_CplCompFinalize(cpl, exportState, importState, tclock, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    print *, "Comp Finalize complete"

    ! Destroy components.
    call ESMF_ClockDestroy(tclock, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    call ESMF_CalendarDestroy(gregorianCalendar, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    call ESMF_CplCompDestroy(cpl, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    

    print *, "Comp Destroy returned"

    print *, "Application Example 1 finished"

    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if    
    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_CplEx.F90"
    else
        print *, "FAIL: ESMF_CplEx.F90"
    end if



    end program ESMF_AppMainEx
    
