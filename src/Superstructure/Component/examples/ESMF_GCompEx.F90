! $Id: ESMF_GCompEx.F90,v 1.20 2004/06/15 15:33:21 nscollins Exp $
!
! Example/test code which shows Gridded Component calls.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  One of many possible examples of a Gridded component.
!  Also see the Programming Model section of this document.
!
!
!EOP
!BOC
!   ! Example module showing Gridded Comp calls to the Component routines.
    module ESMF_GriddedCompEx
    
!   ! ESMF Framework module
    use ESMF_Mod
    implicit none
    public GComp_SetServices
    contains
!-------------------------------------------------------------------------
!   !  The SetServices routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine GComp_SetServices(comp, rc)
        type(ESMF_GridComp) :: comp
        integer :: rc

        ! SetServices the callback routines.
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, GComp_Init, 0, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, GComp_Run, 0, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, GComp_Final, 0, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_GridCompSetData(comp, mydatablock, rc)

    end subroutine

!-------------------------------------------------------------------------
!   !  Gridded Comp Component created by higher level calls, here is the
!   !   Initialization routine.
    
    subroutine GComp_Init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Gridded Comp Init starting"

        ! This is where the model specific setup code goes.  
 
        ! If the initial Export state needs to be filled, do it here.
        !call ESMF_StateAddField(exportState, field, rc)
        !call ESMF_StateAddBundle(exportState, bundle, rc)
        print *, "Gridded Comp Init returning"
   
    end subroutine GComp_Init

!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine GComp_Run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Gridded Comp Run starting"
        ! call ESMF_StateGetField(), etc to get fields, bundles, arrays
        !  from import state.

        ! This is where the model specific computation goes.

        ! Fill export state here using ESMF_StateAddField(), etc

        print *, "Gridded Comp Run returning"

    end subroutine GComp_Run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine GComp_Final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Gridded Comp Final starting"
    
        ! Add whatever code here needed

        print *, "Gridded Comp Final returning"
   
    end subroutine GComp_Final

    end module ESMF_GriddedCompEx

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    program ESMF_AppMainEx
    
!   ! The ESMF Framework module
    use ESMF_Mod
    
    ! User supplied modules
    use ESMF_GriddedCompEx, only: GComp_SetServices
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
    type(ESMF_GridComp) :: gcomp
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework and get the global VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
        print *, "Unable to initialize ESMF Framework"
        stop
    endif
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
    print *, "Application Example 1:"
    ! Create the top level application component
    cname = "Atmosphere Model Gridded Component"
    gcomp = ESMF_GridCompCreate(cname, configFile="setup.rc", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_GridCompSetServices(gcomp, GComp_SetServices, rc)
    print *, "Comp Create returned, name = ", trim(cname)
    ! Create the necessary import and export states used to pass data
    !  between components.
    importState = ESMF_StateCreate(cname, ESMF_STATE_IMPORT, rc=rc)
    exportState = ESMF_StateCreate(cname, ESMF_STATE_EXPORT, rc=rc)
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
    call ESMF_GridCompInitialize(gcomp, importState, exportState, clock=tclock, rc=rc)
    print *, "Comp Initialize complete"

    ! Main run loop.
    finished = .false.
    do while (.not. finished)
        call ESMF_GridCompRun(gcomp, importState, exportState, clock=tclock, rc=rc)
        call ESMF_ClockAdvance(tclock, timestep)
        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.
    enddo
    print *, "Comp Run complete"

    ! Give the component a chance to write out final results, clean up.
    call ESMF_GridCompFinalize(gcomp, importState, exportState, clock=tclock, rc=rc)
    print *, "Comp Finalize complete"

    ! Destroy components.
    call ESMF_ClockDestroy(tclock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)
    call ESMF_GridCompDestroy(gcomp, rc)
    print *, "Comp Destroy returned"
    print *, "Application Example 1 finished"

    call ESMF_Finalize(rc=rc)

    end program ESMF_AppMainEx
    
!EOC
    
