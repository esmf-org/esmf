! $Id: ESMF_FlowWithCouplingSTest.F90,v 1.30 2009/10/28 02:05:36 theurich Exp $
!
! ESMF Coupled Flow Demo
!  Description on Sourceforge under System Test #74559
!

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo
!   2 components and 1 coupler, two-way coupling.
!   Flow application.
!
!
!\begin{verbatim}

    program ESMF_CoupledFlowDemo

#include "ESMF_Macros.inc"

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    use ESMF_TestMod
    
    ! User Component registration routines
    use   InjectorMod, only : Injector_register
    use FlowSolverMod, only : FlowSolver_register
    use    CouplerMod, only : Coupler_register

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_GridComp) :: INcomp, FScomp
    type(ESMF_CplComp) :: cpl
    character(len=ESMF_MAXSTR) :: cnameIN, cnameFS, cplname

    ! States and Virtual Machine
    type(ESMF_VM) :: vm
    type(ESMF_State) :: INimp, INexp, FSimp, FSexp

    integer :: pet_id, npets, rc

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status message
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "Coupled Flow Demo"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

    ! Initialize framework, and get back default VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Find out how many PETs we were given, and our local PET number
    call ESMF_VMGet(vm, petCount=npets, localPet=pet_id, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Make sure we can run successfully on what we were started on.
    if ((npets .lt. 4) .or. (npets .gt. 16)) then
        print *, "This test needs to run at least 4-way and no more"
        print *, " than 16-way, on a multiple of 4 processors."
        print *, "The requested number of processors was ", npets
        goto 10
    endif
    if (mod(npets, 4) .ne. 0) then
        print *, "This test needs to run on a multiple of 4 processors,"
        print *, " at least 4-way and no more than 16-way."
        print *, "The requested number of processors was ", npets
        goto 10
    endif

    ! Create the 2 model components and coupler.
    cnameIN = "Injector model"
    INcomp = ESMF_GridCompCreate(name=cnameIN, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cnameFS = "Flow Solver model"
    FScomp = ESMF_GridCompCreate(name=cnameFS, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(INcomp, Injector_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(FScomp, FlowSolver_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, Coupler_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 2 seconds
      call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to 12May2004, 9:00 am
      call ESMF_TimeSet(startTime, yy=2004, mm=5, dd=12, h=9, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to 12May2004, 12:00 pm
      ! to keep runtime down
      call ESMF_TimeSet(stopTime, yy=2004, mm=5, dd=12, h=12, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      ! Create import/export states for Injection Component
      INimp = ESMF_StateCreate("Injection Input", ESMF_STATE_IMPORT, rc=rc)
      INexp = ESMF_StateCreate("Injection Feedback", ESMF_STATE_EXPORT, rc=rc)

      call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Injection Model Initialize finished, rc =", rc
 
      ! Create import/export states for FlowSolver Component
      FSimp = ESMF_StateCreate("FlowSolver Input", ESMF_STATE_IMPORT, rc=rc)
      FSexp = ESMF_StateCreate("FlowSolver Feedback ", ESMF_STATE_EXPORT, rc=rc)

      call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Flow Model Initialize finished, rc =", rc

      ! initialize the coupler information going from injector to flow solver
      ! and back again.  in this case, it's ok to call init multiple times.
      ! if it wasn't, we could make a 2-phase init and call the first and
      ! second phases independently.
      call ESMF_CplCompInitialize(cpl, INexp, FSimp, clock, rc=rc)
      call ESMF_CplCompInitialize(cpl, FSexp, INimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Initialize finished, rc =", rc
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Run section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      print *, "Run Loop Start time"
      call ESMF_ClockPrint(clock, "currtime string", rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, FSexp, INimp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, INexp, FSimp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, "currtime string", rc)


      enddo
      print *, "Run Loop End time"
      call ESMF_ClockPrint(clock, "currtime string", rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, INexp, FSimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(INimp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(INexp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(FSimp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(FSexp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ClockDestroy(clock, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompDestroy(INcomp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(FScomp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CplCompDestroy(cpl, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
10    print *, "Coupled Flow Demo complete."

      write(failMsg, *)  "System Test failure"
      write(testname, *) "System Test FlowWithCoupling: Coupled Fluid Flow"
  
      if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: See output files for computed values"
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""

      endif
    
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

      call ESMF_Finalize(rc=rc)

      end program ESMF_CoupledFlowDemo
    
!\end{verbatim}
    
