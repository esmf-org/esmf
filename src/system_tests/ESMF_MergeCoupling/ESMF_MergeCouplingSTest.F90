! $Id: ESMF_MergeCouplingSTest.F90,v 1.19 2008/11/14 04:39:17 theurich Exp $
!
! System test code MergeCoupling
!  Description on Sourceforge under System Test #62502

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test MergeCoupling.  
! 3 components and 1 coupler, two-way coupling.
!
!
!\begin{verbatim}

    program MergeCoupling

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_model3, only : userm3_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: pet_id, npets, rc
    character(len=ESMF_MAXSTR) :: cname1, cname2, cname3, cplname
    type(ESMF_VM) :: vm
    type(ESMF_State) :: c1exp, c2exp, c3imp, bothexp
    type(ESMF_GridComp) :: comp1, comp2, comp3
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_KIND_I8) :: advanceCount

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, final output summary line
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test MergeCoupling:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Initialize framework and get default global VM back
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Query for the number of PETs we were started with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (npets .lt. 4) then
        print *, "This system test needs to run at least 4-way, current np = ", npets
        goto 10
    endif



    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname2), "rc =", rc

    cname3 = "user model 3"
    comp3 = ESMF_GridCompCreate(name=cname3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname3), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cplname), ", rc =", rc


    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, userm1_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(comp2, userm2_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 2 SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(comp3, userm3_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 3 SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, usercpl_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Cpl Comp SetServices finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 4 hours
      call ESMF_TimeIntervalSet(timeStep, h=4, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to 3/28/2003
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=28, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to 3/29/2003
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=29, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Initialize finished, rc =", rc
 
      c2exp = ESMF_StateCreate("comp2 export", ESMF_STATE_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp2, exportState=c2exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Initialize finished, rc =", rc
 
      c3imp = ESMF_StateCreate("comp3 import", ESMF_STATE_IMPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp3, importState=c3imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1a Initialize finished, rc =", rc

      bothexp = ESMF_StateCreate("coupler import", ESMF_STATE_IMPORT, rc=rc)
      call ESMF_StateAddState(bothexp, c1exp, rc=rc)
      call ESMF_StateAddState(bothexp, c2exp, rc=rc)

      call ESMF_CplCompInitialize(cpl, bothexp, c3imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Comp 1 Run returned, rc =", rc
  
        call ESMF_GridCompRun(comp2, exportState=c2exp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Comp 2 Run returned, rc =", rc
  
        call ESMF_CplCompRun(cpl, bothexp, c3imp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_GridCompRun(comp3, importState=c3imp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Comp 2 Run returned, rc =", rc

        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, rc=rc)

      enddo
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Finalize finished, rc =", rc

      call ESMF_GridCompFinalize(comp2, exportState=c2exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 2 Finalize finished, rc =", rc

      call ESMF_GridCompFinalize(comp3, importState=c3imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 3 Finalize finished, rc =", rc

      call ESMF_CplCompFinalize(cpl, bothexp, c3imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Finalize finished, rc =", rc


      ! Figure out our local processor id for message below.
      call ESMF_VMGet(vm, localPet=pet_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"
      print *, "Test finished, pet_id = ", pet_id
      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(c1exp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(c2exp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(c3imp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(bothexp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ClockDestroy(clock, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompDestroy(comp1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(comp2, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(comp3, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CplCompDestroy(cpl, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test MergeCoupling complete."

      ! Only output on processor 0, or if error anyplace
      if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then

        ! Standard ESMF Test output to log file
        write(failMsg, *)  "Component Coupling"
        write(testname, *) "System Test MergeCoupling: Merge Component Coupling"
  
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)

        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: Merge Component Coupling complete."
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""
  
      endif
    
      call ESMF_Finalize(rc=rc) 

      end program MergeCoupling
    
!\end{verbatim}
    
