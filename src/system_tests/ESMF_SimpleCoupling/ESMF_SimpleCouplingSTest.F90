! $Id: ESMF_SimpleCouplingSTest.F90,v 1.15 2004/04/15 22:05:13 nscollins Exp $
!
! System test code SimpleCoupling
!  Description on Sourceforge under System Test #62502

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test SimpleCoupling.  
! 2 components and 1 coupler, one-way coupling.
!
!
!\begin{verbatim}

    program SimpleCoupling

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: de_id, ndes, rc, delist(4)
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM) :: vm
    type(ESMF_newDELayout) :: layout1, layout2, layout3
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
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

    print *, "System Test SimpleCoupling:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Initialize framework
    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Query for the default VM
    call ESMF_VMGetGlobal(vm, rc)
    
    ! Query for the default layout
    layout1 = ESMF_newDELayoutCreate(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_newDELayoutPrint(layout1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call ESMF_newDELayoutGet(layout1, deCount=ndes, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    if (ndes .lt. 4) then
        print *, "This system test needs to run at least 4-way, current np = ", ndes
        goto 10
    endif



    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    !delist = (/ 0, 1, 2, 3 /)
    !layout2 = ESMF_newDELayoutCreate(delist, 2, (/ 4, 1 /), (/ 0, 0 /), rc)
    layout2 = ESMF_newDELayoutCreate(vm, (/ 4, 1 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    comp1 = ESMF_GridCompCreate(cname1, delayout=layout2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname1), "rc =", rc
    call ESMF_newDELayoutPrint(layout2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname2 = "user model 2"
    !layout3 = ESMF_newDELayoutCreate(delist, 2, (/ 2, 2 /), (/ 0, 0 /), rc)
    layout3 = ESMF_newDELayoutCreate(vm, (/ 2, 2 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    comp2 = ESMF_GridCompCreate(cname2, delayout=layout3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname2), "rc =", rc
    call ESMF_newDELayoutPrint(layout3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(cplname, delayout=layout1, rc=rc)
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
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(comp2, userm2_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, usercpl_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

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
 
      c1exp = ESMF_StateCreate("comp1 export", ESMF_STATEEXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Initialize finished, rc =", rc
 
      c2imp = ESMF_StateCreate("comp2 import", ESMF_STATEIMPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1a Initialize finished, rc =", rc

      call ESMF_CplCompInitialize(cpl, c1exp, c2imp, clock=clock, rc=rc)
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
  
        call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, rc=rc)
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

      call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 2 Finalize finished, rc =", rc

      call ESMF_CplCompFinalize(cpl, c1exp, c2imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Finalize finished, rc =", rc


      ! Figure out our local processor id for message below.
      call ESMF_newDELayoutGet(layout1, localDe=de_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"
      print *, "Test finished, de_id = ", de_id
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
      call ESMF_StateDestroy(c2imp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ClockDestroy(clock, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompDestroy(comp1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(comp2, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CplCompDestroy(cpl, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_newDELayoutDestroy(layout2, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_newDELayoutDestroy(layout3, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test SimpleCoupling complete!"

      ! Only output on processor 0, or if error anyplace
      if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then

        ! Standard ESMF Test output to log file
        write(failMsg, *)  "Component Coupling"
        write(testname, *) "System Test SimpleCoupling: Simple Component Coupling"
  
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)

        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS!! Component Coupling complete"
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""
  
      endif
    
      call ESMF_Finalize(rc) 

      end program SimpleCoupling
    
!\end{verbatim}
    
