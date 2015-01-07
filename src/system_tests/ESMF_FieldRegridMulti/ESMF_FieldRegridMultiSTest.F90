! $Id$
!
! System test code FieldRegridMulti
!  Description on Sourceforge under System Test #xxxxx

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldRegridMulti.
!   Regrid test with a 2D igrid and a 3D data field, where the 3rd dimension
!                 is multiple values corresponding to the same igrid cell.
!
!                 2 components and 1 coupler, one-way coupling.
!                 The first component has a uniform A-igrid.  It has
!                 a Field whose data is set to a given geometric function,
!
!                 10.0 + 5.0*sin((X/Xmax)*pi) + 2.0*sin((Y/Ymax)*pi)
!
!                 and then regridded to the second component, which has a
!                 non-uniform D-igrid.  The regridded data is then compared
!                 to the function's solution for a measurement of the
!                 accuracy of the Regrid.  Those values are output for
!                 each DE.
!
!\begin{verbatim}

    program FieldRegridMulti

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none

    ! Local variables
    integer :: pet_id, npets, rc
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM):: vm
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

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

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "------------------------------------- "
    print *, "Start of System Test FieldRegridMulti:"
    print *, "------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework and get back default global VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPet=pet_id, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (npets .lt. 6) then
      print *, "This system test needs to run at least 6-way, current np = ", &
               npets
      goto 10
    endif

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname1), "rc =", rc
    !  call ESMF_GridCompPrint(comp1, "", rc)

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname2), "rc =", rc
    !  call ESMF_GridCompPrint(comp2, "", rc)

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cplname), ", rc =", rc
    !  call ESMF_CplCompPrint(cpl, "", rc)

    print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp SetServices finished, rc= ", rc

    call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp SetServices finished, rc= ", rc

    call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp SetServices finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name="Gregorian", rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! initialize stop time to 5/02/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=1, h=6, &
                      calendar=gregorianCalendar, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! initialize the clock with the above values
    clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                             name="Clock 1", rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    c1exp = ESMF_StateCreate(name="comp1 export",  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 1 Initialize finished, rc =", rc

    c2imp = ESMF_StateCreate(name="comp2 import",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 2 Initialize finished, rc =", rc

    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Coupler Initialize finished, rc =", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Run returned, rc =", rc

      call ESMF_CplCompRun(cpl, importState=c1exp, &
        exportState=c2imp, clock=clock, rc=rc)
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

    call ESMF_CplCompFinalize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Coupler Finalize finished, rc =", rc


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

    call ESMF_StateDestroy(c1exp, rc=rc)
    call ESMF_StateDestroy(c2imp, rc=rc)

    call ESMF_ClockDestroy(clock, rc=rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)

    call ESMF_GridCompDestroy(comp1, rc=rc)
    call ESMF_GridCompDestroy(comp2, rc=rc)
    call ESMF_CplCompDestroy(cpl, rc=rc)

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldRegridMulti complete."

    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldRegridMulti: Field Regrid Multiple Values"

    ! Only on PET 0 or any PET with an error.
    if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then

      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS: Multi Regrid test finished correctly."
      else
        write(finalMsg, *) "System Test did not succeed.  Error code ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif


    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
    ! into the Log file that the scripts grep for.
    call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, &
    __FILE__, &
    __LINE__)

    call ESMF_Finalize(rc=rc)

    end program FieldRegridMulti

!\end{verbatim}

