! $Id: ESMF_FieldExclSTest.F90,v 1.3 2004/10/07 16:31:22 nscollins Exp $
!
! System test code FieldExcl
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test FieldExcl.  
!   Exclusive, Concurrent Component test.  
!                 2 components and 1 coupler, one-way coupling.
!                 The first component has a uniform A-grid.  It has
!                 a Field whose data is set to a given geometric function,
!
!                 10.0 + 5.0*sin((X/Xmax)*pi) + 2.0*sin((Y/Ymax)*pi)
!
!                 and then regridded to the second component, which has a
!                 non-uniform D-grid.  The regridded data is then compared
!                 to the function's solution for a measurement of the
!                 accuracy of the Regrid.  Those values are output for
!                 each DE.
!
!\begin{verbatim}

    program FieldExcl

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none
    
    ! Local variables
    integer :: i, pet_id, npets, splitnum, rc
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM):: vm, vmsub1, vmsub2
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl
    logical :: i_am_comp1, i_am_comp2

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "-------------------------------- "
    print *, "Start of System Test FieldExcl:"
    print *, "-------------------------------- "

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
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (npets .lt. 8) then
      print *, "This system test needs to run at least 8-way, current np = ", &
               npets
      goto 10
    endif

   
    ! give PETs (0 to splitnum-1) to comp1, (splitnum to npets-1) to comp2
    splitnum = npets / 2
!!!! If running w/ PTHREADS, set both true.
!!!! If not, set one true and one false.
    ! with PThreads
    !!i_am_comp1 = .TRUE.
    !!i_am_comp2 = .TRUE.
    ! without
    if (pet_id .lt. splitnum) then
        i_am_comp1 = .TRUE.
        i_am_comp2 = .FALSE.
    else
        i_am_comp1 = .FALSE.
        i_am_comp2 = .TRUE.
    endif

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
!!!! TODO: when we want to run exclusive, use the following line instead.
    if (i_am_comp1) then
        comp1 = ESMF_GridCompCreate(vm, cname1, &
                                petList=(/ (i, i=0, splitnum-1) /), rc=rc)
        !!comp1 = ESMF_GridCompCreate(vm, cname1, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Created component ", trim(cname1), "rc =", rc
        call ESMF_GridCompGet(comp1, vm=vmsub1, rc=rc)
    !  call ESMF_GridCompPrint(comp1, "", rc)
    endif

    cname2 = "user model 2"
!!!! TODO: when we want to run exclusive, use the following line instead.
    if (i_am_comp2) then
        comp2 = ESMF_GridCompCreate(vm, cname2, &
                                petList=(/ (i, i=splitnum, npets-1) /), rc=rc)
        !!comp2 = ESMF_GridCompCreate(vm, cname2, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Created component ", trim(cname2), "rc =", rc
        call ESMF_GridCompGet(comp2, vm=vmsub2, rc=rc)
        !  call ESMF_GridCompPrint(comp2, "", rc)
    endif

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(vm, cplname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cplname), ", rc =", rc
    !  call ESMF_CplCompPrint(cpl, "", rc)

    print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    if (i_am_comp1) then
    call ESMF_GridCompSetServices(comp1, userm1_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp SetServices finished, rc= ", rc
    endif

    if (i_am_comp2) then
    call ESMF_GridCompSetServices(comp2, userm2_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp SetServices finished, rc= ", rc
    endif

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
    clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
    c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    if (i_am_comp1) then
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 1 Initialize finished, rc =", rc
    endif
 
    c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    if (i_am_comp2) then
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 2 Initialize finished, rc =", rc
    endif
 
    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, c1exp, c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Coupler Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc))

      if (i_am_comp1) then
      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 1 Run returned, rc =", rc
      endif

      call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Run returned, rc =", rc

      if (i_am_comp2) then
      call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp 2 Run returned, rc =", rc
      endif

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

    if (i_am_comp1) then
    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 1 Finalize finished, rc =", rc
    endif

    if (i_am_comp2) then
    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Comp 2 Finalize finished, rc =", rc
    endif

    call ESMF_CplCompFinalize(cpl, c1exp, c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Coupler Finalize finished, rc =", rc


    ! Figure out our local PET id for message below.
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
    call ESMF_StateDestroy(c2imp, rc)

    call ESMF_ClockDestroy(clock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)

    call ESMF_GridCompDestroy(comp1, rc)
    call ESMF_GridCompDestroy(comp2, rc)
    call ESMF_CplCompDestroy(cpl, rc)

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test FieldExcl complete."

    ! Only on PET 0 or any PET with an error. 
    if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then

      ! Normal ESMF Test output
      write(failMsg, *) "System Test failure"
      write(testname, *) "System Test FieldExcl: Field Exclusive Components"

      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS: Exclusive Component test finished correctly."
      else
        write(finalMsg, *) "System Test did not succeed.  Error code ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
  
    call ESMF_Finalize(rc) 

    end program FieldExcl
    
!\end{verbatim}
    
