! $Id: ESMF_FieldExclSTest.F90,v 1.30 2007/06/23 07:00:58 cdeluca Exp $
!
! System test code FieldExcl
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================


!BOP
!
! !DESCRIPTION:
! System test FieldExcl.  
!   Exclusive, Concurrent Component test.  
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
!                 The IGridded Components are running on separate sets of
!                 PETs, concurrently.  The coupler is running on the
!                 union of PETs (which is one less PET than the total number
!                 of PETs).
!
!                 By default the run loop is written to run comp1 and comp2
!                 concurrently (after the first time through the time loop).
!                 However, there are commented out options that may be used
!                 to force synchronization between the components and have them
!                 run sequentially.
!
!
!\begin{verbatim}

    program FieldExcl

#include <ESMF_Macros.inc>
#include <ESMF_Conf.inc>

    ! ESMF Framework module
    use ESMF_Mod
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
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "Start of System Test FieldExcl."

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
   
    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/ 6,2,4,0 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/ 5,1,3 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Created component ", trim(cname2), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, petList=(/ 0,1,2,3,4,5,6 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Created component ", trim(cplname), ", rc =", rc

    !print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    call ESMF_GridCompSetServices(comp1, userm1_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp SetServices finished, rc= ", rc

    call ESMF_GridCompSetServices(comp2, userm2_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp SetServices finished, rc= ", rc

    call ESMF_CplCompSetServices(cpl, usercpl_register, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp SetServices finished, rc= ", rc

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

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! initialize stop time to 5/02/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=2, &
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
    !print *, "Comp 1 Initialize finished, rc =", rc
 
    c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp 2 Initialize finished, rc =", rc
 
    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, c1exp, c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Coupler Initialize finished, rc =", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc))
    
      !print *, "PET ", pet_id, " starting time step..."

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp2 has returned. Consequently comp1 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp2, blockingflag=ESMF_BLOCKING, rc=rc)
      !print *, "Comp 2 Wait returned, rc =", rc
      !if (rc .ne. ESMF_SUCCESS) goto 10
    
      ! Run the first component:
      ! After the first time thru the loop this will be running concurrently 
      ! with the second component since comp1 and comp2 are defined on 
      ! exclusive sets of PETs
      !print *, "I am calling into GridCompRun(comp1)"
      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
      !print *, "Comp 1 Run returned, rc =", rc
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Uncomment the following calls to ESMF_GridCompWait() to sequentialize
      ! comp1, comp2 and the coupler. The following ESMF_GridCompWait() calls 
      ! will block all PETs until comp1 and comp2 have returned. Consequently 
      ! the coupler component will not be run until comp1 and comp2 have 
      ! returned.
      !call ESMF_GridCompWait(comp1, blockingflag=ESMF_BLOCKING, rc=rc)
      !print *, "Comp 1 Wait returned, rc =", rc
      !if (rc .ne. ESMF_SUCCESS) goto 10
      !call ESMF_GridCompWait(comp2, blockingflag=ESMF_BLOCKING, rc=rc)
      !print *, "Comp 2 Wait returned, rc =", rc
      !if (rc .ne. ESMF_SUCCESS) goto 10

      ! Run the coupler:
      ! The coupler will run in "per-PET sequential" mode because it runs on 
      ! the union of all PETs. Depending on the per-PET runtime of comp1 and
      ! comp2 some PETs may start/finish executing the coupler at different
      ! times. There is no intrinsic inter PET synchronization in calling
      ! component methods via CompI/R/F(). However, collective communication
      ! calls contained in the user written coupler methods will indirectly
      ! lead to inter PET synchronization of the coupler component.
      !print *, "I am calling into CplCompRun(cpl)"
      call ESMF_CplCompRun(cpl, c1exp, c2imp, clock=clock, rc=rc)
      !print *, "Coupler Run returned, rc =", rc
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp1 has returned. Consequently comp2 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp1, blockingflag=ESMF_BLOCKING, rc=rc)
      !print *, "Comp 1 Wait returned, rc =", rc
      !if (rc .ne. ESMF_SUCCESS) goto 10

      ! Run the second component:
      ! Since comp1 and comp2 are defined on exclusive sets of PETs those PET
      ! that are part of comp1 will not block in the following call but proceed
      ! to the next loop increment, executing comp1 concurrently with comp2.
      !print *, "I am calling into GridCompRun(comp2)"
      call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, rc=rc)
      !print *, "Comp 2 Run returned, rc =", rc
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ClockAdvance(clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      !call ESMF_ClockPrint(clock, rc=rc)
      
      !print *, "... time step finished on PET ", pet_id, "."

    enddo
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp 1 Finalize finished, rc =", rc

    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Comp 2 Finalize finished, rc =", rc

    call ESMF_CplCompFinalize(cpl, c1exp, c2imp, clock=clock, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    !print *, "Coupler Finalize finished, rc =", rc


    !print *, "Comp Finalize returned"

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

    !print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldExcl complete."


    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldExcl: Field Exclusive Components"

    call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), &
      testname, failMsg, testresult, ESMF_SRCLINE)

    ! Only on PET 0 or any PET with an error. 
    if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
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
  
    call ESMF_Finalize(rc=rc) 

    end program FieldExcl
    
!\end{verbatim}
    
