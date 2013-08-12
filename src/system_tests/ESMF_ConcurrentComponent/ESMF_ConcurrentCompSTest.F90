! $Id$
!
! System test code ConcurrentComponent
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================


!BOP
!
! !DESCRIPTION:
! System test ConcurrentComponent.
!   Concurrent, Concurrent Component test.
!   This system test demonstrates the use of ESMF coupling framework
! to couple 2 gridded components with 1 coupler component. The coupler
! component runs on the union of the PETs that are exclusively allocated
! to each individual gridded component.
!
! Component 1 exports data to the coupler which then redistributes the
! data to component 2. Component 1 and 2 runs concurrently because they
! reside on different PETs. The coupler also runs concurrently across PETs, however
! on each individual PET, it runs sequentially. In other words, each PET
! has a copy of the coupler that executes different code simultaneously.
! So this pattern constitutes a SPMD concurrency model.
!
! It's possible to reschedule the component execution by introducing wait
! and block semantic. Components on exclusive sets
! of PETs can be rescheduled to execute sequentially.
!
! This system test exercises the ESMF coupling framework to perform
! quick sort that can be useful in search engine, gnome sequencing, etc.
! The sorting result of component 1 is redistributed to component 2. Component
! 2 verifies the sorting result.
!
! This system test also captures the design of a typical hierarchical
! climate model with a coupler component that couples individual gridded components
! (e.g. atmosphere, land, and ocean).
!
!\begin{verbatim}

    program ConcurrentComponent

#include "ESMF_Conf.inc"
#include "ESMF.h"
#define ESMF_METHOD "ConcurrentComponent"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : userm1_setvm, userm1_register
    use user_model2, only : userm2_setvm, userm2_register
    use user_coupler, only : usercpl_setvm, usercpl_register

    implicit none

    ! Local variables
    integer :: pet_id, npets, rc, localrc, userrc
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
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "Start of System Test ConcurrentComponent."
    localrc = ESMF_SUCCESS
    rc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework and get back default global VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="ConcurrentCompSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Check for correct number of PETs
     if ( npets < 8 ) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
            msg="This system test does not run on fewer than 8 PETs.",&
            ESMF_CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      endif

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, petList=(/ 6,2,4,0 /), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, petList=(/ 5,1,3 /), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cname2), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, petList=(/ 0,1,2,3,4,5,6 /), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cplname), ", rc =", rc

    !print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetVM(comp1, userRoutine=userm1_setvm, &
    userRc=userrc, rc=localrc)
  print *, "Comp1 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, &
    userRc=userrc, rc=localrc)
  print *, "Comp1 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetVM(comp2, userRoutine=userm2_setvm, &
    userRc=userrc, rc=localrc)
  print *, "Comp2 SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, &
    userRc=userrc, rc=localrc)
  print *, "Comp2 SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_CplCompSetVM(cpl, userRoutine=usercpl_setvm, &
    userRc=userrc, rc=localrc)
  print *, "Cpl SetVM finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, &
    userRc=userrc, rc=localrc)
  print *, "Cpl SetServices finished, rc= ", localrc, userrc
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                            name="Gregorian", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize time interval to 4 hours
    call ESMF_TimeIntervalSet(timeStep, h=4, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize stop time to 5/02/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize the clock with the above values
    clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                             name="Clock 1", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    c1exp = ESMF_StateCreate(name="comp1 export",  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, &
       userRC=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 1 Initialize finished, rc =", rc

    c2imp = ESMF_StateCreate(name="comp2 import",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, &
       userRC=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 2 Initialize finished, rc =", rc

    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, userRC=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Coupler Initialize finished, rc =", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc=localrc))

      !print *, "PET ", pet_id, " starting time step..."

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp2 has returned. Consequently comp1 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp2, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the first component:
      ! After the first time thru the loop this will be running concurrently
      ! with the second component since comp1 and comp2 are defined on
      ! exclusive sets of PETs
      !print *, "I am calling into GridCompRun(comp1)"
      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, &
        userRc=userrc, rc=localrc)
      !print *, "Comp 1 Run returned, rc =", localrc
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

      ! Uncomment the following calls to ESMF_GridCompWait() to sequentialize
      ! comp1, comp2 and the coupler. The following ESMF_GridCompWait() calls
      ! will block all PETs until comp1 and comp2 have returned. Consequently
      ! the coupler component will not be run until comp1 and comp2 have
      ! returned.
      !call ESMF_GridCompWait(comp1, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_GridCompWait(comp2, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the coupler:
      ! The coupler will run in "per-PET sequential" mode because it runs on
      ! the union of all PETs. Depending on the per-PET runtime of comp1 and
      ! comp2 some PETs may start/finish executing the coupler at different
      ! times. There is no intrinsic inter PET synchronization in calling
      ! component methods via CompI/R/F(). However, collective communication
      ! calls contained in the user written coupler methods will indirectly
      ! lead to inter PET synchronization of the coupler component.
      !print *, "I am calling into CplCompRun(cpl)"
      call ESMF_CplCompRun(cpl, importState=c1exp, &
        exportState=c2imp, clock=clock, userRc=userrc, rc=localrc)
      !print *, "Coupler Run returned, rc =", localrc
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comp1 and comp2. The following ESMF_GridCompWait() call will block
      ! all PETs until comp1 has returned. Consequently comp2 will not be
      ! run until comp2 has returned.
      !call ESMF_GridCompWait(comp1, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the second component:
      ! Since comp1 and comp2 are defined on exclusive sets of PETs those PET
      ! that are part of comp1 will not block in the following call but proceed
      ! to the next loop increment, executing comp1 concurrently with comp2.
      !print *, "I am calling into GridCompRun(comp2)"
      call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, &
        userRc=userrc, rc=localrc)
      !print *, "Comp 2 Run returned, rc =", localrc
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

      call ESMF_ClockAdvance(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_ClockPrint(clock, rc=localrc)

      !print *, "... time step finished on PET ", pet_id, "."

    enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, &
       userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 1 Finalize finished, rc =", rc

    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, &
       userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 2 Finalize finished, rc =", rc

    call ESMF_CplCompFinalize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Coupler Finalize finished, rc =", rc


    !print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

    call ESMF_StateDestroy(c1exp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(c2imp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CalendarDestroy(gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(comp1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(comp2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cpl, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test ConcurrentComponent complete."
    rc = localrc

    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test ConcurrentComponent: Array Concurrent Components"

    if (rc .eq. ESMF_SUCCESS) then
      write(finalMsg, *) "SUCCESS: Concurrent Component test finished correctly."
    else
        write(finalMsg, *) "System Test did not succeed.  Error code ", rc
    endif
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""


    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
    ! into the Log file that the scripts grep for.
    call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, &
    __FILE__, &
    __LINE__)

    call ESMF_Finalize()

    end program ConcurrentComponent

!\end{verbatim}

