! $Id$
!
! System test code MapperTwoConcurrentComps
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!ESMF_disabled_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================


!BOP
!
! !DESCRIPTION:
! System test MapperTwoConcurrentComps.
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

    program MapperTwoConcurrentComps

#include "ESMF_Conf.inc"
#include "ESMF.h"
#define ESMF_METHOD "MapperTwoConcurrentComps"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : userm1_setvm, userm1_register
    use user_model2, only : userm2_setvm, userm2_register
    use user_coupler, only : usercpl_setvm, usercpl_register

    use comp_utils
    use clock_utils

    implicit none

    ! Local variables
    integer, parameter :: NUM_COMPS = 2
    integer, parameter :: NUM_COMPS_PLUS_CPL = NUM_COMPS+1
    integer :: pet_id, npets, rc, localrc, userrc
    integer :: npets_comp1, npets_comp2, npets_cpl
    integer :: petlist_start_comp1, petlist_start_comp2, petlist_start_cpl
    integer :: i, j
    integer, dimension(:), allocatable :: petlist_comp1, petlist_comp2, petlist_cpl
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM):: vm
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    !type(ESMF_Clock) :: clock_comp1, clock_comp2, clock_cpl
    integer, parameter :: CPL_IDX = 1
    integer, parameter :: GCOMP_SIDX = 2
    type(ESMF_Clock) :: clocks(NUM_COMPS_PLUS_CPL)
    type(ESMF_Alarm) :: alarms(NUM_COMPS_PLUS_CPL)
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeSteps(NUM_COMPS_PLUS_CPL)
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    type(ESMF_Mapper) :: mapper
    logical :: is_sync_time(NUM_COMPS_PLUS_CPL)

    type(ESMF_Time) :: dbg_time
    integer(ESMF_KIND_I8) :: dbg_stime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "Start of System Test MapperTwoConcurrentComps."
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
     !if ( npets < 8 ) then
     !   call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
     !       msg="This system test does not run on fewer than 8 PETs.",&
     !       ESMF_CONTEXT, rcToReturn=rc)
     !   call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
     ! endif
      if(npets == 1) then
        ! Not concurrent anymore...
        npets_comp1 = 1
        npets_comp2 = 1
        petlist_start_comp1 = 0
        petlist_start_comp2 = 0
      else
        npets_comp1 = npets/2
        npets_comp2 = npets - npets_comp1
        petlist_start_comp1 = 0
        petlist_start_comp2 = npets_comp1
      end if
      npets_cpl = npets
      petlist_start_cpl = 0

      allocate(petlist_comp1(npets_comp1))
      allocate(petlist_comp2(npets_comp2))
      allocate(petlist_cpl(npets_cpl))
      j = petlist_start_comp1
      do i=1,npets_comp1
        petlist_comp1(i) = j
        j = j + 1
      end do
      j = petlist_start_comp2
      do i=1,npets_comp2
        petlist_comp2(i) = j
        j = j + 1
      end do
      j = petlist_start_cpl
      do i=1,npets_cpl
        petlist_cpl(i) = j
        j = j + 1
      end do
      

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    comp1 = ESMF_GridCompCreate(name=cname1, petList=petlist_comp1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cname1), "rc =", rc

    cname2 = "user model 2"
    comp2 = ESMF_GridCompCreate(name=cname2, petList=petlist_comp2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cname2), "rc =", rc

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, petList=petlist_cpl, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "Created component ", trim(cplname), ", rc =", rc

    deallocate(petlist_comp1)
    deallocate(petlist_comp2)
    deallocate(petlist_cpl)
    !print *, "Comp Creates finished"

  mapper = ESMF_MapperCreate(vm, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

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

  ! Set mapper constraints
  call ESMF_MapperSetConstraints(mapper, execBlock=(/comp1/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! Set mapper constraints
  call ESMF_MapperSetConstraints(mapper, execBlock=(/comp2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize clocks for comp1 and comp2.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                            name="Gregorian", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize time interval to 4 hours
    call ESMF_TimeIntervalSet(timeSteps(CPL_IDX), h=2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeSteps(GCOMP_SIDX), h=8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeSteps(GCOMP_SIDX+1), h=4, rc=localrc)
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

    ! initialize the clock for comp1 with the above values
    clocks(CPL_IDX) = ESMF_ClockCreate(timeSteps(CPL_IDX), startTime, stopTime=stopTime, &
                             name="Cpl Clock 1", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    clocks(GCOMP_SIDX) = ESMF_ClockCreate(timeSteps(GCOMP_SIDX), startTime, stopTime=stopTime, &
                             name="Gcomp Clock 1", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    clocks(GCOMP_SIDX+1) = ESMF_ClockCreate(timeSteps(GCOMP_SIDX+1), startTime, stopTime=stopTime, &
                             name="Gcomp Clock 2", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call get_sync_alarms(clocks, alarms, localrc)
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
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clocks(GCOMP_SIDX), &
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
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clocks(GCOMP_SIDX+1), &
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
      exportState=c2imp, clock=clocks(CPL_IDX), userRC=userrc, rc=localrc)
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

    is_sync_time = .false.
    do while (.not. ESMF_ClockIsStopTime(clocks(CPL_IDX), rc=localrc))

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
      if(.not. ESMF_AlarmIsRinging(alarms(GCOMP_SIDX), rc=localrc)) then
        call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clocks(GCOMP_SIDX), &
          userRc=userrc, rc=localrc)
        !print *, "Comp 1 Run returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end if

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
      if(.not. ESMF_AlarmIsRinging(alarms(CPL_IDX), rc=localrc)) then
        call ESMF_CplCompRun(cpl, importState=c1exp, &
          exportState=c2imp, clock=clocks(CPL_IDX), userRc=userrc, rc=localrc)
        !print *, "Coupler Run returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end if

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
      if(.not. ESMF_AlarmIsRinging(alarms(GCOMP_SIDX+1), rc=localrc)) then
        call ESMF_GridCompRun(comp2, importState=c2imp, clock=clocks(GCOMP_SIDX+1), &
          userRc=userrc, rc=localrc)
        !print *, "Comp 2 Run returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end if

      ! Check if we have comp1 sync alarm ringing
      if(ESMF_AlarmIsRinging(alarms(GCOMP_SIDX), rc=localrc)) then
        call ESMF_ClockGet(clocks(GCOMP_SIDX), currTime=dbg_time, rc=rc)
        call ESMF_TimeGet(dbg_time, s_i8=dbg_stime, rc=rc)
        print *, "Comp1 alarm is ringing !, time = ", dbg_stime
        is_sync_time(GCOMP_SIDX) = .true.
      end if

      ! Check if we have comp2 sync alarm ringing
      if(ESMF_AlarmIsRinging(alarms(GCOMP_SIDX+1), rc=localrc)) then
        call ESMF_ClockGet(clocks(GCOMP_SIDX+1), currTime=dbg_time, rc=rc)
        call ESMF_TimeGet(dbg_time, s_i8=dbg_stime, rc=rc)
        print *, "Comp2 alarm is ringing !, time = ", dbg_stime
        is_sync_time(GCOMP_SIDX+1) = .true.
      end if

      ! Check if coupler sync alarm is ringing
      if(ESMF_AlarmIsRinging(alarms(CPL_IDX), rc=localrc)) then
        call ESMF_ClockGet(clocks(CPL_IDX), currTime=dbg_time, rc=rc)
        call ESMF_TimeGet(dbg_time, s_i8=dbg_stime, rc=rc)
        print *, "Coupler alarm is ringing !, time = ", dbg_stime
        is_sync_time(CPL_IDX) = .true.
      end if
      if(all(is_sync_time)) then
        is_sync_time = .false.
        ! Wait for both components and cpl
        call ESMF_GridCompWait(comp1, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
        print *, "Comp 1 Wait returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridCompWait(comp2, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
        print *, "Comp 2 Wait returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_CplCompWait(cpl, syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
        print *, "Comp 2 Wait returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        
        ! Turn the alarms off
        call ESMF_AlarmRingerOff(alarms(GCOMP_SIDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(GCOMP_SIDX+1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Contact mapper
        call ESMF_MapperCollect(mapper, comp1, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        
        call ESMF_MapperCollect(mapper, comp2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Optimize using the mapper
        call ESMF_MapperOptimize(mapper, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Recreate the components using info from mapper
        !call user_comp_recreate(comp1, model_info(1), mapper, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        !call user_comp_recreate(comp2, model_info(2), mapper, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        call ESMF_AlarmRingerOff(alarms(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end if

      if(.not. ESMF_AlarmIsRinging(alarms(GCOMP_SIDX), rc=localrc)) then
        call ESMF_ClockAdvance(clocks(GCOMP_SIDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !call ESMF_ClockPrint(clock_comp1, rc=localrc)
      end if

      if(.not. ESMF_AlarmIsRinging(alarms(GCOMP_SIDX+1), rc=localrc)) then
        call ESMF_ClockAdvance(clocks(GCOMP_SIDX+1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !call ESMF_ClockPrint(clock_comp2, rc=localrc)
      end if

      if(.not. ESMF_AlarmIsRinging(alarms(CPL_IDX), rc=localrc)) then
        call ESMF_ClockAdvance(clocks(CPL_IDX), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !call ESMF_ClockPrint(clock_cpl, rc=localrc)
      end if

      !print *, "... time step finished on PET ", pet_id, "."

    enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clocks(GCOMP_SIDX), &
       userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 1 Finalize finished, rc =", rc

    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clocks(GCOMP_SIDX+1), &
       userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 2 Finalize finished, rc =", rc

    call ESMF_CplCompFinalize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clocks(CPL_IDX), userRc=userrc, rc=localrc)
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

    call ESMF_ClockDestroy(clocks(GCOMP_SIDX), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockDestroy(clocks(GCOMP_SIDX+1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockDestroy(clocks(CPL_IDX), rc=localrc)
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

    call ESMF_MapperDestroy(mapper, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test MapperTwoConcurrentComps complete."
    rc = localrc

    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test MapperTwoConcurrentComps: Array Concurrent Components"

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

    end program MapperTwoConcurrentComps

!\end{verbatim}

