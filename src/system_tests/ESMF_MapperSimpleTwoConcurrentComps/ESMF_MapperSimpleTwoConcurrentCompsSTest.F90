! $Id$
!
! System test code MapperSimpleTwoConcurrentComps
!  Description on Sourceforge under System Test #79497

!-------------------------------------------------------------------------
!ESMF_disabled_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================


!BOP
!
! !DESCRIPTION:
! System test MapperSimpleTwoConcurrentComps.
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

    program MapperSimpleTwoConcurrentComps

#include "ESMF_Conf.inc"
#include "ESMF.h"
#define ESMF_METHOD "MapperSimpleTwoConcurrentComps"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : userm1_setvm, userm1_register
    use user_model2, only : userm2_setvm, userm2_register
    use user_coupler, only : usercpl_setvm, usercpl_register

    use comp_utils
    use mpi

    implicit none

    ! Dynamic Integer array type
    type DIArray
      integer, dimension(:), allocatable :: arr
    end type

    abstract interface
      subroutine iuser_setvm(comp, rc)
        use ESMF
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc
      end subroutine iuser_setvm
      subroutine iuser_register(comp, rc)
          use ESMF
          type(ESMF_GridComp)  :: comp
          integer, intent(out) :: rc
      end subroutine iuser_register
    end interface

    type UModelFP
      procedure(iuser_setvm), pointer, nopass :: puser_setvm
      procedure(iuser_register), pointer, nopass :: puser_register
    end type

    integer, parameter :: NUM_UMODELS = 2
    type(UModelFP) :: umodel_func_ptrs(NUM_UMODELS)

    ! Local variables
    ! assert(NUM_COMPS > 0)
    integer, parameter :: NUM_COMPS = 8
    integer, parameter :: NUM_COMPS_PLUS_CPL = NUM_COMPS+1
    integer :: pet_id, npets, rc, localrc, userrc
    integer :: npets_comps(NUM_COMPS)
    integer :: npets_cpl
    integer :: petlist_start_comps(NUM_COMPS)
    integer :: petlist_start_cpl
    integer :: petlist_end_comps(NUM_COMPS)
    integer :: petlist_end_cpl
    integer :: i, j, k
    type(DIArray) :: petlist_comps(NUM_COMPS)
    integer, dimension(:), allocatable :: petlist_cpl
    character(len=ESMF_MAXSTR) :: comp_names(NUM_COMPS)
    character(len=ESMF_MAXSTR) :: cplname
    character(len=ESMF_MAXSTR) :: comp_phase_names(NUM_COMPS)
    type(ESMF_VM):: vm
    type(ESMF_State) :: cexps(NUM_COMPS), cimps(NUM_COMPS)
    type(ESMF_GridComp) :: comps(NUM_COMPS)
    real(ESMF_KIND_R8) :: comp_starts(NUM_COMPS), comp_ends(NUM_COMPS)
    double precision :: run_loop_start, run_loop_end
    double precision :: run_loop_wtime
    double precision :: comp_wtimes(NUM_COMPS)
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    !type(ESMF_Clock) :: clock_comps(1), clock_comps(2), clock_cpl
    integer, parameter :: CPL_IDX = 1
    integer, parameter :: GCOMP_SIDX = 2
    type(ESMF_Clock) :: clocks(NUM_COMPS_PLUS_CPL)
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeSteps(NUM_COMPS_PLUS_CPL)
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    type(ESMF_Mapper) :: mapper

    type(ESMF_Time) :: dbg_time
    integer(ESMF_KIND_I8) :: dbg_stime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0
    integer :: result = 0

    character(ESMF_MAXSTR) :: tmpstr
    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "Start of System Test MapperSimpleTwoConcurrentComps."
    localrc = ESMF_SUCCESS
    rc = ESMF_SUCCESS

! Set the model function ptrs
    umodel_func_ptrs(1)%puser_setvm => userm1_setvm
    umodel_func_ptrs(1)%puser_register => userm1_register
    umodel_func_ptrs(2)%puser_setvm => userm2_setvm
    umodel_func_ptrs(2)%puser_register => userm2_register
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    print *, "ESMF_Initialize ..."
    ! Initialize framework and get back default global VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="ConcurrentCompSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    print *, "ESMF_VMGet ..."
    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Check for correct number of PETs
     if ( npets < NUM_COMPS ) then
        write(failMsg,'(A50,I6,A10)') "This system test does not run on fewer than ", NUM_COMPS, " PETs"
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
            msg=failMsg,&
            ESMF_CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      endif
      print *, "Allocating PETs among components ..."
      j = 0
      do i=1,NUM_COMPS-1
        npets_comps(i) = npets/NUM_COMPS
        petlist_start_comps(i) = j
        petlist_end_comps(i) = petlist_start_comps(i) + npets_comps(i) - 1
        j = j + npets_comps(i)
      end do
      npets_comps(NUM_COMPS) = npets - (NUM_COMPS - 1) * (npets/NUM_COMPS)
      petlist_start_comps(NUM_COMPS) = j
      petlist_end_comps(NUM_COMPS) = petlist_start_comps(NUM_COMPS) +&
                                      npets_comps(NUM_COMPS) - 1
      npets_cpl = npets
      petlist_start_cpl = 0
      petlist_end_cpl = petlist_start_cpl + npets_cpl - 1;

      do i=1,NUM_COMPS
        allocate(petlist_comps(i)%arr(npets_comps(i)))
      end do
      allocate(petlist_cpl(npets_cpl))
      do k=1,NUM_COMPS
        j = petlist_start_comps(k)
        do i=1,npets_comps(k)
          petlist_comps(k)%arr(i) = j
          j = j + 1
        end do
      end do
      j = petlist_start_cpl
      do i=1,npets_cpl
        petlist_cpl(i) = j
        j = j + 1
      end do
      
    print *, "Creating N model components..."
    ! Create the N model components and coupler
    do i=1,NUM_COMPS
      write(tmpstr, '(A12,I6)') "user model ", i
      comp_names(i) = trim(tmpstr)
      comps(i) = ESMF_GridCompCreate(name=comp_names(i), petList=petlist_comps(i)%arr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      print *, "Created component ", trim(comp_names(i)), "rc =", rc
    end do

    cplname = "user one-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, petList=petlist_cpl, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    print *, "Created component ", trim(cplname), ", rc =", rc

    do i=1,NUM_COMPS
      deallocate(petlist_comps(i)%arr)
    end do
    deallocate(petlist_cpl)
    !print *, "Comp Creates finished"

  print *, "Creating mapper...."
  !mapper = ESMF_MapperCreate(vm, configFile="./runseq.txt", rc=localrc)
  mapper = ESMF_MapperCreate(vm, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "Registering components ..."
  do i=1,NUM_COMPS
    call ESMF_GridCompSetVM(comps(i),&
      userRoutine=umodel_func_ptrs(mod(i-1,NUM_UMODELS)+1)%puser_setvm,&
      userRc=userrc, rc=localrc)
    print *, "Comp1 SetVM finished, rc= ", localrc, userrc
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetServices(comps(i),&
      userRoutine=umodel_func_ptrs(mod(i-1,NUM_UMODELS)+1)%puser_register,&
      userRc=userrc, rc=localrc)
    print *, "Comp1 SetServices finished, rc= ", localrc, userrc
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  end do

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
  !call ESMF_MapperSetConstraints(mapper, rc=localrc)
  !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
  !  ESMF_CONTEXT, rcToReturn=rc)) &
  !  call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize clocks for comps(1) and comps(2).
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                            name="Gregorian", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize time interval to 4 hours
    call ESMF_TimeIntervalSet(timeSteps(CPL_IDX), h=4, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,NUM_COMPS
      call ESMF_TimeIntervalSet(timeSteps(GCOMP_SIDX+i-1), h=4, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize stop time to 5/06/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=6, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! initialize the clock for comps(1) with the above values
    clocks(CPL_IDX) = ESMF_ClockCreate(timeSteps(CPL_IDX), startTime, stopTime=stopTime, &
                             name="Cpl Clock 1", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,NUM_COMPS
      write(tmpstr,"(A15,I6)") "Gcomp Clock", i
      clocks(GCOMP_SIDX+i-1) = ESMF_ClockCreate(timeSteps(GCOMP_SIDX+i-1), startTime, stopTime=stopTime, &
                               name=tmpstr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  do i=1,NUM_COMPS,2
    write(tmpstr,"(A20,I6)") "export comps", i
    cexps(i) = ESMF_StateCreate(name=tmpstr,  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(comps(i), exportState=cexps(i),&
       clock=clocks(GCOMP_SIDX+i-1), &
       userRC=userrc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !print *, "Comp 1 Initialize finished, rc =", rc

    if((i+1) <= NUM_COMPS) then
      write(tmpstr,"(A20,I6)") "import comps", i+1
      cimps(i+1) = ESMF_StateCreate(name=tmpstr,  &
                               stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridCompInitialize(comps(i+1), importState=cimps(i+1),&
         clock=clocks(GCOMP_SIDX+i), &
         userRC=userrc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      !print *, "Comp 2 Initialize finished, rc =", rc
    end if
  end do

    ! note that the coupler's import is comps(1)'s export
    do i=1,NUM_COMPS,2
      if((i+1) <= NUM_COMPS) then
        call ESMF_CplCompInitialize(cpl, importState=cexps(i), &
          exportState=cimps(i+1), clock=clocks(CPL_IDX), userRC=userrc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end if
    end do
    !print *, "Coupler Initialize finished, rc =", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do i=1,NUM_COMPS
      write(comp_phase_names(i),"(A15,I6)") "comp phase run", i
    end do
    do while (.not. ESMF_ClockIsStopTime(clocks(CPL_IDX), rc=localrc))

      print *, "PET ", pet_id, " starting time step..."

      run_loop_start = MPI_Wtime()
      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comps(1) and comps(2). The following ESMF_GridCompWait() call will block
      ! all PETs until comps(2) has returned. Consequently comps(1) will not be
      ! run until comps(2) has returned.
      !call ESMF_GridCompWait(comps(2), syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the first component:
      ! After the first time thru the loop this will be running concurrently
      ! with the second component since comps(1) and comps(2) are defined on
      ! exclusive sets of PETs
      do i=1,NUM_COMPS,2
        !print *, "I am calling into GridCompRun(comps(1))"
        comp_starts(i) = MPI_Wtime()
        !call ESMF_GridCompRun(comps(1), exportState=cexps(1), clock=clocks(GCOMP_SIDX), &
        !  userRc=userrc, rc=localrc)
        call ESMF_GridCompRun(comps(i), clock=clocks(GCOMP_SIDX+i-1), &
          userRc=userrc, rc=localrc)
        print *, "Comp ", i, " Run returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        comp_ends(i) = MPI_Wtime()
      end do

      ! Uncomment the following calls to ESMF_GridCompWait() to sequentialize
      ! comps(1), comps(2) and the coupler. The following ESMF_GridCompWait() calls
      ! will block all PETs until comps(1) and comps(2) have returned. Consequently
      ! the coupler component will not be run until comps(1) and comps(2) have
      ! returned.
      !call ESMF_GridCompWait(comps(1), syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_GridCompWait(comps(2), syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 2 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the coupler:
      ! The coupler will run in "per-PET sequential" mode because it runs on
      ! the union of all PETs. Depending on the per-PET runtime of comps(1) and
      ! comps(2) some PETs may start/finish executing the coupler at different
      ! times. There is no intrinsic inter PET synchronization in calling
      ! component methods via CompI/R/F(). However, collective communication
      ! calls contained in the user written coupler methods will indirectly
      ! lead to inter PET synchronization of the coupler component.
      !print *, "I am calling into CplCompRun(cpl)"
      !call ESMF_CplCompRun(cpl, importState=cexps(1), &
      !  exportState=cimps(2), clock=clocks(CPL_IDX), userRc=userrc, rc=localrc)
      call ESMF_CplCompRun(cpl,&
        clock=clocks(CPL_IDX), userRc=userrc, rc=localrc)
      !print *, "Coupler Run returned, rc =", localrc
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

      ! Uncomment the following call to ESMF_GridCompWait() to sequentialize
      ! comps(1) and comps(2). The following ESMF_GridCompWait() call will block
      ! all PETs until comps(1) has returned. Consequently comps(2) will not be
      ! run until comps(2) has returned.
      !call ESMF_GridCompWait(comps(1), syncflag=ESMF_SYNC_BLOCKING, rc=localrc)
      !print *, "Comp 1 Wait returned, rc =", localrc
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Run the second component:
      ! Since comps(1) and comps(2) are defined on exclusive sets of PETs those PET
      ! that are part of comps(1) will not block in the following call but proceed
      ! to the next loop increment, executing comps(1) concurrently with comps(2).
      do i=2,NUM_COMPS,2
        !print *, "I am calling into GridCompRun(comps(2))"
        comp_starts(i) = MPI_Wtime()
        !call ESMF_GridCompRun(comps(2), importState=cimps(2), clock=clocks(GCOMP_SIDX+1), &
        !  userRc=userrc, rc=localrc)
        call ESMF_GridCompRun(comps(i), clock=clocks(GCOMP_SIDX+i-1), &
          userRc=userrc, rc=localrc)
        print *, "Comp ", i, " Run returned, rc =", localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
        comp_ends(2) = MPI_Wtime()
      end do

      run_loop_end = MPI_Wtime() - run_loop_start
      do i=1,NUM_COMPS
        call MPI_Allreduce(comp_ends(i), comp_wtimes(i),&
              1, MPI_DOUBLE_PRECISION,&
              MPI_MAX, MPI_COMM_WORLD, rc)
      end do

      call MPI_Allreduce(run_loop_end, run_loop_wtime, 1, MPI_DOUBLE_PRECISION,&
            MPI_MAX, MPI_COMM_WORLD, rc)

      do i=1,NUM_COMPS
        print *, "comps(", i, ") = ", comp_wtimes(i), "s"
      end do
      print *, "run loop = ", run_loop_wtime, "s"

      print *, "Setting comp infos..."
      do i=1,NUM_COMPS
        ! Contact mapper
        call ESMF_MapperSetCompInfo(mapper,&
              len_trim(comp_names(i)), comp_names(i),&
              len_trim(comp_phase_names(i)), comp_phase_names(i),&
              petlist_start_comps(i), petlist_end_comps(i),&      
              comp_starts(i), comp_ends(i),&
              rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end do
      
      print *, "Optimizing PET layouts using mapper..."
      ! Optimize using the mapper
      call ESMF_MapperOptimize(mapper, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=localrc)) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

      print *, "Retrieving optimized PET layouts from mapper..."
      do i=1,NUM_COMPS
        ! Contact mapper
        call ESMF_MapperGetCompInfo(mapper,&
              len_trim(comp_names(i)), comp_names(i),&
              len_trim(comp_phase_names(i)), comp_phase_names(i),&
              startPet=petlist_start_comps(i), endPet=petlist_end_comps(i),&      
              rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end do
      
      print *, "Recreating components..."
      do i=1,NUM_COMPS
        ! Recreate the components using info from mapper
        call user_comp_recreate(comps(i),&
          umodel_func_ptrs(mod(i-1,NUM_UMODELS) + 1)%puser_setvm,&
          umodel_func_ptrs(mod(i-1,NUM_UMODELS) + 1)%puser_register,&
          petlist_start_comps(i), petlist_end_comps(i), mapper, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=localrc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      end do

      do i=1,NUM_COMPS
        call ESMF_ClockAdvance(clocks(GCOMP_SIDX+i-1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !call ESMF_ClockPrint(clock_comps(1), rc=localrc)
      end do

      call ESMF_ClockAdvance(clocks(CPL_IDX), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_ClockPrint(clock_cpl, rc=localrc)

      print *, "... time step finished on PET ", pet_id, "."

    enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    do i=1,NUM_COMPS,2
      call ESMF_GridCompFinalize(comps(i), exportState=cexps(i), clock=clocks(GCOMP_SIDX+i-1), &
         userRc=userrc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      !print *, "Comp 1 Finalize finished, rc =", rc
    end do

    do i=2,NUM_COMPS,2
      call ESMF_GridCompFinalize(comps(i), importState=cimps(i), clock=clocks(GCOMP_SIDX+i-1), &
         userRc=userrc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(userrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
      !print *, "Comp 2 Finalize finished, rc =", rc
    end do

    call ESMF_CplCompFinalize(cpl, importState=cexps(1), &
      exportState=cimps(2), clock=clocks(CPL_IDX), userRc=userrc, rc=localrc)
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

    do i=1,NUM_COMPS,2
      call ESMF_StateDestroy(cexps(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do
    do i=2,NUM_COMPS,2
      call ESMF_StateDestroy(cimps(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    do i=1,NUM_COMPS
      call ESMF_ClockDestroy(clocks(GCOMP_SIDX+i-1), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do
    call ESMF_ClockDestroy(clocks(CPL_IDX), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CalendarDestroy(gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,NUM_COMPS
      call ESMF_GridCompDestroy(comps(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do
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
    print *, "System Test MapperSimpleTwoConcurrentComps complete."
    rc = localrc

    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test MapperSimpleTwoConcurrentComps: Array Concurrent Components"

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

    end program MapperSimpleTwoConcurrentComps

!\end{verbatim}

