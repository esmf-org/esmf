! $Id$
!
! System test CompCreateTwoConcurrentWithResNeg
!  Description on Sourceforge under System Test #63029

!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test CompCreateTwoConcurrentWithResNeg.
!
!
!\begin{verbatim}

    program CompCreateTwoConcurrentWithResNeg

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    use user_model1, only : user_register1 => user_register,&
                            user_setvm1 => user_setvm
    use user_model2, only : user_register2 => user_register,&
                            user_setvm2 => user_setvm
    use user_neg_info
    use user_neg, only : user_neg_routine

    implicit none

!   Local variables
    integer, parameter :: NUM_COMPONENTS = 2
    integer, parameter :: NUM_PHASES = 2
    integer :: my_pet, npets, rc, userrc
    integer, dimension(:), allocatable :: petlist1, petlist2
    integer :: npetlist1, npetlist2
    type(ESMF_VM):: vm
    !type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_GridComp) :: comps(NUM_COMPONENTS)
    !type(ESMF_State) :: imp1, imp2, exp1, exp2
    type(ESMF_State) :: imps(NUM_COMPONENTS), exps(NUM_COMPONENTS)
    !character(len=ESMF_MAXSTR) :: cname1, cname2
    character(len=ESMF_MAXSTR) :: cnames(NUM_COMPONENTS)
    type(ESMF_SetVMInterfaceType) :: user_setvms(NUM_COMPONENTS)
    
    integer :: i, j
    character(len=ESMF_MAXSTR) :: tmpstr

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test CompCreateTwoConcurrentWithResNeg:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_Initialize(defaultlogfilename="CompCreateTwoConcurrentWithResNegSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the default global VM
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get our pet number for output print statements
    call ESMF_VMGet(vm, petCount=npets, localPet=my_pet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

      ! Check for correct number of PETs
    if ( npets < 2 ) then
      print *, "This system test does not run on fewer than 2 PETs"
      rc = ESMF_RC_ARG_BAD
      goto 10
    endif

    npetlist2 = npets/2
    npetlist1 = npets - npetlist2

    allocate(petlist1(npetlist1))
    allocate(petlist2(npetlist2))

    j = 1
    do i=0,npets-1,2
      petlist1(j) = i 
      j = j + 1
    end do
    j = 1
    do i=1,npets-1,2
      petlist2(j) = i 
      j = j + 1
    end do

    print *, "PET list comp 1 : ", petlist1
    print *, "PET list comp 2 : ", petlist2
    cnames(1) = "System Test CompCreateTwoConcurrentWithResNeg Comp 1"
    comps(1) = ESMF_GridCompCreate(name=cnames(1), petList=petlist1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comps(1))

    print *, "Comp Create finished, name = ", trim(cnames(1))

    cnames(2) = "System Test CompCreateTwoConcurrentWithResNeg Comp 2"
    comps(2) = ESMF_GridCompCreate(name=cnames(2), petList=petlist2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompPrint(comps(2))

    print *, "Comp Create finished, name = ", trim(cnames(2))

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  User negotiation section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    user_setvms(1)%pfunc => user_setvm1
    user_setvms(2)%pfunc => user_setvm2
    call user_neg_routine(vm, comps, user_setvms, rc)
    if(rc /= ESMF_SUCCESS) then
      print *, "user negotiation failed"
    end if
    print *, "User negotiation finished for all components"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    call ESMF_GridCompSetServices(comps(1), userRoutine=user_register1, &
    userRc=userrc, rc=rc)
    if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
    print *, "Comp Register finished (comp 1), rc= ", rc
    call ESMF_GridCompSetServices(comps(2), userRoutine=user_register2, &
    userRc=userrc, rc=rc)
    if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
    print *, "Comp Register finished (comp 2), rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do i=1,NUM_COMPONENTS
      write(tmpstr, *) "igrid import state Comp", i 
      imps(i) = ESMF_StateCreate(name=tmpstr,  &
                                  stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      write(tmpstr, *) "igrid export state Comp", i 
      exps(i) = ESMF_StateCreate(name=tmpstr,  &
                                  stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
    end do
    do i=1,NUM_COMPONENTS
      do j=1,NUM_PHASES
        call ESMF_GridCompInitialize(comps(i), importState=imps(i),&
                                      exportState=exps(i), phase=j,&
                                      userRc=userrc, rc=rc)
        if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
        print *, "Comp Initialize 1 finished : Comp ", i, " : Phase ", j
      end do
    end do

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do i=1,NUM_PHASES
      do j=1,NUM_COMPONENTS
        call ESMF_GridCompRun(comps(j), importState=imps(j), &
                              exportState=exps(j), userRc=userrc, rc=rc)
        if ((rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS)) goto 10
        print *, "Comp Run succeeded : Comp ", j, " : Phase ", i
      end do
    end do

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result
      do i=1,NUM_COMPONENTS
        call ESMF_GridCompFinalize(comps(i), importState=imps(i), &
          exportState=exps(i), userRc=userrc, rc=rc)
        if ( (rc .ne. ESMF_SUCCESS) .or. (userrc .ne. ESMF_SUCCESS) ) goto 10
      end do

      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, my_pet = ", my_pet
      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      do i=1,NUM_COMPONENTS
        call ESMF_GridCompDestroy(comps(i), rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        call ESMF_StateDestroy(imps(i), rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        call ESMF_StateDestroy(exps(i), rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
      end do

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

 10   print *, "System Test CompCreateTwoConcurrentWithResNeg complete"

      ! Standard ESMF Test output to log file
      write(failMsg, *) "System Test failure"
      write(testname, *) "System Test CompCreateTwoConcurrentWithResNeg: Component Create Test"

      if (rc .ne. ESMF_SUCCESS) then
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: Component Create complete."
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

      end program CompCreateTwoConcurrentWithResNeg

!\end{verbatim}

